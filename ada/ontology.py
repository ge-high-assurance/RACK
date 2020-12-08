"""RACK Software ontology binding"""

from __future__ import annotations

__copyright__ = "Copyright (c) 2020, Galois, Inc."

from enum import Enum
from typing import List, Optional
from rdflib import Graph, Literal, Namespace, URIRef

NS = Namespace("http://arcos.rack/SOFTWARE#")
RDF = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")

class Thing:
    """RACK-Ontology Thing class"""
    def __init__(self, uri: str, rdf_type: URIRef) -> None:
        self.node: URIRef = URIRef(uri)
        self.rdf_type: URIRef = rdf_type

    def add_to_graph(self, graph: Graph) -> None:
        """Adds this class to the graph received in argument."""
        graph.bind("sw", NS)
        graph.add((self.node, RDF.type, self.rdf_type))

class FileFormat(Thing):
    """RACK-Ontology FileFormat class"""
    def __init__(self, uri: str) -> None:
        super().__init__(uri, NS.FORMAT)

class File(Thing):
    """File entity"""

    def __init__(self, uri: str, filename: str, file_format: FileFormat) -> None:
        super().__init__(uri, NS.FILE)
        self.filename: str = filename
        self.file_format: FileFormat = file_format

    def add_to_graph(self, graph: Graph) -> None:
        super().add_to_graph(graph)
        graph.add((self.node, NS.filename, Literal(self.filename)))
        graph.add((self.node, NS.fileFormat, Literal(self.file_format.node)))

class ComponentType(Enum):
    """Software component types"""
    SOURCE_FUNCTION = "SOURCE_FUNCTION"
    MODULE = "MODULE"

class Component(Thing):
    """Software components"""

    def __init__(self, uri: str, name: str, ty: ComponentType) -> None:
        super().__init__(uri, NS.COMPONENT)
        self.name = name
        self.component_type = ty
        self.defined_in: Optional[File] = None
        self.mentions: List[Component] = []
        self.parents: List[Component] = []

    def add_mention(self, component: Component) -> None:
        """Registers a component that this component mentions."""
        self.mentions.append(component)

    def add_parent(self, parent: Component) -> None:
        """Registers a component that is a parent of this component."""
        self.parents.append(parent)

    def add_to_graph(self, graph: Graph) -> None:
        """Serialize the component into an RDF graph"""
        super().add_to_graph(graph)

        graph.add((self.node, NS.COMPONENT_TYPE, NS[self.component_type.value]))
        graph.add((self.node, NS.name, Literal(self.name)))

        if self.defined_in is not None:
            graph.add((self.node, NS.definedIn, self.defined_in.node))

        for component in self.mentions:
            graph.add((self.node, NS.mentions, component.node))

        for parent in self.parents:
            graph.add((self.node, NS.subcomponentOf, parent.node))
