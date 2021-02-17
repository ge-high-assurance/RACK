"""RACK Software ontology binding"""

from __future__ import annotations

__copyright__ = "Copyright (c) 2020, Galois, Inc."

from enum import Enum
from typing import List, Optional
from rdflib import Graph, Literal, Namespace, URIRef

FILE = Namespace("http://arcos.rack/FILE#")
PROV_S = Namespace("http://arcos.rack/PROV-S#")
SOFTWARE = Namespace("http://arcos.rack/SOFTWARE#")
RDF = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")

class Thing:
    """RACK-Ontology PROV-S#Thing"""
    def __init__(self, identifier: str, uri: str, rdf_type: URIRef) -> None:
        # PROV-S#identifier
        self.identifier = identifier
        self.node: URIRef = URIRef(uri)
        self.rdf_type: URIRef = rdf_type

    def add_to_graph(self, graph: Graph) -> None:
        """Adds this class to the graph received in argument."""
        graph.bind("ontology:file", FILE)
        graph.bind("ontology:prov-s", PROV_S)
        graph.bind("ontology:sw", SOFTWARE)
        graph.add((self.node, PROV_S.identifier, Literal(self.identifier)))
        graph.add((self.node, RDF.type, self.rdf_type))

class FileFormat(Thing):
    """RACK-Ontology File#FORMAT"""
    def __init__(self, identifier: str, uri: str) -> None:
        super().__init__(identifier, uri, FILE.FORMAT)

class File(Thing):
    """File entity"""

    def __init__(self, identifier: str, uri: str, filename: str, file_format: FileFormat) -> None:
        super().__init__(identifier, uri, FILE.FILE)
        # FILE#filename
        self.filename: str = filename
        # FILE#fileFormat
        self.file_format: FileFormat = file_format

    def add_to_graph(self, graph: Graph) -> None:
        super().add_to_graph(graph)
        graph.add((self.node, FILE.filename, Literal(self.filename)))
        graph.add((self.node, FILE.fileFormat, Literal(self.file_format.node)))

class Entity(Thing):
    """PROV-S#ENTITY"""
    def __init__(self, identifier: str, uri: str, rdf_type: URIRef) -> None:
        super().__init__(identifier, uri, rdf_type)
        # FILE#definedIn
        self.defined_in: Optional[File] = None

    def add_to_graph(self, graph: Graph) -> None:
        super().add_to_graph(graph)

        if self.defined_in is not None:
            graph.add((self.node, FILE.definedIn, self.defined_in.node))

class ComponentType(Enum):
    """Software component types"""
    SOURCE_FUNCTION = "SOURCE_FUNCTION"
    MODULE = "MODULE"

class SoftwareComponent(Entity):
    """Software components (SOFTWARE#SWCOMPONENT)"""

    def __init__(self, identifier: str, uri: str, title: str, ty: ComponentType) -> None:
        super().__init__(identifier, uri, SOFTWARE.SWCOMPONENT)
        self.title = title
        self.component_type = ty
        self.mentions: List[SoftwareComponent] = []

    def add_mention(self, component: SoftwareComponent) -> None:
        """Registers a component that this component mentions."""
        self.mentions.append(component)

    def add_to_graph(self, graph: Graph) -> None:
        """Serialize the component into an RDF graph"""
        super().add_to_graph(graph)

        graph.add((self.node, SOFTWARE.componentType, SOFTWARE[self.component_type.value]))
        graph.add((self.node, PROV_S.title, Literal(self.title)))

        for component in self.mentions:
            graph.add((self.node, SOFTWARE.mentions, component.node))
