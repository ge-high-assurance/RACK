"""RACK Software ontology binding"""

from enum import Enum
from typing import List, Optional
from rdflib import Graph, Literal, Namespace, URIRef

NS = Namespace("http://arcos.rack/SOFTWARE#")
RDF = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")

class Thing:
    def __init__(self, uri):
        self.node = URIRef(uri)
        self.rdf_type = None

    def add_to_graph(self, graph):
        graph.bind("sw", NS)
        graph.add((self.node, RDF.type, self.rdf_type))

class FileFormat(Thing):
    def __init__(self, uri):
        super().__init__(uri)
        self.rdf_type = NS.FORMAT

class File(Thing):
    """File entity"""

    def __init__(self, uri, filename, file_format):
        super().__init__(uri)
        self.rdf_type = NS.FILE
        self.filename = filename
        self.file_format = file_format

    def add_to_graph(self, graph):
        super().add_to_graph(graph)
        graph.add((self.node, NS.filename, Literal(self.filename)))
        graph.add((self.node, NS.fileFormat, Literal(self.file_format.node)))

class ComponentType(Enum):
    """Software component types"""
    SOURCE_FUNCTION = "SOURCE_FUNCTION"
    MODULE = "MODULE"

class Component(Thing):
    """Software components"""

    def __init__(self, uri, name, ty: ComponentType):
        super().__init__(uri)
        self.rdf_type = NS.COMPONENT
        self.name = name
        self.component_type = ty
        self.defined_in: Optional[File] = None
        self.mentions: List[Component] = []
        self.parents: List[Component] = []

    def add_mention(self, component):
        self.mentions.append(component)

    def add_parent(self, parent):
        self.parents.append(parent)

    def add_to_graph(self, graph: Graph):
        """Serialize the component into an RDF graph"""
        super().add_to_graph(graph)

        graph.add((self.node, NS.COMPONENT_TYPE, NS[self.component_type.value]))
        graph.add((self.node, NS.name, Literal(self.name)))

        if self.defined_in is not None:
            graph.add((self.node, NS.defined_in, self.defined_in.node))

        for component in self.mentions:
            graph.add((self.node, NS.mentions, component.node))

        for parent in self.parents:
            graph.add((self.node, NS.subcomponentOf, parent.node))
