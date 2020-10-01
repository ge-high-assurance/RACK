"""RACK Software ontology binding"""

from enum import Enum
from rdflib import Namespace, Graph, URIRef, Literal

NS = Namespace("http://arcos.rack/SOFTWARE#")

class ComponentType(Enum):
    """Software component types"""
    SOURCE_FUNCTION = "SOURCE_FUNCTION"
    MODULE = "MODULE"

class Thing:
    def __init__(self, uri):
        self.node = URIRef(uri)

class Component(Thing):
    """Software components"""

    def __init__(self, uri, name, ty: ComponentType):
        super().__init__(uri)
        self.name = name
        self.component_type = ty
        self.defined_in = None
        self.mentions = []
        self.parents = []

    def add_mention(self, component):
        self.mentions.append(component)

    def add_parent(self, parent):
        self.parents.append(parent)

    def add_to_graph(self, graph: Graph):
        """Serialize the component into an RDF graph"""
        graph.bind("sw", NS)

        graph.add((self.node, NS.COMPONENT_TYPE, NS[self.component_type]))
        graph.add((self.node, NS.name, Literal(self.name)))

        if self.defined_in is not None:
            graph.add((self.node, NS.defined_in, self.defined_in.node))

        for component in self.mentions:
            graph.add((self.node, NS.mentions, component.node))

        for parent in self.parents:
            graph.add((self.node, NS.subcomponentOf, parent.node))
