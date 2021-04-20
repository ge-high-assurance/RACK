"""RACK Software ontology binding"""

from __future__ import annotations

__copyright__ = "Copyright (c) 2020, Galois, Inc."

from typing import NewType, Optional, Set
# from typing_extensions import TypedDict
from rdflib import Graph, Literal, Namespace, URIRef

ComponentTypeIdentifier = NewType("ComponentTypeIdentifier", str)
FileIdentifier = NewType("FileIdentifier", str)
FormatIdentifier = NewType("FormatIdentifier", str)
SoftwareComponentIdentifier = NewType("SoftwareComponentIdentifier", str)

MODULE = ComponentTypeIdentifier("Module")
SOURCE_FUNCTION = ComponentTypeIdentifier("SourceFunction")

# SoftwareComponent = TypedDict("SoftwareComponent", {
#     "mentions": Set[SoftwareComponentIdentifier],
#     "title": str,
#     "type": ComponentTypeIdentifier,
# })

# File = TypedDict("File", {
#     "format": FormatIdentifier,
#     "name": str,
# })

FILE_NS = Namespace("http://arcos.rack/FILE#")
PROV_S_NS = Namespace("http://arcos.rack/PROV-S#")
SOFTWARE_NS = Namespace("http://arcos.rack/SOFTWARE#")
RDF_NS = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")

class Thing:
    """RACK-Ontology PROV-S#Thing"""
    def __init__(
        self,
        identifier: str,
        uri: str,
        rdf_type: URIRef
    ) -> None:
        # PROV-S#identifier
        self.identifier = identifier
        self.node: URIRef = URIRef(uri)
        self.rdf_type: URIRef = rdf_type

    def add_to_graph(self, graph: Graph) -> None:
        """Adds this class to the graph received in argument."""
        graph.bind("ontology:file", FILE_NS)
        graph.bind("ontology:prov-s", PROV_S_NS)
        graph.bind("ontology:sw", SOFTWARE_NS)
        graph.add((self.node, PROV_S_NS.identifier, Literal(self.identifier)))
        graph.add((self.node, RDF_NS.type, self.rdf_type))

class Format(Thing):
    """RACK-Ontology File#FORMAT"""
    def __init__(self, identifier: FormatIdentifier, uri: str) -> None:
        super().__init__(identifier, uri, FILE_NS.FORMAT)

    def get_identifier(self) -> FormatIdentifier:
        """Accessor to the identifier field with type FormatIdentifier."""
        return FormatIdentifier(self.identifier)

class File(Thing):
    """File entity"""

    def __init__(
        self,
        identifier: str,
        uri: str,
        name: str,
        format_: Format,
    ) -> None:
        super().__init__(identifier, uri, FILE_NS.FILE)
        # FILE#filename
        self.name: str = name
        # FILE#fileFormat
        self.format: Format = format_

    def add_to_graph(self, graph: Graph) -> None:
        super().add_to_graph(graph)
        graph.add((self.node, FILE_NS.filename, Literal(self.name)))
        graph.add((self.node, FILE_NS.fileFormat, Literal(self.format.identifier)))

class Entity(Thing):
    """PROV-S#ENTITY"""
    def __init__(self, identifier: str, uri: str, rdf_type: URIRef) -> None:
        super().__init__(identifier, uri, rdf_type)
        # FILE#definedIn
        self.defined_in: Optional[File] = None

    def add_to_graph(self, graph: Graph) -> None:
        super().add_to_graph(graph)

        if self.defined_in is not None:
            graph.add((self.node, FILE_NS.definedIn, self.defined_in.node))

class SoftwareComponent(Entity):
    """Software components (SOFTWARE#SWCOMPONENT)"""

    def __init__(
        self,
        component_type: ComponentTypeIdentifier,
        identifier: SoftwareComponentIdentifier,
        title: str,
        uri: str,
    ) -> None:
        super().__init__(identifier, uri, SOFTWARE_NS.SWCOMPONENT)
        self.component_type = component_type
        self.mentions: Set[SoftwareComponent] = set()
        self.title = title
        self.uri = uri

    def add_mention(self, component: SoftwareComponent) -> None:
        """Registers a component that this component mentions."""
        self.mentions.add(component)

    def add_to_graph(self, graph: Graph) -> None:
        """Serialize the component into an RDF graph."""

        super().add_to_graph(graph)

        graph.add((self.node, SOFTWARE_NS.componentType, SOFTWARE_NS[self.component_type]))
        graph.add((self.node, PROV_S_NS.title, Literal(self.title)))

        for callee in self.mentions:
            graph.add((self.node, SOFTWARE_NS.mentions, callee.uri))

    def get_identifier(self) -> SoftwareComponentIdentifier:
        """Accessor to the identifier field with type SoftwareComponentIdentifier."""
        return SoftwareComponentIdentifier(self.identifier)
