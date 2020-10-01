"""Demonstration driver"""
import sys
from rdflib import Graph, URIRef
from ontology import Thing, Component, ComponentType

g = Graph()
g.bind("dat", URIRef("http://data/"))

my_file = Thing("http://data/src.txt")

c1 = Component("http://data/fun1", "fun1", ComponentType.SOURCE_FUNCTION)
c1.defined_in = my_file

c2 = Component("http://data/fun2", "fun2", ComponentType.SOURCE_FUNCTION)
c2.defined_in = my_file
c2.add_mention(c1)

c1.add_to_graph(g)
c2.add_to_graph(g)

sys.stdout.buffer.write(g.serialize(format="turtle"))
