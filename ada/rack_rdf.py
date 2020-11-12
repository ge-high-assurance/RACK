#!/usr/bin/env python

"""Demonstration driver"""

import sys
from rdflib import Graph, Namespace
from ontology import Component, ComponentType, File, FileFormat

g = Graph()
FORMAT = Namespace("http://data/format#")
DATA = Namespace("http://data/")

g.bind("dat", DATA)
g.bind("format", FORMAT)

txt_format = FileFormat(FORMAT.TEXT_FILE)

my_file = File(DATA["src.txt"], "src.txt", txt_format)

c1 = Component(DATA["fun1"], "fun1", ComponentType.SOURCE_FUNCTION)
c1.defined_in = my_file

c2 = Component(DATA["fun2"], "fun2", ComponentType.SOURCE_FUNCTION)
c2.defined_in = my_file
c2.add_mention(c1)

txt_format.add_to_graph(g)
my_file.add_to_graph(g)
c1.add_to_graph(g)
c2.add_to_graph(g)

sys.stdout.buffer.write(g.serialize(format="turtle"))
