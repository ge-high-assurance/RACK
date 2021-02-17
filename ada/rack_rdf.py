#!/usr/bin/env python

"""Demonstration driver"""

import sys
from rdflib import Graph, Namespace

import ontology

g = Graph()
FORMAT = Namespace("http://data/format#")
DATA = Namespace("http://data/")

g.bind("dat", DATA)
g.bind("format", FORMAT)

txt_format = ontology.FileFormat("txt", FORMAT.TEXT_FILE)

my_file = ontology.File("src.txt", DATA["src.txt"], "src.txt", txt_format)

c1 = ontology.SoftwareComponent(
    "fun1",
    DATA["fun1"],
    "fun1",
    ontology.ComponentType.SOURCE_FUNCTION
)
c1.defined_in = my_file

c2 = ontology.SoftwareComponent(
    "fun2",
    DATA["fun2"],
    "fun2",
    ontology.ComponentType.SOURCE_FUNCTION
)
c2.defined_in = my_file
c2.add_mention(c1)

txt_format.add_to_graph(g)
my_file.add_to_graph(g)
c1.add_to_graph(g)
c2.add_to_graph(g)

sys.stdout.buffer.write(g.serialize(format="turtle"))
