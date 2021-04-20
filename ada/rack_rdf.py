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

txt_format = ontology.Format(ontology.FormatIdentifier("txt"), FORMAT.TEXT_FILE)

my_file = ontology.File(
    identifier="src.txt",
    uri=DATA["src.txt"],
    name="src.txt",
    format_=txt_format,
)

c1 = ontology.SoftwareComponent(
    component_type=ontology.SOURCE_FUNCTION,
    identifier=ontology.SoftwareComponentIdentifier("fun1"),
    title="fun1",
    uri=DATA["fun1"],
)
c1.defined_in = my_file

c2 = ontology.SoftwareComponent(
    component_type=ontology.SOURCE_FUNCTION,
    identifier=ontology.SoftwareComponentIdentifier("fun2"),
    title="fun2",
    uri=DATA["fun2"],
)
c2.defined_in = my_file
c2.add_mention(c1)

txt_format.add_to_graph(g)
my_file.add_to_graph(g)
c1.add_to_graph(g)
c2.add_to_graph(g)

sys.stdout.buffer.write(g.serialize(format="turtle"))
