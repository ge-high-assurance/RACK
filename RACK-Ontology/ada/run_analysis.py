#!/usr/bin/env python

import argparse
import libadalang as lal
import os
from typing import Dict
import sys

from ada_print_visitor import AdaPrintVisitor
from ontology import Component, ComponentType, File, FileFormat
from rdflib import Graph, Namespace
import static_call_graph as SCG

# In order to do resolution at call sites, the analysis needs to resolve
# packages to files that contain their spec/implementation.  It can do so with a
# "unit provider", which handles such logic.  The `lal.UnitProvider.for_project`
# contains a default implementation for projects that have a GPR project file.

parser = argparse.ArgumentParser()
parser.add_argument("--project", "-P", help="Project file (.gpr)", type=str)
parser.add_argument("files", help="Files to analyze", type=str, nargs="+")
args = parser.parse_args()

provider = (
    lal.UnitProvider.for_project(args.project)
    if args.project
    else lal.UnitProvider.auto(input_files = args.files)
)

context = lal.AnalysisContext(unit_provider=provider)

debug = False

g = Graph()
FORMAT = Namespace("http://data/format#")
DATA = Namespace("http://data/")

ada_format = FileFormat(FORMAT.ADA_FILE)

def ensure_registered(components, component: SCG.GraphNode):
    if isinstance(component, SCG.ToplevelNode):
        key = component.absolute_file_path
        if key in components:
            return
        components[key] = File(DATA[key], os.path.basename(key), ada_format)
        return
    if isinstance(component, SCG.CallableNode):
        node = component.node
        key = SCG.node_key(node)
        if key in components:
            return
        components[key] = Component(DATA[key], key, ComponentType.SOURCE_FUNCTION)
        return
    raise Exception("Encountered a graph component of an unexpected class!")

for file in args.files:
    # print(f"Analyzing {file}")
    unit = context.get_from_file(file)
    if unit.root:
        if debug:
            adaVisitor = AdaPrintVisitor(max_depth = 20)
            adaVisitor.visit(unit.root)
        staticCallGraphVisitor = SCG.StaticCallGraphVisitor(
            absolute_file_path = unit.filename,
            callable_being_defined = None
        )

        staticCallGraphVisitor.visit(unit.root)

        components: Dict[str, Component] = dict()
        for component_key in staticCallGraphVisitor.nodes:
            component: SCG.GraphNode = staticCallGraphVisitor.nodes[component_key]
            ensure_registered(components, component)
        for caller in staticCallGraphVisitor.edges:
            for callee_key in staticCallGraphVisitor.edges[caller]:
                components[caller].add_mention(components[callee_key])
                # print(f"{caller} calls {callee}")

        g = Graph()
        g.bind("dat", DATA)
        g.bind("format", FORMAT)
        ada_format.add_to_graph(g)
        for component_key in components:
            components[component_key].add_to_graph(g)
        sys.stdout.buffer.write(g.serialize(format = "turtle"))

    else:
        print("No root found, diagnostics:")
        print(unit.diagnostics)
