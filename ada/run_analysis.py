#!/usr/bin/env python

import argparse
import libadalang as lal
import os
from typing import Dict
import sys

from ada_print_visitor import AdaPrintVisitor
from ontology import Component, ComponentType, File, FileFormat
from rdflib import Graph, Namespace
from typing import List
import static_call_graph as SCG

# In order to do resolution at call sites, the analysis needs to resolve
# packages to files that contain their spec/implementation.  It can do so with a
# "unit provider", which handles such logic.  The `lal.UnitProvider.for_project`
# contains a default implementation for projects that have a GPR project file.

parser = argparse.ArgumentParser()
parser.add_argument("--gpr",     help="Project file (.gpr)", type=str)
parser.add_argument("--files",   help="Text file containing a list of project files, one per line", type=str)
parser.add_argument("--analyze", help="File to analyze (.ada, .adb, .ads)", type=str, required=True)
parser.add_argument("others",    help="List of project files (use --project-files if there are many)", type=str, nargs="*")
args = parser.parse_args()

# Default provider just looks through files listed as "others"
provider = lal.UnitProvider.auto(input_files=args.others)

def input_files_from_files_list(files_list: str) -> List[str]:
    """Returns the list of all non-empty lines from a given file."""
    with open(files_list) as f:
        lines = list(filter(None, f.read().splitlines()))
        return lines

# If a gpr project file is passed, it is used.
# If not, and a file is passed that lists project files, it is used.
# Otherwise, we just use the files listed on the remainder of the command line.
provider = (
    lal.UnitProvider.for_project(args.gpr)
    if args.gpr
    else
    lal.UnitProvider.auto(input_files=input_files_from_files_list(args.files))
    if args.files
    else
    lal.UnitProvider.auto(input_files=args.others)
)

context = lal.AnalysisContext(unit_provider=provider)

debug = False

g = Graph()
FORMAT = Namespace("http://data/format#")
DATA = Namespace("http://data/")

ada_format = FileFormat(FORMAT.ADA_FILE)

def register_component(components, component: SCG.GraphNode) -> None:
    """
    Makes sure that the component is already present in the components
    dictionary.  Adds it if necessary.
    """
    key = component.get_key()
    uri = component.get_uri()
    name = component.get_name()
    # TODO: check what we know about the actual component type
    components[key] = Component(DATA[uri], name, ComponentType.SOURCE_FUNCTION)

file_to_analyze = args.analyze

unit = context.get_from_file(file_to_analyze)
if unit.root:
    if debug:
        adaVisitor = AdaPrintVisitor(max_depth=20)
        adaVisitor.visit(unit.root)
    staticCallGraphVisitor = SCG.StaticCallGraphVisitor(
        caller_being_defined = SCG.ToplevelNode(unit.filename)
    )

    staticCallGraphVisitor.visit(unit.root)

    components: Dict[str, Component] = dict()

    # register all components
    for component_key in staticCallGraphVisitor.nodes:
        register_component(components, staticCallGraphVisitor.nodes[component_key])

    # add "mentions" to all components that mention other components
    for caller in staticCallGraphVisitor.edges:
        for callee_key in staticCallGraphVisitor.edges[caller]:
            components[caller].add_mention(components[callee_key])

    g = Graph()
    g.bind("dat", DATA)
    g.bind("format", FORMAT)
    ada_format.add_to_graph(g)
    for component_key in components:
        components[component_key].add_to_graph(g)
    sys.stdout.buffer.write(g.serialize(format="turtle"))

else:
    print("No root found, diagnostics:")
    print(unit.diagnostics)
