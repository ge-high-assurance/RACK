#!/usr/bin/env python

"""
Main module for the Ada analysis.

Runs the analysis and displays the results in Turtle format.
"""

__copyright__ = "Copyright (c) 2020, Galois, Inc."

import argparse
import logging
import os
from typing import Dict, List, Optional
import sys

from colorama import Fore, Style

import libadalang as lal
from rdflib import Graph, Namespace

from ada_print_visitor import AdaPrintVisitor
import ontology
import static_call_graph as SCG

# In order to do resolution at call sites, the analysis needs to resolve
# packages to files that contain their spec/implementation.  It can do so with a
# "unit provider", which handles such logic.  The `lal.UnitProvider.for_project`
# contains a default implementation for projects that have a GPR project file.

parser = argparse.ArgumentParser()

parser.add_argument("--gpr", help="Project file (.gpr)", type=str)

parser.add_argument(
    "--files",
    help="Text file containing a list of project files, one per line",
    type=str
)

parser.add_argument("--analyze", help="File to analyze (.ada, .adb, .ads)", type=str, required=True)

parser.add_argument(
    "others",
    help="List of project files (use --project-files if there are many)",
    type=str,
    nargs="*"
)

args = parser.parse_args()

class CustomFormatter(logging.Formatter):
    """Add custom styles to our log"""

    format_string = "[%(filename)s:%(lineno)d] %(levelname)s: %(message)s"

    FORMATS = {
        logging.DEBUG: format_string,
        logging.INFO: f"{Fore.CYAN}{format_string}{Style.RESET_ALL}",
        logging.WARNING: f"{Fore.YELLOW}{format_string}{Style.RESET_ALL}",
        logging.ERROR: f"{Fore.YELLOW}{format_string}{Style.RESET_ALL}",
        logging.CRITICAL: f"{Style.BRIGHT}{Fore.YELLOW}{format_string}{Style.RESET_ALL}",
    }

    def format(self, record: logging.LogRecord) -> str:
        which_format = self.FORMATS.get(record.levelno)
        formatter = logging.Formatter(which_format, "%H:%M:%S")
        return formatter.format(record)

# Default provider just looks through files listed as "others"
provider = lal.UnitProvider.auto(input_files=args.others)

def input_files_from_files_list(files_file: str) -> List[str]:
    """
    Takes as input a path to a file containing a list of filepaths (one per
    line, empty lines are ignored). Returns the list of filepaths, all
    prefixed by the path to that listing file (so that the paths in that list file
    may be relative to the list file location).
    """
    root = os.path.dirname(files_file)
    with open(files_file) as handle:
        lines = [f"{root}/{file}" for file in list(filter(None, handle.read().splitlines()))]
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

DEBUG = False

FILE = Namespace("http://data/file#")
FORMAT = Namespace("http://data/format#")
SOFTWARE_COMPONENT = Namespace("http://data/software-component#")

ada_format = ontology.FileFormat("Ada", FORMAT.ADA_FILE)

def register_component(components, component: SCG.GraphNode) -> None:
    """
    Makes sure that the component is already present in the components
    dictionary.  Adds it if necessary.
    """
    key = SCG.node_key(component)
    uri = SCG.get_uri(component)
    name = SCG.get_name(component)
    components[key] = ontology.SoftwareComponent(
        identifier=uri,
        uri=SOFTWARE_COMPONENT[uri],
        title=name,
        ty=ontology.ComponentType.SOURCE_FUNCTION
    )

def register_ada_file(
    files: Dict[str, ontology.File],
    file_key: Optional[str]
) -> Optional[ontology.File]:
    """
    Creates an ontology node corresponding to 'file_key', unless it already
    exists, and stores it in 'files'.

    Returns the existing or newly-created node.
    """
    if file_key is None:
        return None
    if file_key not in files:
        files[file_key] = ontology.File(
            identifier=file_key,
            uri=FILE[file_key],
            filename=file_key,
            file_format=ada_format
        )
    return files[file_key]

def analyze_unit(unit: lal.AnalysisUnit) -> None:
    """Computes and displays the static call graph of some unit."""
    if unit.root:
        if DEBUG:
            ada_visitor = AdaPrintVisitor(max_depth=20)
            ada_visitor.visit(unit.root)
        static_call_graph_visitor = SCG.StaticCallGraphVisitor(
            context=context,
            caller_being_defined=None,
            edges=dict(),
            nodes=dict()
        )

        static_call_graph_visitor.visit(unit.root)

        components: Dict[str, ontology.SoftwareComponent] = dict()
        files: Dict[str, ontology.File] = dict()

        # register all files and components (so as to have one unique instance
        # for each)
        for component_key in static_call_graph_visitor.nodes:
            component = static_call_graph_visitor.nodes[component_key]
            register_component(components, component)
            file = register_ada_file(files, SCG.get_definition_file(component))
            components[component_key].defined_in = file

        # add "mentions" to all components that mention other components
        for caller in static_call_graph_visitor.edges:
            for callee_key in static_call_graph_visitor.edges[caller]:
                components[caller].add_mention(components[callee_key])

        graph = Graph()
        graph.bind("data:file", FILE)
        graph.bind("data:format", FORMAT)
        graph.bind("data:software-component", SOFTWARE_COMPONENT)
        ada_format.add_to_graph(graph)

        for component_key in components:
            components[component_key].add_to_graph(graph)

        for file_key in files:
            files[file_key].add_to_graph(graph)

        sys.stdout.buffer.write(graph.serialize(format="turtle"))

    else:
        print("No root found, diagnostics:")
        print(unit.diagnostics)

# Register our custom color formatter for our logger
logger = logging.getLogger('ada')
stream_handler = logging.StreamHandler()
stream_handler.setFormatter(CustomFormatter())
logger.propagate = False
logger.addHandler(stream_handler)

file_to_analyze = args.analyze

analyze_unit(context.get_from_file(file_to_analyze))
