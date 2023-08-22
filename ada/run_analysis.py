#!/usr/bin/env python3

"""
Main module for the Ada analysis.

Runs the analysis and displays the results in Turtle format.
"""

__copyright__ = "Copyright (c) 2020, Galois, Inc."

import argparse
import logging
import os
import sys
from xml.sax.saxutils import escape

from typing import Dict, List, Optional, Set
from typing_extensions import TypedDict

import Evidence
import Evidence.Add
from colorama import Fore, Style
import libadalang as lal
from rdflib import Graph, Namespace

from ada_print_visitor import AdaPrintVisitor
import ontology
import static_call_graph as SCG
import traceability_extraction as TE
import package_structure as PS
from node_naming import GraphNode, get_node_identifier, get_node_uri, get_node_file

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

parser.add_argument("analyze", help="File to analyze (.ada, .adb, .ads)", nargs='*')

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
)

context = lal.AnalysisContext(unit_provider=provider)

# This structure captures the output of the analysis in a structured way that
# can be passed to different backends to output in different formats.
AnalysisOutput = TypedDict(
    "AnalysisOutput",
    {
        "component_types": Set[ontology.ComponentTypeIdentifier],
        "components": Dict[
            ontology.SoftwareComponentIdentifier,
            ontology.SoftwareComponent
        ],
        "files": Dict[ontology.FileIdentifier, ontology.File],
        "formats": Set[ontology.Format],
    }
)

FILE_NS = Namespace("http://data/file#")
FORMAT_NS = Namespace("http://data/format#")
SOFTWARE_COMPONENT_NS = Namespace("http://data/software-component#")

DEBUG = False

DEBUG_TURTLE = False

ADA_FORMAT = ontology.Format(
    identifier=ontology.FormatIdentifier("Ada"),
    uri=FORMAT_NS["ADA_FILE"],
)

def register_component(
    analysis_output: AnalysisOutput,
    component: GraphNode,
    component_type: ontology.ComponentTypeIdentifier,
) -> ontology.SoftwareComponent:
    """
    Makes sure that the component is already present in the components
    dictionary.  Adds it if necessary.
    """
    components = analysis_output["components"]
    component_identifier = ontology.SoftwareComponentIdentifier(get_node_identifier(component))
    if component_identifier not in components:
        components[component_identifier] = ontology.SoftwareComponent(
            identifier=component_identifier,
            # does not work because display names may contain special characters
            # title=SCG.get_node_display_name(component),
            # using this instead even though less ideal:
            title=component.doc_name,
            component_type=ontology.SOURCE_FUNCTION,
            uri=SOFTWARE_COMPONENT_NS[component_identifier],
        )
    return components[component_identifier]

def register_ada_file(
    analysis_output: AnalysisOutput,
    file_identifier: Optional[ontology.FileIdentifier],
) -> Optional[ontology.File]:
    """
    Creates an entry corresponding to 'file_key', unless it already exists,
    and stores it in "files".
    """
    if file_identifier is None:
        return None
    files = analysis_output["files"]
    if file_identifier not in files:
        files[file_identifier] = ontology.File(
            format_=ADA_FORMAT,
            identifier=file_identifier,
            name=file_identifier,
            uri=FILE_NS[file_identifier],
        )
    return files[file_identifier]

def as_optional_file_identifier(
    filename: Optional[str]
) -> Optional[ontology.FileIdentifier]:
    """Applies the FileIdentifier newtype within an Optional."""
    if filename is None:
        return None
    return ontology.FileIdentifier(filename)

def make_empty_analysis_output() -> AnalysisOutput:
    """Creates an empty output, to be populated by mutation."""
    return AnalysisOutput({
        "component_types": set(),
        "components": dict(),
        "files": dict(),
        "formats": set(),
    })

def output_as_turtle(
    analysis_output: AnalysisOutput,
) -> None:
    """
    Outputs the analysis results as Turtle, currently to stdout but could be
    made to output in a file.
    """

    graph = Graph()

    graph.bind("data:file", FILE_NS)
    graph.bind("data:format", FORMAT_NS)
    graph.bind("data:software-component", SOFTWARE_COMPONENT_NS)

    for format_ in analysis_output["formats"]:
        format_.add_to_graph(graph)

    for file_key in analysis_output["files"]:
        file = analysis_output["files"][file_key]
        file.add_to_graph(graph)

    for component_key in analysis_output["components"]:
        component = analysis_output["components"][component_key]
        component.add_to_graph(graph)

    sys.stdout.buffer.write(graph.serialize(format="turtle"))

def output_using_scrapingtoolkit(
    analysis_output: AnalysisOutput,
) -> None:
    """Outputs the analysis output using ScrapingToolKit."""

    for component_type in analysis_output["component_types"]:
        Evidence.Add.SOFTWARE.SWCOMPONENT_TYPE(
            identifier=component_type,
        )

    for format_ in analysis_output["formats"]:
        Evidence.Add.FILE.FORMAT(
            identifier=format_.identifier,
        )

    files = analysis_output["files"]
    for file_identifier in files:
        file: ontology.File = files[file_identifier]
        Evidence.Add.FILE.FILE(
            fileFormat_identifier=format_.identifier,
            filename=file.name,
            identifier=file_identifier,
        )

    components = analysis_output["components"]
    for component_identifier in components:
        component: ontology.SoftwareComponent = components[component_identifier]
        Evidence.Add.SOFTWARE.SWCOMPONENT(
            identifier=component_identifier,
            componentType_identifier=component.component_type,
            title=escape(component.title),
            definedIn_identifier=component.defined_in.identifier if component.defined_in else None
        )
        for callee in component.mentions:
            Evidence.Add.SOFTWARE.SWCOMPONENT(
                identifier=component_identifier,
                mentions_identifier=callee.identifier,
            )

def analyze_traceability(unit: lal.AnalysisUnit) -> None:
    """Extracts traceability identifiers from subprograms."""

    if not unit.root:
        return

    visitor = TE.TraceabilityExtraction(
        context=context,
    )

    visitor.visit(unit.root)
    analysis_output = visitor.traceability

    for component_id, requirement_ids in analysis_output.items():
        for requirement_id in requirement_ids:
            Evidence.Add.REQUIREMENTS.REQUIREMENT(
                identifier=requirement_id
            )
            Evidence.Add.SOFTWARE.SWCOMPONENT(
                identifier=component_id,
                wasImpactedBy_identifier=requirement_id,
            )

def analyze_structure(unit: lal.AnalysisUnit) -> None:
    """Extracts traceability identifiers from subprograms."""

    if not unit.root:
        return
        
    visitor = PS.StructureExtractor(
    )

    visitor.visit(unit.root)
    analysis_output = visitor.packages

    for package, components in analysis_output.items():
        filename = package.p_relative_name.unit.filename.rsplit('/', 1)[-1]
        Evidence.Add.SOFTWARE.SWCOMPONENT(
            identifier=get_node_identifier(package),
            title=escape(package.doc_name),
            componentType_identifier=ontology.MODULE,
            definedIn_identifier=filename,
        )
        for component in components:
            Evidence.Add.SOFTWARE.SWCOMPONENT(
                identifier=get_node_identifier(component),
                partOf_identifier=get_node_identifier(package),
            )

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

        analysis_output = make_empty_analysis_output()

        component_types = analysis_output["component_types"]
        component_types.add(ontology.SOURCE_FUNCTION)
        component_types.add(ontology.MODULE)

        formats = analysis_output["formats"]
        formats.add(ADA_FORMAT)

        components = analysis_output["components"]

        # register all components and the files they live in
        for component_key in static_call_graph_visitor.nodes:
            component_node = static_call_graph_visitor.nodes[component_key]
            component = register_component(analysis_output, component_node, ontology.SOURCE_FUNCTION)
            file_ = register_ada_file(
                analysis_output,
                as_optional_file_identifier(get_node_file(component_node))
            )
            component.defined_in = file_

        # add "mentions"
        for caller_key in static_call_graph_visitor.edges:
            caller_node = static_call_graph_visitor.nodes[caller_key]
            caller_identifier = ontology.SoftwareComponentIdentifier(get_node_identifier(caller_node))
            for callee_key in static_call_graph_visitor.edges[caller_key]:
                callee_node = static_call_graph_visitor.nodes[callee_key]
                callee_identifier = ontology.SoftwareComponentIdentifier(get_node_identifier(callee_node))
                callee = components[callee_identifier]
                caller = components[caller_identifier]
                caller.add_mention(callee)

        if DEBUG_TURTLE:
            output_as_turtle(analysis_output)
        else:
            output_using_scrapingtoolkit(analysis_output)

    else:
        print("No root found, diagnostics:")
        print(unit.diagnostics)

# Register our custom color formatter for our logger
logger = logging.getLogger('ada')
stream_handler = logging.StreamHandler()
stream_handler.setFormatter(CustomFormatter())
logger.propagate = False
logger.addHandler(stream_handler)

Evidence.createEvidenceFile(
        ingestionTitle="AdaSourceIngestion",
        ingestionDescription="libadalang-based extraction of source code defined functions and links to requirements")
for file_to_analyze in args.analyze:
    unit = context.get_from_file(file_to_analyze)
    analyze_unit(unit)
    analyze_traceability(unit)
    analyze_structure(unit)
Evidence.createCDR()
