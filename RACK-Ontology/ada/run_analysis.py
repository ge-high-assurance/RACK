#!/usr/bin/env python

import argparse
import libadalang as lal
import os

from ada_print_visitor import AdaPrintVisitor
from static_call_graph import StaticCallGraphVisitor

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

debug = True

for file in args.files:
    print(f"Analyzing {file}")
    unit = context.get_from_file(file)
    if unit.root:
        if debug:
            adaVisitor = AdaPrintVisitor(max_depth = 20)
            adaVisitor.visit(unit.root)
        staticCallGraphVisitor = StaticCallGraphVisitor(
            body_being_defined = unit.filename,
            namespace = [unit.filename]
        )
        staticCallGraphVisitor.visit(unit.root)
        for caller in staticCallGraphVisitor.call_graph:
            for callee in staticCallGraphVisitor.call_graph[caller]:
                print(f"{caller} calls {callee}")
    else:
        print("No root found, diagnostics:")
        print(unit.diagnostics)
