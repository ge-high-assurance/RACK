#!/usr/bin/env python

import libadalang as lal
import os

from ada_print_visitor import AdaPrintVisitor
from static_call_graph import StaticCallGraphVisitor

context = lal.AnalysisContext()

libadalang_ada_files = os.environ['LIBADALANG_ADA_FILES']

unit = context.get_from_file(libadalang_ada_files + '/libadalang-helpers.adb')

if unit.root:
    debug = True
    if debug:
        adaVisitor = AdaPrintVisitor(max_depth = 20)
        adaVisitor.visit(unit.root)
    staticCallGraphVisitor = StaticCallGraphVisitor()
    staticCallGraphVisitor.visit(unit.root)
