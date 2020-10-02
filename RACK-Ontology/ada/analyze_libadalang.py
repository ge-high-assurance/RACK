#!/usr/bin/env python

import libadalang as lal
import os

from ada_print_visitor import AdaPrintVisitor

context = lal.AnalysisContext()

libadalang_ada_files = os.environ['LIBADALANG_ADA_FILES']

unit = context.get_from_file(libadalang_ada_files + '/libadalang-helpers.adb')

if unit.root:
    adaVisitor = AdaPrintVisitor(max_depth = 10)
    adaVisitor.visit(unit.root)
