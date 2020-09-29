#!/usr/bin/env python

import libadalang as lal
import os

context = lal.AnalysisContext()

libadalang_ada_files = os.environ['LIBADALANG_ADA_FILES']

unit = context.get_from_file(libadalang_ada_files + "/libadalang-c.adb")

if unit.root:
  for node in unit.root.finditer([lal.CallStmt, lal.CallExpr]):
    if node.is_a(lal.CallExpr):
      print(node, node.f_name)
    if isinstance(node, lal.CallStmt):
      print(node, node.f_call)
