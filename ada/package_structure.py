"""
This module contains facilities for extracting traceability-comments Ada program.
"""

__copyright__ = "Copyright (c) 2020, Galois, Inc."

import logging
from typing import Callable, Dict, NewType, Optional, Set
import urllib.parse
import re

import libadalang as lal

from ada_visitor import AdaVisitor
from node_naming import get_node_identifier

class StructureExtractor(AdaVisitor):

    """
    Traverse the source unit extracting traceability identifiers attributed
    to subprograms.
    """

    def __init__(
        self,
    ) -> None:
        self.packages = {}
        self.current_package = None

    # pylint: disable=invalid-name, missing-function-docstring
    def visit_PackageBody(self, node: lal.PackageBody) -> None:
        name = node.f_package_name
        functions = []
        self.packages[name] = functions
        self.current_package = functions
        self.visit(node.f_decls)
    # pylint: enable=invalid-name, missing-function-docstring

    # pylint: disable=invalid-name, missing-function-docstring
    def visit_SubpBody(self, node: lal.SubpBody) -> None:
        spec = node.f_subp_spec
        name = spec.f_subp_name
        if self.current_package is not None:
            self.current_package.append(name)
    # pylint: enable=invalid-name, missing-function-docstring
