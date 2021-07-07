"""
This module contains facilities for extracting traceability-comments Ada program.
"""

__copyright__ = "Copyright (c) 2020, Galois, Inc."

import logging
from typing import Callable, Dict, NewType, Optional, Set, List
import urllib.parse
import re

import libadalang as lal

from ada_visitor import AdaVisitor
from node_naming import GraphNode, get_node_identifier

class TraceabilityExtraction(AdaVisitor):

    """
    Traverse the source unit extracting traceability identifiers attributed
    to subprograms.
    """

    def __init__(
        self,
        context: lal.AnalysisContext,
    ) -> None:
        self.context: lal.AnalysisContext = context
        self.traceability: Dict[GraphNode, List[str]] = {}
        self.seen: Set[int] = set()

    def match_trace_token(self, token: lal.Token):
        """Detect unmatched traceability comments and return traceability identifier."""

        if token.is_trivia and token.kind == 'Comment' and token.index not in self.seen:
            match = re.match(r'^--\* \^(.*);$', token.text)
            if match:
                self.seen.add(token.index)
                return match.group(1)

    # pylint: disable=invalid-name, missing-function-docstring
    def visit_PackageBody(self, node: lal.PackageBody) -> None:
        self.generic_visit(node)
        name = node.f_package_name
        for token in node.tokens:
            trace_id = self.match_trace_token(token)
            if trace_id is not None:
                self.traceability.setdefault(get_node_identifier(name), []).append(trace_id)
    # pylint: enable=invalid-name, missing-function-docstring

    # pylint: disable=invalid-name, missing-function-docstring
    def visit_SubpBody(self, node: lal.SubpBody) -> None:
        self.generic_visit(node)

        spec = node.f_subp_spec
        name = spec.f_subp_name
        for token in node.tokens:
            trace_id = self.match_trace_token(token)
            if trace_id:
                self.traceability.setdefault(get_node_identifier(name), []).append(trace_id)
    # pylint: enable=invalid-name, missing-function-docstring
