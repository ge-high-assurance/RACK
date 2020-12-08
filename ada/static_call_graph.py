"""
This module contains facilities for building a call graph for an Ada program.

The 'StaticCallGraphVisitor' class runs a call graph analysis on a libadalang
AdaNode via a visitor pattern (inherit from 'AdaVisitor') and makes available
its result call graph in the 'nodes' and 'edges' member variables.
"""

__copyright__ = "Copyright (c) 2020, Galois, Inc."

import logging
from typing import Callable, Dict, Optional, Set

import libadalang as lal

from ada_visitor import AdaVisitor

# Type to abstract over what nodes we put in the graph. Can be turned into a
# concrete or abstract class if the complexity ever demands it.
GraphNode = lal.Name

logger = logging.getLogger('ada')

def warn_about_node(node: lal.AdaNode) -> None:
    """
    Emits a warning because we could not resolve the definition site for the
    given node.
    """
    name = node.text
    loc = node.full_sloc_image[:-2]
    logger.warning(f"Could not resolve the name {name} as it appears in {loc}.")

def safe_xref(node: lal.AdaNode) -> Optional[lal.DefiningName]:
    """
    p_gnat_xref fails catastrophically (dereferences a null pointer), this
    wraps it up nicely.
    """
    try:
        return node.p_gnat_xref()
    except lal.PropertyError:
        warn_about_node(node)
        return None

def node_key(node: GraphNode) -> str:
    """
    Computes a key we can use for identifying this node uniquely.
    """
    xref = safe_xref(node)
    if xref is None:
        return str(node)
    return str(xref)

def get_definition_file(node: GraphNode) -> Optional[str]:
    """"
    Returns the name of the file within which this node was defined, assuming
    we succeed to resolve the reference.
    """
    xref = safe_xref(node)
    # NOTE: if we need the full path, we can use:
    #   xref.p_basic_decl.unit.filename
    # NOTE: full_sloc_image returns "file.ada:<line>:<column>: " but we just
    # want the filename
    if xref is None:
        return None
    return xref.p_basic_decl.full_sloc_image.split(":")[0]

def get_name(node: GraphNode) -> str:
    """Computes the name to display for a node."""
    return node.text

def get_uri(node: GraphNode) -> str:
    """Computes the URI to use for a node."""
    xref = node.p_gnat_xref()
    if not xref:
        return f"__RACK__UNRESOLVED__{node.p_relative_name.p_canonical_text}"
        # raise Exception(f"The reference to node {node} could not be resolved.")
    return xref.p_basic_decl.p_canonical_fully_qualified_name

class StaticCallGraphVisitor(AdaVisitor):

    """
    Computes the static call graph within some AST node. Once `visit()` has
    completed, you can read the call graph in the `edges` instance variable.
    """

    def __init__(
        self,
        context: lal.AnalysisContext,
        caller_being_defined: Optional[GraphNode],
        nodes: Dict[str, GraphNode],
        edges: Dict[str, Set[str]]
    ) -> None:
        """
        Initialize the visitor.  Because it is not very practical to locally
        update the parameters when doing recursive calls, we suggest instead to
        instantiate a new local visitor, run it, and then gather from its final
        state whatever data you need.  Avoids code duplication, at the price of
        creating a bunch of short-lived instances.
        """

        self.context = context

        self.caller_being_defined = caller_being_defined
        """
        Name of the caller currently being defined, that will be deemed the
        caller of whatever call expression we encounter. This can either be a
        function/procedure, or a file if we're at the top level.
        """

        # INVARIANT
        # All nodes appearing in edges, either via their key, or in the set of
        # destinations, should be in the nodes set.
        # There may be nodes that participate in no edges.

        # We store nodes by key so that we can retrieve node instances by their
        # key and avoid creating duplicates.
        self.nodes = nodes
        """All nodes in the graph, unsorted."""

        if caller_being_defined:
            # Register the caller as a node
            caller_key = node_key(caller_being_defined)
            if caller_key is not None and caller_key not in nodes:
                nodes[caller_key] = caller_being_defined

        self.edges: Dict[str, Set[str]] = edges
        """
        Edges of the graph, keyed by their origin, valued by the set of
        destinations for that origin.
        """

    def get_graph_node_for_name(self, node: lal.Name) -> Optional[GraphNode]:
        """Returns the graph node for a name, creating it if none exists yet."""
        key = node_key(node)
        if key is None:
            return None
        if key not in self.nodes:
            self.nodes[key] = node
        return self.nodes[key]

    def record_call(self, callee: lal.Name) -> None:
        """Records a witnessed static function/procedure call to callee."""
        if self.caller_being_defined is not None:
            caller_key = node_key(self.caller_being_defined)
            if caller_key is None:
                return
            callee_node = self.get_graph_node_for_name(callee)
            if callee_node is None:
                return
            callee_key = node_key(callee_node)
            if callee_key is None:
                return
            if caller_key not in self.edges:
                self.edges[caller_key] = set()
            self.edges[caller_key].add(callee_key)

    def locally_visit(
        self,
        caller_being_defined: Optional[GraphNode],
        callback: Callable[[AdaVisitor], None]
    ) -> None:
        """
        Do something with a visitor locally overriding the values of certain
        variables.
        """
        local_visitor = StaticCallGraphVisitor(
            context=self.context,
            caller_being_defined=caller_being_defined,
            nodes=self.nodes,
            edges=self.edges
        )
        callback(local_visitor)

    # pylint: disable=invalid-name, missing-function-docstring
    def visit_CallExpr(self, node: lal.CallExpr):
        self.record_call(node.f_name)
    # pylint: enable=invalid-name, missing-function-docstring

    # pylint: disable=invalid-name, missing-function-docstring
    def visit_PackageBody(self, node: lal.PackageBody) -> None:

        def callback(visitor):
            visitor.generic_visit(node.f_decls)
            visitor.generic_visit(node.f_stmts)

        self.locally_visit(
            caller_being_defined=self.caller_being_defined,
            callback=callback
        )
    # pylint: enable=invalid-name, missing-function-docstring

    # pylint: disable=invalid-name, missing-function-docstring
    def visit_SubpBody(self, node: lal.SubpBody) -> None:
        spec = node.f_subp_spec
        name = spec.f_subp_name

        def callback(visitor):
            # assumption: the spec does not contain calls, skipping it
            visitor.visit(node.f_decls)
            visitor.visit(node.f_stmts)

        self.locally_visit(
            caller_being_defined=name,
            callback=callback
        )
    # pylint: enable=invalid-name, missing-function-docstring
