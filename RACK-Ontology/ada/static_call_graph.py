from abc import ABC
import libadalang as lal
from typing import Callable, Dict, List, Optional, Set

from ada_visitor import AdaVisitor

def node_key(node: lal.AdaNode) -> str:
    return node.p_gnat_xref().p_basic_decl.p_canonical_fully_qualified_name

class GraphNode(ABC):
    pass

class ToplevelNode(GraphNode):
    def __init__(self, absolute_file_path: str):
        self.absolute_file_path = absolute_file_path

class CallableNode(GraphNode):
    def __init__(self, node: lal.AdaNode):
        self.node = node

class StaticCallGraphVisitor(AdaVisitor):

    """
    Computes the static call graph within some AST node. Once `visit()` has
    completed, you can read the call graph in the `edges` instance variable.
    """

    def __init__(
        self,
        absolute_file_path: str,
        callable_being_defined: Optional[lal.DefiningName]
    ) -> None:
        """
        Initialize the visitor.  Because it is not very practical to locally
        update the parameters when doing recursive calls, we suggest instead to
        instantiate a new local visitor, run it, and then gather from its final
        state whatever data you need.  Avoids code duplication, at the price of
        creating a bunch of short-lived instances.
        """

        self.absolute_file_path = absolute_file_path
        """
        Absolute file path of the file being analyzed.  Useful to give an
        identity to the top-level node of the call graph.
        """

        self.callable_being_defined: Optional[lal.DefiningName] = callable_being_defined
        """
        Name of the callable (function/procedure) currently being defined,
        that will be deemed the caller of whatever call expression we
        encounter. Technically can be any name, so the top-level code can use
        the file name if needed.
        """

        # INVARIANT
        # All nodes appearing in edges, either via their key, or in the set of
        # destinations, should be in the nodes set.
        # There may be nodes that participate in no edges.

        # We store nodes by key so that we can retrieve node instances by their
        # key and avoid creating duplicates.
        self.nodes: Dict[str, GraphNode] = dict()
        """All nodes in the graph, unsorted."""

        if callable_being_defined:
            key = node_key(callable_being_defined)
            self.nodes[key] = CallableNode(callable_being_defined)
        else:
            self.nodes[absolute_file_path] = ToplevelNode(absolute_file_path)

        self.edges: Dict[str, Set[str]] = dict()
        """
        Edges of the graph, keyed by their origin, valued by the set of
        destinations for that origin.
        """

    def get_callable_being_defined_key(self) -> str:
        if not self.callable_being_defined:
            return "__TOPLEVEL__"
        return node_key(self.callable_being_defined)

    def ensure_callable_is_registered(self, node: lal.Name):
        key = node_key(node)
        if key in self.nodes:
            return
        self.nodes[key] = CallableNode(node)

    def record_call(self, callee: lal.Name) -> None:
        """Records a witnessed static function/procedure call to callee."""
        caller_key = self.get_callable_being_defined_key()
        callee_key = node_key(callee)
        self.ensure_callable_is_registered(callee)
        if caller_key not in self.edges:
            self.edges[caller_key] = set()
        self.edges[caller_key].add(callee_key)

    def locally_visit(
        self,
        absolute_file_path: str,
        callable_being_defined: Optional[lal.DefiningName],
        callback: Callable[[AdaVisitor], None]
    ) -> None:
        """
        Do something with a visitor locally overriding the values of certain
        variables.
        """
        local_visitor = StaticCallGraphVisitor(
            absolute_file_path = absolute_file_path,
            callable_being_defined = callable_being_defined
        )
        callback(local_visitor)

    # NOTE (val) I made it so that local visitors have their own set of nodes
    # and edges, that the caller may then choose to merge. In practice we
    # always do, so maybe we should just pass the nodes and edges and modify
    # them...

    def merge_nodes(self, nodes: Dict[str, GraphNode]) -> None:
        """
        Merges the given nodes to the self ones.
        """
        self.nodes.update(nodes)

    def merge_edges(self, edges: Dict[str, str]) -> None:
        """
        Merges the given call graph to the current call graph (essentially a
        key-wise union).
        """
        for key in edges:
            if not key in self.edges:
                self.edges[key] = set()
            self.edges[key] = self.edges[key].union(edges[key])

    def visit_CallExpr(self, node: lal.CallExpr):
        self.record_call(node.f_name)

    def visit_PackageBody(self, node: lal.PackageBody) -> None:

        def callback(visitor):
            visitor.generic_visit(node.f_decls)
            visitor.generic_visit(node.f_stmts)
            self.merge_nodes(visitor.nodes)
            self.merge_edges(visitor.edges)

        self.locally_visit(
            absolute_file_path = self.absolute_file_path,
            callable_being_defined = self.callable_being_defined,
            callback = callback
        )

    def visit_SubpBody(self, node: lal.SubpBody) -> None:
        spec = node.f_subp_spec
        name = spec.f_subp_name

        def callback(visitor):
            # assumption: the spec does not contain calls, skipping it
            visitor.visit(node.f_decls)
            visitor.visit(node.f_stmts)
            self.merge_nodes(visitor.nodes)
            self.merge_edges(visitor.edges)

        self.locally_visit(
            absolute_file_path = self.absolute_file_path,
            callable_being_defined = name,
            callback = callback
        )
