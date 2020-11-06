from abc import ABC, abstractmethod
import libadalang as lal
import os
from typing import Callable, Dict, List, Optional, Set

from ada_visitor import AdaVisitor

def node_key(node: lal.Name) -> str:
    xref = node.p_gnat_xref()
    if xref:
        return str(xref)
    name = node.text
    loc = node.full_sloc_image[:-2]
    print(f"Could not resolve the name {name} as it appears in {loc}.")
    print("Make sure to include all project files.")
    exit(1)

class GraphNode(ABC):
    @abstractmethod
    def get_key(self) -> str:
        """
        Returns a unique identifying string suitable for use as dictionary
        key.
        """
        ...

    @abstractmethod
    def get_name(self) -> str:
        """
        Returns a string suitable for displaying this node to the user.
        """
        ...

    @abstractmethod
    def get_uri(self) -> str:
        """
        Returns an identifying string suitable for putting in a RDF URI.
        """
    pass

class ToplevelNode(GraphNode):
    def __init__(self, absolute_file_path: str):
        self.absolute_file_path = absolute_file_path

    def get_key(self) -> str:
        return self.absolute_file_path

    def get_name(self) -> str:
        return os.path.basename(self.absolute_file_path)

    def get_uri(self) -> str:
        return self.absolute_file_path

class CallableNode(GraphNode):
    def __init__(self, node: lal.Name):
        self.node = node

    def get_key(self):
        return node_key(self.node)

    def get_name(self) -> str:
        if isinstance(self.node, lal.DefiningName):
            return f"{self.node.f_name.p_relative_name.p_canonical_text} {self.node.full_sloc_image[:-2]}"
        if isinstance(self.node, lal.Identifier):
            return f"{self.node.p_canonical_text} {self.node.full_sloc_image[-2]}"
        raise Exception(f"get_name: no implementation for CallableNode {self.node}")

    def get_uri(self) -> str:
        xref = self.node.p_gnat_xref()
        if not xref:
            raise Exception(f"The reference to node {self} could not be resolved.")
        return xref.p_basic_decl.p_canonical_fully_qualified_name

class StaticCallGraphVisitor(AdaVisitor):

    """
    Computes the static call graph within some AST node. Once `visit()` has
    completed, you can read the call graph in the `edges` instance variable.
    """

    def __init__(
        self,
        caller_being_defined: GraphNode,
        nodes: Dict[str, GraphNode] = dict(),
        edges: Dict[str, Set[str]] = dict()
    ) -> None:
        """
        Initialize the visitor.  Because it is not very practical to locally
        update the parameters when doing recursive calls, we suggest instead to
        instantiate a new local visitor, run it, and then gather from its final
        state whatever data you need.  Avoids code duplication, at the price of
        creating a bunch of short-lived instances.
        """

        self.caller_being_defined: GraphNode = caller_being_defined
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
        self.nodes: Dict[str, GraphNode] = nodes
        """All nodes in the graph, unsorted."""

        # Register the caller as a node
        caller_key = caller_being_defined.get_key()
        if caller_key not in nodes:
            nodes[caller_key] = caller_being_defined

        self.edges: Dict[str, Set[str]] = edges
        """
        Edges of the graph, keyed by their origin, valued by the set of
        destinations for that origin.
        """

    def get_graph_node_for_name(self, node: lal.Name) -> GraphNode:
        """Returns the graph node for a name, creating it if none exists yet."""
        key = node_key(node)
        if key not in self.nodes:
            self.nodes[key] = CallableNode(node)
        return self.nodes[key]

    def record_call(self, callee: lal.Name) -> None:
        """Records a witnessed static function/procedure call to callee."""
        caller_key = self.caller_being_defined.get_key()
        callee_key = self.get_graph_node_for_name(callee).get_key()
        if caller_key not in self.edges:
            self.edges[caller_key] = set()
        self.edges[caller_key].add(callee_key)

    def locally_visit(
        self,
        caller_being_defined: GraphNode,
        callback: Callable[[AdaVisitor], None]
    ) -> None:
        """
        Do something with a visitor locally overriding the values of certain
        variables.
        """
        local_visitor = StaticCallGraphVisitor(
            caller_being_defined=caller_being_defined,
            nodes=self.nodes,
            edges=self.edges
        )
        callback(local_visitor)

    def visit_CallExpr(self, node: lal.CallExpr):
        self.record_call(node.f_name)

    def visit_PackageBody(self, node: lal.PackageBody) -> None:

        def callback(visitor):
            visitor.generic_visit(node.f_decls)
            visitor.generic_visit(node.f_stmts)

        self.locally_visit(
            caller_being_defined=self.caller_being_defined,
            callback=callback
        )

    def visit_SubpBody(self, node: lal.SubpBody) -> None:
        spec = node.f_subp_spec
        name = spec.f_subp_name

        def callback(visitor):
            # assumption: the spec does not contain calls, skipping it
            visitor.visit(node.f_decls)
            visitor.visit(node.f_stmts)

        self.locally_visit(
            caller_being_defined=CallableNode(name),
            callback=callback
        )
