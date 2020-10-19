import libadalang as lal
from typing import Callable, Dict, List, Set

from ada_visitor import AdaVisitor

def namespace_str(namespace: List[str]) -> str:
    if len(namespace) == 1:
        return namespace[0]
    path = ".".join(namespace[1:])
    return f"{namespace[0]}:{path}"

CallGraphType = Dict[str, Set[str]]

class StaticCallGraphVisitor(AdaVisitor):

    """
    Computes the static call graph within some AST node. Once visit() has
    completed, you can read the call graph in the call_graph instance
    variable.
    """

    def __init__(self, callable_being_defined: str, namespace: List[str]) -> None:
        """
        Initialize the visitor.  Because it is not very practical to locally
        update the parameters when doing recursive calls, we suggest instead to
        instantiate a new local visitor, run it, and then gather from its final
        state whatever data you need.  Avoids code duplication, at the price of
        creating a bunch of short-lived instances.
        """

        self.callable_being_defined: str = callable_being_defined
        """
        Name of the callable (function/procedure) currently being defined,
        that will be deemed the caller of whatever call expression we
        encounter. Technically can be any name, so the top-level code can use
        the file name if needed.
        """

        self.call_graph: CallGraphType = dict()
        """
        The call graph being computed.
        """

        # current enclosing namespace (can be arbitrarily initialized, say with
        # a file name) then, traversed namespaces are added onto
        self.namespace = namespace
        """
        The current enclosing namespace, as a sequence enclosing namespaces
        (currently, the top-level will actually be the name of the enclosing
        file).
        """

    def record_call(self, callee: str) -> None:
        """Records a witnessed static function/procedure call to callee."""
        if self.callable_being_defined not in self.call_graph:
            self.call_graph[self.callable_being_defined] = set()
        self.call_graph[self.callable_being_defined].add(callee)

    def locally_visit(
        self,
        callable_being_defined: str,
        namespace: List[str],
        callback: Callable[[AdaVisitor], None]
    ) -> None:
        """
        Do something with a visitor locally overriding the values of certain
        variables.
        """
        local_visitor = StaticCallGraphVisitor(
            callable_being_defined = callable_being_defined,
            namespace = namespace
        )
        callback(local_visitor)

    def merge_call_graph(self, call_graph: CallGraphType) -> None:
        """
        Merges the given call graph to the current call graph (essentially a
        key-wise union).
        """
        for key in call_graph:
            if not key in self.call_graph:
                self.call_graph[key] = set()
            self.call_graph[key] = self.call_graph[key].union(call_graph[key])

    def visit_PackageBody(self, node: lal.PackageBody) -> None:
        name = node.f_package_name
        def callback(visitor):
            visitor.generic_visit(node.f_decls)
            visitor.generic_visit(node.f_stmts)
            self.merge_call_graph(visitor.call_graph)
        self.locally_visit(
            callable_being_defined = self.callable_being_defined,
            namespace = self.namespace + [name.text],
            callback = callback
        )

    def visit_SubpBody(self, node: lal.SubpBody) -> None:
        spec = node.f_subp_spec
        name = spec.f_subp_name

        def callback(visitor):
            # assumption: the spec does not contain calls, skipping it
            visitor.visit(node.f_decls)
            visitor.visit(node.f_stmts)
            self.merge_call_graph(visitor.call_graph)
        self.locally_visit(
            callable_being_defined = name.text,
            namespace = self.namespace,
            callback = callback
        )

    def visit_CallExpr(self, node: lal.CallExpr):
        defined = (
            f"(defined at {node.f_name.p_referenced_decl()})"
            if node.f_name.p_resolve_names
            else "(could not locate definition)"
        )
        self.record_call(f"{node.f_name.text} {defined}")
