import libadalang as lal
from typing import Dict, List, Set

from ada_visitor import AdaVisitor

def namespace_str(namespace: List[str]) -> str:
    if len(namespace) == 1:
        return namespace[0]
    path = ".".join(namespace[1:])
    return f"{namespace[0]}:{path}"

def get_params(node: lal.SubpSpec) -> List[str]:
    subp_params = node.f_subp_params
    params = subp_params.f_params if subp_params != None else []
    return [param.f_ids.text for param in params]

class StaticCallGraphVisitor(AdaVisitor):

    """
    Computes the static call graph within some AST node. Once visit() has
    completed, you can read the call graph in the call_graph instance
    variable.
    """

    def __init__(self, body_being_defined: str, namespace: List[str], params: List[str] = []) -> None:
        """
        Initialize the visitor.  Because it is not very practical to locally
        update the parameters when doing recursive calls, we suggest instead to
        instantiate a new local visitor, run it, and then gather from its final
        state whatever data you need.  Avoids code duplication, at the price of
        creating a bunch of short-lived instances.
        """

        self.body_being_defined: str = body_being_defined
        """
        Name of the body (function/procedure) currently being defined, that will
        be deemed the caller of whatever call expression we encounter.
        """

        self.call_graph: Dict[str, Set[str]] = dict()
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
        """Records a witnessed static function/procedure call to callee"""
        if self.body_being_defined not in self.call_graph:
            self.call_graph[self.body_being_defined] = set()
        self.call_graph[self.body_being_defined].add(callee)

    def visit_PackageBody(self, node: lal.PackageBody) -> None:
        # here we register the namespace and keep visiting with the same visitor
        name = node.f_package_name
        self.namespace += [name.text]
        super().generic_visit(node.f_decls)
        super().generic_visit(node.f_stmts)
        pass

    def visit_SubpBody(self, node: lal.SubpBody) -> None:
        spec = node.f_subp_spec
        name = spec.f_subp_name
        params = get_params(spec)
        local_visitor = StaticCallGraphVisitor(
            body_being_defined = name.text,
            namespace = self.namespace + [name.text],
            params = params
        )
        # assumption: the spec does not contain calls
        local_visitor.visit(node.f_decls)
        local_visitor.visit(node.f_stmts)

        # hoist all the local callee information to the current graph
        for key in local_visitor.call_graph:
            if not key in self.call_graph:
                self.call_graph[key] = set()
            self.call_graph[key] = self.call_graph[key].union(local_visitor.call_graph[key])

    def visit_CallExpr(self, node: lal.CallExpr):
        defined = (
            f"(defined at {node.f_name.p_referenced_decl()})"
            if node.f_name.p_resolve_names
            else "(could not locate definition)"
        )
        self.record_call(f"{node.f_name.text} {defined}")
