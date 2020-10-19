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
    Computes the static call graph within some AST node.
    """

    def __init__(self, namespace: List[str], params: List[str] = []) -> None:
        # set of all callees witnessed for the current function/procedure
        self.callees: Set[str] = set()
        # current enclosing namespace
        self.namespace = namespace
        # formal names of parameters to the function we are analyzing
        self.params: List[str] = params

    def record_call(self, callee: str) -> None:
        '''Records a witnessed static function/procedure call to callee'''
        self.callees.add(callee)

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
            namespace=self.namespace + [name.text],
            params=params
        )
        # assumption: the spec does not contain calls
        local_visitor.visit(node.f_decls)
        local_visitor.visit(node.f_stmts)

        defined = f'in {namespace_str(self.namespace)}'
        if not list(local_visitor.callees):
            print(f'{name.text} (defined {defined}) does not make any call.')
            return
        print(f'{name.text} (defined {defined}) calls:')
        for callee in local_visitor.callees:
            extra = '' if callee not in params else ' (which it receives as parameter)'
            print(f' - {callee}{extra}')

    def visit_CallExpr(self, node: lal.CallExpr):
        defined = (
            f'(defined at {node.f_name.p_referenced_decl()})'
            if node.f_name.p_resolve_names
            else '(could not locate definition)'
        )
        self.record_call(f'{node.f_name.text} {defined}')
