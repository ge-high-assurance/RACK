import libadalang as lal
from typing import List, Set

from ada_visitor import AdaVisitor

def get_params(node: lal.SubpSpec) -> List[str]:
    subp_params = node.f_subp_params
    params = subp_params.f_params if subp_params != None else []
    return [param.f_ids.text for param in params]

class StaticCallGraphVisitor(AdaVisitor):

    '''
    Computes the static call graph within some AST node.
    '''

    def __init__(self, caller: lal.AdaNode, params: List[str] = []) -> None:
        # set of all callees witnessed for the current function/procedure
        self.callees: Set[str] = set()
        # which function/procedure we are currently analyzing
        self.caller: lal.AdaNode = caller
        # formal names of parameters to the function we are analyzing
        self.params: List[str] = params

    def record_call(self, callee: str) -> None:
        '''Records a witnessed static function/procedure call to callee'''
        self.callees.add(callee)

    def visit_SubpBody(self, node: lal.SubpBody) -> None:
        spec = node.f_subp_spec
        name = spec.f_subp_name
        params = get_params(spec)
        local_visitor = StaticCallGraphVisitor(caller=name, params=params)
        # assumption: the spec does not contain calls
        local_visitor.visit(node.f_decls)
        local_visitor.visit(node.f_stmts)

        caller = self.caller
        defined = 'at the top-level'if caller == None else f'in {caller.text}'
        if not list(local_visitor.callees):
            print(f'{name.text} (defined {defined}) does not make any call.')
            return
        print(f'{name.text} (defined {defined}) calls:')
        for callee in local_visitor.callees:
            extra = '' if callee not in params else ' (which it receives as parameter)'
            print(f' - {callee}{extra}')

    def visit_CallExpr(self, node: lal.CallExpr):
        self.record_call(node.f_name.text)
