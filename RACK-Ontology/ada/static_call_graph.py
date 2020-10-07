import  libadalang as lal
from typing import List

from ada_visitor import AdaVisitor

def get_params(subp_spec_node: lal.SubpSpec) -> List[str]:
    subp_params = subp_spec_node.f_subp_params
    params = subp_params.f_params if subp_params != None else []
    return [param.f_ids.text for param in params]

class StaticCallGraphVisitor(AdaVisitor):

    '''
    Computes the static call graph within some AST node.
    '''

    def __init__(self, caller=None, params=[], parent=None):
        # set of all callees witnessed for the current function/procedure
        self.callees = set()
        # which function/procedure we are currently analyzing
        self.caller = caller
        # formal names of parameters to the function we are analyzing
        self.params = params
        # within which function/procedure was the current one defined
        self.parent = parent
        pass

    def record_call(self, callee):
        '''Records a witnessed static function/procedure call to callee'''
        self.callees.add(callee)

    def visit_SubpBody(self, node):
        spec = node.f_subp_spec
        name = spec.f_subp_name
        params = get_params(spec)
        local_visitor = StaticCallGraphVisitor(name, params, self)
        local_visitor.visit(node.f_stmts)

        defined = 'at the top-level'if self.caller == None else f'in {self.caller}'
        print(f'{name.text} (defined {defined}) calls:')
        for callee in local_visitor.callees:
            extra = '' if callee not in params else ' (which it receives as parameter)'
            print(f' - {callee}{extra}')

    def visit_CallExpr(self, node):
        self.record_call(node.f_name.text)
