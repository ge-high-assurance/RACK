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
        self.callees = set()
        self.caller = caller
        self.params = params
        self.parent = parent
        pass

    def record_call(self, callee):
        self.callees.add(callee)

    def visit_SubpBody(self, node):
        spec = node.f_subp_spec
        name = spec.f_subp_name
        params = get_params(spec)
        local_visitor = StaticCallGraphVisitor(name, params, self)
        local_visitor.visit(node.f_stmts)
        for callee in local_visitor.callees:
            if callee in params:
                print(f'{name.text} calls its parameter {callee}')
            else:
                print(f'{name.text} calls {callee}')

    def visit_CallExpr(self, node):
        self.record_call(node.f_name.text)
