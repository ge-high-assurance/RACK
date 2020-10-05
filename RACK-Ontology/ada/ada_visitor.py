import libadalang as lal
from typing import TypeVar

AdaVisitorT = TypeVar('AdaVisitorT', bound='AdaVisitor')
AdaNodeT = TypeVar('AdaNodeT', bound='lal.AdaNode')

class AdaVisitor():

    '''
    This is a very simple class you can inherit from to visit the Ada AST.  For
    any node you want to handle, you can define a method `generic_<NodeName>`,
    which will be called automatically.

    If you want the visit to continue recursively once you are done, you can
    call `super().generic_visit(node)`, which will visit all the `children` of
    that node.

    Note that by default, the Ada AST goes extremely deep in nodes.  If you do
    not care about whole classes of nodes and their descendants, you should
    define a `generic_<...>` method that just `pass`es to save some pointless
    tree exploration.

    You can use the `print_ada_visitor` to get an idea of what the AST for a
    given file looks like.
    '''

    def generic_visit(self: AdaVisitorT, node: AdaNodeT) -> None:
        '''Generically visit some node, recursively visiting its children'''
        children = getattr(node, 'children', [])
        for child in children:
            self.visit(child)

    def visit(self: AdaVisitorT, node: AdaNodeT) -> None:
        '''Entry point to visit an arbitrary Ada AST node'''
        method = 'visit_' + node.__class__.__name__
        visitor = getattr(self, method, self.generic_visit)
        return visitor(node)
