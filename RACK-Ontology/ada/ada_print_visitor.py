from ada_visitor import AdaVisitor

class AdaPrintVisitor(AdaVisitor):

    '''
    Prints out the AST from a given node, up to some `max_depth`.
    '''

    def __init__(self, max_depth):
        self.max_depth = max_depth
        self.depth = 0

    def print_then_super_generic_visit(self, node):
        self.print(node.__class__.__name__)
        super().generic_visit(node)

    def visit(self, node):
        if node == None or self.depth >= self.max_depth:
            return
        self.depth += 1
        method = 'visit_' + node.__class__.__name__
        visitor = getattr(self, method, self.print_then_super_generic_visit)
        visitor(node)
        self.depth -= 1

    def print(self, str):
        indent = '  ' * (self.depth - 1)
        print(f'{indent}{str}')

    def visit_CompilationUnit(self, node):
        name = '.'.join(node.p_syntactic_fully_qualified_name)
        self.print(f'CompilationUnit {name}')
        self.generic_visit(node)

    def visit_DottedName(self, node):
        self.print(f'DottedName {node.text}')

    def visit_Identifier(self, node):
        self.print(f'Identifier {node.text}')

    def visit_NameList(self, list):
        self.print(f'{list.__class__.__name__} {list.text}')

    def visit_TypeDecl(self, node):
        name = '.'.join(node.f_name.f_name.p_as_symbol_array)
        self.print(f'TypeDecl {name}')
        self.print_then_super_generic_visit(node)
