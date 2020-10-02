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
        if getattr(self, method, None) == None:
            self.print_then_super_generic_visit(node)
        visitor = getattr(self, method, self.generic_visit)
        visitor(node)
        self.depth -= 1

    def print(self, str):
        indent = '  ' * (self.depth - 1)
        print(f'{indent}{str}')

    def visit_AdaNodeList(self, list):
        self.print(list.__class__.__name__)
        for node in list:
            self.generic_visit(node)

    def visit_NameList(self, list):
        self.print(f'{list.__class__.__name__} {list.text}')

    def visit_PragmaNodeList(self, list):
        self.print(list.__class__.__name__)
        for pragma in list:
            self.generic_visit(pragma)

    def visit_Identifier(self, node):
        self.print(f'Identifier {node.text}')

    def visit_CompilationUnit(self, node):
        name = '.'.join(node.p_syntactic_fully_qualified_name)
        self.print(f'CompilationUnit {name}')
        self.generic_visit(node)

    def visit_DeclarativePart(self, node):
        # self.print(f'DeclarativePart')
        self.print_then_super_generic_visit(node)

    def visit_DeclList(self, node):
        self.print(f'DeclList[...]')

    def visit_DottedName(self, node):
        self.print(f'DottedName {node.text}')

    def visit_Params(self, node):
        # don't recurse more or it will take forever
        self.print(f'Params[...]')

    def visit_SingleProtectedDecl(self, node):
        print(node.children)
        self.print_then_super_generic_visit(node)

    def visit_StmtList(self, node):
        self.print(f'StmtList[...]')

    def visit_TypeDecl(self, node):
        name = '.'.join(node.f_name.f_name.p_as_symbol_array)
        self.print(f'TypeDecl {name}')
        self.print_then_super_generic_visit(node)
