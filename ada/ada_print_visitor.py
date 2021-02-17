"""
A simple instantiation of the 'AdaVisitor' to print out the program structure
of an Ada file in a somewhat human-readable way.
"""

__copyright__ = "Copyright (c) 2020, Galois, Inc."

from colorama import Fore

from ada_visitor import AdaVisitor

colors = [Fore.RED, Fore.GREEN, Fore.BLUE, Fore.YELLOW, Fore.MAGENTA, Fore.CYAN, Fore.WHITE]

class AdaPrintVisitor(AdaVisitor):

    """
    Prints out the AST from a given node, up to some `max_depth`.
    """

    def __init__(self, max_depth):
        self.max_depth = max_depth
        self.depth = 0

    def print_then_super_generic_visit(self, node):
        """Prints the given node, then visits it."""
        self.print(node.__class__.__name__)
        super().generic_visit(node)

    def visit(self, node):
        """
        Default method for visiting a node.

        Called in the absence of a 'visit_<NodeName>' method.

        Can be overridden by children classes.
        """
        if node is None or self.depth >= self.max_depth:
            return
        self.depth += 1
        method = "visit_" + node.__class__.__name__
        visitor = getattr(self, method, self.print_then_super_generic_visit)
        visitor(node)
        self.depth -= 1

    def print(self, msg):
        """"Prints a message at the proper indentation level."""
        indent = ""
        # - 1 so that depth 1 shows with 0 indentation, etc.
        for i in range(self.depth - 1):
            indent += f"{colors[i % len(colors)]}| {Fore.RESET}"
        print(f"{indent}{msg}")

    # pylint: disable=invalid-name, missing-function-docstring
    def visit_CompilationUnit(self, node):
        name = ".".join(node.p_syntactic_fully_qualified_name)
        self.print(f"CompilationUnit {name}")
        self.generic_visit(node)
    # pylint: enable=invalid-name, missing-function-docstring

    # pylint: disable=invalid-name, missing-function-docstring
    def visit_DottedName(self, node):
        self.print(f"DottedName {node.text}")
    # pylint: enable=invalid-name, missing-function-docstring

    # pylint: disable=invalid-name, missing-function-docstring
    def visit_Identifier(self, node):
        self.print(f"Identifier {node.text}")
    # pylint: enable=invalid-name, missing-function-docstring

    # pylint: disable=invalid-name, missing-function-docstring
    def visit_NameList(self, namelist):
        self.print(f"{namelist.__class__.__name__} {namelist.text}")
    # pylint: enable=invalid-name, missing-function-docstring

    # pylint: disable=invalid-name, missing-function-docstring
    def visit_TypeDecl(self, node):
        name = ".".join(node.f_name.f_name.p_as_symbol_array)
        self.print(f"TypeDecl {name}")
        self.print_then_super_generic_visit(node)
    # pylint: enable=invalid-name, missing-function-docstring
