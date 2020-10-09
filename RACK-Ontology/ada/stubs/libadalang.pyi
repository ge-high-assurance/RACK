# NOTE: Currently, I'm only putting the minimal amount of definitions here so
# that mypy is happy. This is **not** exhaustive.

from typing import Iterable

class AdaNode:
    @property
    def text(self) -> str: ...

class BasicDecl(AdaNode):
    @property
    def p_canonical_fully_qualified_name(self) -> str: ...

class Name(AdaNode):
    @property
    def p_referenced_decl(self) -> BasicDecl: ...

class CallExpr(AdaNode):
    @property
    def f_name(self) -> Name: ...

class DeclarativePart(AdaNode): ...

class DefiningName(AdaNode):
    @property
    def f_name(self) -> Name: ...

class DefiningNameList(AdaNode, Iterable[DefiningName]):
    def __iter__(self): ...

class HandledStmts(AdaNode): ...

class PackageBody(AdaNode):
    @property
    def f_decls(self) -> DeclarativePart: ...
    @property
    def f_package_name(self) -> DefiningName: ...
    @property
    def f_stmts(self) -> HandledStmts: ...

class ParamSpec(AdaNode):
    @property
    def f_ids(self) -> DefiningNameList: ...

class ParamSpecList(AdaNode, Iterable[ParamSpec]):
    def __iter__(self): ...

class Params(AdaNode):
    @property
    def f_params(self) -> ParamSpecList: ...

class SubpSpec(AdaNode):
    @property
    def f_subp_name(self) -> DefiningName: ...
    @property
    def f_subp_params(self) -> Params: ...

class SubpBody(AdaNode):
    @property
    def f_decls(self) -> DeclarativePart: ...
    @property
    def f_stmts(self) -> HandledStmts: ...
    @property
    def f_subp_spec(self) -> SubpSpec: ...

class AnalysisUnit:
    @property
    def root(self) -> AdaNode: ...

class AnalysisContext:
    def get_from_file(self, file: str) -> AnalysisUnit: ...
