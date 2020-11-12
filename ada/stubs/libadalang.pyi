# NOTE: Currently, I'm only putting the minimal amount of definitions here so
# that mypy is happy. This is **not** exhaustive.

# needed for postponing type annotations, to allow a method returning its class
# should be unnecessary in Python 3.10+
from __future__ import annotations
from typing import Iterable, List, Optional

class Sloc(object):

    @property
    def line(self) -> int: ...

    @property
    def column(self) -> int: ...

class SlocRange(object):

    def __str__(self) -> str: ...

    @property
    def start(self) -> Sloc: ...

    @property
    def end(self) -> Sloc: ...

class AdaNode(object):

    @property
    def image(self) -> str: ...

    @property
    def full_sloc_image(self) -> str: ...

    def p_gnat_xref(self, imprecise_fallback: Optional[bool] = False) -> Optional[DefiningName]: ...

    @property
    def sloc_range(self) -> SlocRange: ...

    @property
    def text(self) -> str: ...

class AdaList(AdaNode): ...

class Expr(AdaNode): ...

class Name(Expr):

    @property
    def p_referenced_decl(self) -> BasicDecl: ...

    @property
    def p_relative_name(self) -> SingleTokNode: ...

    @property
    def p_resolve_names(self) -> bool: ...

class SingleTokNode(Name):

    @property
    def p_canonical_text(self) -> str: ...

class BaseId(SingleTokNode): ...

class BasicDecl(AdaNode):

    @property
    def p_defining_name(self) -> DefiningName: ...

    @property
    def p_relative_name(self) -> SingleTokNode: ...

    @property
    def p_canonical_fully_qualified_name(self) -> str: ...

class CallExpr(Name):

    @property
    def f_name(self) -> Name: ...

class DeclarativePart(AdaNode): ...

class DefiningName(Name):

    @property
    def f_name(self) -> BaseId: ...

    @property
    def p_basic_decl(self) -> BasicDecl: ...

class DefiningNameList(AdaList, Iterable[DefiningName]):

    def __iter__(self): ...

class HandledStmts(AdaNode): ...

class Identifier(BaseId): ...

class BodyNode(BasicDecl): ...

class PackageBody(BodyNode):

    @property
    def f_decls(self) -> DeclarativePart: ...

    @property
    def f_package_name(self) -> DefiningName: ...

    @property
    def f_stmts(self) -> HandledStmts: ...

class BaseFormalParamDecl(BasicDecl): ...

class ParamSpec(BaseFormalParamDecl):

    @property
    def f_ids(self) -> DefiningNameList: ...

class ParamSpecList(AdaList, Iterable[ParamSpec]):

    def __iter__(self): ...

class Params(AdaNode):

    @property
    def f_params(self) -> ParamSpecList: ...

class BaseFormalParamHolder(AdaNode): ...

class SubpSpec(BaseFormalParamHolder):

    @property
    def f_subp_name(self) -> DefiningName: ...

    @property
    def f_subp_params(self) -> Params: ...

class BaseSubpBody(BodyNode): ...

class SubpBody(BaseSubpBody):

    @property
    def f_decls(self) -> DeclarativePart: ...

    @property
    def f_stmts(self) -> HandledStmts: ...

    @property
    def f_subp_spec(self) -> SubpSpec: ...

class AnalysisUnit(object):

    @property
    def diagnostics(self) -> str: ...

    @property
    def filename(self) -> str: ...

    @property
    def root(self) -> AdaNode: ...

class _Enum(object): ...

class AnalysisUnitKind(_Enum):

    def unit_body(self) -> AnalysisUnitKind: ...

    def unit_specification(self) -> AnalysisUnitKind: ...

class AnalysisContext(object):

    def __init__(self, unit_provider: UnitProvider) -> None: ...

    def get_from_file(self, file: str) -> AnalysisUnit: ...

    def get_from_provider(self, name: str, kind: AnalysisUnitKind) -> AnalysisUnit: ...

class UnitProvider(object):

    @classmethod
    def auto(klass, input_files: List[str]) -> UnitProvider: ...

    @classmethod
    def for_project(klass, project: str) -> UnitProvider: ...
