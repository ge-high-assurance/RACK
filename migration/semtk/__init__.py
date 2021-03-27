# Copyright (c) 2020, Galois, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

from __future__ import annotations

from abc import ABC, abstractmethod
from pydantic import BaseModel
from typing import Dict, List, Optional


class SemTKVisitor(ABC):
    @abstractmethod
    def visit_SemTKJSON(self, element: SemTKJSON) -> None:
        pass


class DefaultSemTKVisitor(SemTKVisitor):
    def visit_ImportSpec(self, json: SemTKJSON, path: str, importSpec: ImportSpec) -> None:
        for index, importSpecNode in enumerate(importSpec.nodes):
            self.visit_ImportSpecNode(json, f"{path}.nodes[{index}]", importSpecNode)

    def visit_ImportSpecNode(self, json: SemTKJSON, path: str, importSpecNode: ImportSpecNode) -> None:
        for index, importSpecProp in enumerate(importSpecNode.props):
            self.visit_ImportSpecProp(json, f"{path}.props[{index}]", importSpecProp)

    def visit_ImportSpecProp(self, json: SemTKJSON, path: str, importSpecProp: ImportSpecProp) -> None:
        pass

    def visit_ModelOrData(self, modelOrData: ModelOrData) -> None:
        pass

    def visit_SNode(self, json: SemTKJSON, path: str, sNode: SNode) -> None:
        for index, sNodeNode in enumerate(sNode.nodeList):
            self.visit_SNodeNode(json, f"{path}.nodeList[{index}]", sNodeNode)
        for index, sNodeProp in enumerate(sNode.propList):
            self.visit_SNodeProp(json, f"{path}.propList[{index}]", sNodeProp)

    def visit_SNodeGroup(self, json: SemTKJSON, path: str, sNodeGroup: SNodeGroup) -> None:
        for index, sNode in enumerate(sNodeGroup.sNodeList):
            self.visit_SNode(json, f"{path}.sNodeList[{index}]", sNode)

    def visit_SNodeNode(self, json: SemTKJSON, path: str, sNode: SNodeNode) -> None:
        pass

    def visit_SNodeProp(self, json: SemTKJSON, path: str, sNodeProp: SNodeProp) -> None:
        pass

    def visit_SemTKJSON(self, json: SemTKJSON) -> None:
        self.visit_ImportSpec(json, "importSpec", json.importSpec)
        self.visit_SNodeGroup(json, "sNodeGroup", json.sNodeGroup)
        self.visit_SparqlConn(json, "sparqlConn", json.sparqlConn)

    def visit_SparqlConn(self, json: SemTKJSON, path: str, sparqlConn: SparqlConn) -> None:
        # self.visit_SparqlConn(json.sparqlConn)
        pass


def visit_some_SemTKJSON(json: SemTKJSON) -> None:
    visitor = DefaultSemTKVisitor()
    json.accept(visitor)


class ModelOrData(BaseModel):
    type: str
    url: str
    graph: str


class SNodeProp(BaseModel):
    KeyName: str
    ValueType: str
    relationship: str
    UriRelationship: str
    Constraints: str
    fullURIName: str
    SparqlID: str
    isReturned: bool
    optMinus: int
    isRuntimeConstrained: bool
    instanceValues: List[str]
    isMarkedForDeletion: bool


class SNodeNode(BaseModel):
    SnodeSparqlIDs: List[str]
    OptionalMinus: List[int]
    Qualifiers: List[str]
    DeletionMarkers: List[bool]
    KeyName: str
    ValueType: str
    UriValueType: str
    ConnectBy: str
    Connected: bool
    UriConnectBy: str


class SNode(BaseModel):
    deletionMode: str
    fullURIName: str
    instanceValue: Optional[str]
    isReturned: bool
    isRuntimeConstrained: bool
    nodeList: List[SNodeNode]
    NodeName: str
    propList: List[SNodeProp]
    SparqlID: str
    subClassNames: Optional[List[str]]
    valueConstraint: str


class SparqlConn(BaseModel):
    name: str
    domain: str
    enableOwlImports: bool
    model: List[ModelOrData]
    data: List[ModelOrData]


class SNodeGroup(BaseModel):
    version: int
    limit: int
    offset: int
    sNodeList: List[SNode]
    orderBy: List[Dict[str, str]]  # not sure what goes in there
    unionHash: Optional[Dict[str, List[str]]]


class Mapping(BaseModel):
    colId: str
    colName: Optional[str]
    transformList: Optional[List[str]]


class ImportSpecProp(BaseModel):
    mapping: List[Mapping]
    URILookup: Optional[List[str]]
    URIRelation: str


class ImportSpecNode(BaseModel):
    sparqlID: str
    type: str
    URILookupMode: str
    mapping: List[Mapping]
    props: List[ImportSpecProp]


class Transform(BaseModel):
    arg1: str
    arg2: str
    name: str
    transId: str
    transType: str


class DataValidator(BaseModel):
    colName: str
    gt: Optional[int]
    gte: Optional[int]
    lt: Optional[int]
    lte: Optional[int]
    mustExist: Optional[bool]
    ne: Optional[int]
    notEmpty: Optional[bool]
    type: Optional[str]
    regexMatches: Optional[str]
    regexNoMatch: Optional[str]


class Texts(BaseModel):
    textId: str
    text: str


class ImportSpec(BaseModel):
    version: int
    baseURI: str
    columns: List[Mapping]
    dataValidator: List[DataValidator]
    texts: List[Texts]
    transforms: List[Transform]
    nodes: List[ImportSpecNode]


class SemTKJSON(BaseModel):
    version: Optional[int]
    sparqlConn: SparqlConn
    sNodeGroup: SNodeGroup
    importSpec: ImportSpec

    def accept(self, visitor: SemTKVisitor) -> None:
        visitor.visit_SemTKJSON(self)
