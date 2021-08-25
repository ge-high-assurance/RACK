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

from dataclasses import dataclass
from typing import Callable

import semtk
from migration_helpers.name_space import NameSpace, get_uri

from ontology_changes.ontology_change import (
    OntologyChange,
    log_additional_change,
    log_apply_change,
    log_change,
    stylize_class,
    stylize_json,
    stylize_property,
)


@dataclass
class ChangePropertyRange(OntologyChange):
    """
    Represents an ontology change from:

    property_id describes X values of type A.

    to:

    property_id describes X values of type B.
    """

    prop_name_space: NameSpace
    prop_name: str

    from_name_space: NameSpace
    from_range: str

    to_name_space: NameSpace
    to_range: str

    def text_description(self) -> str:
        prop = stylize_property(get_uri(self.prop_name_space, self.prop_name))
        from_range = stylize_class(get_uri(self.from_name_space, self.from_range))
        to_range = stylize_class(get_uri(self.to_name_space, self.to_range))
        return f"Range of property {prop} was changed from {from_range} to {to_range}."

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        log_apply_change(self.text_description())
        json.accept(MigrationVisitor(self))


class MigrationVisitor(semtk.DefaultSemTKVisitor):
    def __init__(self, data: ChangePropertyRange):
        self.data = data

        self.prop_uri = get_uri(self.data.prop_name_space, self.data.prop_name)
        self.from_uri = get_uri(self.data.from_name_space, self.data.from_range)
        self.to_uri = get_uri(self.data.to_name_space, self.data.to_range)

        self.prop_str = stylize_property(self.prop_uri)
        self.from_str = stylize_class(self.from_uri)
        self.to_str = stylize_class(self.to_uri)

    def rename(
        self, value: str, path: str, cbk: Callable[[], None] = lambda: None
    ) -> str:
        """
        Checks whether the given value needs to be renamed, and returns the
        old or new value it should be set to.  Logs the change if it happens.
        """
        if value == self.from_uri:
            log_change(
                f"Changing property range from {self.from_str} to {self.to_str} in {stylize_json(path)}"
            )
            cbk()
            return self.to_uri
        return value

    def visit_SNodeNode(
        self, json: semtk.SemTKJSON, path: str, node: semtk.SNodeNode
    ) -> None:
        super().visit_SNodeNode(json, path, node)

        if node.UriConnectBy == self.prop_uri:

            def on_change() -> None:
                log_additional_change(
                    f"{path}.ValueType", node.ValueType, self.data.to_range
                )
                node.ValueType = self.data.to_range

            node.UriValueType = self.rename(
                node.UriValueType, f"{path}.UriValueType", on_change
            )

            # cleanup
            for sparqlID in node.SnodeSparqlIDs:
                if json.importSpec is None: continue
                for index, importSpecNode in enumerate(json.importSpec.nodes):
                    if importSpecNode.sparqlID == sparqlID:
                        importSpecNode.type = self.rename(
                            importSpecNode.type, f"importSpec.nodes[{index}].type"
                        )
                for index, sNode in enumerate(json.sNodeGroup.sNodeList):
                    if sNode.SparqlID == sparqlID:

                        def on_fullURIName_change() -> None:
                            log_additional_change(
                                f"sNodeGroup.sNodeList[{index}].NodeName",
                                sNode.NodeName,
                                self.data.to_range,
                            )
                            sNode.NodeName = self.data.to_range
                            # TODO: clean subClassNames

                        sNode.fullURIName = self.rename(
                            sNode.fullURIName,
                            f"sNodeGroup.sNodeList[{index}].fullURIName",
                            on_fullURIName_change,
                        )
