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
)


@dataclass
class RenameClass(OntologyChange):
    """
    Represents an ontology change where a class has been renamed, also
    encompassing cases where a class has been superseded.
    """

    from_name_space: NameSpace
    from_name: str
    to_name_space: NameSpace
    to_name: str

    def text_description(self) -> str:
        from_class = stylize_class(get_uri(self.from_name_space, self.from_name))
        to_class = stylize_class(get_uri(self.to_name_space, self.to_name))
        return f"Class {from_class} was renamed to {to_class}."

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        log_apply_change(self.text_description())
        json.accept(MigrationVisitor(self))


# TODO: probably need to refine subClassNames when the type narrows
class MigrationVisitor(semtk.DefaultSemTKVisitor):
    def __init__(self, data: RenameClass):
        self.data = data
        self.from_uri = get_uri(data.from_name_space, data.from_name)
        self.to_uri = get_uri(data.to_name_space, data.to_name)
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
                f"Renaming class {self.from_str} to {self.to_str} in {stylize_json(path)}"
            )
            cbk()
            return self.to_uri
        return value

    def visit_ImportSpecNode(
        self, json: semtk.SemTKJSON, path: str, node: semtk.ImportSpecNode
    ) -> None:
        super().visit_ImportSpecNode(json, path, node)
        node.type = self.rename(node.type, f"{path}.type")

    def visit_SNode(self, json: semtk.SemTKJSON, path: str, node: semtk.SNode) -> None:
        super().visit_SNode(json, path, node)

        # additional nearby changes
        def on_change() -> None:
            log_additional_change(f"{path}.NodeName", node.NodeName, self.data.to_name)
            node.NodeName = self.data.to_name

        node.fullURIName = self.rename(
            node.fullURIName, f"{path}.fullURIName", on_change
        )

        if node.subClassNames is not None:
            for index, subClassName in enumerate(node.subClassNames):
                node.subClassNames[index] = self.rename(
                    subClassName, f"{path}.subClassNames[{index}]"
                )

    def visit_SNodeNode(
        self, json: semtk.SemTKJSON, path: str, node: semtk.SNodeNode
    ) -> None:
        super().visit_SNodeNode(json, path, node)
        node.UriValueType = self.rename(node.UriValueType, f"{path}.UriValueType")
