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
class RenameProperty(OntologyChange):
    """
    Represents an ontology change where a property has been renamed.  This also
    encompasses cases where a property has effectively been moved across
    classes.
    """

    from_name_space: NameSpace
    from_class: str
    from_name: str

    to_name_space: NameSpace
    to_class: str
    to_name: str

    def text_description(self) -> str:
        from_prop = stylize_property(get_uri(self.from_name_space, self.from_name))
        to_prop = stylize_property(get_uri(self.from_name_space, self.from_name))
        from_class = stylize_class(get_uri(self.from_name_space, self.from_class))
        to_class = stylize_class(get_uri(self.from_name_space, self.to_class))
        return f"Property {from_prop} (ranging over {from_class}) was renamed to {to_prop} (ranging over {to_class})."

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        log_apply_change(self.text_description())
        json.accept(MigrationVisitor(self))


class MigrationVisitor(semtk.DefaultSemTKVisitor):
    def __init__(self, data: RenameProperty):
        self.data = data
        self.from_uri = get_uri(self.data.from_name_space, self.data.from_name)
        self.to_uri = get_uri(self.data.to_name_space, self.data.to_name)
        self.from_str = stylize_property(self.from_uri)
        self.to_str = stylize_property(self.to_uri)

    def rename(
        self, value: str, path: str, cbk: Callable[[], None] = lambda: None
    ) -> str:
        """
        Checks whether the given value needs to be renamed, and returns the
        old or new value it should be set to.  Logs the change if it happens.
        """
        if value == self.from_uri:
            log_change(
                f"Renaming property {self.from_str} to {self.to_str} in {stylize_json(path)}"
            )
            cbk()
            return self.to_uri
        return value

    def visit_ImportSpecProp(
        self, json: semtk.SemTKJSON, path: str, prop: semtk.ImportSpecProp
    ) -> None:
        super().visit_ImportSpecProp(json, path, prop)

        prop.URIRelation = self.rename(prop.URIRelation, f"{path}.URIRelation")

    def visit_SNodeNode(
        self, json: semtk.SemTKJSON, path: str, node: semtk.SNodeNode
    ) -> None:
        super().visit_SNodeNode(json, path, node)

        # additional nearby changes
        def on_change() -> None:
            if node.ConnectBy != self.data.to_name:
                log_additional_change(
                    f"{path}.ConnectBy", node.ConnectBy, self.data.to_name
                )
                node.ConnectBy = self.data.to_name
            if node.KeyName != self.data.to_name:
                log_additional_change(
                    f"{path}.KeyName", node.KeyName, self.data.to_name
                )
                node.KeyName = self.data.to_name

        node.UriConnectBy = self.rename(
            node.UriConnectBy, f"{path}.UriConnectBy", on_change
        )

    def visit_SNodeProp(
        self, json: semtk.SemTKJSON, path: str, prop: semtk.SNodeProp
    ) -> None:
        super().visit_SNodeProp(json, path, prop)

        def on_change() -> None:
            log_additional_change(f"{path}.KeyName", prop.KeyName, self.data.to_name)
            prop.KeyName = self.data.to_name

        prop.UriRelationship = self.rename(
            prop.UriRelationship, f"{path}.UriRelationship", on_change
        )
