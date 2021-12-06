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
from ontology_changes.utils import delete_property_from_node_list

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
class SubsumeProperty(OntologyChange):
    """
    Represents an ontology change where a property has been removed with the
    intent that another existing property is more general and should be used
    instead.
    """

    from_name_space: NameSpace
    from_class: str
    from_name: str

    to_name_space: NameSpace
    to_class: str
    to_name: str

    def text_description(self) -> str:
        from_prop = stylize_property(get_uri(self.from_name_space, self.from_name))
        to_prop = stylize_property(get_uri(self.to_name_space, self.to_name))
        from_class = stylize_class(get_uri(self.from_name_space, self.from_class))
        to_class = stylize_class(get_uri(self.to_name_space, self.to_class))
        return f"Property {from_prop} (ranging over {from_class}) has been subsumed by {to_prop} (ranging over {to_class})."

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        log_apply_change(self.text_description())
        json.accept(MigrationVisitor(self))


class MigrationVisitor(semtk.DefaultSemTKVisitor):
    def __init__(self, data: SubsumeProperty):
        self.data = data
        self.from_class_uri = get_uri(self.data.from_name_space, self.data.from_class)
        self.to_class_uri = get_uri(self.data.to_name_space, self.data.to_class)
        self.from_uri = get_uri(self.data.from_name_space, self.data.from_name)
        self.to_uri = get_uri(self.data.to_name_space, self.data.to_name)
        self.from_str = stylize_property(self.from_uri)
        self.to_str = stylize_property(self.to_uri)

    def subsume(
        self, value: str, path: str, cbk: Callable[[], None] = lambda: None
    ) -> str:
        """
        Checks whether the given value needs to be subsumed, and returns the
        old or new value it should be set to.  Logs the change if it happens.
        """
        if value == self.from_uri:
            log_change(
                f"Subsuming property {self.from_str} with {self.to_str} in {stylize_json(path)}"
            )
            cbk()
            return self.to_uri
        return value

    def visit_ImportSpecProp(
        self, json: semtk.SemTKJSON, path: str, prop: semtk.ImportSpecProp
    ) -> None:
        super().visit_ImportSpecProp(json, path, prop)

        prop.URIRelation = self.subsume(prop.URIRelation, f"{path}.URIRelation")

    def visit_SNodeProp(
        self, json: semtk.SemTKJSON, path: str, prop: semtk.SNodeProp
    ) -> None:
        super().visit_SNodeProp(json, path, prop)

        def on_change() -> None:
            if prop.KeyName is not None:
                log_additional_change(f"{path}.KeyName", prop.KeyName, self.data.to_name)
                prop.KeyName = self.data.to_name

        prop.UriRelationship = self.subsume(
            prop.UriRelationship, f"{path}.UriRelationship", on_change
        )

    def visit_SNode(self, json: semtk.SemTKJSON, path: str, sNode: semtk.SNode) -> None:
        # We can delete references to this property in the node list because it
        # no longer exists.
        delete_property_from_node_list(json, path, self.from_uri, sNode.nodeList)
