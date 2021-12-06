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
from ontology_changes.utils import delete_property_from_node_list

import semtk
from migration_helpers.name_space import NameSpace, get_uri

from ontology_changes.ontology_change import (
    OntologyChange,
    log_additional_deletion,
    log_apply_change,
    log_change,
    stylize_json,
    stylize_property,
)


@dataclass
class DeleteProperty(OntologyChange):
    """
    Represents an ontology change where a property is no longer part of the
    ontology.
    """

    name_space: NameSpace
    property_id: str

    def text_description(self) -> str:
        prop = stylize_property(get_uri(self.name_space, self.property_id))
        return f"Property {prop} was deleted."

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        log_apply_change(self.text_description())
        json.accept(MigrationVisitor(self))


class MigrationVisitor(semtk.DefaultSemTKVisitor):
    def __init__(self, data: DeleteProperty):
        self.data = data
        self.uri = get_uri(self.data.name_space, self.data.property_id)

    def visit_SNode(self, json: semtk.SemTKJSON, path: str, sNode: semtk.SNode) -> None:
        super().visit_SNode(json, path, sNode)

        delete_property_from_node_list(json, path, self.uri, sNode.nodeList)

        # unconditional cleanup
        for index, prop in enumerate(sNode.propList):
            if prop.UriRelationship == self.uri:
                this_path = stylize_json(f"{path}.propList[{index}]")
                field = stylize_json("UriRelationship")
                log_change(f"Deleting {this_path} because it has {field} = {self.uri}")
                sNode.propList.pop(index)
