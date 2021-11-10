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

        for index, node in enumerate(sNode.nodeList):
            if node.UriConnectBy == self.uri:
                this_path = f"{path}.nodeList[{index}].UriConnectBy"
                log_change(
                    f"Deleting property {stylize_property(self.uri)} in {stylize_json(this_path)}"
                )
                sNode.nodeList.pop(index)

                # cleanup
                for sparqlID in node.SnodeSparqlIDs:
                    if json.importSpec is None: continue
                    for index, importSpecNode in enumerate(json.importSpec.nodes):
                        if importSpecNode.sparqlID == sparqlID:
                            field = stylize_json("sparqlID")
                            log_additional_deletion(
                                f"importSpec.nodes[{index}]",
                                f"it has {field} = {sparqlID}",
                            )
                            json.importSpec.nodes.pop(index)
                    for index, sNode in enumerate(json.sNodeGroup.sNodeList):
                        if sNode.SparqlID == sparqlID:
                            log_additional_deletion(
                                f"sNodeGroup.sNodeList[{index}]",
                                f"it has {field} = {sparqlID}",
                            )
                            json.sNodeGroup.sNodeList.pop(index)

            # unconditional cleanup
            for index, prop in enumerate(sNode.propList):
                if prop.UriRelationship == self.uri:
                    this_path = stylize_json(f"{path}.propList[{index}]")
                    field = stylize_json("UriRelationship")
                    log_change(
                        f"Deleting {this_path} because it has {field} = {self.uri}"
                    )
                    sNode.propList.pop(index)
