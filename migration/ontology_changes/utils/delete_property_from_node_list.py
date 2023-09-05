# Copyright (c) 2021, Galois, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).


from typing import List

from semtk import SemTKJSON, SNodeNode

from ontology_changes.ontology_change import (
    log_additional_deletion,
    log_change,
    stylize_json,
    stylize_property,
)


def delete_property_from_node_list(
    json: SemTKJSON,
    path: str,
    propertyURI: str,
    nodeList: List[SNodeNode],
) -> None:

    for index, node in enumerate(nodeList):
        if node.UriConnectBy == propertyURI:
            this_path = f"{path}.nodeList[{index}].UriConnectBy"
            log_change(
                f"Deleting property {stylize_property(propertyURI)} in {stylize_json(this_path)}"
            )
            nodeList.pop(index)

            # cleanup
            for sparqlID in node.SnodeSparqlIDs:
                if json.importSpec is None:
                    continue
                field = stylize_json("sparqlID")
                for index, importSpecNode in enumerate(json.importSpec.nodes):
                    if importSpecNode.sparqlID == sparqlID:
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
