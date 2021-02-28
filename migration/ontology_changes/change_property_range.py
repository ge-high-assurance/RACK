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

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        json.accept(MigrationVisitor(self))


class MigrationVisitor(semtk.DefaultSemTKVisitor):
    def __init__(self, data: ChangePropertyRange):
        self.data = data

        self.prop_uri = get_uri(self.data.prop_name_space, self.data.prop_name)
        self.from_uri = get_uri(self.data.from_name_space, self.data.from_range)
        self.to_uri = get_uri(self.data.to_name_space, self.data.to_range)

    def visit_SNodeNode(
        self, json: semtk.SemTKJSON, path: str, node: semtk.SNodeNode
    ) -> None:
        super().visit_SNodeNode(json, path, node)

        if node.UriConnectBy == self.prop_uri:
            if node.UriValueType == self.from_uri:
                from_class = stylize_class(self.from_uri)
                to_class = stylize_class(self.to_uri)
                prop = stylize_property(self.prop_uri)
                log_change(
                    f"Changing range of property {prop} from {from_class} to {to_class} in {stylize_json(path)}"
                )
                node.UriValueType = self.to_uri
                node.ValueType = self.data.to_range
            # else WARNING?

            # cleanup
            for sparqlID in node.SnodeSparqlIDs:
                for index, importSpecNode in enumerate(json.importSpec.nodes):
                    if importSpecNode.sparqlID == sparqlID:
                        importSpecNode.type = self.to_uri
                        log_change("TODO")
                        # TODO: self.rename with log
                for index, sNode in enumerate(json.sNodeGroup.sNodeList):
                    if sNode.SparqlID == sparqlID:
                        sNode.fullURIName = self.to_uri
                        sNode.NodeName = self.data.to_range
                        # TODO: clean subClassNames
                        log_change("TODO")
