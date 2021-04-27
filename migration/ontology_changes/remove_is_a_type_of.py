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
from ontology_changes.ontology_change import stylize_property, OntologyChange


@dataclass
class RemoveIsATypeOf(OntologyChange):
    """
    Represents an ontology change where:

    property_id is a type of from_property_id.

    has been removed.
    """

    name_space: NameSpace
    class_id: str
    property_id: str
    range_id: str

    def text_description(self) -> str:
        prop = stylize_property(self.property_id)
        range_str = stylize_property(self.range_id)
        return f"Property {prop} used to be a type of {range_str}, no longer."

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        json.accept(MigrationVisitor(self))


class MigrationVisitor(semtk.DefaultSemTKVisitor):
    def __init__(self, data: RemoveIsATypeOf):
        self.data = data

    # TODO?
