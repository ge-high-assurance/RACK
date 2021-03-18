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

from ontology_changes.ontology_change import stylize_property, OntologyChange


@dataclass
class ChangeIsATypeOf(OntologyChange):
    """
    Represents an ontology change from:

    property_id is a type of from_property_id.

    to:

    property_id is a type of to_property_id.
    """

    class_id: str
    property_id: str
    from_property_id: str
    to_property_id: str

    def text_description(self) -> str:
        prop = stylize_property(self.property_id)
        from_str = stylize_property(self.from_property_id)
        to_str = stylize_property(self.to_property_id)
        return f"Property {prop} used to be a type of {from_str}, is now a type of {to_str}"

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        json.accept(MigrationVisitor(self))


class MigrationVisitor(semtk.DefaultSemTKVisitor):
    def __init__(self, data: ChangeIsATypeOf):
        self.data = data

    # TODO
