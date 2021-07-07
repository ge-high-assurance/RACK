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

from ontology_changes.cardinality import Cardinality
from ontology_changes.ontology_change import (
    OntologyChange,
    log_apply_change,
    stylize_class,
    stylize_property,
)


@dataclass
class ChangeCardinality(OntologyChange):
    """
    Represents some change of cardinality.  We don't use this for migration yet.
    """

    name_space: NameSpace
    class_id: str
    property_id: str
    to_cardinality: Cardinality

    def text_description(self) -> str:
        prop = stylize_property(get_uri(self.name_space, self.property_id))
        class_str = stylize_class(get_uri(self.name_space, self.class_id))
        return f"Property {prop} (ranging over {class_str}) changed cardinality to {self.to_cardinality.text_description()}."

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        log_apply_change(self.text_description())
        # Currently, do nothing, uncomment if you want to migrate:
        # json.accept(MigrationVisitor(self))


# class MigrationVisitor(semtk.DefaultSemTKVisitor):
#     def __init__(self, data: ChangeCardinality):
#         self.data = data
