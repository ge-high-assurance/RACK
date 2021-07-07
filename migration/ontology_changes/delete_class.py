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
    log_apply_change,
    stylize_property,
)


@dataclass
class DeleteClass(OntologyChange):
    """
    Represents an ontology change where a class is no longer part of the
    ontology.
    """

    name_space: NameSpace
    class_id: str

    def text_description(self) -> str:
        class_ = stylize_property(get_uri(self.name_space, self.class_id))
        return f"Class {class_} was deleted."

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        log_apply_change(self.text_description())
        json.accept(MigrationVisitor(self))


class MigrationVisitor(semtk.DefaultSemTKVisitor):
    def __init__(self, data: DeleteClass):
        self.data = data
        self.uri = get_uri(self.data.name_space, self.data.class_id)

    # TODO: this can probably be implemented
