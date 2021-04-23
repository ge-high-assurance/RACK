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
    stylize_class,
    OntologyChange,
)


@dataclass
class ChangeClassIsATypeOf(OntologyChange):
    """
    Represents an ontology change from:

    class_id is a type of from_class_id.

    to:

    class_id is a type of to_class_id.
    """

    name_space: NameSpace
    class_id: str

    from_name_space: NameSpace
    from_class_id: str

    to_name_space: NameSpace
    to_class_id: str

    def text_description(self) -> str:
        class_str = stylize_class(get_uri(self.name_space, self.class_id))
        from_str = stylize_class(get_uri(self.name_space, self.from_class_id))
        to_str = stylize_class(get_uri(self.name_space, self.to_class_id))
        return f"Class {class_str} used to be a type of {from_str}, is now a type of {to_str}."

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        json.accept(MigrationVisitor(self))


class MigrationVisitor(semtk.DefaultSemTKVisitor):
    def __init__(self, data: ChangeClassIsATypeOf):
        self.data = data

    # TODO
