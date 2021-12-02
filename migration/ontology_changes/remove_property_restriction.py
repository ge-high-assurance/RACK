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
class RemovePropertyRestriction(OntologyChange):
    """
    Represents the removal of some restriction from the range of a property.
    """

    prop_name_space: NameSpace
    prop_name: str
    prop_restriction: str

    def text_description(self) -> str:
        prop = stylize_property(get_uri(self.prop_name_space, self.prop_name))
        return f"Restriction {self.prop_restriction} of property {prop} was removed."

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        pass
