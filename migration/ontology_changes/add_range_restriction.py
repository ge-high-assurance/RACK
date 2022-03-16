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
from ontology_changes.range_restriction import RangeRestriction


@dataclass
class AddRangeRestriction(OntologyChange):
    """
    Represents the addition of a range restriction to some property for some
    choice of domain.
    """

    domain_name_space: NameSpace
    domain_class: str
    prop_name_space: NameSpace
    prop_name: str
    restriction: RangeRestriction

    def text_description(self) -> str:
        domain = stylize_class(get_uri(self.domain_name_space, self.domain_class))
        prop = stylize_property(get_uri(self.prop_name_space, self.prop_name))
        restriction = self.restriction.text_description()
        return f"Restricted range of `{domain}`'s `{prop}` to `{restriction}`."

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        pass
