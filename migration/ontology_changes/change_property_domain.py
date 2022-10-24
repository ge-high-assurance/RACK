# Copyright (c) 2022, Galois, Inc.
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
class ChangePropertyDomain(OntologyChange):
    """
    Represents an ontology change from:

    P describes X with values of type A.

    to:

    P describes Y with values of type B.
    """

    prop_name_space: NameSpace
    prop_name: str

    from_name_space: NameSpace
    from_domain: str

    to_name_space: NameSpace
    to_domain: str

    def text_description(self) -> str:
        prop = stylize_property(get_uri(self.prop_name_space, self.prop_name))
        from_domain = stylize_class(get_uri(self.from_name_space, self.from_domain))
        to_domain = stylize_class(get_uri(self.to_name_space, self.to_domain))
        return f"Domain of property {prop} was changed from {from_domain} to {to_domain}."

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        # TODO
        pass
