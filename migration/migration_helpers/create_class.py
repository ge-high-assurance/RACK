
# Copyright (c) 2023, Galois, Inc.
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

from migration_helpers.name_space import NameSpace
from ontology_changes import (
    AddClassIsATypeOf,
    CreateClass,
)
from ontology_changes.cardinality import Cardinality
from ontology_changes.ontology_change import OntologyChange
from ontology_changes.range_restriction import RangeRestriction

def create_class_with_type_of(
    namespace: NameSpace,
    class_id: str,
    type_of_namespace: NameSpace,
    type_of_class: str,
) -> List[OntologyChange]:
    return [
        CreateClass(
            name_space=namespace,
            class_id=class_id,
        ),
        AddClassIsATypeOf(
            name_space=namespace,
            class_id=class_id,
            range_name_space=type_of_namespace,
            range_id=type_of_class,
        ),
    ]
