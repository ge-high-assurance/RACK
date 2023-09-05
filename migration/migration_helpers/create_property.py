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

from typing import List, List

from migration_helpers.name_space import NameSpace
from ontology_changes import (
    AddRangeRestriction,
    ChangeCardinality,
    CreateProperty,
)
from ontology_changes.cardinality import Cardinality
from ontology_changes.ontology_change import OntologyChange
from ontology_changes.range_restriction import RangeRestriction

def create_property_with_cardinality_and_range(
    namespace: NameSpace,
    class_id: str,
    property_id: str,
    range: RangeRestriction,
    cardinality: Cardinality,
) -> List[OntologyChange]:
    return [
        CreateProperty(
            name_space=namespace,
            class_id=class_id,
            property_id=property_id,
        ),
        AddRangeRestriction(
            domain_name_space=namespace,
            domain_class=class_id,
            prop_name_space=namespace,
            prop_name=property_id,
            restriction=range,
        ),
        ChangeCardinality(
            name_space=namespace,
            class_id=class_id,
            property_id=property_id,
            to_cardinality=cardinality,
        ),

    ]
