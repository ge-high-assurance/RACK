# Copyright (c) 2021, Galois, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

from ontology_changes import (
    Commit,
    ChangeCardinality,
    ChangePropertyIsATypeOf,
    ChangePropertyRange,
    SingleValue,
)
from rack.namespaces.rack_ontology import PROV_S, TESTING

commit = Commit(
    number="2e079bb2a32b3cc1b3153d44ad0c21e27507937f",
    changes=[
        # TESTING.sadl
        ChangePropertyIsATypeOf(
            name_space=TESTING,
            class_id="TEST",
            property_id="verifies",
            from_name_space=PROV_S,
            from_property_id="wasDerivedFrom",
            to_name_space=PROV_S,
            to_property_id="wasImpactedBy",
        ),
        ChangeCardinality(
            name_space=TESTING,
            class_id="TEST_RESULT",
            property_id="result",
            to_cardinality=SingleValue(),
        ),
        ChangePropertyRange(
            prop_name_space=TESTING,
            prop_name="confirms",
            from_name_space=PROV_S,
            from_range="ENTITY",
            to_name_space=TESTING,
            to_range="TEST",
        ),
        ChangePropertyIsATypeOf(
            name_space=TESTING,
            class_id="TEST_RESULT",
            property_id="confirms",
            from_name_space=PROV_S,
            from_property_id="wasDerivedFrom",
            to_name_space=PROV_S,
            to_property_id="wasImpactedBy",
        ),
    ],
)
