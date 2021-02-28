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

from migration_helpers.name_space import rack
from ontology_changes import ChangeIsATypeOf, ChangePropertyRange, Commit

PROV_S = rack("PROV-S")
TESTING = rack("TESTING")

commit: Commit = {
    "number": "1c25a1b83e76eb9ca94b86402bcd335f79f80528",
    "changes": [
        # TESTING.sadl
        ChangeIsATypeOf(
            class_id="TEST",
            property_id="verifies",
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
        ChangePropertyRange(
            prop_name_space=TESTING,
            prop_name="confirms",
            from_name_space=PROV_S,
            from_range="ENTITY",
            to_name_space=TESTING,
            to_range="TEST",
        ),
    ],
}
