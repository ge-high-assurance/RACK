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

# from migration_helpers.name_space import rack
from ontology_changes import Commit, ChangeIsATypeOf

commit = Commit(
    number="2e079bb2a32b3cc1b3153d44ad0c21e27507937f",
    changes=[
        # TESTING.sadl
        ChangeIsATypeOf(
            class_id="TEST",
            property_id="verifies",
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
        ChangeIsATypeOf(
            class_id="TEST_RESULT",
            property_id="confirms",
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
    ],
)
