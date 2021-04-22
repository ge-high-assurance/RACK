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

from ontology_changes import Commit, ChangeIsATypeOf

commit = Commit(
    number="fa603aad886439eb6a94e44e2c6f4851af16c9a3",
    changes=[
        # CONFIDENCE.sadl
        ChangeIsATypeOf(
            class_id="CONFIDENCE_ASSESSMENT",
            property_id="assesses",
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
        # RemoveClass Probability
    ],
)
