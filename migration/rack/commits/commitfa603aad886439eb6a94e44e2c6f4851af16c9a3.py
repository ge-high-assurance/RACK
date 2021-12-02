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

from ontology_changes import Commit, ChangePropertyIsATypeOf, DeleteClass
from rack.namespaces.rack_ontology import CONFIDENCE, PROV_S

commit = Commit(
    number="fa603aad886439eb6a94e44e2c6f4851af16c9a3",
    changes=[
        # CONFIDENCE.sadl
        ChangePropertyIsATypeOf(
            name_space=CONFIDENCE,
            class_id="CONFIDENCE_ASSESSMENT",
            property_id="assesses",
            from_name_space=PROV_S,
            from_property_id="wasDerivedFrom",
            to_name_space=PROV_S,
            to_property_id="wasImpactedBy",
        ),
        DeleteClass(
            name_space=CONFIDENCE,
            class_id="Probability",
        ),
        # Technically the ranges of belief, disbelief, uncertainty have changed.
    ],
)
