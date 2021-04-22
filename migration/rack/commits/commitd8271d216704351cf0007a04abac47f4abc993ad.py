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
    number="d8271d216704351cf0007a04abac47f4abc993ad",
    changes=[
        # REQUIREMENTS.sadl
        ChangeIsATypeOf(
            class_id="REQUIREMENT",
            property_id="governs",
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
        ChangeIsATypeOf(
            class_id="REQUIREMENT",
            property_id="satisfies",
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
        ChangeIsATypeOf(
            class_id="REQUIREMENT",
            property_id="mitigates",
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
        ChangeIsATypeOf(
            class_id="DATA_DICTIONARY_TERM",
            property_id="providedBy",
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
        ChangeIsATypeOf(
            class_id="DATA_DICTIONARY_TERM",
            property_id="consumedBy",
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
    ],
)
