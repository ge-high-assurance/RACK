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

from ontology_changes import Commit

from ontology_changes.add_range_restriction import AddRangeRestriction
from ontology_changes.range_restriction import OnlyValuesOfType, RangeRestriction

from rack.namespaces.rack_ontology import (
    ANALYSIS,
    CONFIDENCE,
    HAZARD,
    PROV_S,
    REQUIREMENTS,
    REVIEW,
    SYSTEM,
    TESTING,
)

commit = Commit(
    number="b865c663351f39c275f5fb9985b681a6ae432cac",
    changes=[
        # ANALYSIS.sadl
        AddRangeRestriction(
            domain_name_space=ANALYSIS,
            domain_class="ANALYSIS_OUTPUT",
            prop_name_space=PROV_S,
            prop_name="wasGeneratedBy",
            restriction=OnlyValuesOfType(ANALYSIS, "ANALYSIS"),
        ),
        # CONFIDENCE.sadl
        AddRangeRestriction(
            domain_name_space=CONFIDENCE,
            domain_class="CONFIDENCE_ASSESSMENT",
            prop_name_space=PROV_S,
            prop_name="wasGeneratedBy",
            restriction=OnlyValuesOfType(CONFIDENCE, "ASSESSING_CONFIDENCE"),
        ),
        # HAZARD.sadl
        AddRangeRestriction(
            domain_name_space=HAZARD,
            domain_class="HAZARD",
            prop_name_space=PROV_S,
            prop_name="wasGeneratedBy",
            restriction=OnlyValuesOfType(HAZARD, "HAZARD_IDENTIFICATION"),
        ),
        # REQUIREMENTS.sadl
        AddRangeRestriction(
            domain_name_space=REQUIREMENTS,
            domain_class="REQUIREMENT",
            prop_name_space=PROV_S,
            prop_name="wasGeneratedBy",
            restriction=OnlyValuesOfType(REQUIREMENTS, "REQUIREMENT_DEVELOPMENT"),
        ),
        # REVIEW.sadl
        AddRangeRestriction(
            domain_name_space=REVIEW,
            domain_class="REVIEW_LOG",
            prop_name_space=PROV_S,
            prop_name="wasGeneratedBy",
            restriction=OnlyValuesOfType(REVIEW, "REVIEW"),
        ),
        # SYSTEM.sadl
        AddRangeRestriction(
            domain_name_space=SYSTEM,
            domain_class="SYSTEM",
            prop_name_space=PROV_S,
            prop_name="wasGeneratedBy",
            restriction=OnlyValuesOfType(SYSTEM, "SYSTEM_DEVELOPMENT"),
        ),
        # TESTING.sadl
        AddRangeRestriction(
            domain_name_space=TESTING,
            domain_class="TEST",
            prop_name_space=PROV_S,
            prop_name="wasGeneratedBy",
            restriction=OnlyValuesOfType(TESTING, "TEST_DEVELOPMENT"),
        ),
        AddRangeRestriction(
            domain_name_space=TESTING,
            domain_class="TEST_RESULT",
            prop_name_space=PROV_S,
            prop_name="wasGeneratedBy",
            restriction=OnlyValuesOfType(TESTING, "TEST_EXECUTION"),
        ),
    ],
)
