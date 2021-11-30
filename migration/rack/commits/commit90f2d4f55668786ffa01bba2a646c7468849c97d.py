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
    AtMost,
    Commit,
    ChangeCardinality,
    ChangePropertyIsATypeOf,
    RenameClass,
    SingleValue,
)

commit = Commit(
    number="90f2d4f55668786ffa01bba2a646c7468849c97d",
    changes=[
        # ANALYSIS.sadl
        RenameClass(
            from_name_space=ANALYSIS,
            from_name="ANALYSIS_REPORT",
            to_name_space=ANALYSIS,
            to_name="ANALYSIS_OUTPUT",
        ),
        ChangeCardinality(
            name_space=ANALYSIS,
            class_id="ANALYSIS_OUTPUT",
            property_id="result",
            to_cardinality=AtMost(1),
        ),
        ChangePropertyIsATypeOf(
            name_space=ANALYSIS,
            class_id="ANALYSIS_OUTPUT",
            property_id="analyzes",
            from_name_space=PROV_S,
            from_property_id="wasDerivedFrom",
            to_name_space=PROV_S,
            to_property_id="wasImpactedBy",
        ),
        ChangeCardinality(
            name_space=ANALYSIS,
            class_id="ANALYSIS_ANNOTATION",
            property_id="fromReport",
            to_cardinality=SingleValue(),
        ),
        ChangeCardinality(
            name_space=ANALYSIS,
            class_id="ANALYSIS_ANNOTATION",
            property_id="annotationType",
            to_cardinality=SingleValue(),
        ),
    ],
)
