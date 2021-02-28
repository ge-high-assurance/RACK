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
from ontology_changes import ChangeIsATypeOf, Commit, RenameClass

ANALYSIS = rack("ANALYSIS")


commit: Commit = {
    "number": "90f2d4f55668786ffa01bba2a646c7468849c97d",
    "changes": [
        # ANALYSIS.sadl
        RenameClass(
            from_name_space=ANALYSIS,
            from_name="ANALYSIS_REPORT",
            to_name_space=ANALYSIS,
            to_name="ANALYSIS_OUTPUT",
        ),
        ChangeIsATypeOf(
            class_id="ANALYSIS_OUTPUT",
            property_id="analyzes",
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
    ],
}
