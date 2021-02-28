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

from ontology_changes import ChangeIsATypeOf, Commit


commit: Commit = {
    "number": "05a03cd687e3bdce425794763e0957d3ccaf8ff0",
    "changes": [
        # HAZARD.sadl
        ChangeIsATypeOf(
            class_id="HAZARD",
            property_id="source",  # H:source?
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
        # RemoveIsATypeOf on identified?
    ],
}
