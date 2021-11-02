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

from migration_helpers.name_space import rack
from ontology_changes import Commit, RemovePropertyRestriction

CONFIDENCE = rack("CONFIDENCE")

commit = Commit(
    number="3908d68df1143537a49e1df9556dae8066b0e25f",
    changes=[
        # CONFIDENCE.sadl
        RemovePropertyRestriction(
            prop_name_space=CONFIDENCE,
            prop_name="belief",
            prop_restriction="[0, 1]",
        ),
        RemovePropertyRestriction(
            prop_name_space=CONFIDENCE,
            prop_name="disbelief",
            prop_restriction="[0, 1]",
        ),
        RemovePropertyRestriction(
            prop_name_space=CONFIDENCE,
            prop_name="uncertainty",
            prop_restriction="[0, 1]",
        ),
    ],
)
