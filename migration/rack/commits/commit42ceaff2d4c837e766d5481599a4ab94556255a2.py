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
from ontology_changes.freeform_notes import FreeformNotes

commit = Commit(
    number="42ceaff2d4c837e766d5481599a4ab94556255a2",
    changes=[
        FreeformNotes(text="Removed inverse relationships for SYSTEM#source and SYSTEM#destination."),
    ],
)
