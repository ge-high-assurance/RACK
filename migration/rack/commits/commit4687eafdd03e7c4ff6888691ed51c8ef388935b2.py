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
    number="4687eafdd03e7c4ff6888691ed51c8ef388935b2",
    changes=[
        FreeformNotes("Added note"),
    ],
)
