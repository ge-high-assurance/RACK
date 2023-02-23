# Copyright (c) 2023, Galois, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

from ontology_changes import Commit, FreeformNotes

commit = Commit(
    number="8a01ff1b53e0b4979f0120a362b8fd3776a6586c",
    changes=[
        # RESOLUTIONS.sadl
        FreeformNotes("Alias entityResolution changed to EntityResolution")
    ],
)
