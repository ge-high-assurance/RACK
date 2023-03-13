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
    number="96c4d5d8672bbb8e8b5ff44ea928638092f91b82",
    changes=[
        # PROV-S.sadl
        FreeformNotes("Minor change")
    ],
)
