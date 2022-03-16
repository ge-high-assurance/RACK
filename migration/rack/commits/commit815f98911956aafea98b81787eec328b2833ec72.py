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
    number="815f98911956aafea98b81787eec328b2833ec72",
    changes=[
        FreeformNotes("Change outside of RACK core: added MITRE-CWE.sadl"),
    ],
)
