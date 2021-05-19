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

# from migration_helpers.name_space import rack
from ontology_changes import Commit, FreeformNotes

commit = Commit(
    number="0a89f70ff929380269a79fe2fc82f5dde346ed8c",
    changes=[
        FreeformNotes(
            text="SACM-S.sadl was removed from the RACK-Ontology and moved to LM-Ontology",
        )
    ],
)
