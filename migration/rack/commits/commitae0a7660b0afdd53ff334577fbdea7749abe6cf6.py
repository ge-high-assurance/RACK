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

commit = Commit(
    number="ae0a7660b0afdd53ff334577fbdea7749abe6cf6",
    changes=[
        # PROV-S.sadl
        # AddedProperty ENTITY wasRevisionOf
        # AddedProperty ENTITY wasImpactedBy
    ],
)
