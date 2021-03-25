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
from ontology_changes import Commit, RenameProperty

SYSTEM = rack("SYSTEM")

commit: Commit = {
    "number": "49db2186193711fa5d2609af0c6c30f56ea6ebbd",
    "changes": [
        # SYSTEM.sadl
        RenameProperty(
            from_name_space=SYSTEM,
            from_name="functionalityOverview",
            to_name_space=SYSTEM,
            to_name="function",
        )
    ],
}
