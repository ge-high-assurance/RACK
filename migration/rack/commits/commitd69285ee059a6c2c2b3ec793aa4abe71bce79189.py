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

AGENTS = rack("AGENTS")
PROV_S = rack("PROV-S")

commit: Commit = {
    "number": "d69285ee059a6c2c2b3ec793aa4abe71bce79189",
    "changes": [
        # AGENTS.sadl
        RenameProperty(
            from_name_space=AGENTS,
            from_name="name",
            to_name_space=PROV_S,
            to_name="agentName",
        ),
        # AddedProperty toolVersion?
    ],
}
