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
    "number": "833ef18f5024fee255f77887de2c8e9bc136e56d",
    "changes": [
        # AGENTS.sadl
        RenameProperty(
            from_name_space=PROV_S,
            from_name="agentName",
            to_name_space=PROV_S,
            to_name="title",
        ),
        # AddedProperty toolVersion?
    ],
}
