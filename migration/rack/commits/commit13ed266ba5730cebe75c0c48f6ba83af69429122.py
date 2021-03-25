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

PROV_S = rack("PROV-S")
REQUIREMENTS = rack("REQUIREMENTS")


commit: Commit = {
    "number": "13ed266ba5730cebe75c0c48f6ba83af69429122",
    "changes": [
        # REQUIREMENTS.sadl
        RenameProperty(
            from_name_space=REQUIREMENTS,
            from_name="text",
            to_name_space=PROV_S,
            to_name="description",
        ),
        RenameProperty(
            from_name_space=REQUIREMENTS,
            from_name="text",
            to_name_space=PROV_S,
            to_name="description",
        ),
        # RemoveIsATypeOf on identified?
    ],
}
