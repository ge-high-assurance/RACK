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
from ontology_changes import (
    ChangePropertyRange,
    Commit,
)

PROV_S = rack("PROV-S")
SYSTEM = rack("SYSTEM")

commit: Commit = {
    "number": "404e3e78ff5f554d8edbc4238f64bd3797d8829f",
    "changes": [
        # SYSTEM.sadl
        ChangePropertyRange(
            prop_name_space=SYSTEM,
            prop_name="producedBy",
            from_name_space=SYSTEM,
            from_range="SYSTEM_DEVELOPMENT",
            to_name_space=PROV_S,
            to_range="ACTIVITY",
        ),
    ],
}
