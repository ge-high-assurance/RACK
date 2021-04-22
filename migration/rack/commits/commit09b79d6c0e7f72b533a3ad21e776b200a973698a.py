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

from migration_helpers.name_space import rack
from ontology_changes import ChangeCardinality, Commit

HAZARD = rack("HAZARD")
SYSTEM = rack("SYSTEM")

commit = Commit(
    number="09b79d6c0e7f72b533a3ad21e776b200a973698a",
    changes=[

        # HAZARD.sadl
        ChangeCardinality(
            name_space=HAZARD,
            class_id="HAZARD",
            property_id="effect",
        ),
        ChangeCardinality(
            name_space=HAZARD,
            class_id="HAZARD",
            property_id="severity",
        ),
        ChangeCardinality(
            name_space=HAZARD,
            class_id="HAZARD",
            property_id="likelihood",
        ),

        # SYSTEM.sadl
        ChangeCardinality(
            name_space=SYSTEM,
            class_id="FUNCTION",
            property_id="parentFunction",
        ),

    ],
)
