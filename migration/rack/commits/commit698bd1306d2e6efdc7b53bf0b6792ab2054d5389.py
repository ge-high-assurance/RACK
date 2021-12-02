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

from ontology_changes import AddClass, Commit
from rack.namespaces.rack_ontology import BASELINE

commit = Commit(
    number="698bd1306d2e6efdc7b53bf0b6792ab2054d5389",
    changes=[
        # BASELINE.sadl
        AddClass(
            name_space=BASELINE,
            class_id="BASELINE",
        ),
    ],
)
