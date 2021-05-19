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
from ontology_changes import AddClass, Commit, DeleteProperty

MODEL = rack("MODEL")
REQUIREMENTS = rack("REQUIREMENTS")

commit = Commit(
    number="b25d07626e4693cd370a2070e17f6baa825a1d43",
    changes=[
        # MODEL.sadl
        AddClass(
            name_space=MODEL,
            class_id="MODEL",
        ),
        # REQUIREMENTS.sadl
        DeleteProperty(name_space=REQUIREMENTS, property_id="givenText"),
        DeleteProperty(name_space=REQUIREMENTS, property_id="ifText"),
        DeleteProperty(name_space=REQUIREMENTS, property_id="thenText"),
    ],
)
