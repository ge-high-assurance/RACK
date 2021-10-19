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
from ontology_changes.cardinality import AtMost

AGENTS = rack("AGENTS")
PROV_S = rack("PROV-S")

commit = Commit(
    number="c41222325db52df0eb5c1e7cb3a091f8c62f5b57",
    changes=[
        # AGENTS.sadl
        ChangeCardinality(
            name_space=AGENTS,
            class_id="PERSON",
            property_id="employedBy",
            to_cardinality=AtMost(at_most=1),
        ),
        # PROV_S.sadl
        ChangeCardinality(
            name_space=PROV_S,
            class_id="THING",
            property_id="dataInsertedBy",
            to_cardinality=AtMost(at_most=1),
        ),
    ],
)
