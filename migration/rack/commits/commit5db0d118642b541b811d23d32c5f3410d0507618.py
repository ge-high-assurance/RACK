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
from ontology_changes.cardinality import Unconstrained
from ontology_changes.change_cardinality import ChangeCardinality
from rack.namespaces.rack_ontology import (
    AGENTS,
    ANALYSIS,
    FILE,
    PROCESS,
    PROV_S,
    TESTING,
)

commit = Commit(
    number="5db0d118642b541b811d23d32c5f3410d0507618",
    changes=[
        # PROV-S.sadl
        ChangeCardinality(
            name_space=PROV_S,
            class_id="THING",
            property_id="dataInsertedBy",
            to_cardinality=Unconstrained(),
        ),
    ],
)
