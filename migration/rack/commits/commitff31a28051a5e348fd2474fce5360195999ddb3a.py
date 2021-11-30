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

from ontology_changes import ChangeCardinality, Commit, Unconstrained
from rack.namespaces.rack_ontology import REQUIREMENTS

commit = Commit(
    number="ff31a28051a5e348fd2474fce5360195999ddb3a",
    changes=[
        ChangeCardinality(
            name_space=REQUIREMENTS,
            class_id="REQUIREMENT",
            property_id="givenText",
            to_cardinality=Unconstrained(),
        ),
        ChangeCardinality(
            name_space=REQUIREMENTS,
            class_id="REQUIREMENT",
            property_id="ifText",
            to_cardinality=Unconstrained(),
        ),
        ChangeCardinality(
            name_space=REQUIREMENTS,
            class_id="REQUIREMENT",
            property_id="thenText",
            to_cardinality=Unconstrained(),
        ),
    ],
)
