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
from rack.namespaces.rack_ontology import REVIEW

commit = Commit(
    number="b721c16f0f7420a8ccd92bda0d98a96c16dc62b8",
    changes=[
        ChangeCardinality(
            name_space=REVIEW,
            class_id="REVIEW",
            property_id="author",
            to_cardinality=Unconstrained(),
        ),
    ],
)
