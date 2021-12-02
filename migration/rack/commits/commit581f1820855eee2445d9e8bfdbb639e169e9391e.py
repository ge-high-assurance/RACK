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

from ontology_changes import ChangeClassIsATypeOf, Commit
from rack.namespaces.rack_ontology import DOCUMENT, PROV_S, REVIEW

commit = Commit(
    number="581f1820855eee2445d9e8bfdbb639e169e9391e",
    changes=[
        # REVIEW.sadl
        ChangeClassIsATypeOf(
            name_space=REVIEW,
            class_id="REVIEW_LOG",
            from_name_space=DOCUMENT,
            from_class_id="REPORT",
            to_name_space=PROV_S,
            to_class_id="ENTITY",
        ),
    ],
)
