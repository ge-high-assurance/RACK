# Copyright (c) 2022, Galois, Inc.
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
from ontology_changes.change_class_is_a_type_of import ChangeClassIsATypeOf
from rack.namespaces.rack_ontology import CONFIDENCE, PROV_S

commit = Commit(
    number="b85a66b005f4105ac5195cfd2cefec475f9e1f21",
    changes=[
        # CONFIDENCE.sadl
        ChangeClassIsATypeOf(
            name_space=CONFIDENCE,
            class_id="CONFIDENCE_ASSESSMENT",
            from_name_space=PROV_S,
            from_class_id="THING",
            to_name_space=PROV_S,
            to_class_id="ENTITY",
        )
    ],
)
