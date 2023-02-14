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
from ontology_changes.change_property_domain import ChangePropertyDomain
from rack.namespaces.rack_ontology import CONFIDENCE

commit = Commit(
    number="16f6fe3e2bb5c8c6fae59b10f400380a76863452",
    changes=[
        # CONFIDENCE.sadl
        ChangePropertyDomain(
            prop_name_space=CONFIDENCE,
            prop_name="belief",
            from_name_space=CONFIDENCE,
            from_domain="CONFIDENCE_ASSESSMENT",
            to_name_space=CONFIDENCE,
            to_domain="BDU_CONFIDENCE_ASSESSMENT",
        ),
        ChangePropertyDomain(
            prop_name_space=CONFIDENCE,
            prop_name="disbelief",
            from_name_space=CONFIDENCE,
            from_domain="CONFIDENCE_ASSESSMENT",
            to_name_space=CONFIDENCE,
            to_domain="BDU_CONFIDENCE_ASSESSMENT",
        ),
        ChangePropertyDomain(
            prop_name_space=CONFIDENCE,
            prop_name="uncertainty",
            from_name_space=CONFIDENCE,
            from_domain="CONFIDENCE_ASSESSMENT",
            to_name_space=CONFIDENCE,
            to_domain="BDU_CONFIDENCE_ASSESSMENT",
        ),
    ],
)
