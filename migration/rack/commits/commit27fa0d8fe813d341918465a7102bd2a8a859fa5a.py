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

from ontology_changes import Commit, RenameProperty
from rack.namespaces.rack_ontology import (
    CONFIDENCE,
    FILE,
    HAZARD,
    PROV_S,
    REQUIREMENTS,
    REVIEW,
    SOFTWARE,
    SYSTEM,
    TESTING,
)

commit = Commit(
    number="27fa0d8fe813d341918465a7102bd2a8a859fa5a",
    changes=[
        # CONFIDENCE.sadl
        RenameProperty(
            from_name_space=CONFIDENCE,
            from_class="CONFIDENCE_ASSESSMENT",
            from_name="createBy",
            to_name_space=CONFIDENCE,
            to_class="ENTITY",
            to_name="wasGeneratedBy",
        ),
        RenameProperty(
            from_name_space=CONFIDENCE,
            from_class="ASSESSING_CONFIDENCE",
            from_name="createBy",
            to_name_space=CONFIDENCE,
            to_class="ENTITY",
            to_name="wasGeneratedBy",
        ),
        # FILE.sadl
        RenameProperty(
            from_name_space=FILE,
            from_class="FILE",
            from_name="createdBy",
            to_name_space=PROV_S,
            to_class="ENTITY",
            to_name="wasGeneratedBy",
        ),
        # HAZARD.sadl
        RenameProperty(
            from_name_space=HAZARD,
            from_class="HAZARD",
            from_name="identified",
            to_name_space=PROV_S,
            to_class="ENTITY",
            to_name="wasGeneratedBy",
        ),
        RenameProperty(
            from_name_space=HAZARD,
            from_class="HAZARD",
            from_name="H:author",
            to_name_space=PROV_S,
            to_class="ACTIVITY",
            to_name="wasAssociatedWith",
        ),
        # REQUIREMENTS.sadl
        RenameProperty(
            from_name_space=REQUIREMENTS,
            from_class="REQUIREMENT",
            from_name="Rq:createdBy",
            to_name_space=PROV_S,
            to_class="ENTITY",
            to_name="wasGeneratedBy",
        ),
        RenameProperty(
            from_name_space=REQUIREMENTS,
            from_class="DATA_DICTIONARY_TERM",
            from_name="Rq:createdBy",
            to_name_space=PROV_S,
            to_class="ENTITY",
            to_name="wasGeneratedBy",
        ),
        RenameProperty(
            from_name_space=REQUIREMENTS,
            from_class="REQUIREMENT_DEVELOPMENT",
            from_name="Rq:author",
            to_name_space=PROV_S,
            to_class="ACTIVITY",
            to_name="wasAssociatedWith",
        ),
        # REVIEW.sadl
        RenameProperty(
            from_name_space=REVIEW,
            from_class="REVIEW_LOG",
            from_name="createBy",
            to_name_space=PROV_S,
            to_class="ENTITY",
            to_name="wasGeneratedBy",
        ),
        # SOFTWARE.sadl
        RenameProperty(
            from_name_space=SOFTWARE,
            from_class="CODE_DEVELOPMENT",
            from_name="author",
            to_name_space=PROV_S,
            to_class="ACTIVITY",
            to_name="wasAssociatedWith",
        ),
        RenameProperty(
            from_name_space=SOFTWARE,
            from_class="CODE_GEN",
            from_name="sw:performedBy",
            to_name_space=PROV_S,
            to_class="ACTIVITY",
            to_name="wasAssociatedWith",
        ),
        # SYSTEM.sadl
        RenameProperty(
            from_name_space=SYSTEM,
            from_class="SYSTEM",
            from_name="producedBy",
            to_name_space=PROV_S,
            to_class="ENTITY",
            to_name="wasGeneratedBy",
        ),
        RenameProperty(
            from_name_space=REQUIREMENTS,
            from_class="INTERFACE",
            from_name="identifiedBy",
            to_name_space=PROV_S,
            to_class="ENTITY",
            to_name="wasGeneratedBy",
        ),
        RenameProperty(
            from_name_space=REQUIREMENTS,
            from_class="SYSTEM_DEVELOPMENT",
            from_name="developedBy",
            to_name_space=PROV_S,
            to_class="ACTIVITY",
            to_name="wasAssociatedWith",
        ),
        # TESTING.sadl
        RenameProperty(
            from_name_space=TESTING,
            from_class="TEST_RESULT",
            from_name="executedBy",
            to_name_space=PROV_S,
            to_class="ENTITY",
            to_name="wasGeneratedBy",
        ),
        RenameProperty(
            from_name_space=TESTING,
            from_class="TEST_DEVELOPMENT",
            from_name="developedBy",
            to_name_space=PROV_S,
            to_class="ACTIVITY",
            to_name="wasAssociatedWith",
        ),
        RenameProperty(
            from_name_space=TESTING,
            from_class="TEST_EXECUTION",
            from_name="executedOn",
            to_name_space=PROV_S,
            to_class="ACTIVITY",
            to_name="wasAssociatedWith",
        ),
    ],
)
