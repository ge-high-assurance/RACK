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

from ontology_changes import (
    Commit,
    CreateProperty,
    DeleteClass,
    DeleteProperty,
    RenameProperty,
    SubsumeProperty,
)
from ontology_changes.create_class import CreateClass
from rack.namespaces.rack_ontology import (
    AGENTS,
    ANALYSIS,
    FILE,
    PROCESS,
    PROV_S,
    TESTING,
)

commit = Commit(
    number="40955e24b4e38d45df2ffd0ad8aa47a827a4c72f",
    changes=[
        # AGENTS.sadl
        CreateProperty(
            name_space=AGENTS,
            class_id="TOOL",
            property_id="toolInstallationConfiguration",
        ),
        # ANALYSIS.sadl
        DeleteProperty(name_space=ANALYSIS, property_id="result"),
        DeleteProperty(name_space=ANALYSIS, property_id="metric"),
        DeleteProperty(name_space=ANALYSIS, property_id="producedBy"),
        DeleteClass(name_space=ANALYSIS, class_id="ANALYSIS_RESULT"),
        RenameProperty(
            from_name_space=ANALYSIS,
            from_class="ANALYSIS",
            from_name="performedBy",
            to_name_space=ANALYSIS,
            to_class="ANALYSIS",
            to_name="runBy",
        ),
        CreateProperty(
            name_space=ANALYSIS,
            class_id="ANALYSIS",
            property_id="analyzedWith",
        ),
        CreateProperty(
            name_space=ANALYSIS,
            class_id="ANALYSIS",
            property_id="analysisInput",
        ),
        CreateProperty(
            name_space=ANALYSIS,
            class_id="ANALYSIS",
            property_id="analysisConfiguration",
        ),
        DeleteClass(name_space=ANALYSIS, class_id="ANALYSIS_ANNOTATION_TYPE"),
        DeleteClass(name_space=ANALYSIS, class_id="PRECONDITION"),
        DeleteClass(name_space=ANALYSIS, class_id="POSTCONDITION"),
        DeleteClass(name_space=ANALYSIS, class_id="INVARIANT"),
        DeleteClass(name_space=ANALYSIS, class_id="ANALYSIS_ANNOTATION"),
        DeleteProperty(name_space=ANALYSIS, property_id="fromReport"),
        DeleteProperty(name_space=ANALYSIS, property_id="annotationType"),
        # FILE.sadl
        RenameProperty(
            from_name_space=FILE,
            from_class="FILE",
            from_name="createBy",
            to_name_space=FILE,
            to_class="FILE",
            to_name="createdBy",
        ),
        # PROCESS.sadl
        CreateClass(name_space=PROCESS, class_id="PROPERTY"),
        CreateProperty(
            name_space=PROCESS, class_id="PROPERTY", property_id="partiallySupports"
        ),
        CreateProperty(name_space=PROCESS, class_id="PROPERTY", property_id="scopeOf"),
        CreateProperty(
            name_space=PROCESS, class_id="PROPERTY", property_id="mitigates"
        ),
        # PROV-S.sadl
        CreateProperty(name_space=PROV_S, class_id="ACTIVITY", property_id="goal"),
        # TESTING.sadl
        SubsumeProperty(
            from_name_space=TESTING,
            from_class="TEST",
            from_name="producedBy",
            to_name_space=PROV_S,
            to_class="ENTITY",
            to_name="wasGeneratedBy",
        ),
    ],
)
