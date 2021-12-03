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
    AtMost,
    ChangeCardinality,
    Commit,
    CreateProperty,
    DeleteClass,
    SubsumeProperty,
)
from rack.namespaces.rack_ontology import AGENTS, PROV_S

commit = Commit(
    number="9af9030fe191d564875c067f6e0319ca6b52b798",
    changes=[
        # AGENTS.sadl
        ChangeCardinality(
            name_space=AGENTS,
            class_id="PERSON",
            property_id="employedBy",
            to_cardinality=AtMost(1),
        ),
        SubsumeProperty(
            from_name_space=AGENTS,
            from_class="ORGANIZATION",
            from_name="name",
            to_name_space=PROV_S,
            to_class="AGENT",
            to_name="agentName",
        ),
        SubsumeProperty(
            from_name_space=AGENTS,
            from_class="PERSON",
            from_name="name",
            to_name_space=PROV_S,
            to_class="AGENT",
            to_name="agentName",
        ),
        DeleteClass(
            name_space=AGENTS,
            class_id="SOFTWAREAGENT",
        ),
        CreateProperty(
            name_space=AGENTS,
            class_id="TOOL",
            property_id="toolVersion",
        ),
        # PROV-S.sadl
        CreateProperty(
            name_space=PROV_S,
            class_id="AGENT",
            property_id="agentName",
        ),
    ],
)
