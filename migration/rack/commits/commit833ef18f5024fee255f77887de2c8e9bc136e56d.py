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

from ontology_changes import AtMost, ChangeCardinality, Commit, RenameProperty
from rack.namespaces.rack_ontology import PROV_S

commit = Commit(
    number="833ef18f5024fee255f77887de2c8e9bc136e56d",
    changes=[
        # PROV-S.sadl
        ChangeCardinality(
            name_space=PROV_S,
            class_id="THING",
            property_id="identifier",
            to_cardinality=AtMost(1),
        ),
        ChangeCardinality(
            name_space=PROV_S,
            class_id="THING",
            property_id="title",
            to_cardinality=AtMost(1),
        ),
        ChangeCardinality(
            name_space=PROV_S,
            class_id="THING",
            property_id="description",
            to_cardinality=AtMost(1),
        ),
        ChangeCardinality(
            name_space=PROV_S,
            class_id="THING",
            property_id="dataInsertedBy",
            to_cardinality=AtMost(1),
        ),
        ChangeCardinality(
            name_space=PROV_S,
            class_id="ENTITY",
            property_id="wasGeneratedBy",
            to_cardinality=AtMost(1),
        ),
        ChangeCardinality(
            name_space=PROV_S,
            class_id="ENTITY",
            property_id="generatedAtTime",
            to_cardinality=AtMost(1),
        ),
        ChangeCardinality(
            name_space=PROV_S,
            class_id="ENTITY",
            property_id="invalidatedAtTime",
            to_cardinality=AtMost(1),
        ),
        ChangeCardinality(
            name_space=PROV_S,
            class_id="ACTIVITY",
            property_id="startedAtTime",
            to_cardinality=AtMost(1),
        ),
        ChangeCardinality(
            name_space=PROV_S,
            class_id="ACTIVITY",
            property_id="endedAtTime",
            to_cardinality=AtMost(1),
        ),
        RenameProperty(
            from_name_space=PROV_S,
            from_class="AGENT",
            from_name="agentName",
            to_name_space=PROV_S,
            to_class="THING",
            to_name="title",
        ),
    ],
)
