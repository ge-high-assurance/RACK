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

from ontology_changes import (
    AddClassIsATypeOf,
    AddPropertyIsATypeOf,
    Commit,
    CreateClass,
    CreateProperty,
)
from rack.namespaces.rack_ontology import PROV_S, RESOLUTIONS
from rack.namespaces.semtk_ontology import ENTITY_RESOLUTION

commit = Commit(
    number="096749fb6a984d801d9ace5ccf5ec269de390a66",
    changes=[
        # PROV-S.sadl
        CreateClass(
            name_space=PROV_S,
            class_id="NODE",
        ),
        AddClassIsATypeOf(
            name_space=PROV_S,
            class_id="THING",
            range_name_space=PROV_S,
            range_id="NODE",
        ),
        # RESOLUTIONS.sadl
        AddClassIsATypeOf(
            name_space=RESOLUTIONS,
            class_id="SAME_AS",
            range_name_space=PROV_S,
            range_id="NODE",
        ),
        CreateProperty(
            name_space=RESOLUTIONS,
            class_id="SAME_AS",
            property_id="primary",
        ),
        CreateProperty(
            name_space=RESOLUTIONS,
            class_id="SAME_AS",
            property_id="secondary",
        ),
        AddClassIsATypeOf(
            name_space=RESOLUTIONS,
            class_id="SAME_AS",
            range_name_space=ENTITY_RESOLUTION,
            range_id="SameAs",
        ),
        AddPropertyIsATypeOf(
            name_space=RESOLUTIONS,
            class_id="SAME_AS",
            property_id="primary",
            range_name_space=ENTITY_RESOLUTION,
            range="target",
        ),
        AddPropertyIsATypeOf(
            name_space=RESOLUTIONS,
            class_id="SAME_AS",
            property_id="secondary",
            range_name_space=ENTITY_RESOLUTION,
            range="duplicate",
        ),
    ],
)
