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

from migration_helpers.name_space import rack
from ontology_changes import (
    AddPropertyIsATypeOf,
    AtMost,
    ChangeCardinality,
    ChangePropertyIsATypeOf,
    ChangePropertyRange,
    Commit,
    CreateProperty,
    DeleteProperty,
    RenameClass,
    RenameProperty,
    RemoveIsATypeOf,
)

PROV_S = rack("PROV-S")
SOFTWARE = rack("SOFTWARE")
SYSTEM = rack("SYSTEM")

commit = Commit(
    number="bdfef3d7ea9b3c9fc085defa8e26256f646097d9",
    changes=[
        # SOFTWARE.sadl
        ChangePropertyIsATypeOf(
            name_space=SOFTWARE,
            class_id="COMPILE",
            property_id="compiledBy",
            from_name_space=PROV_S,
            from_property_id="used",
            to_name_space=PROV_S,
            to_property_id="wasAssociatedWith",
        ),
        ChangePropertyIsATypeOf(
            name_space=SOFTWARE,
            class_id="PACKAGE",
            property_id="packagedBy",
            from_name_space=PROV_S,
            from_property_id="used",
            to_name_space=PROV_S,
            to_property_id="wasAssociatedWith",
        ),
        ChangePropertyRange(
            prop_name_space=SOFTWARE,
            prop_name="mentions",
            from_name_space=PROV_S,
            from_range="ENTITY",
            to_name_space=SOFTWARE,
            to_range="COMPONENT",
        ),
        ChangePropertyRange(
            prop_name_space=SOFTWARE,
            prop_name="subcomponentOf",
            from_name_space=PROV_S,
            from_range="ENTITY",
            to_name_space=SOFTWARE,
            to_range="COMPONENT",
        ),
        RenameProperty(
            from_name_space=SOFTWARE,
            from_class="COMPONENT",
            from_name="name",
            to_name_space=PROV_S,
            to_class="THING",
            to_name="title",
        ),
        # NOTE: renaming class last so that the previous changes can be in terms
        # of 'COMPONENT' rather than 'SWCOMPONENT'
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="COMPONENT",
            to_name_space=SOFTWARE,
            to_name="SWCOMPONENT",
        ),
        DeleteProperty(
            name_space=SOFTWARE,
            property_id="requirements",
        ),
        DeleteProperty(
            name_space=SOFTWARE,
            property_id="annotations",
        ),
        DeleteProperty(
            name_space=SOFTWARE,
            property_id="controlFlowsToUnconditionally",
        ),
        DeleteProperty(
            name_space=SOFTWARE,
            property_id="controlFlowsToConditionally",
        ),
        # SYSTEM.sadl
        RemoveIsATypeOf(
            name_space=SYSTEM,
            class_id="SYSTEM",
            property_id="partOf",
            range_id="wasDerivedFrom",
        ),
        ChangeCardinality(
            name_space=SYSTEM,
            class_id="SYSTEM",
            property_id="producedBy",
            to_cardinality=AtMost(1),
        ),
        RemoveIsATypeOf(
            name_space=SYSTEM,
            class_id="SYSTEM",
            property_id="provides",
            range_id="wasDerivedFrom",
        ),
        RemoveIsATypeOf(
            name_space=SYSTEM,
            class_id="SYSTEM",
            property_id="requires",
            range_id="wasDerivedFrom",
        ),
        ChangePropertyIsATypeOf(
            name_space=SYSTEM,
            class_id="SYSTEM",
            property_id="function",
            from_name_space=PROV_S,
            from_property_id="wasDerivedFrom",
            to_name_space=PROV_S,
            to_property_id="wasImpactedBy",
        ),
        CreateProperty(
            name_space=SYSTEM,
            class_id="INTERFACE",
            property_id="commodity",
        ),
        ChangePropertyIsATypeOf(
            name_space=SYSTEM,
            class_id="INTERFACE",
            property_id="source",
            from_name_space=PROV_S,
            from_property_id="wasDerivedFrom",
            to_name_space=PROV_S,
            to_property_id="wasImpactedBy",
        ),
        ChangePropertyRange(
            prop_name_space=SYSTEM,
            prop_name="destination",
            from_name_space=PROV_S,
            from_range="ENTITY",
            to_name_space=SYSTEM,
            to_range="SYSTEM",
        ),
        ChangePropertyIsATypeOf(
            name_space=SYSTEM,
            class_id="INTERFACE",
            property_id="destination",
            from_name_space=PROV_S,
            from_property_id="wasDerivedFrom",
            to_name_space=PROV_S,
            to_property_id="wasImpactedBy",
        ),
        RemoveIsATypeOf(
            name_space=SYSTEM,
            class_id="INTERFACE",
            property_id="identifiedBy",
            range_id="wasGeneratedBy",
        ),
        AddPropertyIsATypeOf(
            name_space=SYSTEM,
            class_id="SYSTEM_DEVELOPMENT",
            property_id="developedBy",
            range_name_space=PROV_S,
            range="wasAssociatedWith",
        ),
        RemoveIsATypeOf(
            name_space=SYSTEM,
            class_id="FUNCTION",
            property_id="parentFunction",
            range_id="wasDerivedFrom",
        ),
        DeleteProperty(
            name_space=SYSTEM,
            property_id="envConstraint",
        ),
    ],
)
