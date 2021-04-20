# Copyright (c) 2020, Galois, Inc.
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
    ChangeIsATypeOf,
    ChangePropertyRange,
    Commit,
    RenameProperty,
)

PROV_S = rack("PROV-S")
SYSTEM = rack("SYSTEM")

commit: Commit = {
    "number": "7d88c0285a32107a18e439242b9ba326d6a02210",
    "changes": [
        # SYSTEM.sadl
        ChangePropertyRange(
            prop_name_space=SYSTEM,
            prop_name="partOf",
            from_name_space=PROV_S,
            from_range="ENTITY",
            to_name_space=SYSTEM,
            to_range="SYSTEM",
        ),
        ChangePropertyRange(
            prop_name_space=SYSTEM,
            prop_name="producedBy",
            from_name_space=PROV_S,
            from_range="ACTIVITY",
            to_name_space=SYSTEM,
            to_range="SYSTEM_DEVELOPMENT",
        ),
        # TODO partOf no longer a type of wasDerivedFrom?
        ChangePropertyRange(
            prop_name_space=SYSTEM,
            prop_name="provides",
            from_name_space=PROV_S,
            from_range="ENTITY",
            to_name_space=SYSTEM,
            to_range="FUNCTION",
        ),
        # TODO provides no longer a type of wasDerivedFrom?
        ChangePropertyRange(
            prop_name_space=SYSTEM,
            prop_name="requires",
            from_name_space=PROV_S,
            from_range="ENTITY",
            to_name_space=SYSTEM,
            to_range="FUNCTION",
        ),
        # TODO requires no longer a type of wasDerivedFrom?
        RenameProperty(
            from_name_space=SYSTEM,
            from_name="function",
            to_name_space=SYSTEM,
            to_name="functionalityOverview",
        ),
        ChangePropertyRange(
            prop_name_space=SYSTEM,
            prop_name="functionalityOverview",
            from_name_space=SYSTEM,
            from_range="FUNCTION",
            to_name_space=rack("BAD"),
            to_range="string",
        ),
        # TODO: AddedProperty commodity?
        ChangePropertyRange(
            prop_name_space=SYSTEM,
            prop_name="source",
            from_name_space=PROV_S,
            from_range="ENTITY",
            to_name_space=SYSTEM,
            to_range="SYSTEM",
        ),
        ChangeIsATypeOf(
            class_id="SYSTEM",
            property_id="source",
            from_property_id="wasDerivedFrom",
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
        ChangeIsATypeOf(
            class_id="SYSTEM",
            property_id="destination",
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
        # TODO RemoveIsATypeOf on identifiedBy?
        # TODO AddIsATypeOf on developedBy?
        # TODO RemoveIsATypeOf on parentFunction?
        # TODO RemoveProperty envConstraint
    ],
}
