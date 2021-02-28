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
    DeleteProperty,
    RenameClass,
    RenameProperty,
)

FILE = rack("FILE")
PROV_S = rack("PROV-S")
SOFTWARE = rack("SOFTWARE")

commit: Commit = {
    "number": "182d0483a73cdc221c692c71e0d4e64f92f5079a",
    "changes": [
        # SOFTWARE.sadl
        ChangeIsATypeOf(
            class_id="COMPILE",
            property_id="compiledBy",
            from_property_id="used",
            to_property_id="wasAssociatedWith",
        ),
        ChangeIsATypeOf(
            class_id="PACKAGE",
            property_id="packagedBy",
            from_property_id="used",
            to_property_id="wasAssociatedWith",
        ),
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="COMPONENT",
            to_name_space=SOFTWARE,
            to_name="SWCOMPONENT",
        ),
        # WARNING: if you move the above RenameClass further down, you need to
        # beware of using the correct one of COMPONENT vs. SWCOMPONENT
        ChangePropertyRange(
            prop_name_space=SOFTWARE,
            prop_name="mentions",
            from_name_space=PROV_S,
            from_range="ENTITY",
            to_name_space=SOFTWARE,
            to_range="SWCOMPONENT",
        ),
        ChangePropertyRange(
            prop_name_space=SOFTWARE,
            prop_name="subcomponentOf",
            from_name_space=PROV_S,
            from_range="ENTITY",
            to_name_space=SOFTWARE,
            to_range="SWCOMPONENT",
        ),
        RenameProperty(
            from_name_space=SOFTWARE,
            from_name="name",
            to_name_space=PROV_S,
            to_name="title",
        ),
        # Technically, this commit deletes the COMPONENT#definedIn property.
        # However, a concurrent commit (643839e7) renamed it to FILE#definedIn,
        # so let's just skip it here so that the other commit does the patching
        # work.
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
    ],
}
