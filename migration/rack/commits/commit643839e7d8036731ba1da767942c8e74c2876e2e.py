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
    AtMost,
    ChangeCardinality,
    ChangePropertyIsATypeOf,
    ChangePropertyRange,
    Commit,
    RenameProperty,
    SingleValue,
)

FILE = rack("FILE")
PROV_S = rack("PROV-S")
SOFTWARE = rack("SOFTWARE")

commit = Commit(
    number="643839e7d8036731ba1da767942c8e74c2876e2e",
    changes=[
        # FILE.sadl
        ChangeCardinality(
            name_space=FILE,
            class_id="FILE",
            property_id="filename",
            to_cardinality=SingleValue(),
        ),
        RenameProperty(
            from_name_space=FILE,
            from_class="FILE",
            from_name="fileParent",
            to_name_space=FILE,
            to_class="FILE",
            to_name="definedIn",
        ),
        ChangePropertyIsATypeOf(
            name_space=FILE,
            class_id="FILE",
            property_id="satisfies",
            from_name_space=PROV_S,
            from_property_id="wasDerivedFrom",
            to_name_space=PROV_S,
            to_property_id="wasImpactedBy",
        ),
        ChangeCardinality(
            name_space=FILE,
            class_id="FILE",
            property_id="createBy",
            to_cardinality=AtMost(1),
        ),
        # FILE.sadl / SOFTWARE.sadl
        RenameProperty(
            from_name_space=SOFTWARE,
            from_class="FILE",
            from_name="definedIn",
            to_name_space=FILE,
            to_class="FILE",
            to_name="definedIn",
        ),
        ChangePropertyRange(
            prop_name_space=FILE,
            prop_name="definedIn",
            from_name_space=PROV_S,
            from_range="ENTITY",
            to_name_space=FILE,
            to_range="FILE",
        ),
    ],
)
