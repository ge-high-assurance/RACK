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
from ontology_changes import ChangeIsATypeOf, Commit, DeleteProperty, RenameProperty

FILE = rack("FILE")
SOFTWARE = rack("SOFTWARE")

commit: Commit = {
    "number": "643839e7d8036731ba1da767942c8e74c2876e2e",
    "changes": [
        # FILE.sadl
        RenameProperty(
            from_name_space=FILE,
            from_class="ENTITY",
            from_name="fileParent",
            to_name_space=FILE,
            to_class="ENTITY",
            to_name="definedIn",
        ),
        ChangeIsATypeOf(
            class_id="FILE",
            property_id="satisfies",
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
        # SOFTWARE.sadl
        DeleteProperty(name_space=SOFTWARE, property_id="definedIn"),
    ],
}
