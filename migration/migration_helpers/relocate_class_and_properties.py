# Copyright (c) 2023, Galois, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

from typing import List

from migration_helpers.name_space import NameSpace
from ontology_changes import (
    RenameClass,
    RenameProperty,
)
from ontology_changes.ontology_change import OntologyChange


def relocate_class_and_properties(
    from_namespace: NameSpace,
    to_namespace: NameSpace,
    class_id: str,
    properties: List[str],
) -> List[OntologyChange]:
    # Explicit type ascriptions to circumvent the fact that List is not
    # covariant
    rename_class: List[OntologyChange] = [
        RenameClass(
            from_name_space=from_namespace,
            from_name=class_id,
            to_name_space=to_namespace,
            to_name=class_id,
        )
    ]
    rename_properties: List[OntologyChange] = [
        RenameProperty(
            from_name_space=from_namespace,
            from_class=class_id,
            from_name=property_id,
            to_name_space=to_namespace,
            to_class=class_id,
            to_name=property_id,
        )
        for property_id in properties
    ]
    return rename_class + rename_properties
