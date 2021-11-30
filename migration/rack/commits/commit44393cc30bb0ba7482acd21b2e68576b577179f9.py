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
    ChangeCardinality,
    ChangeClassIsATypeOf,
    ChangePropertyIsATypeOf,
    Commit,
    SingleValue,
)
from rack.namespaces.rack_ontology import DOCUMENT, PROV_S, REVIEW

commit = Commit(
    number="44393cc30bb0ba7482acd21b2e68576b577179f9",
    changes=[
        # REVIEW.sadl
        ChangeClassIsATypeOf(
            name_space=REVIEW,
            class_id="REVIEW_LOG",
            from_name_space=PROV_S,
            from_class_id="ENTITY",
            to_name_space=DOCUMENT,
            to_class_id="REPORT",
        ),
        ChangePropertyIsATypeOf(
            name_space=REVIEW,
            class_id="REVIEW_LOG",
            property_id="reviews",
            from_name_space=PROV_S,
            from_property_id="wasDerivedFrom",
            to_name_space=PROV_S,
            to_property_id="wasImpactedBy",
        ),
        ChangeCardinality(
            name_space=REVIEW,
            class_id="REVIEW",
            property_id="author",
            to_cardinality=SingleValue(),
        ),
    ],
)
