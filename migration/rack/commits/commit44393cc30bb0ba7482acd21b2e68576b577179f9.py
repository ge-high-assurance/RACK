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

# from migration_helpers.name_space import rack
from ontology_changes import Commit, ChangeIsATypeOf

commit = Commit(
    number="44393cc30bb0ba7482acd21b2e68576b577179f9",
    changes=[
        # REVIEW.sadl
        # ChangeClassIsATypeOf
        ChangeIsATypeOf(
            class_id="REVIEW_LOG",
            property_id="reviews",
            from_property_id="wasDerivedFrom",
            to_property_id="wasImpactedBy",
        ),
    ],
)
