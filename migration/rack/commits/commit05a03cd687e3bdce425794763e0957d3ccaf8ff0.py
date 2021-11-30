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
    AtMost,
    ChangeCardinality,
    ChangePropertyIsATypeOf,
    Commit,
    RemoveIsATypeOf,
)
from rack.namespaces.rack_ontology import HAZARD, PROV_S

commit = Commit(
    number="05a03cd687e3bdce425794763e0957d3ccaf8ff0",
    changes=[
        # HAZARD.sadl
        ChangeCardinality(
            name_space=HAZARD,
            class_id="HAZARD",
            property_id="definition",
            to_cardinality=AtMost(1),
        ),
        RemoveIsATypeOf(
            name_space=HAZARD,
            class_id="HAZARD",
            property_id="identified",
            range_id="wasGeneratedBy",
        ),
        # 'severity' range changed
        # 'likelihood' range changed
        ChangePropertyIsATypeOf(
            name_space=HAZARD,
            class_id="HAZARD",
            property_id="source",  # H:source?
            from_name_space=PROV_S,
            from_property_id="wasDerivedFrom",
            to_name_space=PROV_S,
            to_property_id="wasImpactedBy",
        ),
        RemoveIsATypeOf(
            name_space=HAZARD,
            class_id="HAZARD",
            property_id="identified",
            range_id="wasGeneratedBy",
        ),
        # Technically the range of 'severity' and 'likelihood' have changed, if
        # we ever track that.
    ],
)
