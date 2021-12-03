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

from ontology_changes import Commit, DeleteProperty
from rack.namespaces.rack_ontology import HAZARD

commit = Commit(
    number="84bad08fee850046ef1b328b2b393322b48d5e09",
    changes=[
        # HAZARD.sadl
        DeleteProperty(name_space=HAZARD, property_id="definition"),
    ],
)
