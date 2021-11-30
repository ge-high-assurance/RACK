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

from ontology_changes import Commit, CreateProperty
from rack.namespaces.rack_ontology import PROV_S

commit = Commit(
    number="e696969a9d85ca8f894eea12305412bdc05521b3",
    changes=[
        # PROV-S.sadl
        CreateProperty(name_space=PROV_S, class_id="ENTITY", property_id="entityURL"),
    ],
)
