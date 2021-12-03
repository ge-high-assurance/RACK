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

from ontology_changes import Commit, ChangePropertyRange
from rack.namespaces.rack_ontology import PROV_S
from rack.namespaces.xml_schema import XMLSCHEMA

commit = Commit(
    number="1834d0201254907fa50a32945716a3e0de985cad",
    changes=[
        # PROV-S.sadl
        ChangePropertyRange(
            prop_name_space=PROV_S,
            prop_name="entityURL",
            from_name_space=XMLSCHEMA,
            from_range="anyURI",
            to_name_space=XMLSCHEMA,
            to_range="string",
        )
    ],
)
