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

from ontology_changes import ChangePropertyRange, Commit, RenameProperty
from rack.namespaces.rack_ontology import FILE, PROV_S

commit = Commit(
    number="183dbba72623c2585a0451a19ac1ddb30f8a0ea6",
    changes=[
        RenameProperty(
            from_name_space=FILE,
            from_class="THING",
            from_name="definedIn",
            to_name_space=PROV_S,
            to_class="ENTITY",
            to_name="definedIn",
        ),
        ChangePropertyRange(
            prop_name_space=FILE,
            prop_name="definedIn",
            from_name_space=FILE,
            from_range="FILE",
            to_name_space=PROV_S,
            to_range="ENTITY",
        ),
    ],
)
