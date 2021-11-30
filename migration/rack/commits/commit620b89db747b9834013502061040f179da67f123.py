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
    number="620b89db747b9834013502061040f179da67f123",
    changes=[
        RenameProperty(
            from_name_space=PROV_S,
            from_class="ENTITY",
            from_name="definedIn",
            to_name_space=FILE,
            to_class="THING",
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
