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

from ontology_changes import Commit, RenameClass
from rack.namespaces.rack_ontology import HARDWARE

commit = Commit(
    number="6ca3b8884f159233b417917f5a6dfc3ed699d1f7",
    changes=[
        RenameClass(
            from_name_space=HARDWARE,
            from_name="COMPONENT_TYPE",
            to_name_space=HARDWARE,
            to_name="HWCOMPONENT_TYPE",
        )
    ],
)
