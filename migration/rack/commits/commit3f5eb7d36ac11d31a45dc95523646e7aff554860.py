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

from ontology_changes import Commit, RemovePropertyRestriction
from rack.namespaces.rack_ontology import SOFTWARE

commit = Commit(
    number="3f5eb7d36ac11d31a45dc95523646e7aff554860",
    changes=[
        # SOFTWARE.sadl
        RemovePropertyRestriction(
            prop_name_space=SOFTWARE,
            prop_name="sw:performedBy",
            prop_restriction="one of {Ag:PERSON, Ag:ORGANIZATION}",
        ),
    ],
)
