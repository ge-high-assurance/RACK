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

from ontology_changes import Commit
from ontology_changes.subsume_property import SubsumeProperty
from rack.namespaces.rack_ontology import (
    PROV_S,
    SOFTWARE,
)

commit = Commit(
    number="c5306ce176984770b93070da829f60769cb19628",
    changes=[
        # SOFTWARE.sadl
        SubsumeProperty(
            from_name_space=SOFTWARE,
            from_class="COMPILE",
            from_name="sw:performedBy",
            to_name_space=PROV_S,
            to_class="ACTIVITY",
            to_name="wasAssociatedWith",
        ),
        SubsumeProperty(
            from_name_space=SOFTWARE,
            from_class="COMPILE",
            from_name="compiledBy",
            to_name_space=PROV_S,
            to_class="ACTIVITY",
            to_name="wasAssociatedWith",
        ),
        SubsumeProperty(
            from_name_space=SOFTWARE,
            from_class="PACKAGE",
            from_name="sw:performedBy",
            to_name_space=PROV_S,
            to_class="ACTIVITY",
            to_name="wasAssociatedWith",
        ),
        SubsumeProperty(
            from_name_space=SOFTWARE,
            from_class="PACKAGE",
            from_name="packagedBy",
            to_name_space=PROV_S,
            to_class="ACTIVITY",
            to_name="wasAssociatedWith",
        ),
    ],
)
