# Copyright (c) 2022, Galois, Inc.
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
from ontology_changes.create_property import CreateProperty
from ontology_changes.rename_class import RenameClass
from ontology_changes.rename_property import RenameProperty

from rack.namespaces.rack_ontology import HARDWARE, SOFTWARE

commit = Commit(
    number="5c7920fe44a3a60c76fefddd2b88cd27851f37ed",
    changes=[
        CreateProperty(
            name_space=HARDWARE,
            class_id="HWCOMPONENT",
            property_id="partOf",
        ),
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="COMPONENT_TYPE",
            to_name_space=SOFTWARE,
            to_name="SWCOMPONENT_TYPE",
        ),
        RenameProperty(
            from_name_space=SOFTWARE,
            from_class="SWCOMPONENT",
            from_name="subcomponentOf",
            to_name_space=SOFTWARE,
            to_class="SWCOMPONENT",
            to_name="partOf",
        ),
    ],
)
