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

from ontology_changes import Commit, CreateClass, CreateProperty
from rack.namespaces.rack_ontology import HARDWARE, TESTING

commit = Commit(
    number="09962dd9ab9d252639d9e4324288fd6b47cbd91f",
    changes=[
        # HARDWARE.sadl
        CreateClass(name_space=HARDWARE, class_id="HWCOMPONENT"),
        CreateProperty(
            name_space=HARDWARE, class_id="HWCOMPONENT", property_id="instantiates"
        ),
        CreateProperty(
            name_space=HARDWARE, class_id="HWCOMPONENT", property_id="componentType"
        ),
        CreateProperty(
            name_space=HARDWARE, class_id="HWCOMPONENT", property_id="partitions"
        ),
        CreateClass(name_space=HARDWARE, class_id="PARTITION"),
        CreateClass(name_space=HARDWARE, class_id="COMPONENT_TYPE"),
        # TESTING.sadl
        CreateClass(name_space=TESTING, class_id="TEST_PROCEDURE"),
        CreateClass(name_space=TESTING, class_id="TEST_STEP"),
        CreateProperty(
            name_space=TESTING, class_id="TEST_STEP", property_id="nextStep"
        ),
        CreateClass(name_space=TESTING, class_id="TEST_LOG"),
        CreateClass(name_space=TESTING, class_id="TEST_RECORD"),
        CreateProperty(
            name_space=TESTING, class_id="TEST_RECORD", property_id="nextRecord"
        ),
        CreateProperty(name_space=TESTING, class_id="TEST_RECORD", property_id="logs"),
        CreateProperty(
            name_space=TESTING, class_id="TEST_EXECUTION", property_id="testProcedure"
        ),
    ],
)
