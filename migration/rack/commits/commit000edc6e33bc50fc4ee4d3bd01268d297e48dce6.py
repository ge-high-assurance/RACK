# Copyright (c) 2023, Galois, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

from migration_helpers.create_property import create_property_with_cardinality_and_range
from ontology_changes import (
    AddClassIsATypeOf,
    AddRangeRestriction,
    AddPropertyIsATypeOf,
    ChangeClassIsATypeOf,
    Commit,
    CreateClass,
    CreateProperty,
    DeleteProperty,
)
from ontology_changes.cardinality import SingleValue, Unconstrained
from ontology_changes.range_restriction import OnlyValuesOfType
from rack.namespaces.rack_ontology import AGENTS, FILE, PROV_S, TESTING
from rack.namespaces.xml_schema import XMLSCHEMA

commit = Commit(
    number="000edc6e33bc50fc4ee4d3bd01268d297e48dce6",
    changes=[
        # AGENTS.sadl
        CreateProperty(
            name_space=AGENTS,
            class_id="PERSON",
            property_id="role",
        ),
        # TESTING.sadl
        CreateProperty(
            name_space=TESTING,
            class_id="TEST_PROCEDURE",
            property_id="independentTest",
        ),
        AddRangeRestriction(
            domain_name_space=TESTING,
            domain_class="TEST_PROCEDURE",
            prop_name_space=TESTING,
            prop_name="independentTest",
            restriction=OnlyValuesOfType(
                type_namespace=TESTING,
                type_class="TEST_STEP",
            ),
        ),
        AddPropertyIsATypeOf(
            name_space=TESTING,
            class_id="TEST_PROCEDURE",
            property_id="independentTest",
            range_name_space=PROV_S,
            range="content",
        ),
        ChangeClassIsATypeOf(
            name_space=TESTING,
            class_id="TEST_STEP",
            from_name_space=PROV_S,
            from_class_id="COLLECTION",
            to_name_space=PROV_S,
            to_class_id="ENTITY",
        ),
        DeleteProperty(
            name_space=TESTING,
            property_id="content",
        ),
    ]
    + create_property_with_cardinality_and_range(
        namespace=TESTING,
        class_id="TEST_STEP",
        property_id="thisStep",
        range=OnlyValuesOfType(
            type_namespace=TESTING,
            type_class="TEST",
        ),
        cardinality=Unconstrained(),
    )
    + [
        ChangeClassIsATypeOf(
            name_space=TESTING,
            class_id="TEST_RECORD",
            from_name_space=PROV_S,
            from_class_id="COLLECTION",
            to_name_space=PROV_S,
            to_class_id="ENTITY",
        ),
        # NOTE: this is redundant because we did not distinguish multiple
        # properties with the same name in the same namespace.  Looks like maybe
        # SADL actually supports it?
        DeleteProperty(
            name_space=TESTING,
            property_id="content",
        ),
    ]
    + create_property_with_cardinality_and_range(
        namespace=TESTING,
        class_id="TEST_RECORD",
        property_id="testRecordProcedure",
        range=OnlyValuesOfType(
            type_namespace=TESTING,
            type_class="TEST_PROCEDURE",
        ),
        cardinality=SingleValue(),
    )
    + create_property_with_cardinality_and_range(
        namespace=TESTING,
        class_id="TEST_RECORD",
        property_id="testRecordSteps",
        range=OnlyValuesOfType(
            type_namespace=TESTING,
            type_class="TEST_STEP",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=TESTING,
        class_id="TEST_RECORD",
        property_id="testConfiguration",
        range=OnlyValuesOfType(
            type_namespace=PROV_S,
            type_class="ENTITY",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=TESTING,
        class_id="TEST_RECORD",
        property_id="targetPackage",
        range=OnlyValuesOfType(
            type_namespace=FILE,
            type_class="FILE",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=TESTING,
        class_id="TEST_RECORD",
        property_id="targetVersion",
        range=OnlyValuesOfType(
            type_namespace=XMLSCHEMA,
            type_class="string",
        ),
        cardinality=SingleValue(),
    )
    + create_property_with_cardinality_and_range(
        namespace=TESTING,
        class_id="TEST_RECORD",
        property_id="testPackage",
        range=OnlyValuesOfType(
            type_namespace=FILE,
            type_class="FILE",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=TESTING,
        class_id="TEST_RECORD",
        property_id="testVersion",
        range=OnlyValuesOfType(
            type_namespace=XMLSCHEMA,
            type_class="string",
        ),
        cardinality=SingleValue(),
    )
    + create_property_with_cardinality_and_range(
        namespace=TESTING,
        class_id="TEST_EXECUTION",
        property_id="testLog",
        range=OnlyValuesOfType(
            type_namespace=TESTING,
            type_class="TEST_LOG",
        ),
        cardinality=Unconstrained(),
    )
    + [
        AddPropertyIsATypeOf(
            name_space=TESTING,
            class_id="TEST_EXECUTION",
            property_id="testLog",
            range_name_space=PROV_S,
            range="goal",
        ),
        CreateClass(name_space=TESTING, class_id="TEST_ANNOTATION"),
        AddClassIsATypeOf(
            name_space=TESTING,
            class_id="TEST_ANNOTATION",
            range_name_space=PROV_S,
            range_id="ENTITY",
        ),
    ]
    + create_property_with_cardinality_and_range(
        namespace=TESTING,
        class_id="TEST_ANNOTATION",
        property_id="annotatedValue",
        range=OnlyValuesOfType(
            type_namespace=TESTING,
            type_class="TEST_ANNOTATION_VALUE",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=TESTING,
        class_id="TEST_ANNOTATION",
        property_id="annotatedResult",
        range=OnlyValuesOfType(
            type_namespace=TESTING,
            type_class="TEST_RESULT",
        ),
        cardinality=Unconstrained(),
    )
    + [
        CreateClass(name_space=TESTING, class_id="TEST_ANNOTATION_VALUE"),
        AddClassIsATypeOf(
            name_space=TESTING,
            class_id="TEST_ANNOTATION_VALUE",
            range_name_space=PROV_S,
            range_id="THING",
        ),
        # TODO: OneOf
        # TODO? Instance
    ],
)
