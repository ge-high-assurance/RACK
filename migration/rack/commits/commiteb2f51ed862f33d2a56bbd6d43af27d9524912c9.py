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

from ontology_changes import (
    AddClassIsATypeOf,
    AddRangeRestriction,
    ChangeCardinality,
    Commit,
    CreateClass,
    CreateProperty,
    DeleteProperty,
)
from ontology_changes.cardinality import SingleValue
from ontology_changes.range_restriction import OnlyValuesOfType
from rack.namespaces.xml_schema import XMLSCHEMA
from rack.namespaces.rack_ontology import CLAIM, PROCESS, PROV_S

commit = Commit(
    number="eb2f51ed862f33d2a56bbd6d43af27d9524912c9",
    changes=[
        # CLAIM.sadl
        CreateClass(name_space=CLAIM, class_id="CLAIM"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="CLAIM",
            range_name_space=PROV_S,
            range_id="ENTITY",
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="addresses"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="CLAIM",
            prop_name_space=CLAIM,
            prop_name="addresses",
            restriction=OnlyValuesOfType(type_namespace=PROV_S, type_class="ENTITY"),
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="declares"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="CLAIM",
            prop_name_space=CLAIM,
            prop_name="declares",
            restriction=OnlyValuesOfType(type_namespace=PROCESS, type_class="PROPERTY"),
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="appliesWhen"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="CLAIM",
            prop_name_space=CLAIM,
            prop_name="appliesWhen",
            restriction=OnlyValuesOfType(
                type_namespace=CLAIM, type_class="ENVIRONMENT_RANGE"
            ),
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="usesTheory"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="CLAIM",
            prop_name_space=CLAIM,
            prop_name="usesTheory",
            restriction=OnlyValuesOfType(type_namespace=CLAIM, type_class="THEORY"),
        ),
        CreateProperty(
            name_space=CLAIM, class_id="CLAIM", property_id="partiallySupports"
        ),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="CLAIM",
            prop_name_space=CLAIM,
            prop_name="partiallySupports",
            restriction=OnlyValuesOfType(type_namespace=CLAIM, type_class="OBJECTIVE"),
        ),
        CreateClass(name_space=CLAIM, class_id="THEORY"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="THEORY",
            range_name_space=PROV_S,
            range_id="THING",
        ),
        CreateClass(name_space=CLAIM, class_id="PROPERTY_RESULT"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="THEORY",
            range_name_space=PROV_S,
            range_id="ENTITY",
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="demonstrates"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="CLAIM",
            prop_name_space=CLAIM,
            prop_name="demonstrates",
            restriction=OnlyValuesOfType(type_namespace=CLAIM, type_class="PROPERTY"),
        ),
        ChangeCardinality(
            name_space=CLAIM,
            class_id="CLAIM",
            property_id="demonstrates",
            to_cardinality=SingleValue(),
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="supportedBy"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="CLAIM",
            prop_name_space=CLAIM,
            prop_name="supportedBy",
            restriction=OnlyValuesOfType(type_namespace=PROV_S, type_class="ENTITY"),
        ),
        CreateClass(name_space=CLAIM, class_id="COVERAGE_PROPERTY_RESULT"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="COVERAGE_PROPERTY_RESULT",
            range_name_space=CLAIM,
            range_id="PROPERTY_RESULT",
        ),
        CreateProperty(
            name_space=CLAIM, class_id="CLAIM", property_id="coverageResult"
        ),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="COVERAGE_PROPERTY_RESULT",
            prop_name_space=CLAIM,
            prop_name="coverageResultdemonstrates",
            restriction=OnlyValuesOfType(type_namespace=CLAIM, type_class="PROPERTY"),
        ),
        ChangeCardinality(
            name_space=CLAIM,
            class_id="COVERAGE_PROPERTY_RESULT",
            property_id="coverageResult",
            to_cardinality=SingleValue(),
        ),
        CreateClass(name_space=CLAIM, class_id="SUPPORTED_PROPERTY_RESULT"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="SUPPORTED_PROPERTY_RESULT",
            range_name_space=CLAIM,
            range_id="PROPERTY_RESULT",
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="supportLevel"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="SUPPORTED_PROPERTY_RESULT",
            prop_name_space=CLAIM,
            prop_name="supportLevel",
            restriction=OnlyValuesOfType(
                type_namespace=CLAIM, type_class="SUPPORT_LEVEL"
            ),
        ),
        ChangeCardinality(
            name_space=CLAIM,
            class_id="SUPPORTED_PROPERTY_RESULT",
            property_id="supportLevel",
            to_cardinality=SingleValue(),
        ),
        CreateClass(name_space=CLAIM, class_id="SUPPORT_LEVEL"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="SUPPORT_LEVEL",
            range_name_space=PROV_S,
            range_id="THING",
        ),
        # TODO: OneOf
        # TODO?: instance SupportLevelSupported
        # TODO?: instance SupportLevelUnsupported
        # TODO?: instance SupportLevelCountermanded
        CreateClass(name_space=CLAIM, class_id="ROBUSTNESS_PROPERTY_RESULT"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="ROBUSTNESS_PROPERTY_RESULT",
            range_name_space=CLAIM,
            range_id="PROPERTY_RESULT",
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="robustness"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="ROBUSTNESS_PROPERTY_RESULT",
            prop_name_space=CLAIM,
            prop_name="robustness",
            restriction=OnlyValuesOfType(type_namespace=XMLSCHEMA, type_class="double"),
        ),
        ChangeCardinality(
            name_space=CLAIM,
            class_id="ROBUSTNESS_PROPERTY_RESULT",
            property_id="robustness",
            to_cardinality=SingleValue(),
        ),
        CreateClass(name_space=CLAIM, class_id="BOOLEAN_PROPERTY_RESULT"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="BOOLEAN_PROPERTY_RESULT",
            range_name_space=CLAIM,
            range_id="PROPERTY_RESULT",
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="booleanResult"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="BOOLEAN_PROPERTY_RESULT",
            prop_name_space=CLAIM,
            prop_name="booleanResult",
            restriction=OnlyValuesOfType(
                type_namespace=XMLSCHEMA, type_class="boolean"
            ),
        ),
        ChangeCardinality(
            name_space=CLAIM,
            class_id="BOOLEAN_PROPERTY_RESULT",
            property_id="booleanResult",
            to_cardinality=SingleValue(),
        ),
        CreateClass(name_space=CLAIM, class_id="REAL_PROPERTY_RESULT"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="REAL_PROPERTY_RESULT",
            range_name_space=CLAIM,
            range_id="PROPERTY_RESULT",
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="realResult"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="REAL_PROPERTY_RESULT",
            prop_name_space=CLAIM,
            prop_name="realResult",
            restriction=OnlyValuesOfType(
                type_namespace=XMLSCHEMA, type_class="boolean"
            ),
        ),
        ChangeCardinality(
            name_space=CLAIM,
            class_id="REAL_PROPERTY_RESULT",
            property_id="realResult",
            to_cardinality=SingleValue(),
        ),
        CreateClass(name_space=CLAIM, class_id="DECISION_PROPERTY_RESULT"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="DECISION_PROPERTY_RESULT",
            range_name_space=CLAIM,
            range_id="PROPERTY_RESULT",
        ),
        CreateProperty(
            name_space=CLAIM, class_id="CLAIM", property_id="decisionOutcome"
        ),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="DECISION_PROPERTY_RESULT",
            prop_name_space=CLAIM,
            prop_name="decisionOutcome",
            restriction=OnlyValuesOfType(
                type_namespace=CLAIM, type_class="DECISION_OUTCOME"
            ),
        ),
        ChangeCardinality(
            name_space=CLAIM,
            class_id="DECISION_PROPERTY_RESULT",
            property_id="decisionOutcome",
            to_cardinality=SingleValue(),
        ),
        CreateClass(name_space=CLAIM, class_id="DECISION_PROPERTY_RESULT"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="DECISION_PROPERTY_RESULT",
            range_name_space=CLAIM,
            range_id="PROPERTY_RESULT",
        ),
        # TODO: OneOf
        # TODO?: instance DecisionOutcomeSatisfied
        # TODO?: instance DecisionOutcomeNotSatisfied
        # TODO?: instance DecisionOutcomeUnknown
        CreateClass(name_space=CLAIM, class_id="TEST_EXECUTION_PROPERTY_RESULT"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="TEST_EXECUTION_PROPERTY_RESULT",
            range_name_space=CLAIM,
            range_id="PROPERTY_RESULT",
        ),
        CreateProperty(
            name_space=CLAIM, class_id="CLAIM", property_id="testExecutionOutcome"
        ),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="TEST_EXECUTION_PROPERTY_RESULT",
            prop_name_space=CLAIM,
            prop_name="testExecutionOutcome",
            restriction=OnlyValuesOfType(
                type_namespace=CLAIM, type_class="TEST_EXECUTION_OUTCOME"
            ),
        ),
        ChangeCardinality(
            name_space=CLAIM,
            class_id="TEST_EXECUTION_PROPERTY_RESULT",
            property_id="testExecutionOutcome",
            to_cardinality=SingleValue(),
        ),
        CreateClass(name_space=CLAIM, class_id="TEST_EXECUTION_OUTCOME"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="TEST_EXECUTION_OUTCOME",
            range_name_space=PROV_S,
            range_id="THING",
        ),
        # TODO: OneOf
        # TODO?: instance TestExecutionOutcomePass
        # TODO?: instance TestExecutionOutcomeFail
        CreateClass(name_space=CLAIM, class_id="STATIC_ANALYSIS_PROPERTY_RESULT"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="STATIC_ANALYSIS_PROPERTY_RESULT",
            range_name_space=CLAIM,
            range_id="PROPERTY_RESULT",
        ),
        CreateProperty(
            name_space=CLAIM, class_id="CLAIM", property_id="staticAnalysisOutcome"
        ),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="STATIC_ANALYSIS_PROPERTY_RESULT",
            prop_name_space=CLAIM,
            prop_name="staticAnalysisOutcome",
            restriction=OnlyValuesOfType(
                type_namespace=CLAIM, type_class="STATIC_ANALYSIS_OUTCOME"
            ),
        ),
        ChangeCardinality(
            name_space=CLAIM,
            class_id="STATIC_ANALYSIS_PROPERTY_RESULT",
            property_id="staticAnalysisOutcome",
            to_cardinality=SingleValue(),
        ),
        CreateClass(name_space=CLAIM, class_id="STATIC_ANALYSIS_OUTCOME"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="STATIC_ANALYSIS_OUTCOME",
            range_name_space=PROV_S,
            range_id="THING",
        ),
        # TODO: OneOf
        # TODO?: instance StaticAnalysisOutcomeAbsent
        # TODO?: instance StaticAnalysisOutcomeMitigated
        # TODO?: instance StaticAnalysisOutcomeUnmitigated
        CreateClass(name_space=CLAIM, class_id="CONCERN_TYPE"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="CONCERN_TYPE",
            range_name_space=PROV_S,
            range_id="THING",
        ),
        CreateClass(name_space=CLAIM, class_id="CONCERN"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="CONCERN",
            range_name_space=PROV_S,
            range_id="THING",
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="questions"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="CONCERN",
            prop_name_space=CLAIM,
            prop_name="questions",
            restriction=OnlyValuesOfType(type_namespace=CLAIM, type_class="CLAIM"),
        ),
        CreateProperty(
            name_space=CLAIM, class_id="CLAIM", property_id="concernCategory"
        ),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="CONCERN",
            prop_name_space=CLAIM,
            prop_name="concernCategory",
            restriction=OnlyValuesOfType(
                type_namespace=CLAIM, type_class="CONCERN_TYPE"
            ),
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="raisedBy"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="CONCERN",
            prop_name_space=CLAIM,
            prop_name="raisedBy",
            restriction=OnlyValuesOfType(type_namespace=PROV_S, type_class="ENTITY"),
        ),
        CreateClass(name_space=CLAIM, class_id="ENVIRONMENT_FACTOR"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="ENVIRONMENT_FACTOR",
            range_name_space=PROV_S,
            range_id="THING",
        ),
        CreateClass(name_space=CLAIM, class_id="ENVIRONMENT_RANGE"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="ENVIRONMENT_RANGE",
            range_name_space=PROV_S,
            range_id="THING",
        ),
        CreateProperty(
            name_space=CLAIM, class_id="CLAIM", property_id="environmentFactor"
        ),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="ENVIRONMENT_RANGE",
            prop_name_space=CLAIM,
            prop_name="environmentFactor",
            restriction=OnlyValuesOfType(
                type_namespace=CLAIM, type_class="ENVIRONMENT_FACTOR"
            ),
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="lowerBound"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="ENVIRONMENT_RANGE",
            prop_name_space=CLAIM,
            prop_name="lowerBound",
            restriction=OnlyValuesOfType(type_namespace=XMLSCHEMA, type_class="double"),
        ),
        CreateProperty(name_space=CLAIM, class_id="CLAIM", property_id="upperBound"),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="ENVIRONMENT_RANGE",
            prop_name_space=CLAIM,
            prop_name="upperBound",
            restriction=OnlyValuesOfType(type_namespace=XMLSCHEMA, type_class="double"),
        ),
        # PROCESS.sadl
        DeleteProperty(name_space=PROCESS, property_id="partiallySupports"),
        CreateProperty(
            name_space=PROCESS, class_id="PROPERTY", property_id="propertyType"
        ),
        AddRangeRestriction(
            domain_name_space=CLAIM,
            domain_class="PROPERTY",
            prop_name_space=CLAIM,
            prop_name="propertyType",
            restriction=OnlyValuesOfType(
                type_namespace=PROCESS, type_class="PROPERTY_TYPE"
            ),
        ),
        CreateClass(name_space=CLAIM, class_id="PROPERTY_TYPE"),
        AddClassIsATypeOf(
            name_space=CLAIM,
            class_id="PROPERTY_TYPE",
            range_name_space=PROV_S,
            range_id="THING",
        ),
    ],
)
