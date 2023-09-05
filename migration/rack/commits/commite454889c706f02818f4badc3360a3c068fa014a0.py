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

from migration_helpers.create_class import create_class_with_type_of
from migration_helpers.create_property import create_property_with_cardinality_and_range
from migration_helpers.relocate_class_and_properties import (
    relocate_class_and_properties,
)
from ontology_changes import (
    AddClassIsATypeOf,
    AddPropertyIsATypeOf,
    AddRangeRestriction,
    Commit,
    CreateClass,
    RenameClass,
)
from ontology_changes.cardinality import AtMost, SingleValue, Unconstrained
from rack.namespaces.rack_ontology import (
    HARDWARE,
    HAZARD,
    PROCESS,
    PROV_S,
    REQUIREMENTS,
    SAFETY_SECURITY,
    SECURITY,
    SOFTWARE,
    SYSTEM,
)
from rack.namespaces.xml_schema import XMLSCHEMA
from ontology_changes.range_restriction import OnlyValuesOfType

commit = Commit(
    number="e454889c706f02818f4badc3360a3c068fa014a0",
    changes=[
        # REQUIREMENTS.sadl
        CreateClass(
            name_space=REQUIREMENTS,
            class_id="REQUIREMENT_SET",
        ),
        AddClassIsATypeOf(
            name_space=REQUIREMENTS,
            class_id="REQUIREMENT_SET",
            range_name_space=PROV_S,
            range_id="COLLECTION",
        ),
        AddRangeRestriction(
            domain_name_space=REQUIREMENTS,
            domain_class="REQUIREMENT_SET",
            prop_name_space=PROV_S,
            prop_name="content",
            restriction=OnlyValuesOfType(
                type_namespace=REQUIREMENTS,
                type_class="REQUIREMENT",
            ),
        ),
    ]
    + create_property_with_cardinality_and_range(
        namespace=REQUIREMENTS,
        class_id="REQUIREMENT_SET",
        property_id="specifies",
        range=OnlyValuesOfType(
            type_namespace=PROV_S,
            type_class="ENTITY",
        ),
        cardinality=Unconstrained(),
    )
    # SAFETY_SECURITY.sadl
    # NOTE: The following were moved from SECURITY.sadl, see further below:
    # - THREAT
    # - THREAT_IDENTIFICATION
    # - SECURITY_LABEL
    # - CONTROL
    # - CONTROLSET
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="PORT",
        type_of_namespace=SAFETY_SECURITY,
        type_of_class="PORT_DIRECTION",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="PORT_DIRECTION",
        type_of_namespace=PROV_S,
        type_of_class="THING",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="GUARANTEE",
        type_of_namespace=PROCESS,
        type_of_class="PROPERTY",
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="PORT",
        property_id="direction",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="PORT_DIRECTION",
        ),
        cardinality=SingleValue(),
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="COMPONENT_GUARANTEE",
        type_of_namespace=SAFETY_SECURITY,
        type_of_class="GUARANTEE",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="INTERFACE_GUARANTEE",
        type_of_namespace=SAFETY_SECURITY,
        type_of_class="GUARANTEE",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="GUARANTEE_TYPE",
        type_of_namespace=PROV_S,
        type_of_class="THING",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="PEDIGREE",
        type_of_namespace=PROV_S,
        type_of_class="THING",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="TRUSTWORTHINESS",
        type_of_namespace=PROV_S,
        type_of_class="THING",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="INTERFACE_TYPE",
        type_of_namespace=PROV_S,
        type_of_class="THING",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="HWCOMPONENT_SS",
        type_of_namespace=HARDWARE,
        type_of_class="HWCOMPONENT",
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="HWCOMPONENT_SS",
        property_id="port",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="PORT",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="HWCOMPONENT_SS",
        property_id="partOf",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="HWCOMPONENT_SS",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="HWCOMPONENT_SS",
        property_id="hosts",
        range=OnlyValuesOfType(
            type_namespace=SOFTWARE,
            type_class="SWCOMPONENT",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="HWCOMPONENT_SS",
        property_id="pedigree",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="PEDIGREE",
        ),
        cardinality=AtMost(1),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="HWCOMPONENT_SS",
        property_id="trustworthiness",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="TRUSTWORTHINESS",
        ),
        cardinality=AtMost(1),
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="SWCOMPONENT_SS",
        type_of_namespace=SOFTWARE,
        type_of_class="SWCOMPONENT",
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="SWCOMPONENT_SS",
        property_id="instantiates",
        range=OnlyValuesOfType(
            type_namespace=SYSTEM,
            type_class="FUNCTION",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="SWCOMPONENT_SS",
        property_id="partOf",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="SWCOMPONENT_SS",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="SWCOMPONENT_SS",
        property_id="pedigree",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="PEDIGREE",
        ),
        cardinality=AtMost(1),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="SWCOMPONENT_SS",
        property_id="trustworthiness",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="TRUSTWORTHINESS",
        ),
        cardinality=AtMost(1),
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="PHYSICAL_INTERFACE",
        type_of_namespace=PROV_S,
        type_of_class="ENTITY",
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="PHYSICAL_INTERFACE",
        property_id="srcPort",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="PORT",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="PHYSICAL_INTERFACE",
        property_id="dstPort",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="PORT",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="PHYSICAL_INTERFACE",
        property_id="protocol",
        range=OnlyValuesOfType(
            type_namespace=PROV_S,
            type_class="ENTITY",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="PHYSICAL_INTERFACE",
        property_id="pedigree",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="PEDIGREE",
        ),
        cardinality=AtMost(1),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="PHYSICAL_INTERFACE",
        property_id="trustworthiness",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="TRUSTWORTHINESS",
        ),
        cardinality=AtMost(1),
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="VIRTUAL_CHANNEL",
        type_of_namespace=SYSTEM,
        type_of_class="INTERFACE",
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="VIRTUAL_CHANNEL",
        property_id="utilizes",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="PHYSICAL_INTERFACE",
        ),
        cardinality=Unconstrained(),
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="DATA_FLOW",
        type_of_namespace=PROV_S,
        type_of_class="ENTITY",
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="DATA_FLOW",
        property_id="communicatesOver",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="VIRTUAL_CHANNEL",
        ),
        cardinality=SingleValue(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="DATA_FLOW",
        property_id="source",
        range=OnlyValuesOfType(
            type_namespace=SYSTEM,
            type_class="FUNCTION",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="DATA_FLOW",
        property_id="destination",
        range=OnlyValuesOfType(
            type_namespace=SYSTEM,
            type_class="FUNCTION",
        ),
        cardinality=Unconstrained(),
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="SAFETY_DESIGN_ASSURANCE_LEVEL",
        type_of_namespace=PROV_S,
        type_of_class="THING",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="SECURITY_ASSURANCE_LEVEL",
        type_of_namespace=PROV_S,
        type_of_class="THING",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="SECURITY_ENCLAVE",
        type_of_namespace=PROV_S,
        type_of_class="COLLECTION",
    )
    + [
        AddRangeRestriction(
            domain_name_space=SAFETY_SECURITY,
            domain_class="SECURITY_ENCLAVE",
            prop_name_space=PROV_S,
            prop_name="content",
            # FIXME: need disjunctions
            restriction=OnlyValuesOfType(
                type_namespace=SAFETY_SECURITY,  # or others...
                type_class="{HWCOMPONENT or PHYSICAL_INTERFACE or SWCOMPONENT or PORT}",
            ),
        )
    ]
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="SECURITY_ENCLAVE",
        property_id="dal",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="SAFETY_DESIGN_ASSURANCE_LEVEL",
        ),
        cardinality=AtMost(1),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="SECURITY_ENCLAVE",
        property_id="sal",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="SECURITY_ASSURANCE_LEVEL",
        ),
        cardinality=AtMost(1),
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="SECURITY_PERIMETER",
        type_of_namespace=PROV_S,
        type_of_class="COLLECTION",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="VULNERABILITY",
        type_of_namespace=PROV_S,
        type_of_class="ENTITY",
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="VULNERABILITY",
        property_id="mitigatedBy",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,  # or PROCESS
            # TODO: support disjunctions
            type_class="{CONTROL or PROPERTY}",
        ),
        cardinality=Unconstrained(),
    )
    + [
        AddPropertyIsATypeOf(
            name_space=SAFETY_SECURITY,
            class_id="VULNERABILITY",
            property_id="mitigatedBy",
            range_name_space=PROV_S,
            range="wasImpactedBy",
        )
    ]
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="VULNERABILITY",
        property_id="enhancedBy",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,  # or PROCESS
            # TODO: support disjunctions
            type_class="{CONTROL or PROPERTY}",
        ),
        cardinality=Unconstrained(),
    )
    + [
        AddPropertyIsATypeOf(
            name_space=SAFETY_SECURITY,
            class_id="VULNERABILITY",
            property_id="enhancedBy",
            range_name_space=PROV_S,
            range="wasImpactedBy",
        )
    ]
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="VULNERABILITY",
        property_id="recoveredBy",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,  # or PROCESS
            # TODO: support disjunctions
            type_class="{CONTROL or PROPERTY}",
        ),
        cardinality=Unconstrained(),
    )
    + [
        AddPropertyIsATypeOf(
            name_space=SAFETY_SECURITY,
            class_id="VULNERABILITY",
            property_id="recoveredBy",
            range_name_space=PROV_S,
            range="wasImpactedBy",
        )
    ]
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="VULNERABILITY",
        property_id="vulnerabilityTouchPoints",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="ARCHITECTURE_TOUCHPOINTS",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="VULNERABILITY",
        property_id="cweReference",
        range=OnlyValuesOfType(
            type_namespace=PROV_S,
            type_class="ENTITY",
        ),
        cardinality=Unconstrained(),
    )
    + [
        AddPropertyIsATypeOf(
            name_space=SAFETY_SECURITY,
            class_id="VULNERABILITY",
            property_id="cweReference",
            range_name_space=PROV_S,
            range="wasImpactedBy",
        )
    ]
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="ATTACKER",
        type_of_namespace=PROV_S,
        type_of_class="AGENT",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="ATTACK",
        type_of_namespace=PROV_S,
        type_of_class="ACTIVITY",
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="VULNERABILITY",
        property_id="attacker",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="ATTACKER",
        ),
        cardinality=Unconstrained(),
    )
    + [
        AddPropertyIsATypeOf(
            name_space=SAFETY_SECURITY,
            class_id="VULNERABILITY",
            property_id="attacker",
            range_name_space=PROV_S,
            range="wasAssociatedWith",
        )
    ]
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="ATTACK_ACCESS_VECTORS",
        type_of_namespace=PROV_S,
        type_of_class="ENTITY",
    )
    + [
        AddRangeRestriction(
            domain_name_space=SAFETY_SECURITY,
            domain_class="ATTACK_ACCESS_VECTORS",
            prop_name_space=PROV_S,
            prop_name="wasGeneratedBy",
            restriction=OnlyValuesOfType(
                type_namespace=SAFETY_SECURITY,
                type_class="ATTACK",
            ),
        )
    ]
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="ATTACK_ACCESS_VECTORS",
        property_id="attackTouchPoints",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="ATTACK",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="ATTACK_ACCESS_VECTORS",
        property_id="capecReference",
        range=OnlyValuesOfType(
            type_namespace=PROV_S,
            type_class="ENTITY",
        ),
        cardinality=Unconstrained(),
    )
    + [
        AddPropertyIsATypeOf(
            name_space=SAFETY_SECURITY,
            class_id="ATTACK_ACCESS_VECTORS",
            property_id="capecReference",
            range_name_space=PROV_S,
            range="wasImpactedBy",
        )
    ]
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="ARCHITECTURE_TOUCHPOINTS",
        type_of_namespace=PROV_S,
        type_of_class="COLLECTION",
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="CONTROL",
        property_id="mitigatesHazard",
        range=OnlyValuesOfType(
            type_namespace=HAZARD,
            type_class="HAZARD_CONDITION",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="CONTROL",
        property_id="enhancesHazard",
        range=OnlyValuesOfType(
            type_namespace=HAZARD,
            type_class="HAZARD_CONDITION",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="CONTROL",
        property_id="location",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="ARCHITECTURE_TOUCHPOINTS",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="CONTROL",
        property_id="nist_800_53Reference",
        range=OnlyValuesOfType(
            type_namespace=PROV_S,
            type_class="ENTITY",
        ),
        cardinality=Unconstrained(),
    )
    + [
        AddPropertyIsATypeOf(
            name_space=SAFETY_SECURITY,
            class_id="CONTROL",
            property_id="nist_800_53Reference",
            range_name_space=PROV_S,
            range="wasImpactedBy",
        )
    ]
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="LOSS_CATEGORY",
        type_of_namespace=SAFETY_SECURITY,
        type_of_class="SECURITY_LABEL",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="HAZARD_CONDITION",
        type_of_namespace=HAZARD,
        type_of_class="HAZARD",
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="HAZARD_CONDITION",
        property_id="mitigatesControl",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="CONTROL",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="HAZARD_CONDITION",
        property_id="enhancesControl",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="CONTROL",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="HAZARD_CONDITION",
        property_id="lossCategory",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="LOSS_CATEGORY",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="HAZARD_CONDITION",
        property_id="triggers",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="SAFETY_ACCIDENT",
        ),
        cardinality=Unconstrained(),
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="EXPLOITATION",
        type_of_namespace=PROV_S,
        type_of_class="ENTITY",
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="EXPLOITATION",
        property_id="uses",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="ATTACK_ACCESS_VECTORS",
        ),
        cardinality=Unconstrained(),
    )
    + [
        AddPropertyIsATypeOf(
            name_space=SAFETY_SECURITY,
            class_id="EXPLOITATION",
            property_id="uses",
            range_name_space=PROV_S,
            range="wasImpactedBy",
        )
    ]
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="EXPLOITATION",
        property_id="exploits",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="VULNERABILITY",
        ),
        cardinality=Unconstrained(),
    )
    + [
        AddPropertyIsATypeOf(
            name_space=SAFETY_SECURITY,
            class_id="EXPLOITATION",
            property_id="exploits",
            range_name_space=PROV_S,
            range="wasImpactedBy",
        )
    ]
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="THREAT_CONDITION",
        type_of_namespace=SAFETY_SECURITY,
        type_of_class="THREAT",
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="THREAT_CONDITION",
        property_id="source",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="EXPLOITATION",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="THREAT_CONDITION",
        property_id="securityLabel",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="SECURITY_LABEL",
        ),
        cardinality=Unconstrained(),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="THREAT_CONDITION",
        property_id="triggers",
        range=OnlyValuesOfType(
            type_namespace=SAFETY_SECURITY,
            type_class="SECURITY_VIOLATION",
        ),
        cardinality=Unconstrained(),
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="SAFETY_REQUIREMENT",
        type_of_namespace=REQUIREMENTS,
        type_of_class="REQUIREMENT",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="SECURITY_REQUIREMENT",
        type_of_namespace=REQUIREMENTS,
        type_of_class="REQUIREMENT",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="SAFETY_REQUIREMENT_SET",
        type_of_namespace=REQUIREMENTS,
        type_of_class="REQUIREMENT_SET",
    )
    + [
        AddRangeRestriction(
            domain_name_space=SAFETY_SECURITY,
            domain_class="SAFETY_REQUIREMENT_SET",
            prop_name_space=PROV_S,
            prop_name="content",
            restriction=OnlyValuesOfType(
                type_namespace=SAFETY_SECURITY,
                type_class="SAFETY_REQUIREMENT",
            ),
        )
    ]
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="SECURITY_REQUIREMENT_SET",
        type_of_namespace=REQUIREMENTS,
        type_of_class="REQUIREMENT_SET",
    )
    + [
        AddRangeRestriction(
            domain_name_space=SAFETY_SECURITY,
            domain_class="SECURITY_REQUIREMENT_SET",
            prop_name_space=PROV_S,
            prop_name="content",
            restriction=OnlyValuesOfType(
                type_namespace=SAFETY_SECURITY,
                type_class="SECURITY_REQUIREMENT",
            ),
        )
    ]
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="RISK_EVENT",
        type_of_namespace=PROV_S,
        type_of_class="ENTITY",
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="RISK_EVENT",
        property_id="severity",
        range=OnlyValuesOfType(
            type_namespace=XMLSCHEMA,
            type_class="float",
        ),
        cardinality=AtMost(1),
    )
    + create_property_with_cardinality_and_range(
        namespace=SAFETY_SECURITY,
        class_id="RISK_EVENT",
        property_id="probability",
        range=OnlyValuesOfType(
            type_namespace=XMLSCHEMA,
            type_class="float",
        ),
        cardinality=AtMost(1),
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="SECURITY_VIOLATION",
        type_of_namespace=SAFETY_SECURITY,
        type_of_class="RISK_EVENT",
    )
    + create_class_with_type_of(
        namespace=SAFETY_SECURITY,
        class_id="SAFETY_ACCIDENT",
        type_of_namespace=SAFETY_SECURITY,
        type_of_class="RISK_EVENT",
    )
    # SECURITY.sadl
    + relocate_class_and_properties(
        from_namespace=SECURITY,
        to_namespace=SAFETY_SECURITY,
        class_id="THREAT",
        properties=["source", "identified", "effect", "severity", "likelihood"],
    )
    + relocate_class_and_properties(
        from_namespace=SECURITY,
        to_namespace=SAFETY_SECURITY,
        class_id="THREAT_IDENTIFICATION",
        properties=["author"],
    )
    + relocate_class_and_properties(
        from_namespace=SECURITY,
        to_namespace=SAFETY_SECURITY,
        class_id="SECURITY_LABEL",
        properties=[],
    )
    + relocate_class_and_properties(
        from_namespace=SECURITY,
        to_namespace=SAFETY_SECURITY,
        class_id="CONTROL",
        properties=[],
    )
    + relocate_class_and_properties(
        from_namespace=SECURITY,
        to_namespace=SAFETY_SECURITY,
        class_id="CONTROLSET",
        properties=["content", "mitigates"],
    ),
)
