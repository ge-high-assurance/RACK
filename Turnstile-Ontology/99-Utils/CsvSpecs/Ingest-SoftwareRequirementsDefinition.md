# NodeGroups/Ingest-SoftwareRequirementsDefinition.json
## Nodes

>**SystemInterfaceDefinition** : SystemInterfaceDefinition

>>Node Notes...

>>**identifier_SystemInterfaceDefinition** : string
    
>>>Prop Notes...

>**SystemRequirement** : SystemRequirement

>>Node Notes...

>>**identifier_SystemRequirement** : string
    
>>>Prop Notes...

>**RequirementStandard** : SPECIFICATION

>>Node Notes...

>>**identifier_RequirementStandard** : string
    
>>>Prop Notes...

>**Engineer** : Engineer

>>Node Notes...

>>**identifier_Engineer** : string
    
>>>Prop Notes...

>**SoftwareRequirementsDefinition** : SoftwareRequirementsDefinition

>>Node Notes...

>>**identifier_SoftwareRequirementsDefinition** : string
    
>>>Prop Notes...

## Edges

>**SoftwareRequirementsDefinition** - author -> **Engineer**

>**SoftwareRequirementsDefinition** - governedBy -> **RequirementStandard**

>**SoftwareRequirementsDefinition** - used -> **SystemRequirement**

>**SoftwareRequirementsDefinition** - used -> **SystemInterfaceDefinition**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_SystemInterfaceDefinition| primaryKey Key for SystemInterfaceDefinition | Yes
identifier_SystemRequirement| primaryKey Key for SystemRequirement | Yes
identifier_RequirementStandard| primaryKey Key for RequirementStandard | Yes
identifier_Engineer| primaryKey Key for Engineer | Yes
identifier_SoftwareRequirementsDefinition| primaryKey Key for SoftwareRequirementsDefinition | No
