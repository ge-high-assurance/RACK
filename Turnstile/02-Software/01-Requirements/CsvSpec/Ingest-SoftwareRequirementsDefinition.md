# Ingest-SoftwareRequirementsDefinition.json
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
identifier_SystemInterfaceDefinition| Primary Key for SystemInterfaceDefinition | Yes
identifier_SystemRequirement| Primary Key for SystemRequirement | Yes
identifier_RequirementStandard| Primary Key for RequirementStandard | Yes
identifier_Engineer| Primary Key for Engineer | Yes
identifier_SoftwareRequirementsDefinition| Primary Key for SoftwareRequirementsDefinition | No
