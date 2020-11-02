# Ingest-HighLevelRequirements.json
## Nodes

>**SystemRequirement** : SystemRequirement

>>Node Notes...

>>**identifier_SystemRequirement** : string
    
>>>Prop Notes...

>**HAZARD** : HAZARD

>>Node Notes...

>>**identifier_HAZARD** : string
    
>>>Prop Notes...

>**SYSTEM** : SYSTEM

>>Node Notes...

>>**identifier_SYSTEM** : string
    
>>>Prop Notes...

>**SoftwareRequirementsDefinition** : SoftwareRequirementsDefinition

>>Node Notes...

>>**identifier_SoftwareRequirementsDefinition** : string
    
>>>Prop Notes...

>**HighLevelRequirement** : HighLevelRequirement

>>Node Notes...

>>**identifier_HighLevelRequirement** : string
    
>>>Prop Notes...

>>**text_HighLevelRequirement** : string
    
>>>Prop Notes...

## Edges

>**HighLevelRequirement** - createdBy -> **SoftwareRequirementsDefinition**

>**HighLevelRequirement** - governs -> **SYSTEM**

>**HighLevelRequirement** - mitigates -> **HAZARD**

>**HighLevelRequirement** - satisfies -> **SystemRequirement**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_SystemRequirement| Primary Key for SystemRequirement | Yes
identifier_HAZARD| Primary Key for HAZARD | Yes
identifier_SYSTEM| Primary Key for SYSTEM | Yes
identifier_SoftwareRequirementsDefinition| Primary Key for SoftwareRequirementsDefinition | Yes
identifier_HighLevelRequirement| Primary Key for HighLevelRequirement | No
text_HighLevelRequirement| | No