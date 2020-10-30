# Ingest-LowLevelRequirements.json
## Nodes

>**HighLevelRequirement** : HighLevelRequirement

>>Node Notes...

>>**identifier_HighLevelRequirement** : string
    
>>>Prop Notes...

>**HAZARD** : HAZARD

>>Node Notes...

>>**identifier_HAZARD** : string
    
>>>Prop Notes...

>**SYSTEM** : SYSTEM

>>Node Notes...

>>**identifier_SYSTEM** : string
    
>>>Prop Notes...

>**SoftwareDesign** : SoftwareDesign

>>Node Notes...

>>**identifier_SoftwareDesign** : string
    
>>>Prop Notes...

>**LowLevelRequirement** : LowLevelRequirement

>>Node Notes...

>>**identifier_LowLevelRequirement** : string
    
>>>Prop Notes...

>>**text_LowLevelRequirement** : string
    
>>>Prop Notes...

## Edges

>**LowLevelRequirement** - createdBy -> **SoftwareDesign**

>**LowLevelRequirement** - governs -> **SYSTEM**

>**LowLevelRequirement** - mitigates -> **HAZARD**

>**LowLevelRequirement** - satisfies -> **HighLevelRequirement**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_HighLevelRequirement| primaryKey Key for HighLevelRequirement | Yes
identifier_HAZARD| primaryKey Key for HAZARD | Yes
identifier_SYSTEM| primaryKey Key for SYSTEM | Yes
identifier_SoftwareDesign| primaryKey Key for SoftwareDesign | Yes
identifier_LowLevelRequirement| primaryKey Key for LowLevelRequirement | No
text_LowLevelRequirement| | Yes
