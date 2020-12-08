# NodeGroups/Ingest-DataAndControlCouple.json
## Nodes

>**ProducingLowLevelRequirement** : LowLevelRequirement

>>Node Notes...

>>**identifier_ProducingLowLevelRequirement** : string
    
>>>Prop Notes...

>**SoftwareDesign** : SoftwareDesign

>>Node Notes...

>>**identifier_SoftwareDesign** : string
    
>>>Prop Notes...

>**ConsumingLowLevelRequirement** : LowLevelRequirement

>>Node Notes...

>>**identifier_ConsumingLowLevelRequirement** : string
    
>>>Prop Notes...

>**DataAndControlCouple** : DataAndControlCouple

>>Node Notes...

>>**identifier_DataAndControlCouple** : string
    
>>>Prop Notes...

>>**text_DataAndControlCouple** : string
    
>>>Prop Notes...

## Edges

>**DataAndControlCouple** - consumedBy -> **ConsumingLowLevelRequirement**

>**DataAndControlCouple** - createdBy -> **SoftwareDesign**

>**DataAndControlCouple** - providedBy -> **ProducingLowLevelRequirement**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_ProducingLowLevelRequirement| primaryKey Key for ProducingLowLevelRequirement | Yes
identifier_SoftwareDesign| primaryKey Key for SoftwareDesign | Yes
identifier_ConsumingLowLevelRequirement| primaryKey Key for ConsumingLowLevelRequirement | Yes
identifier_DataAndControlCouple| primaryKey Key for DataAndControlCouple | No
text_DataAndControlCouple| | Yes
