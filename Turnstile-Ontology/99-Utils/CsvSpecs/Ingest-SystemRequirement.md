# NodeGroups/Ingest-SystemRequirement.json
## Nodes

>**SystemComponent** : SystemComponent

>>Node Notes...

>>**identifier_SystemComponent** : string
    
>>>Prop Notes...

>**SystemRequirement** : SystemRequirement

>>Node Notes...

>>**identifier_SystemRequirement** : string
    
>>>Prop Notes...

>>**text_SystemRequirement** : string
    
>>>Prop Notes...

## Edges

>**SystemRequirement** - governs -> **SystemComponent**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_SystemComponent| primaryKey Key for SystemComponent | Yes
identifier_SystemRequirement| primaryKey Key for SystemRequirement | No
text_SystemRequirement| | Yes
