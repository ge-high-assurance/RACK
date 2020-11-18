# NodeGroups/Ingest-HAZARD.json
## Nodes

>**PARENT_HAZARD** : HAZARD

>>Node Notes...

>>**identifier_PARENT_HAZARD** : string
    
>>>Prop Notes...

>**SYSTEM** : SYSTEM

>>Node Notes...

>>**identifier_SYSTEM** : string
    
>>>Prop Notes...

>**HAZARD** : HAZARD

>>Node Notes...

>>**definition_HAZARD** : string
    
>>>Prop Notes...

>>**identifier_HAZARD** : string
    
>>>Prop Notes...

## Edges

>**HAZARD** - source -> **SYSTEM**

>**HAZARD** - source -> **PARENT_HAZARD**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_PARENT_HAZARD| primaryKey Key for PARENT_HAZARD | Yes
identifier_SYSTEM| primaryKey Key for SYSTEM | Yes
definition_HAZARD| | Yes
identifier_HAZARD| primaryKey Key for HAZARD | No
