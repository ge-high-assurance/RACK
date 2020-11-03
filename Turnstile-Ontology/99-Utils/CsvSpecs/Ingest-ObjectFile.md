# NodeGroups/Ingest-ObjectFile.json
## Nodes

>**FORMAT** : FORMAT

>>Node Notes...

>>**identifier_FORMAT** : string
    
>>>Prop Notes...

>**SoftwareIntegration** : SoftwareIntegration

>>Node Notes...

>>**identifier_SoftwareIntegration** : string
    
>>>Prop Notes...

>**ObjectFile** : ObjectFile

>>Node Notes...

>>**filename_ObjectFile** : string
    
>>>Prop Notes...

>>**identifier_ObjectFile** : string
    
>>>Prop Notes...

## Edges

>**ObjectFile** - createBy -> **SoftwareIntegration**

>**ObjectFile** - fileFormat -> **FORMAT**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_FORMAT| primaryKey Key for FORMAT | Yes
identifier_SoftwareIntegration| primaryKey Key for SoftwareIntegration | Yes
filename_ObjectFile| | No
identifier_ObjectFile| primaryKey Key for ObjectFile | No
