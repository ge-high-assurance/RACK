# NodeGroups/Ingest-SoftwareThread.json
## Nodes

>**SoftwareDesign** : SoftwareDesign

>>Node Notes...

>>**identifier_SoftwareDesign** : string
    
>>>Prop Notes...

>**SystemComponent** : SystemComponent

>>Node Notes...

>>**identifier_SystemComponent** : string
    
>>>Prop Notes...

>**SoftwareThread** : SoftwareThread

>>Node Notes...

>>**identifier_SoftwareThread** : string
    
>>>Prop Notes...

## Edges

>**SystemComponent** - producedBy -> **SoftwareDesign**

>**SoftwareThread** - partOf -> **SystemComponent**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_SoftwareDesign| primaryKey Key for SoftwareDesign | Yes
identifier_SystemComponent| primaryKey Key for SystemComponent | Yes
identifier_SoftwareThread| primaryKey Key for SoftwareThread | No
