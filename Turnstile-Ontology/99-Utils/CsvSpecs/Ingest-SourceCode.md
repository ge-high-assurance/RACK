# NodeGroups/Ingest-SourceCode.json
## Nodes

>**FORMAT** : FORMAT

>>Node Notes...

>>**identifier_FORMAT** : string
    
>>>Prop Notes...

>**SoftwareCoding** : SoftwareCoding

>>Node Notes...

>>**identifier_SoftwareCoding** : string
    
>>>Prop Notes...

>**SourceCode** : SourceCode

>>Node Notes...

>>**filename_SourceCode** : string
    
>>>Prop Notes...

>>**identifier_SourceCode** : string
    
>>>Prop Notes...

## Edges

>**SourceCode** - createBy -> **SoftwareCoding**

>**SourceCode** - fileFormat -> **FORMAT**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_FORMAT| primaryKey Key for FORMAT | Yes
identifier_SoftwareCoding| primaryKey Key for SoftwareCoding | Yes
filename_SourceCode| | No
identifier_SourceCode| primaryKey Key for SourceCode | No
