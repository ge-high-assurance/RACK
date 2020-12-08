# NodeGroups/Ingest-SoftwareIntegrationCompileInput.json
## Nodes

>**ObjectFile** : ObjectFile

>>Node Notes...

>>**identifier_ObjectFile** : string
    
>>>Prop Notes...

>**SourceCode** : SourceCode

>>Node Notes...

>>**identifier_SourceCode** : string
    
>>>Prop Notes...

>**SoftwareIntegration** : SoftwareIntegration

>>Node Notes...

>>**identifier_SoftwareIntegration** : string
    
>>>Prop Notes...

## Edges

>**SoftwareIntegration** - compileInput -> **SourceCode**

>**SoftwareIntegration** - compileInput -> **ObjectFile**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_ObjectFile| primaryKey Key for ObjectFile | Yes
identifier_SourceCode| primaryKey Key for SourceCode | Yes
identifier_SoftwareIntegration| primaryKey Key for SoftwareIntegration | No
