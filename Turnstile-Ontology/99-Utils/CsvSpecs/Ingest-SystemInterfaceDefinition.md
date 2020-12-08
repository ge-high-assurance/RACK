# NodeGroups/Ingest-SystemInterfaceDefinition.json
## Nodes

>**SourceSystemComponent** : SystemComponent

>>Node Notes...

>>**identifier_SourceSystemComponent** : string
    
>>>Prop Notes...

>**DestinationSystemComponent** : SystemComponent

>>Node Notes...

>>**identifier_DestinationSystemComponent** : string
    
>>>Prop Notes...

>**SystemInterfaceDefinition** : SystemInterfaceDefinition

>>Node Notes...

>>**identifier_SystemInterfaceDefinition** : string
    
>>>Prop Notes...

## Edges

>**SystemInterfaceDefinition** - destination -> **DestinationSystemComponent**

>**SystemInterfaceDefinition** - source -> **SourceSystemComponent**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_SourceSystemComponent| primaryKey Key for SourceSystemComponent | Yes
identifier_DestinationSystemComponent| primaryKey Key for DestinationSystemComponent | Yes
identifier_SystemInterfaceDefinition| primaryKey Key for SystemInterfaceDefinition | No
