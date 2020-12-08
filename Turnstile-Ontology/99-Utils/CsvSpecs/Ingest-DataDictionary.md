# NodeGroups/Ingest-DataDictionary.json
## Nodes

>**ProducingSystemInterfaceDefinition** : SystemInterfaceDefinition

>>Node Notes...

>>**identifier_ProducingSystemInterfaceDefinition** : string
    
>>>Prop Notes...

>**ProvidingHighLevelRequirement** : HighLevelRequirement

>>Node Notes...

>>**identifier_ProvidingHighLevelRequirement** : string
    
>>>Prop Notes...

>**SoftwareRequirementsDefinition** : SoftwareRequirementsDefinition

>>Node Notes...

>>**identifier_SoftwareRequirementsDefinition** : string
    
>>>Prop Notes...

>**ConsumingSystemInterfaceDefinition** : SystemInterfaceDefinition

>>Node Notes...

>>**identifier_ConsumingSystemInterfaceDefinition** : string
    
>>>Prop Notes...

>**ConsumingHighLevelRequirement** : HighLevelRequirement

>>Node Notes...

>>**identifier_ConsumingHighLevelRequirement** : string
    
>>>Prop Notes...

>**DataDictionary** : DataDictionary

>>Node Notes...

>>**identifier_DataDictionary** : string
    
>>>Prop Notes...

>>**text_DataDictionary** : string
    
>>>Prop Notes...

## Edges

>**DataDictionary** - consumedBy -> **ConsumingHighLevelRequirement**

>**DataDictionary** - consumedBy -> **ConsumingSystemInterfaceDefinition**

>**DataDictionary** - createdBy -> **SoftwareRequirementsDefinition**

>**DataDictionary** - providedBy -> **ProvidingHighLevelRequirement**

>**DataDictionary** - providedBy -> **ProducingSystemInterfaceDefinition**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_ProducingSystemInterfaceDefinition| primaryKey Key for ProducingSystemInterfaceDefinition | Yes
identifier_ProvidingHighLevelRequirement| primaryKey Key for ProvidingHighLevelRequirement | Yes
identifier_SoftwareRequirementsDefinition| primaryKey Key for SoftwareRequirementsDefinition | Yes
identifier_ConsumingSystemInterfaceDefinition| primaryKey Key for ConsumingSystemInterfaceDefinition | Yes
identifier_ConsumingHighLevelRequirement| primaryKey Key for ConsumingHighLevelRequirement | Yes
identifier_DataDictionary| primaryKey Key for DataDictionary | No
text_DataDictionary| | Yes
