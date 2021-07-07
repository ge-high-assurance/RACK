# NodeGroups/Ingest-SoftwareComponent.json
## Nodes

>**SUBCOMPONENT** : SWCOMPONENT

>>Node Notes...

>>**identifier_SUBCOMPONENT** : string
    
>>>Prop Notes...

>**REQUIREMENT** : ENTITY

>>Node Notes...

>>**identifier_REQUIREMENT** : string
    
>>>Prop Notes...

>**MENTION** : ENTITY

>>Node Notes...

>>**identifier_MENTION** : string
    
>>>Prop Notes...

>**INSTANTIATE** : ENTITY

>>Node Notes...

>>**identifier_INSTANTIATE** : string
    
>>>Prop Notes...

>**DEFINE_IN** : ENTITY

>>Node Notes...

>>**identifier_DEFINE_IN** : string
    
>>>Prop Notes...

>**COMPONENT_TYPE** : COMPONENT_TYPE

>>Node Notes...

>>**identifier_COMPONENT_TYPE** : string
    
>>>Prop Notes...

>**ANNOTATION** : ENTITY

>>Node Notes...

>>**identifier_ANNOTATION** : string
    
>>>Prop Notes...

>**COMPONENT** : COMPONENT

>>Node Notes...

>>**name_COMPONENT** : string
    
>>>Prop Notes...

>>**identifier_COMPONENT** : string
    
>>>Prop Notes...

>>**valueType_COMPONENT** : string
    
>>>Prop Notes...

## Edges

>**COMPONENT** - componentType -> **COMPONENT_TYPE**

>**COMPONENT** - definedIn -> **DEFINE_IN**

>**COMPONENT** - instantiates -> **INSTANTIATE**

>**COMPONENT** - mentions -> **MENTION**

>**COMPONENT** - subcomponentOf -> **SUBCOMPONENT**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_SUBCOMPONENT| primaryKey Key for SUBCOMPONENT | Yes
identifier_REQUIREMENT| primaryKey Key for REQUIREMENT | Yes
identifier_MENTION| primaryKey Key for MENTION | Yes
identifier_INSTANTIATE| primaryKey Key for INSTANTIATE | Yes
identifier_DEFINE_IN| primaryKey Key for DEFINE_IN | Yes
identifier_COMPONENT_TYPE| primaryKey Key for COMPONENT_TYPE | No
identifier_ANNOTATION| primaryKey Key for ANNOTATION | Yes
name_COMPONENT| | Yes
identifier_COMPONENT| primaryKey Key for COMPONENT | No
valueType_COMPONENT| | Yes
