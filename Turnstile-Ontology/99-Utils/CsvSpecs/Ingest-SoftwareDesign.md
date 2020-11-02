# NodeGroups/Ingest-SoftwareDesign.json
## Nodes

>**HighLevelRequirement** : HighLevelRequirement

>>Node Notes...

>>**identifier_HighLevelRequirement** : string
    
>>>Prop Notes...

>**DesignStandard** : SPECIFICATION

>>Node Notes...

>>**identifier_DesignStandard** : string
    
>>>Prop Notes...

>**Engineer** : Engineer

>>Node Notes...

>>**identifier_Engineer** : string
    
>>>Prop Notes...

>**SoftwareDesign** : SoftwareDesign

>>Node Notes...

>>**identifier_SoftwareDesign** : string
    
>>>Prop Notes...

## Edges

>**SoftwareDesign** - author -> **Engineer**

>**SoftwareDesign** - governedBy -> **DesignStandard**

>**SoftwareDesign** - used -> **HighLevelRequirement**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_HighLevelRequirement| primaryKey Key for HighLevelRequirement | Yes
identifier_DesignStandard| primaryKey Key for DesignStandard | Yes
identifier_Engineer| primaryKey Key for Engineer | Yes
identifier_SoftwareDesign| primaryKey Key for SoftwareDesign | No
