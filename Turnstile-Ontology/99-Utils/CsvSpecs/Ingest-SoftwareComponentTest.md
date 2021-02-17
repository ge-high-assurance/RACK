# NodeGroups/Ingest-SoftwareComponentTest.json
## Nodes

>**HighLevelRequirement** : HighLevelRequirement

>>Node Notes...

>>**identifier_HighLevelRequirement** : string
    
>>>Prop Notes...

>**DevelopComponentTests** : DevelopComponentTests

>>Node Notes...

>>**identifier_DevelopComponentTests** : string
    
>>>Prop Notes...

>**SoftwareComponentTest** : SoftwareComponentTest

>>Node Notes...

>>**identifier_SoftwareComponentTest** : string
    
>>>Prop Notes...

## Edges

>**SoftwareComponentTest** - producedBy -> **DevelopComponentTests**

>**SoftwareComponentTest** - verifies -> **HighLevelRequirement**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_HighLevelRequirement| primaryKey Key for HighLevelRequirement | No
identifier_DevelopComponentTests| primaryKey Key for DevelopComponentTests | Yes
identifier_SoftwareComponentTest| primaryKey Key for SoftwareComponentTest | No
