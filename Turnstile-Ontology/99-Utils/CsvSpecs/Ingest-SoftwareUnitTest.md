# NodeGroups/Ingest-SoftwareUnitTest.json
## Nodes

>**LowLevelRequirement** : LowLevelRequirement

>>Node Notes...

>>**identifier_LowLevelRequirement** : string
    
>>>Prop Notes...

>**DevelopUnitTests** : DevelopUnitTests

>>Node Notes...

>>**identifier_DevelopUnitTests** : string
    
>>>Prop Notes...

>**SoftwareUnitTest** : SoftwareUnitTest

>>Node Notes...

>>**identifier_SoftwareUnitTest** : string
    
>>>Prop Notes...

## Edges

>**SoftwareUnitTest** - producedBy -> **DevelopUnitTests**

>**SoftwareUnitTest** - verifies -> **LowLevelRequirement**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_LowLevelRequirement| primaryKey Key for LowLevelRequirement | Yes
identifier_DevelopUnitTests| primaryKey Key for DevelopUnitTests | Yes
identifier_SoftwareUnitTest| primaryKey Key for SoftwareUnitTest | No
