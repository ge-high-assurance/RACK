# NodeGroups/Ingest-SoftwareRequirementsReview.json
## Nodes

>**Reviewer** : Engineer

>>Node Notes...

>>**identifier_Reviewer** : string
    
>>>Prop Notes...

>**DataDictionary** : DataDictionary

>>Node Notes...

>>**identifier_DataDictionary** : string
    
>>>Prop Notes...

>**HighLevelRequirement** : HighLevelRequirement

>>Node Notes...

>>**identifier_HighLevelRequirement** : string
    
>>>Prop Notes...

>**SPECIFICATION** : SPECIFICATION

>>Node Notes...

>>**identifier_SPECIFICATION** : string
    
>>>Prop Notes...

>**Author** : Engineer

>>Node Notes...

>>**identifier_Author** : string
    
>>>Prop Notes...

>**SoftwareRequirementsReview** : SoftwareRequirementsReview

>>Node Notes...

>>**identifier_SoftwareRequirementsReview** : string
    
>>>Prop Notes...

## Edges

>**SoftwareRequirementsReview** - author -> **Author**

>**SoftwareRequirementsReview** - governedBy -> **SPECIFICATION**

>**SoftwareRequirementsReview** - reviewed -> **HighLevelRequirement**

>**SoftwareRequirementsReview** - reviewed -> **DataDictionary**

>**SoftwareRequirementsReview** - reviewer -> **Reviewer**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_Reviewer| primaryKey Key for Reviewer | Yes
identifier_DataDictionary| primaryKey Key for DataDictionary | Yes
identifier_HighLevelRequirement| primaryKey Key for HighLevelRequirement | Yes
identifier_SPECIFICATION| primaryKey Key for SPECIFICATION | Yes
identifier_Author| primaryKey Key for Author | Yes
identifier_SoftwareRequirementsReview| primaryKey Key for SoftwareRequirementsReview | No
