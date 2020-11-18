# NodeGroups/Ingest-SoftwareDesignReview.json
## Nodes

>**Reviewer** : Engineer

>>Node Notes...

>>**identifier_Reviewer** : string
    
>>>Prop Notes...

>**LowLevelRequirement** : LowLevelRequirement

>>Node Notes...

>>**identifier_LowLevelRequirement** : string
    
>>>Prop Notes...

>**SPECIFICATION** : SPECIFICATION

>>Node Notes...

>>**identifier_SPECIFICATION** : string
    
>>>Prop Notes...

>**Author** : Engineer

>>Node Notes...

>>**identifier_Author** : string
    
>>>Prop Notes...

>**SoftwareDesignReview** : SoftwareDesignReview

>>Node Notes...

>>**identifier_SoftwareDesignReview** : string
    
>>>Prop Notes...

## Edges

>**SoftwareDesignReview** - author -> **Author**

>**SoftwareDesignReview** - governedBy -> **SPECIFICATION**

>**SoftwareDesignReview** - reviewed -> **LowLevelRequirement**

>**SoftwareDesignReview** - reviewer -> **Reviewer**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_Reviewer| primaryKey Key for Reviewer | No
identifier_LowLevelRequirement| primaryKey Key for LowLevelRequirement | No
identifier_SPECIFICATION| primaryKey Key for SPECIFICATION | Yes
identifier_Author| primaryKey Key for Author | Yes
identifier_SoftwareDesignReview| primaryKey Key for SoftwareDesignReview | No
