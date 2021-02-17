# NodeGroups/Ingest-SoftwareRequirementsReviewArtifacts.json
## Nodes

>**HighLevelRequirement** : HighLevelRequirement

>>Node Notes...

>>**identifier_HighLevelRequirement** : string
    
>>>Prop Notes...

>**REVIEW_STATE** : REVIEW_STATE

>>Node Notes...

>>**identifier_REVIEW_STATE** : string
    
>>>Prop Notes...

>**SoftwareRequirementsReview** : SoftwareRequirementsReview

>>Node Notes...

>>**identifier_SoftwareRequirementsReview** : string
    
>>>Prop Notes...

>**SoftwareRequirementReviewArtifacts** : SoftwareRequirementReviewArtifacts

>>Node Notes...

>>**identifier_SoftwareRequirementReviewArtifacts** : string
    
>>>Prop Notes...

## Edges

>**SoftwareRequirementReviewArtifacts** - createBy -> **SoftwareRequirementsReview**

>**SoftwareRequirementReviewArtifacts** - reviewResult -> **REVIEW_STATE**

>**SoftwareRequirementReviewArtifacts** - reviews -> **HighLevelRequirement**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_HighLevelRequirement| primaryKey Key for HighLevelRequirement | No
identifier_REVIEW_STATE| primaryKey Key for REVIEW_STATE | No
identifier_SoftwareRequirementsReview| primaryKey Key for SoftwareRequirementsReview | Yes
identifier_SoftwareRequirementReviewArtifacts| primaryKey Key for SoftwareRequirementReviewArtifacts | No
