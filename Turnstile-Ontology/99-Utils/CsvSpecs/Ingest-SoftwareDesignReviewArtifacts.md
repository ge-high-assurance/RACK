# NodeGroups/Ingest-SoftwareDesignReviewArtifacts.json
## Nodes

>**LowLevelRequirement** : LowLevelRequirement

>>Node Notes...

>>**identifier_LowLevelRequirement** : string
    
>>>Prop Notes...

>**REVIEW_STATE_0** : REVIEW_STATE

>>Node Notes...

>>**identifier_REVIEW_STATE_0** : string
    
>>>Prop Notes...

>**REVIEW_STATE** : REVIEW_STATE

>>Node Notes...

>**SoftwareRequirementsReview** : SoftwareRequirementsReview

>>Node Notes...

>>**identifier_SoftwareRequirementsReview** : string
    
>>>Prop Notes...

>**SoftwareDesignReviewArtifacts** : SoftwareDesignReviewArtifacts

>>Node Notes...

>>**identifier_SoftwareDesignReviewArtifacts** : string
    
>>>Prop Notes...

## Edges

>**SoftwareDesignReviewArtifacts** - createBy -> **SoftwareRequirementsReview**

>**SoftwareDesignReviewArtifacts** - reviewResult -> **REVIEW_STATE**

>**SoftwareDesignReviewArtifacts** - reviewResult -> **REVIEW_STATE_0**

>**SoftwareDesignReviewArtifacts** - reviews -> **LowLevelRequirement**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_LowLevelRequirement| primaryKey Key for LowLevelRequirement | No
identifier_REVIEW_STATE_0| primaryKey Key for REVIEW_STATE_0 | No
identifier_SoftwareRequirementsReview| primaryKey Key for SoftwareRequirementsReview | Yes
identifier_SoftwareDesignReviewArtifacts| primaryKey Key for SoftwareDesignReviewArtifacts | No
