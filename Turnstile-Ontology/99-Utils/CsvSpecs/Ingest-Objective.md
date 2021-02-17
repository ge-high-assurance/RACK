# NodeGroups/Ingest-Objective.json
## Nodes

>**SoftwareRequirementsDefinition** : SoftwareRequirementsDefinition

>>Node Notes...

>>**identifier_SoftwareRequirementsDefinition** : string
    
>>>Prop Notes...

>**SoftwareDesignReview** : SoftwareDesignReview

>>Node Notes...

>>**identifier_SoftwareDesignReview** : string
    
>>>Prop Notes...

>**SoftwareCodeReview** : SoftwareCodeReview

>>Node Notes...

>>**identifier_SoftwareCodeReview** : string
    
>>>Prop Notes...

>**SystemRequirementsDefinition** : SystemRequirementsDefinition

>>Node Notes...

>>**identifier_SystemRequirementsDefinition** : string
    
>>>Prop Notes...

>**SoftwareDesign** : SoftwareDesign

>>Node Notes...

>>**identifier_SoftwareDesign** : string
    
>>>Prop Notes...

>**DevelopUnitTests** : DevelopUnitTests

>>Node Notes...

>>**identifier_DevelopUnitTests** : string
    
>>>Prop Notes...

>**DevelopComponentTests** : DevelopComponentTests

>>Node Notes...

>>**identifier_DevelopComponentTests** : string
    
>>>Prop Notes...

>**OBJECTIVE** : OBJECTIVE

>>Node Notes...

>>**identifier_OBJECTIVE** : string
    
>>>Prop Notes...

## Edges

>**OBJECTIVE** - satisfiedBy -> **DevelopComponentTests**

>**OBJECTIVE** - satisfiedBy -> **DevelopUnitTests**

>**OBJECTIVE** - satisfiedBy -> **SoftwareDesign**

>**OBJECTIVE** - satisfiedBy -> **SystemRequirementsDefinition**

>**OBJECTIVE** - satisfiedBy -> **SoftwareCodeReview**

>**OBJECTIVE** - satisfiedBy -> **SoftwareDesignReview**

>**OBJECTIVE** - satisfiedBy -> **SoftwareRequirementsDefinition**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_SoftwareRequirementsDefinition| primaryKey Key for SoftwareRequirementsDefinition | Yes
identifier_SoftwareDesignReview| primaryKey Key for SoftwareDesignReview | Yes
identifier_SoftwareCodeReview| primaryKey Key for SoftwareCodeReview | Yes
identifier_SystemRequirementsDefinition| primaryKey Key for SystemRequirementsDefinition | Yes
identifier_SoftwareDesign| primaryKey Key for SoftwareDesign | Yes
identifier_DevelopUnitTests| primaryKey Key for DevelopUnitTests | Yes
identifier_DevelopComponentTests| primaryKey Key for DevelopComponentTests | Yes
identifier_OBJECTIVE| primaryKey Key for OBJECTIVE | No
