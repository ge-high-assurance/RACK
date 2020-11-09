# NodeGroups/Ingest-SoftwareComponentTestResult.json
## Nodes

>**TEST_STATUS** : TEST_STATUS

>>Node Notes...

>>**identifier_TEST_STATUS** : string
    
>>>Prop Notes...

>**SoftwareComponentTestExecution** : SoftwareComponentTestExecution

>>Node Notes...

>>**identifier_SoftwareComponentTestExecution** : string
    
>>>Prop Notes...

>**SoftwareComponentTest** : SoftwareComponentTest

>>Node Notes...

>>**identifier_SoftwareComponentTest** : string
    
>>>Prop Notes...

>**SoftwareComponentTestResult** : SoftwareComponentTestResult

>>Node Notes...

>>**identifier_SoftwareComponentTestResult** : string
    
>>>Prop Notes...

## Edges

>**SoftwareComponentTestResult** - confirms -> **SoftwareComponentTest**

>**SoftwareComponentTestResult** - executedBy -> **SoftwareComponentTestExecution**

>**SoftwareComponentTestResult** - result -> **TEST_STATUS**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_TEST_STATUS| primaryKey Key for TEST_STATUS | No
identifier_SoftwareComponentTestExecution| primaryKey Key for SoftwareComponentTestExecution | Yes
identifier_SoftwareComponentTest| primaryKey Key for SoftwareComponentTest | No
identifier_SoftwareComponentTestResult| primaryKey Key for SoftwareComponentTestResult | No
