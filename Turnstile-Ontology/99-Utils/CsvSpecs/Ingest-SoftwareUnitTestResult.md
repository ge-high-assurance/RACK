# NodeGroups/Ingest-SoftwareUnitTestResult.json
## Nodes

>**TEST_STATUS** : TEST_STATUS

>>Node Notes...

>>**identifier_TEST_STATUS** : string
    
>>>Prop Notes...

>**SoftwareUnitTestExecution** : SoftwareUnitTestExecution

>>Node Notes...

>>**identifier_SoftwareUnitTestExecution** : string
    
>>>Prop Notes...

>**SoftwareUnitTest** : SoftwareUnitTest

>>Node Notes...

>>**identifier_SoftwareUnitTest** : string
    
>>>Prop Notes...

>**SoftwareUnitTestResult** : SoftwareUnitTestResult

>>Node Notes...

>>**identifier_SoftwareUnitTestResult** : string
    
>>>Prop Notes...

## Edges

>**SoftwareUnitTestResult** - confirms -> **SoftwareUnitTest**

>**SoftwareUnitTestResult** - executedBy -> **SoftwareUnitTestExecution**

>**SoftwareUnitTestResult** - result -> **TEST_STATUS**

## CSV

Column Name | Description |Optional
------------|-------------|---
identifier_TEST_STATUS| primaryKey Key for TEST_STATUS | No
identifier_SoftwareUnitTestExecution| primaryKey Key for SoftwareUnitTestExecution | Yes
identifier_SoftwareUnitTest| primaryKey Key for SoftwareUnitTest | No
identifier_SoftwareUnitTestResult| primaryKey Key for SoftwareUnitTestResult | No
