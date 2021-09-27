#!/usr/bin/env python3
#
# Copyright (c) 2021, General Electric Company, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

from Evidence import createEvidenceFile, createCDR
import Evidence.Add as Add
import shutil
import os.path


def CreateCdrs():
    
    #Logging.TRACE = True
    #Logging.DEBUG = True

    createEvidenceFile(ingestionTitle="TurnstileIngestion-Testing", ingestionDescription="Manual ingestion of Counter Application Testing")

    Add.TOOL(identifier="ASSERT", toolVersion = "V4.2.3", actedOnBehalfOf_identifier="General_Electric")
    Add.ORGANIZATION(identifier="General_Electric")
    
    #------------ CompTestDevelopment ------------
    Add.turnstile_DevelopComponentTests(identifier="CompTestDevelopment",
                endedAtTime = "2020-07-26 10:53:38",
                developedBy_identifier = "ASSERT",
                used_identifier = "VER-STD:v1")
    Add.turnstile_DevelopComponentTests(identifier="CompTestDevelopment",
    	        used_identifier = "ATCG-Config-File")
    Add.turnstile_DevelopComponentTests(identifier="CompTestDevelopment",
    	        used_identifier = "HLR-1-Model")
    Add.DOCUMENT(identifier = "VER-STD:v1")
    Add.FILE(identifier = "ATCG-Config-File", fileFormat_identifier = "XML")
    Add.FORMAT(identifier ="XML")
    Add.ENTITY(identifier = "IncludeBVA", definedIn_identifier = "ATCG-Config-File", description="Flag Indicating to include Boundary Value Analysis in testcase generation.")
    Add.ENTITY(identifier = "IncludeLCA", definedIn_identifier = "ATCG-Config-File", description="Flag Indicating to include Logic Condition Analysis in testcase generation.")
    #------------ TC-1-1 ------------
    Add.turnstile_SoftwareComponentTest(identifier="TC-1-1",
                verifies_identifier = "HLR-1:v1",
                producedBy_identifier = "CompTestDevelopment")
    Add.turnstile_HighLevelRequirement(identifier="HLR-1:v1")

    #------------ TC-1-2 ------------
    Add.turnstile_SoftwareComponentTest(identifier="TC-1-2",
                verifies_identifier = "HLR-1:v1",
                producedBy_identifier = "CompTestDevelopment")
    Add.turnstile_HighLevelRequirement(identifier="HLR-1:v1")

    #------------ TC-1-3 ------------
    Add.turnstile_SoftwareComponentTest(identifier="TC-1-3",
                verifies_identifier = "HLR-1:v1",
                producedBy_identifier = "CompTestDevelopment")
    Add.turnstile_HighLevelRequirement(identifier="HLR-1:v1")
    
    #------------ TC-1-4 ------------
    Add.turnstile_SoftwareComponentTest(identifier="TC-1-4",
                verifies_identifier = "HLR-1:v1",
                producedBy_identifier = "CompTestDevelopment")
    Add.turnstile_HighLevelRequirement(identifier="HLR-1:v1")

    #------------ TR-1-1-1 ------------
    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-1-1",
                confirms_identifier = "TC-1-1",
                result_identifier = "Passed",
                executedBy_identifier = "TestRun1")

    #------------ TR-1-2-1 ------------
    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-2-1",
                confirms_identifier = "TC-1-2",
                result_identifier = "Passed",
                executedBy_identifier = "TestRun1")

    #------------ TR-1-3-1 ------------
    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-3-1",
                confirms_identifier = "TC-1-3",
                result_identifier = "Passed",
                executedBy_identifier = "TestRun1")

    #------------ TR-1-4-1 ------------
    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-4-1",
                confirms_identifier = "TC-1-4",
                result_identifier = "Failed",
                executedBy_identifier = "TestRun1")
    
    #------------ TargetHardware ------------
    Add.AGENT(identifier="TargetHardware")
    
    #------------ TestRun1 ------------
    Add.turnstile_SoftwareComponentTestExecution(identifier="TestRun1",
                endedAtTime = "2020-07-28 11:53:38",
                executedOn_identifier = "TargetHardware")

    #------------ TR-1-1-2 ------------
    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-1-2",
                confirms_identifier = "TC-1-1",
                result_identifier = "Passed",
                executedBy_identifier = "TestRun2")
    
    #------------ TR-1-2-2 ------------
    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-2-2",
                confirms_identifier = "TC-1-2",
                result_identifier = "Passed",
                executedBy_identifier = "TestRun2")
    
    #------------ TR-1-3-2 ------------
    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-3-2",
                confirms_identifier = "TC-1-3",
                result_identifier = "Passed",
                executedBy_identifier = "TestRun2")
    
    #------------ TR-1-4-2 ------------
    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-4-2",
                confirms_identifier = "TC-1-4",
                result_identifier = "Failed",
                executedBy_identifier = "TestRun2")
    
    #------------ TestRun2 ------------
    Add.turnstile_SoftwareComponentTestExecution(identifier="TestRun2",
                endedAtTime = "2020-07-30 11:02:38",
                executedOn_identifier = "TargetHardware")

    createCDR("http://rack001/turnstiledata")

if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/CounterApplicationTesting")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/CounterApplicationTesting"))
    CreateCdrs()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/CounterApplicationTesting"))
