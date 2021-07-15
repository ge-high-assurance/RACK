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

import XML
import XML.SysML as SysML
from Evidence import createEvidenceFile, createCDR
import Evidence.Add as Add
import shutil
import os.path


def CreateCdrs():
    
    #Logging.TRACE = True
    #Logging.DEBUG = True

    createEvidenceFile(ingestionTitle="TurnstileIngestion-Testing", ingestionDescription="Manual ingestion of Counter Application Testing")

    Add.AGENT(identifier="ASSERT")
    
    Add.turnstile_DevelopComponentTests(identifier="CompTestDevelopment",
                developedBy_identifier = "ASSERT",
                used_identifier = "VER-STD")

    Add.turnstile_SoftwareComponentTest(identifier="TC-1-1",
                verifies_identifier = "HLR-1",
                producedBy_identifier = "CompTestDevelopment")

    Add.turnstile_SoftwareComponentTest(identifier="TC-1-2",
                verifies_identifier = "HLR-1",
                producedBy_identifier = "CompTestDevelopment")

    Add.turnstile_SoftwareComponentTest(identifier="TC-1-3",
                verifies_identifier = "HLR-1",
                producedBy_identifier = "CompTestDevelopment")

    Add.turnstile_SoftwareComponentTest(identifier="TC-1-4",
                verifies_identifier = "HLR-1",
                producedBy_identifier = "CompTestDevelopment")

    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-1-1",
                confirms_identifier = "TC-1-1",
                result_identifier = "Passed",
                executedBy_identifier = "TestRun1")

    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-2-1",
                confirms_identifier = "TC-1-2",
                result_identifier = "Passed",
                executedBy_identifier = "TestRun1")

    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-3-1",
                confirms_identifier = "TC-1-3",
                result_identifier = "Passed",
                executedBy_identifier = "TestRun1")

    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-4-1",
                confirms_identifier = "TC-1-4",
                result_identifier = "Failed",
                executedBy_identifier = "TestRun1")

    Add.AGENT(identifier="TargetHardware")
    
    Add.turnstile_SoftwareComponentTestExecution(identifier="TestRun1",
                executedOn_identifier = "TargetHardware")

    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-1-2",
                confirms_identifier = "TC-1-1",
                result_identifier = "Passed",
                executedBy_identifier = "TestRun2")

    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-2-2",
                confirms_identifier = "TC-1-2",
                result_identifier = "Passed",
                executedBy_identifier = "TestRun2")

    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-3-2",
                confirms_identifier = "TC-1-3",
                result_identifier = "Passed",
                executedBy_identifier = "TestRun2")

    Add.turnstile_SoftwareComponentTestResult(identifier="TR-1-4-2",
                confirms_identifier = "TC-1-4",
                result_identifier = "Failed",
                executedBy_identifier = "TestRun2")

    Add.turnstile_SoftwareComponentTestExecution(identifier="TestRun2",
                executedOn_identifier = "TargetHardware")

    createCDR()


if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/CounterApplicationTesting")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/CounterApplicationTesting"))
    CreateCdrs()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/CounterApplicationTesting"))
