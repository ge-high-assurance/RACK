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

    ################################################
    #    System Requirements
    ################################################
    createEvidenceFile(ingestionTitle="TurnstileIngestion-System Requirements", ingestionDescription="Manual ingestion of Turnstile System Requirements")
    Add.turnstile_SystemRequirement(identifier="Sys-1",
                          description="Turnstile system shall track the number of people that travel through the in gate.",
                          governs_identifier="Turnstile")
    Add.turnstile_SystemRequirement(identifier="Sys-2",
                          description="Turnstile system shall track the number of people that travel through the out gate.",
                          governs_identifier="Turnstile")
    Add.turnstile_SystemRequirement(identifier="Sys-3",
                          description="Turnstile system shall track the number of people are currently in the park.",
                          governs_identifier="Turnstile")
    createCDR()


    ################################################
    #    High-level Requirements Version 1
    ################################################    
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemRequirements"))
    
    createEvidenceFile(ingestionTitle="TurnstileIngestion-High Level Requirements", ingestionDescription="Manual ingestion of Turnstile High Level Requirements")

    Add.turnstile_HighLevelRequirement(identifier="HLR-1:v1",
                             description="The Computer shall increment the counter when a inflow event is received and the counter is less than max int.",
                             satisfies_identifier="Sys-1",
                             mitigates_identifier ="H-1.2",
                             governs_identifier="CounterApplication",
                             createdBy_identifier="HlrDev1")	
	
    Add.turnstile_HighLevelRequirement(identifier="HLR-2:v1",
                             description = "The Computer shall decrement the counter when a outflow event is received and the counter is greater than 0.",
                             satisfies_identifier = "Sys-2",
                             mitigates_identifier = "H-1.1",
                             governs_identifier ="CounterApplication",
                             createdBy_identifier="HlrDev1")	
	
    Add.turnstile_HighLevelRequirement(identifier = "HLR-3:v1",
                             description = "The Computer shall publish the counter at a 1 htz rate.",
                             satisfies_identifier = "Sys-3",
                             governs_identifier = "CounterApplication",
                             createdBy_identifier = "HlrDev1")	

    Add.turnstile_Engineer(identifier = "125569538",
                 title = "Doe, John",
                 emailAddress = "john.doe@ge.com",
                 employedBy_identifier = "General_Electric")

    Add.turnstile_SoftwareRequirementsDefinition(identifier = "HLR Dev 1",
                                       endedAtTime = "2020-07-15 10:56:38",
                                       author_identifier = "125569538",
                                       referenced_identifier = "RequirementStandard")
    ################################################
    #    High-level Requirements Version 2
    ################################################   
    Add.turnstile_HighLevelRequirement(identifier="HLR-1:v2",
                             description="The Computer shall increment the counter when a inflow event is received and the counter is less than max int.",
                             satisfies_identifier="Sys-1",
                             mitigates_identifier ="H-1.2",
                             governs_identifier="CounterApplication",
                             createdBy_identifier="HlrDev1",
                             wasRevisionOf_identifier="HLR-1:v1")	
	
    Add.turnstile_HighLevelRequirement(identifier="HLR-2:v2",
                             description = "The Computer shall decrement the counter when a outflow event is received and the counter is greater than 0.",
                             satisfies_identifier = "Sys-2",
                             mitigates_identifier = "H-1.1",
                             governs_identifier ="CounterApplication",
                             createdBy_identifier="HlrDev1",
                             wasRevisionOf_identifier="HLR-1:v1")
    
    Add.turnstile_SoftwareRequirementsDefinition(identifier = "HLR Dev 2",
                                       endedAtTime = "2020-07-25 10:53:38",
                                       author_identifier = "125569538",
                                       referenced_identifier = "RequirementStandard")

    Add.turnstile_DataDictionary(identifier = "inflowEvent",
                       description = "Signal indicating that a person has passed through the ingate",
                       createdBy_identifier = "HlrDev1")
    Add.turnstile_DataDictionary(identifier = "inflowEvent",
                       providedBy_identifier = "inflow")
    Add.turnstile_DataDictionary(identifier = "inflowEvent",
                       consumedBy_identifier = "HLR-1:v1")
    
    Add.turnstile_DataDictionary(identifier = "outflowEvent",
                       description = "Signal indicating that a person has passed through the outgate",
                       createdBy_identifier = "HlrDev1")
    Add.turnstile_DataDictionary(identifier = "outflowEvent",
                       providedBy_identifier = "outflow")
    Add.turnstile_DataDictionary(identifier = "outflowEvent",
                       consumedBy_identifier = "HLR-2:v1")
	
    Add.turnstile_DataDictionary(identifier = "counter",
                       description = "running total people in the park.",
                       createdBy_identifier = "HlrDev1")
    Add.turnstile_DataDictionary(identifier = "counter",
                       providedBy_identifier = "HLR-1:v1")
    Add.turnstile_DataDictionary(identifier = "counter",
                       providedBy_identifier = "HLR-2:v1")
    Add.turnstile_DataDictionary(identifier = "counter",
                       consumedBy_identifier = "HLR-3:v1")	

    Add.turnstile_DataDictionary(identifier = "display",
                       createdBy_identifier = "HlrDev1")
    Add.turnstile_DataDictionary(identifier = "display",
                       providedBy_identifier = "HLR-3:v1")
    Add.turnstile_DataDictionary(identifier = "display",
                       consumedBy_identifier = "census")
    createCDR()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileHighLevelRequirements"))

if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemRequirements")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemRequirements"))
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileHighLevelRequirements")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileHighLevelRequirements"))
    CreateCdrs()
    
