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

input_path = "RequirementsDocument"
def CreateCdrs():
    
    ####################################
    def read_store (l):
           start = l.find(":")
           end = l.rfind(".")
           return l[start+1:end].split(",")
    
    
    #####################################
  
    #Logging.TRACE = True
    #Logging.DEBUG = True

    ################################################
    #    System Requirements
    ################################################
    createEvidenceFile(ingestionTitle="TurnstileIngestion-System Requirements", ingestionDescription="Ingestion of Turnstile System Requirements using Scraping Tool Kit")
    
    
    def SystemRequirement(txt):
      for l in txt.readlines():
       if l.startswith("Requirement title"):
        titles = read_store(l)
       elif l.startswith("Requirement identification"):
        ids = read_store(l)
       elif l.startswith("Description"):
        descIds = read_store(l)
       elif l.startswith("Governs"): 
        governsIds = read_store(l)
        for descId in descIds: 
         for title1 in titles:
           for sysId in ids:
             for gov in governsIds:
              Add.turnstile_SystemRequirement(identifier = sysId, title = title1, governs_identifier = gov,description = descId)   
              Add.SYSTEM(identifier = gov)
              
    def ingest_SystemRequirement(filePath):
       with open(filePath, "r") as txtFile:
         lastSysReqId = None
         SystemRequirement(txtFile)
          
    ingest_SystemRequirement(input_path+"/Sys_Req.txt")   
    createCDR("http://rack001/turnstiledata")
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemRequirements"))

    ################################################
    #    High-level Requirements Version 1
    ################################################        
    createEvidenceFile(ingestionTitle="TurnstileIngestion-High Level Requirements", ingestionDescription="Manual ingestion of Turnstile High Level Requirements")
    #------------ HLR-1 ------------
    Add.turnstile_HighLevelRequirement(identifier="HLR-1:v1",
                             description="The Computer shall increment the counter when a inflow event is received and the counter is less than 1000.",
                             satisfies_identifier="Sys-1",
                             mitigates_identifier ="H-1.2",
                             governs_identifier="CounterApplication",
                             createdBy_identifier="HlrDev1")
    Add.turnstile_SystemRequirement(identifier="Sys-1")
    Add.HAZARD(identifier="H-1.2")
    Add.turnstile_SystemComponent(identifier="CounterApplication")

    #------------ HLR-2 ------------
    Add.turnstile_HighLevelRequirement(identifier="HLR-2:v1",
                                       description = "The Computer shall decrement the counter when a outflow event is received and the counter is greater than 0.",
                                       satisfies_identifier = "Sys-2",
                                       mitigates_identifier = "H-1.1",
                                       governs_identifier ="CounterApplication",
                                       createdBy_identifier="HlrDev1")
    Add.turnstile_SystemRequirement(identifier="Sys-2")
    Add.HAZARD(identifier="H-1.1")
    Add.turnstile_SystemComponent(identifier="CounterApplication")
    
    #------------ HLR-3 ------------
    Add.turnstile_HighLevelRequirement(identifier = "HLR-3:v1",
                             description = "The Computer shall publish the counter at a 1 htz rate.",
                             satisfies_identifier = "Sys-3",
                             governs_identifier = "CounterApplication",
                             createdBy_identifier = "HlrDev1")	
    Add.turnstile_SystemRequirement(identifier="Sys-3")
    Add.turnstile_SystemComponent(identifier="CounterApplication")
    
    #------------ 125569538 ------------
    Add.turnstile_Engineer(identifier = "125569538",
                 title = "Doe, John",
                 emailAddress = "john.doe@ge.com",
                 employedBy_identifier = "General_Electric")
    Add.ORGANIZATION(identifier = "General_Electric")
    
    #------------ HlrDev1 ------------
    Add.turnstile_SoftwareRequirementsDefinition(identifier = "HlrDev1",
                                       endedAtTime = "2020-07-15 10:56:38",
                                       author_identifier = "125569538",
                                       referenced_identifier = "RQ-STD:v1")
    Add.DOCUMENT(identifier = "RQ-STD:v1")
    #------------ inflowEvent ------------                                
    Add.turnstile_DataDictionary(identifier = "inflowEvent",
                       description = "Signal indicating that a person has passed through the ingate",
                       createdBy_identifier = "HlrDev1")                       
    Add.turnstile_DataDictionary(identifier = "inflowEvent",
                       providedBy_identifier = "inflow")
    Add.turnstile_SystemInterfaceDefinition (identifier="inflow")
    Add.turnstile_DataDictionary(identifier = "inflowEvent",
                       consumedBy_identifier = "HLR-1:v1")
                 
    #------------ outflowEvent ------------  
    Add.turnstile_DataDictionary(identifier = "outflowEvent",
                       description = "Signal indicating that a person has passed through the outgate",
                       createdBy_identifier = "HlrDev1")
    Add.turnstile_DataDictionary(identifier = "outflowEvent",
                       providedBy_identifier = "outflow")
    Add.turnstile_SystemInterfaceDefinition (identifier="outflow")
    Add.turnstile_DataDictionary(identifier = "outflowEvent",
                       consumedBy_identifier = "HLR-2:v1")
	
	#------------ counter ------------ 
    Add.turnstile_DataDictionary(identifier = "counter",
                       description = "running total people in the park.",
                       createdBy_identifier = "HlrDev1")
    Add.turnstile_DataDictionary(identifier = "counter",
                       providedBy_identifier = "HLR-1:v1")
    Add.turnstile_DataDictionary(identifier = "counter",
                       providedBy_identifier = "HLR-2:v1")
    Add.turnstile_DataDictionary(identifier = "counter",
                       consumedBy_identifier = "HLR-3:v1")	

    #------------ display ------------ 
    Add.turnstile_DataDictionary(identifier = "display",
                       createdBy_identifier = "HlrDev1")
    Add.turnstile_DataDictionary(identifier = "display",
                       providedBy_identifier = "HLR-3:v1")
    Add.turnstile_DataDictionary(identifier = "display",
                       consumedBy_identifier = "census")
    Add.turnstile_SystemInterfaceDefinition (identifier="census")
    ################################################
    #    High-level Requirements Version 2
    ################################################  
    #------------ HLR-1 ------------ 
    Add.turnstile_HighLevelRequirement(identifier="HLR-1:v2",
                             description="The Computer shall increment the counter when a inflow event is received and the counter is less than max int.",
                             satisfies_identifier="Sys-1",
                             mitigates_identifier ="H-1.2",
                             governs_identifier="CounterApplication",
                             createdBy_identifier="HlrDev2",
                             wasRevisionOf_identifier="HLR-1:v1")	
    Add.turnstile_SystemRequirement(identifier="Sys-1")
    Add.HAZARD(identifier="H-1.2")
    Add.turnstile_SystemComponent(identifier="CounterApplication")
    Add.turnstile_HighLevelRequirement(identifier="HLR-1:v1")
	
	#------------ HLR-2 ------------ 
    Add.turnstile_HighLevelRequirement(identifier="HLR-2:v2",
                             description = "The Computer shall decrement the counter when a outflow event is received and the counter is greater than 0.",
                             satisfies_identifier = "Sys-2",
                             mitigates_identifier = "H-1.1",
                             governs_identifier ="CounterApplication",
                             createdBy_identifier="HlrDev2",
                             wasRevisionOf_identifier="HLR-2:v1")
    Add.turnstile_SystemRequirement(identifier="Sys-2")
    Add.HAZARD(identifier="H-1.1")
    Add.turnstile_SystemComponent(identifier="CounterApplication")
    Add.turnstile_HighLevelRequirement(identifier="HLR-2:v1")
    
    #------------ HlrDev2 ------------
    Add.turnstile_SoftwareRequirementsDefinition(identifier = "HlrDev2",
                                       endedAtTime = "2020-07-25 10:53:38",
                                       author_identifier = "125569538",
                                       referenced_identifier = "RQ-STD:v1")
    Add.DOCUMENT(identifier = "RQ-STD:v1")
    #------------ inflowEvent ------------
    Add.turnstile_DataDictionary(identifier = "inflowEvent",
                       consumedBy_identifier = "HLR-1:v2")

    #------------ outflowEvent ------------
    Add.turnstile_DataDictionary(identifier = "outflowEvent",
                       consumedBy_identifier = "HLR-2:v2")

    #------------ counter ------------
    Add.turnstile_DataDictionary(identifier = "counter",
                       providedBy_identifier = "HLR-1:v2")                   
    Add.turnstile_DataDictionary(identifier = "counter",
                       providedBy_identifier = "HLR-2:v2")
                       
    createCDR("http://rack001/turnstiledata")
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileHighLevelRequirements"))

    ################################################
    #    Low-level Requirements 
    ################################################   
    createEvidenceFile(ingestionTitle="TurnstileIngestion-Low Level Requirements", ingestionDescription="Manual ingestion of Turnstile Low Level Requirements")
    #------------ 2125895152 ------------
    Add.turnstile_Engineer(identifier = "2125895152",
                           title = "Doe, Jane",
                           emailAddress = "jane.doe@ge.com",
                           employedBy_identifier = "General_Electric")
    Add.ORGANIZATION(identifier = "General_Electric")
    #------------ LlrDev1 ------------
    Add.turnstile_SoftwareDesign(identifier = "LlrDev1",
                                 endedAtTime = "2020-07-19 11:48:38",
                                 author_identifier = "2125895152",
                                 referenced_identifier = "SW-STD:v1")
    Add.DOCUMENT(identifier = "SW-STD:v1")
    #------------ SwDesign ------------
    Add.turnstile_SoftwareDesign(identifier = "SwDesign",
                                 endedAtTime = "2020-07-23 09:52:38",
                                 author_identifier = "2125895152",
                                 referenced_identifier = "SW-STD:v1")
    Add.DOCUMENT(identifier = "SW-STD:v1")
    #------------ InputThread ------------
    Add.turnstile_SoftwareThread(identifier = "InputThread",
                                 partOf_identifier = "CounterApplication",
		                         producedBy_identifier = "SwDesign")
    #------------ OutputThread ------------
    Add.turnstile_SoftwareThread(identifier = "OutputThread",
                                 partOf_identifier = "CounterApplication",
		                         producedBy_identifier = "SwDesign")
    #------------ ExecutiveThread ------------
    Add.turnstile_SoftwareThread(identifier = "ExecutiveThread",
                                 partOf_identifier = "CounterApplication",
		                         producedBy_identifier = "SwDesign")
    #------------ EXE-LLR-1 ------------
    Add.turnstile_LowLevelRequirement(identifier = "EXE-LLR-1",
                                      description = "Executive shall spawn Input Thread on powerup.",
                                      governs_identifier = "ExecutiveThread",
                                      createdBy_identifier = "LlrDev1")
	
    #------------ EXE-LLR-2 ------------
    Add.turnstile_LowLevelRequirement(identifier = "EXE-LLR-2",
                                      description = "Executive shall spawn Output Thread on powerup.",
                                      governs_identifier = "ExecutiveThread",
                                      createdBy_identifier = "LlrDev1")
	
    #------------ EXE-LLR-3 ------------
    Add.turnstile_LowLevelRequirement(identifier = "EXE-LLR-3",
                                      description = "Executive shall print a single '.' character to the console every second when running.",
                                      governs_identifier = "ExecutiveThread",
                                      createdBy_identifier = "LlrDev1")

    #------------ IN-LLR-1 ------------
    Add.turnstile_LowLevelRequirement(identifier = "IN-LLR-1",
                                      description = "Input Thread shall initialize the park_count to 0 on powerup.",
                                      governs_identifier = "InputThread",
                                      createdBy_identifier = "LlrDev1")
    #------------ IN-LLR-2 ------------
    Add.turnstile_LowLevelRequirement(identifier = "IN-LLR-2",
                                      description = "Input Thread shall check for a incoming UDP message on port 62000.",
                                      governs_identifier = "InputThread",
                                      createdBy_identifier = "LlrDev1")
    Add.turnstile_LowLevelRequirement(identifier = "IN-LLR-2",
			                          satisfies_identifier = "HLR-1:v2")
    Add.turnstile_HighLevelRequirement(identifier="HLR-1:v2")
    Add.turnstile_LowLevelRequirement(identifier = "IN-LLR-2",
			                          satisfies_identifier = "HLR-2:v2")
    Add.turnstile_HighLevelRequirement(identifier="HLR-2:v2")

    #------------ IN-LLR-3 ------------
    Add.turnstile_LowLevelRequirement(identifier = "IN-LLR-3",
                                      description = "Input Thread shall add the delta value received by the UDP to the park_count and send the updated park_count to the Output Thread when a valid UDP message is received and the park_count range is not exceed.",
                                      governs_identifier = "InputThread",
                                      createdBy_identifier = "LlrDev1")
    Add.turnstile_LowLevelRequirement(identifier = "IN-LLR-3",
			                          satisfies_identifier = "HLR-1:v2")
    Add.turnstile_HighLevelRequirement(identifier="HLR-1:v2")
    Add.turnstile_LowLevelRequirement(identifier = "IN-LLR-3",
			                          satisfies_identifier = "HLR-2:v2")
    Add.turnstile_HighLevelRequirement(identifier="HLR-2:v2")

    #------------ IN-LLR-4 ------------
    Add.turnstile_LowLevelRequirement(identifier = "IN-LLR-4",
                                      description = "Input Thread shall limit park_count to between 0 and 1500.",
                                      governs_identifier = "InputThread",
                                      createdBy_identifier = "LlrDev1")

    #------------ IN-LLR-5 ------------
    Add.turnstile_LowLevelRequirement(identifier = "IN-LLR-5",
                                      description = "Input Thread shall print 'Invalid Message' to the console when a invalid UDP message is received.",
                                      governs_identifier = "InputThread",
                                      createdBy_identifier = "LlrDev1")

    #------------ IN-LLR-6 ------------	
    Add.turnstile_LowLevelRequirement(identifier = "IN-LLR-6",
                                      description = "Input Thread shall print 'Limit Exceeded'' to the console when a valid UDP message is received and the park_count range is exceed.",
                                      governs_identifier = "InputThread",
                                      createdBy_identifier = "LlrDev1")
    #------------ OUT-LLR-1 ------------
    Add.turnstile_LowLevelRequirement(identifier = "OUT-LLR-1",
                                      description = "Output Thread shall initialize the park_count to 0 on powerup.",
                                      governs_identifier = "OutputThread",
                                      createdBy_identifier = "LlrDev1")

    #------------ OUT-LLR-2 ------------	
    Add.turnstile_LowLevelRequirement(identifier = "OUT-LLR-2",
                                      description = "Output Thread shall broadcast a UDP message on port 62001 with the park_count every second.",
                                      satisfies_identifier = "HLR-3:v1",
                                      governs_identifier = "OutputThread",
                                      createdBy_identifier = "LlrDev1")
    Add.turnstile_HighLevelRequirement(identifier="HLR-3:v1")    
    
    
    #------------ DCC-1 ------------	
    Add.turnstile_DataAndControlCouple(identifier = "DCC-1",
                                       description = "PowerUp",
                                       createdBy_identifier = "LlrDev1")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-1",
                                       consumedBy_identifier = "EXE-LLR-1")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-1",
                                       consumedBy_identifier = "EXE-LLR-2")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-1",
                                       consumedBy_identifier = "IN-LLR-1")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-1",
                                       consumedBy_identifier = "OUT-LLR-1")

    #------------ DCC-2 ------------	
    Add.turnstile_DataAndControlCouple(identifier = "DCC-2", 
                                       description = "incoming UDP message",
                                       createdBy_identifier = "LlrDev1")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-2",
                                       consumedBy_identifier = "IN-LLR-2")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-2",
                                       consumedBy_identifier = "IN-LLR-3")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-2",
                                       consumedBy_identifier = "IN-LLR-5")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-2",
                                       consumedBy_identifier = "IN-LLR-6")

		
    #------------ DCC-3 ------------	
    Add.turnstile_DataAndControlCouple(identifier = "DCC-3",
                                       description = "input_park_count",
                                       createdBy_identifier = "LlrDev1")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "IN-LLR-2")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "IN-LLR-3")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "IN-LLR-4")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "IN-LLR-5")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "IN-LLR-6")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-3",
                                       providedBy_identifier = "IN-LLR-1")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-3",
                                       providedBy_identifier = "IN-LLR-4")

		
    #------------ DCC-4 ------------	
    Add.turnstile_DataAndControlCouple(identifier = "DCC-4",
                                       description = "output_park_count",
                                       createdBy_identifier = "LlrDev1")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "OUT-LLR-2")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-3",
                                       providedBy_identifier = "OUT-LLR-1")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-3",
                                       providedBy_identifier = "IN-LLR-3")

	
    #------------ DCC-5 ------------	
    Add.turnstile_DataAndControlCouple(identifier = "DCC-5",
                                       description = "outgoing UDP message",
                                       createdBy_identifier = "LlrDev1")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-5",
                                       providedBy_identifier = "OUT-LLR-2")

		
    #------------ DCC-6 ------------	
    Add.turnstile_DataAndControlCouple(identifier = "DCC-6",
                                       description = "console",
                                       createdBy_identifier = "LlrDev1")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-6",
                                       providedBy_identifier = "EXE-LLR-3")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-6",
                                       providedBy_identifier = "IN-LLR-5")
    Add.turnstile_DataAndControlCouple(identifier = "DCC-6",
                                       providedBy_identifier = "IN-LLR-6")

    
    createCDR("http://rack001/turnstiledata")
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileLowLevelRequirements"))
if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemRequirements")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemRequirements"))
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileHighLevelRequirements")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileHighLevelRequirements"))
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileLowLevelRequirements")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileLowLevelRequirements"))
    CreateCdrs()
    
