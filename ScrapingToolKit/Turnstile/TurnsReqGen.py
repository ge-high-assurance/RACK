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
import os
local_dir=(os.getcwd()).split('ScrapingToolKit')[0]
out_path = local_dir+"/Turnstile-Example/Turnstile-IngestionPackage"
def CreateCdrs():
    
    #Logging.TRACE = True
    #Logging.DEBUG = True
    def SystemRequirement(txt):
      for l in txt.readlines():
       if l.startswith("Requirement title"):
        start = l.find(":")
        end = l.rfind(".")
        titles = l[start+1:end].split(",")
       elif l.startswith("Requirement identification"):
        start = l.find(":")
        end = l.rfind(".")
        ids = l[start+1:end].split(",")
       elif l.startswith("Description"):
        # Found Parent Requirement List
        start = l.find(":")
        end = l.rfind("}")
        descIds = l[start+1:end].split(",")
        #for descId in descIds:
         # Add.turnstile_SystemRequirement()#(identifier = lastReqId, description = descId)
       elif l.startswith("Governs"): 
        # Found mitigates requirement list
        start = l.find(":")
        end = l.rfind("}")
        governsIds = l[start+1:end].split(",")
        for descId in descIds: 
         for title1 in titles:
           for sysId in ids:
             for gov in governsIds:
              Add.turnstile_SystemRequirement(identifier = sysId, title = title1, governs_identifier = gov,description = descId)   
              Add.SYSTEM(identifier = gov)

    def HighLevelRequirement(txt):
      for l in txt.readlines():
       if l.startswith("Requirement title"):
        start = l.find(":")
        end = l.rfind(".")
        titles = l[start+1:end].split(",")
       elif l.startswith("Requirement identification"):
        start = l.find(":")
        end = l.rfind(".")
        ids = l[start+1:end].split(",")
       elif l.startswith("Description"):
        # Found Parent Requirement List
        start = l.find(":")
        end = l.rfind(".")
        descIds = l[start+1:end].split(",")
        #for descId in descIds:
         # Add.turnstile_SystemRequirement()#(identifier = lastReqId, description = descId)
       elif l.startswith("Satisfies"): 
        # Found satisfies requirement list
        start = l.find(":")
        end = l.rfind("}")
        satisfiesIds = l[start+1:end].split(",")
       elif l.startswith("Mitigates"): 
        # Found mitigates requirement list
        start = l.find(":")
        end = l.rfind("}")
        mitigatesIds = l[start+1:end].split(",") 
       elif l.startswith("Governs"): 
        # Found governs requirement list
        start = l.find(":")
        end = l.rfind("}")
        governsIds = l[start+1:end].split(",")
       elif l.startswith("Created by"): 
        # Found createdBy requirement list
        start = l.find(":")
        end = l.rfind("}")
        createdByIds = l[start+1:end].split(",")
        for descId in descIds:
          for title1 in titles:
           for reqId in ids:  
            for gov in governsIds:
             for sat in satisfiesIds:
               for mit in mitigatesIds:
                 for cre in createdByIds:  
                   Add.turnstile_HighLevelRequirement(identifier = reqId, governs_identifier = gov,title = title1,
                                                  description = descId, satisfies_identifier = sat, mitigates_identifier = mit, createdBy_identifier = cre)   
                   Add.turnstile_SystemComponent(identifier = gov)
                   Add.turnstile_SystemRequirement(identifier = sat)
                   Add.HAZARD(identifier = mit)

    def read_store (store,l):
           start = l.find(":")
           end = l.rfind(".")
           store = l[start+1:end].split(",")
        
    def LowLevelRequirement(txt):
      satisfiesIds=[""]
      for l in txt.readlines():
       if l.startswith("Requirement title"):
        start = l.find(":")
        end = l.rfind(".")
        titles = l[start+1:end].split(",")
       elif l.startswith("Requirement identification"):
        start = l.find(":")
        end = l.rfind(".")
        ids = l[start+1:end].split(",")
       elif l.startswith("Description"):
        start = l.find(":")
        end = l.rfind(".")
        descIds = l[start+1:end].split(",")
       elif l.startswith("Satisfies"): 
        # Found satisfies requirement list
        start = l.find(":")
        end = l.rfind(".")
        satisfiesIds = l[start+1:end].split(",")
       elif l.startswith("Mitigates"): 
        # Found mitigates requirement list
        start = l.find(":")
        end = l.rfind(".")
        mitigatesIds = l[start+1:end].split(",")
        if mitigatesIds ==[]:mitigatesIds =["NA"]
       elif l.startswith("Governs"): 
        # Found governs requirement list
        start = l.find(":")
        end = l.rfind(".")
        governsIds = l[start+1:end].split(",")
       elif l.startswith("Created by"): 
        # Found createdBy requirement list
        start = l.find(":")
        end = l.rfind(".")
        createdByIds = l[start+1:end].split(",")
        for title1 in titles:
         for descId in descIds:  
           for gov in governsIds:
             for sat in satisfiesIds:
                for reqId in ids:
                  for cre in createdByIds:
                   #if title1 == "Low Level Requirement" :  
                     Add.turnstile_LowLevelRequirement(identifier = reqId, title= title1, governs_identifier = gov,satisfies_identifier = sat,
                                                  description = descId,   createdBy_identifier = cre)
                     if satisfiesIds!=[""]:Add.turnstile_HighLevelRequirement(identifier = sat)
           satisfiesIds=[""]  
                   
    #Class: Code Development 
    def SoftwareDesign(txt):
      for l in txt.readlines():
       if   l.startswith("Design identification"):
        start = l.find(":")
        end = l.rfind("}")
        Ids = l[start+1:end].split(",")
       elif l.startswith("Design title"):
        start = l.find(":")
        end = l.rfind("}")
        titles = l[start+1:end].split(",") 
       elif l.startswith("Ended at time"):
        start = l.find(":")
        end = l.rfind("}")
        endTimes = l[start+1:end].split(",")
       elif l.startswith("Author"):
        # Found Parent List
        start = l.find(":")
        end = l.rfind("}")
        authors = l[start+1:end].split(",")
       elif l.startswith("Referenced"):
        # Found Parent List
        start = l.find(":")
        end = l.rfind("}")
        referenceds = l[start+1:end].split(",")  
        for endTime in endTimes:
           for author in authors:
             for softDesId in Ids:
              for title1 in titles:
               for referenced in referenceds:  
                Add.turnstile_SoftwareDesign(identifier = softDesId, title =title1, endedAtTime = endTime,author_identifier = author,
                                            referenced_identifier = referenced)
                Add.DOCUMENT(identifier = referenced)

    def SoftwareThread(txt):
      for l in txt.readlines():
       if l.startswith("Thread title"):
        start = l.find(":")
        end = l.rfind(".")
        titles = l[start+1:end].split(",")
       elif l.startswith("Thread identification"):
        start = l.find(":")
        end = l.rfind(".")
        ids = l[start+1:end].split(",") 
       elif l.startswith("Part of"):
        start = l.find(":")
        end = l.rfind(".")
        parts = l[start+1:end].split(",")
       elif l.startswith("Produced by"):
        start = l.find(":")
        end = l.rfind(".")
        producedBys = l[start+1:end].split(",")
        for title1 in titles:
         for softThrId in ids:
           for part in parts:
            for producedBy in producedBys:
                Add.turnstile_SoftwareThread(identifier = softThrId, partOf_identifier = part,title=title1 ,producedBy_identifier = producedBy)
    
    #Class: Agent
    def engineer(txt):
      for l in txt.readlines():
       if l.startswith("Engineer identification"):
        start = l.find(":")
        end = l.rfind(".")
        identification = l[start+1:end].split(";")#HHZ separet by ; instate of ,
       elif l.startswith("Engineer title"):
        # Found Parent List
        start = l.find(":")
        end = l.rfind(".")
        Titles = l[start+1:end].split(",") 
       elif l.startswith("Engineer name"):
        #Found Parent List
        start = l.find(":")
        end = l.rfind(".")
        descrip = l[start+1:end].split(";")#HHZ separet by ; instate of ,
       elif l.startswith("Email address"):
        # Found Parent List
        start = l.find(":")
        end = l.rfind(".")
        emails = l[start+1:end].split(",")
       elif l.startswith("Company name"):
         #Found Parent List
        start = l.find(":")
        end = l.rfind(".")
        employers = l[start+1:end].split(",")
        for desc in descrip:
         for engiId in identification:
          for title1 in Titles:
           for email in emails:
             for employer in employers: 
                Add.turnstile_Engineer(identifier = engiId, description = desc,title= title1, emailAddress = email,employedBy_identifier = employer)
                Add.ORGANIZATION(identifier = employer)

    def dataAndControlCouple(txt):
      providedBys =[""]
      consumedBys=[""]
      for l in txt.readlines():
       if   l.startswith("Data and control couple title"):
        start = l.find(":")
        end = l.rfind(".")
        titles = l[start+1:end].split(",")
       elif l.startswith("Data and control couple identification"):
        start = l.find(":")
        end = l.rfind(".")
        dataIds = l[start+1:end].split(",") 
       elif l.startswith("Data and control couple description"):
        start = l.find(":")
        end = l.rfind(".")
        descriptions = l[start+1:end].split(",")
       elif l.startswith("Data and control couple provided by"):
        # Found Parent List
        start = l.find(":")
        end = l.rfind(".")
        providedBys = l[start+1:end].split(",") 
       elif l.startswith("Data and control couple consumed by"):
        # Found Parent List
        start = l.find(":")
        end = l.rfind(".")
        consumedBys = l[start+1:end].split(",")
       elif l.startswith("Data and control couple created by"):
        # Found Parent List
        start = l.find(":")
        end = l.rfind(".")
        createdBys = l[start+1:end].split(",") 
        for desc in descriptions:
         for createdBy in createdBys:
          for dataId in dataIds:
            for provided in providedBys: 
               for consumed in consumedBys:
                for title1 in titles:
                #if reqTitle == "DataAndControlCouple" :     
                 Add.turnstile_DataAndControlCouple(identifier = dataId, title= title1, description = desc, providedBy_identifier = provided,
                                                   consumedBy_identifier = consumed,  createdBy_identifier = createdBy )
        providedBys =[""]
        consumedBys =[""]

    def ingest_SystemRequirement(filePath):
      with open(filePath, "r") as txtFile:
        ##for l in txtFile.readlines():
        lastSysReqId = None
        #lastHLReqId = None
        SystemRequirement(txtFile)
        #HL_req(txtFile)

    def ingest_HighLevelRequirement(filePath):
      with open(filePath, "r") as txtFile:
        lastHLReqId = None
        HighLevelRequirement(txtFile)
        
    def ingest_LowLevelRequirement(filePath):
      with open(filePath, "r") as txtFile:
        satisfiesIds= None  
        lastLLReqId = None
        LowLevelRequirement(txtFile)
    def ingest_SoftwareDesign(filePath):
      with open(filePath, "r") as txtFile:
        SoftwareDesign(txtFile)
    def ingest_SoftwareThread(filePath):
      with open(filePath, "r") as txtFile:
        SoftwareThread(txtFile)
    def ingest_Engineer(filePath):
      with open(filePath, "r") as txtFile:
        engineer(txtFile)
    def ingest_dataAndControlCouple(filePath):
      with open(filePath, "r") as txtFile:
        providedBys = None  
        dataAndControlCouple(txtFile)    
    ################################################
    #    System Requirements
    ################################################
   
    createEvidenceFile(ingestionTitle="TurnstileIngestion-System Requirements", ingestionDescription="Manual ingestion of Turnstile System Requirements")
    Add.SYSTEM(identifier="Turnstile")

    ingest_SystemRequirement("Sys_Req.txt")
    createCDR("http://rack001/turnstiledata")
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".",out_path+"/TurnstileSystemRequirements"))

    ################################################
    #    High-level Requirements Version 1,2
    ################################################        
    createEvidenceFile(ingestionTitle="TurnstileIngestion-High Level Requirements", ingestionDescription="Manual ingestion of Turnstile High Level Requirements")
    

    ingest_HighLevelRequirement("HL_Req.txt")
    createCDR("http://rack001/turnstiledata")
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".",out_path+"/TurnstileHighLevelRequirements"))

    ################################################
    #    Low-level Requirements 
    ################################################   
    createEvidenceFile(ingestionTitle="TurnstileIngestion-Low Level Requirements", ingestionDescription="Manual ingestion of Turnstile Low Level Requirements") 
    ingest_Engineer("LL_Req.txt")
    ingest_SoftwareDesign("LL_Req.txt")
    ingest_SoftwareThread("LL_Req.txt")
    ingest_LowLevelRequirement("LL_Req.txt")
    #ingest_dataAndControlCouple("Req1/LL_Req.txt")
    
    createCDR("http://rack001/turnstiledata")
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".",out_path+"/TurnstileLowLevelRequirements"))
if __name__=="__main__":
    if os.path.exists(os.path.join(".",out_path+"/TurnstileSystemRequirements")):
        shutil.rmtree(os.path.join(".",out_path+"/TurnstileSystemRequirements"))
    if os.path.exists(os.path.join(".",out_path+"/TurnstileHighLevelRequirements")):
        shutil.rmtree(os.path.join(".",out_path+"/TurnstileHighLevelRequirements"))
    if os.path.exists(os.path.join(".",out_path+"/TurnstileLowLevelRequirements")):
        shutil.rmtree(os.path.join(".",out_path+"/TurnstileLowLevelRequirements"))
    CreateCdrs()
    
