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

    createEvidenceFile(ingestionTitle="TurnstileIngestion-RequirementModel", ingestionDescription="Manual ingestion of Turnstile Requirement Model")
    
    
    
    
    #------------ HLR-1-Model ------------ 
    Add.MODEL(identifier="HLR-1-Model", models_identifier="HLR-1:v1")
    Add.turnstile_HighLevelRequirement(identifier="HLR-1:v1")
    
    #------------ HLR-2-Model ------------ 
    Add.MODEL(identifier="HLR-2-Model", models_identifier="HLR-2:v1")
    Add.turnstile_HighLevelRequirement(identifier="HLR-2:v1")
    
    #------------ HLR-3-Model ------------ 
    Add.MODEL(identifier="HLR-3-Model", models_identifier="HLR-3:v1")
    Add.turnstile_HighLevelRequirement(identifier="HLR-3:v1")

    #------------ HLR Analysis ------------ 
    Add.ANALYSIS(identifier="HLR Analysis", description="HLR Analysis performs automated analysis of a modeled requirement in relation to completeness and conflicts.")
    Add.ANALYSIS(identifier="HLR Analysis", used_identifier="HLR-1-Model")
    Add.ANALYSIS(identifier="HLR Analysis", used_identifier="HLR-2-Model")
    Add.ANALYSIS(identifier="HLR Analysis", used_identifier="HLR-3-Model")
    
    #------------ HLR Analysis-Completeness ------------ 
    Add.ANALYSIS_OUTPUT(identifier="HLR Analysis-Completeness", wasGeneratedBy_identifier="HLR Analysis", result_identifier="Passed")
    
    #------------ HLR Analysis-Conflict ------------ 
    Add.ANALYSIS_OUTPUT(identifier="HLR Analysis-Conflict", wasGeneratedBy_identifier="HLR Analysis", result_identifier="Passed")
    

    createCDR("http://rack001/turnstiledata")


if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileRequirementModel")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileRequirementModel"))
    CreateCdrs()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileRequirementModel"))
