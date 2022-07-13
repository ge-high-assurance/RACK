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

from Logging import *
from Evidence import *
import Evidence.Add as Add 
import shutil
import os.path


def CreateCdrs():
    
    #Logging.TRACE = True
    #Logging.DEBUG = True

    createEvidenceFile(ingestionTitle="TurnstileIngestion-PlanningDocuments", ingestionDescription="Manual ingestion of Turnstile Planning documents")

    #######################################################################
    #  Planning Documents
    #######################################################################
    #-------------------------- 
    Add.DOCUMENT.DOCUMENT(identifier="SW-STD:v1",
                title = "Turnstile Software Development Standards")
    #-------------------------- 
    Add.DOCUMENT.DOCUMENT(identifier="SQ-STD:v1",
                title = "Turnstile Requirement Standards")
    #-------------------------- 
    Add.DOCUMENT.DOCUMENT(identifier="VER-STD:v1",
                title = "Turnstile Verification Standards")

    createCDR("http://rack001/turnstiledata")


if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/PlanningDocuments")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/PlanningDocuments"))
    CreateCdrs()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/PlanningDocuments"))
