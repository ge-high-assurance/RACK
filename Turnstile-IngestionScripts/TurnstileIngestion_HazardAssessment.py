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

    createEvidenceFile(ingestionTitle="TurnstileIngestion-HazardAssessment", ingestionDescription="Manual ingestion of Hazard Assessment")

    Add.SYSTEM(identifier="Turnstile")

    
    Add.HAZARD(identifier="H-1",
                definition="System Crash",
                source_identifier = "Turnstile")

    Add.HAZARD(identifier="H-1.1",
                definition="Integer Under Flow",
                wasDerivedFrom_identifier = "H-1")

    Add.HAZARD(identifier="H-1.2",
                definition="Integer Over Flow",
                wasDerivedFrom_identifier = "H-1")

    Add.HAZARD(identifier="H-2",
                definition="Park Exceeds Capacity",
                source_identifier = "Turnstile")

    createCDR("http://rack001/turnstiledata")

if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/HazardAssessment")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/HazardAssessment"))
    CreateCdrs()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/HazardAssessment"))
