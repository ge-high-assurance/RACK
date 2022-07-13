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

    createEvidenceFile(ingestionTitle="TurnstileIngestion-SystemDesign", ingestionDescription="Manual ingestion of Turnstile System Design")

    Add.SYSTEM.SYSTEM(identifier="Turnstile")

    
    Add.GE.SystemComponent(identifier="InGate",
                partOf_identifier = "Turnstile")

    Add.GE.SystemComponent(identifier="OutGate",
                partOf_identifier = "Turnstile")

    Add.GE.SystemComponent(identifier="CounterApplication",
                partOf_identifier = "Turnstile")

    Add.GE.SystemComponent(identifier="Display",
                partOf_identifier = "Turnstile")

    Add.GE.SystemInterfaceDefinition (identifier="inflow",
                source_identifier = "InGate",
                destination_identifier = "CounterApplication")

    Add.GE.SystemInterfaceDefinition (identifier="outflow",
                source_identifier = "OutGate",
                destination_identifier = "CounterApplication")

    Add.GE.SystemInterfaceDefinition (identifier="census",
                source_identifier = "CounterApplication",
                destination_identifier = "Display")

    Add.GE.SystemInterfaceDefinition (identifier="census",
                destination_identifier = "InGate")

    createCDR("http://rack001/turnstiledata")

if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemDesign")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemDesign"))
    CreateCdrs()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemDesign"))
