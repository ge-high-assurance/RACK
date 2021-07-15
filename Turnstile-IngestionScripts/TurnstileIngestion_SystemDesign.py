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

    createEvidenceFile(ingestionTitle="TurnstileIngestion-SystemDesign", ingestionDescription="Manual ingestion of Turnstile System Design")

    Add.SYSTEM(identifier="Turnstile")

    
    Add.turnstile_SystemComponent(identifier="In Gate",
                partOf_identifier = "Turnstile")

    Add.turnstile_SystemComponent(identifier="Out Gate",
                partOf_identifier = "Turnstile")

    Add.turnstile_SystemComponent(identifier="CounterApplication",
                partOf_identifier = "Turnstile")

    Add.turnstile_SystemComponent(identifier="Display",
                partOf_identifier = "Turnstile")

    Add.turnstile_SystemInterfaceDefinition (identifier="inflow",
                source_identifier = "In Gate",
                destination_identifier = "CounterApplication")

    Add.turnstile_SystemInterfaceDefinition (identifier="outflow",
                source_identifier = "Out Gate",
                destination_identifier = "CounterApplication")

    Add.turnstile_SystemInterfaceDefinition (identifier="census",
                source_identifier = "CounterApplication",
                destination_identifier = "Display")

    Add.turnstile_SystemInterfaceDefinition (identifier="census",
                destination_identifier = "In Gate")

    createCDR("http://rack001/turnstiledata")

if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemDesign")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemDesign"))
    CreateCdrs()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemDesign"))
