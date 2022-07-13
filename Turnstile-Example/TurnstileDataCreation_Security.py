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

    createEvidenceFile(ingestionTitle="TurnstileIngestion-Security", ingestionDescription="Manual ingestion of Turnstile Security Design")

    Add.CPS.Cps(identifier="TurnstileCps",
            wasDerivedFrom_identifier="Turnstile",
            insideTrustedBoundary="true")

    
    Add.CPS.Cps(identifier="InGateCps", 
            wasDerivedFrom_identifier="InGate",
            partOf_identifier = "TurnstileCps",
            insideTrustedBoundary="true")

    Add.CPS.Cps(identifier="OutGateCps", 
            wasDerivedFrom_identifier="OutGate",
            partOf_identifier = "TurnstileCps",
            insideTrustedBoundary="true")

    Add.CPS.Cps(identifier="CounterApplicationCps", 
            wasDerivedFrom_identifier="CounterApplication",
            partOf_identifier = "TurnstileCps",
            insideTrustedBoundary="true")

    Add.CPS.Cps(identifier="DisplayCps", 
            wasDerivedFrom_identifier="Display",
            partOf_identifier = "TurnstileCps",
            insideTrustedBoundary="true")
    
    Add.CPS.ConnectionType(identifier="Untrusted")

    Add.CPS.Connection(identifier="inflowConn",
                   wasDerivedFrom_identifier="inflow", 
                   source_identifier = "InGateCps",
                   destination_identifier = "CounterApplicationCps",
                   connectionType_identifier ="Untrusted")

    Add.SECURITY.THREAT(identifier="CAPEC-148",
               description = "Content Spoofing - An adversary modifies content...")
               
    Add.SECURITY.CONTROL(identifier = "IA-3",
                description = "Device Identification And Authentication - The information system uniquely identifies and authenticates [Assignment: organization-defined specific and/or types of devices] before establishing a [Selection (one or more): local; remote; network] connection..")
                
    Add.SECURITY.CONTROL(identifier = "IA-3-1",
                description = "Cryptographic Bidirectional Authentication - The information system authenticates [Assignment: organization-defined specific devices and/or types of devices] before establishing [Selection (one or more): local; remote; network] connection using bidirectional authentication that is crypto-graphically based..")
                
    Add.SECURITY.CONTROLSET(identifier="DeviceAuthentication",
                   content_identifier="IA-3",
                   mitigates_identifier="CAPEC-148")
                   
    Add.SECURITY.CONTROLSET(identifier="DeviceAuthentication",
                   content_identifier="IA-3-1")

    Add.CPS.ImplControl(identifier="ic1",
                    control_identifier="IA-3",
                    dal="7")

    Add.CPS.ImplControl(identifier="ic2",
                    control_identifier="IA-3-1",
                    dal="6")

    Add.GE.HighLevelRequirement(identifier="HLR-4:v1",
                                       description = "CounterApplication shall verify that the data received on inflow is from InGate and is uncorrupted.",
                                       governs_identifier="CounterApplication")

    Add.GE.HighLevelRequirement(identifier="HLR-4:v1",
                                       satisfies_identifier="IA-3")
                                       
    Add.GE.HighLevelRequirement(identifier="HLR-4:v1",
                                       satisfies_identifier="IA-3-1")
    Add.GE.LowLevelRequirement(identifier="IN-LLR-7",
                                      description="Input Thread shall validate udp messages via the use of a 256-bit crytographic key",
                                      satisfies_identifier="HLR-4:v1")
    Add.GE.LowLevelRequirement(identifier="EXE-LLR-8",
                                      description="On initialization Executive shall exchange cryptographic keys with the Ingate and outgate via tcp via port 63432.",
                                      satisfies_identifier="HLR-4:v1")
    createCDR("http://rack001/turnstiledata")

if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileSecurity")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileSecurity"))
    CreateCdrs()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileSecurity"))
