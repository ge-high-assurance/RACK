#!/bin/python3
import os.path
import shutil
from Evidence import createEvidenceFile, createCDR
import Evidence.Add as Add
if __name__ == "__main__":
    #################################################################
    if os.path.exists(os.path.join(".","Package-1")):
        shutil.rmtree(os.path.join(".","Package-1"))
    createEvidenceFile(ingestionTitle="Package 1")
    Add.TESTING.TEST(identifier="{L1-TST-1}", description="This is test L1-1.", verifies_identifier="{L1-REQ-1}")
    Add.TESTING.TEST(identifier="{L1-TST-2}", description="This is test L1-2.", verifies_identifier="{L1-REQ-1}")
    Add.REQUIREMENTS.REQUIREMENT(identifier="{L1-REQ-1}")
    createCDR()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Package-1"))
    
    #################################################################                  
    if os.path.exists(os.path.join(".","Package-2")):
        shutil.rmtree(os.path.join(".","Package-2"))
    createEvidenceFile(ingestionTitle="Package 2")
    Add.REQUIREMENTS.REQUIREMENT(identifier="L1-REQ-1", description="This is requirement L1-1.", satisfies_identifier="L0-REQ-1")
    Add.REQUIREMENTS.REQUIREMENT(identifier="L1-REQ-2", description="This is requirement L1-2.", satisfies_identifier="L0-REQ-1")
    Add.REQUIREMENTS.REQUIREMENT(identifier="L0-REQ-1")
    createCDR()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Package-2"))
    
    #################################################################    
    if os.path.exists(os.path.join(".","Package-3")):
        shutil.rmtree(os.path.join(".","Package-3"))
    createEvidenceFile(ingestionTitle="Package 3")
    Add.REQUIREMENTS.REQUIREMENT(identifier="[L1-REQ-2]",  satisfies_identifier="[L0-REQ-2]")
    Add.REQUIREMENTS.REQUIREMENT(identifier="[L1-REQ-3]",  satisfies_identifier="[L0-REQ-2]")
    Add.REQUIREMENTS.REQUIREMENT(identifier="[L0-REQ-2]")
    createCDR()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Package-3"))
    
    #################################################################    
    if os.path.exists(os.path.join(".","Resolutions-1")):
        shutil.rmtree(os.path.join(".","Resolutions-1"))
    createEvidenceFile(ingestionTitle="Resolutions-1")
    Add.RESOLUTIONS.SAME_AS(primary_identifier="L1-REQ-1", secondary_identifier="{L1-REQ-1}")
    createCDR()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Resolutions-1"))
    
    #################################################################    
    if os.path.exists(os.path.join(".","Resolutions-2")):
        shutil.rmtree(os.path.join(".","Resolutions-2"))
    createEvidenceFile(ingestionTitle="Resolutions-2")
    Add.RESOLUTIONS.SAME_AS(primary_identifier="L1-REQ-2", secondary_identifier="[L1-REQ-2]")
    createCDR()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Resolutions-2"))
