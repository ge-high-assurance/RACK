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


createEvidenceFile()

Add.PROV_S.ACTIVITY(identifier="TST-ACTIVITY-1")
Add.REQUIREMENTS.REQUIREMENT(identifier="TST-REQUIREMENT-1", description="Thing shall do what it should do.")
Add.REQUIREMENTS.REQUIREMENT(identifier="TST-REQUIREMENT-2", 
                description="Thing2 shall do what it should do.", 
                satisfies_identifier="TST-REQUIREMENT-1",
                wasAttributedTo_identifier="TST-ACTIVITY-1")
                
#Add.REQUIREMENTS.REQUIREMENT(ientifier="TST-REQUIREMENT-2")

createCDR()
