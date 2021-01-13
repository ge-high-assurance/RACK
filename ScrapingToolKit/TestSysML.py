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
import Logging as Logging


Logging.startLog("TestSysML.log")
#Logging.TRACE = True
Logging.DEBUG = True

createEvidenceFile()
XML.scrap("./SysMLData/test_case_10_valid_r2.xmi", SysML)
createCDR()


Logging.closeLog()