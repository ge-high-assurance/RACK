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

import csv
import os.path
import sys

rackDataDict = {}

class ontProperty:
    name = ""
    dataType = ""
    comment = ""
    def __init__(self, rowData):
        self.name = rowData['PROPERTY'].replace(" (SHARED PROPERTY)","")
        self.dataType = rowData['DATA TYPE']
        self.comment = rowData['COMMENT(S)']
    def getPy(self):
        primitiveDataTypes = ["xs:string",
                              "xs:boolean",
                              "xs:decimal",
                              "xs:float",
                              "xs:double",
                              "xs:duration",
                              "xs:dateTime",
                              "xs:time",
                              "xs:date",
                              "xs:gYearMonth",
                              "xs:gYear",
                              "xs:gMonthDay",
                              "xs:gDay",
                              "xs:gMonth",
                              "xs:hexBinary",
                              "xs:base64Binary"
                              "xs:anyURI",
                              "xs:QName",
                              "xs:NOTATION"]
        if "xs:"+self.dataType not in primitiveDataTypes:
            return self.name+"_identifier"
        else:
            return self.name        
    def getXsd(self):
        primitiveDataTypes = ["xs:string",
                              "xs:boolean",
                              "xs:decimal",
                              "xs:float",
                              "xs:double",
                              "xs:duration",
                              "xs:dateTime",
                              "xs:time",
                              "xs:date",
                              "xs:gYearMonth",
                              "xs:gYear",
                              "xs:gMonthDay",
                              "xs:gDay",
                              "xs:gMonth",
                              "xs:hexBinary",
                              "xs:base64Binary"
                              "xs:anyURI",
                              "xs:QName",
                              "xs:NOTATION"]
        dataTypeStr = "xs:"+self.dataType
        nameStr = self.name
        if dataTypeStr not in primitiveDataTypes:
            dataTypeStr = "xs:string"
            nameStr = self.name+"_identifier"
        return '''
                            <xs:element name="'''+nameStr+'''" type="'''+dataTypeStr+'''"/>'''
    
class OntClass:
    name = None
    properties = None
    
    def __init__(self, className):
        self.name = className
        self.properties = list()
    def addRow(self, rowData):
        if rowData['PROPERTY']!="" and rowData['DATA TYPE']!="":
            self.properties.append(ontProperty(rowData))
    def getPy(self):
        propList = list()
        pyStr = 'def '+self.name+'('
        for p in self.properties:
            if p.getPy() not in propList:
                pyStr += p.getPy() + "=None, "
                propList.append(p.getPy())
        pyStr = pyStr.rstrip(', ') + '):\n'
        pyStr += '    trace()\n'
        pyStr += '    log("Adding Evidence:",str_good("'+self.name+'"))\n'
        pyStr += '    objStr = "<'+self.name+'>"\n'
        propList = list()
        for p in self.properties:
            if p.getPy() not in propList:
                pyStr += '    objStr += objectDataString("{propName}", {propName})\n'.replace('{propName}',p.getPy())
                propList.append(p.getPy())
        pyStr += '    objStr += "</'+self.name+'>"\n'
        pyStr += '    addEvidenceObject(etree.fromstring(objStr))\n\n'
        return(pyStr)

    def getXsd(self):
        xsdStr = '''
                <xs:element name="'''+self.name+'''">
                    <xs:complexType>
                        <xs:sequence>'''
        for p in self.properties:
            xsdStr += p.getXsd()
        xsdStr += '''
                        </xs:sequence>
                    </xs:complexType>
                </xs:element>'''
        return xsdStr


# Read the SemTK Data Dictionary
with open("table.csv","r") as csvFile:
    reader = csv.DictReader(csvFile)
    for row in reader:
        if row['CLASS'] not in rackDataDict:
            rackDataDict[row['CLASS']]=OntClass(row['CLASS'])
        rackDataDict[row['CLASS']].addRow(row)

keyList = list(rackDataDict.keys())
keyList.sort()

# Create the XSD for the SemTK Data Dictionary
XsdFilePath = os.path.join(os.path.split(sys.argv[0])[0],"..","Evidence","RACK-DATA.xsd")
with open(XsdFilePath,"w") as xmlFile:
    xmlFile.write('''<?xml version="1.0" encoding="UTF-8"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:element name="RACK-DATA">
        <xs:complexType>
            <xs:sequence>''')
    for k in keyList:
        xmlFile.write(rackDataDict[k].getXsd())
    xmlFile.write('''
            </xs:sequence>
        </xs:complexType>
    </xs:element>
</xs:schema>''')

EvidenceFilePath = os.path.join(os.path.split(sys.argv[0])[0],"..","Evidence","Add.py")
with open(EvidenceFilePath,"w") as pyFile:
    pyFile.write("""#!/usr/bin/env python3
#
# Copyright (c) 2021, General Electric Company, Galois, Inc.
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
'''

=======================================================================

    This file is Auto-Generated by GenFiles.py. 
        Any changes made to this file may be over written.

=======================================================================
'''
""")
    for k in keyList:
        pyFile.write(rackDataDict[k].getPy())
        
