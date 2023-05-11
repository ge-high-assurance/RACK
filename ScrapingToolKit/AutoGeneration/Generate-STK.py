#!/usr/bin/env python3
#
# Copyright (c) 2021-2022, General Electric Company, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

import os
import json
import semtk3
xmlProperty = '''
                            <xs:element name="{}" type="xs:{}"/>'''

xmlElementHeader ='''
                <xs:element name="{}_{}">
                    <xs:complexType>
                        <xs:sequence>'''

xmlElementFooter ='''
                        </xs:sequence>
                    </xs:complexType>
                </xs:element>'''

xmlFooter = '''
            </xs:sequence>
        </xs:complexType>
    </xs:element>
</xs:schema>'''

xmlHeader = '''<?xml version="1.0" encoding="UTF-8"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:element name="RACK-DATA">
        <xs:complexType>
            <xs:sequence>'''
addFunctionComment = """    '''
    |===============================================================|
    | {:^61} |
    |===============================================================|
    | {:61} |
    |---------------------------------------------------------------|{}
    |===============================================================|

    '''
    @staticmethod
"""
addDataTypeComment = """
    |    {:40}: {:16} |"""
addFileHeader = """#!/usr/bin/env python3
#
# Copyright (c) 2022, General Electric Company, Galois, Inc.
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
"""

connString = """{  
    "model":[
        {"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/model"}
    ],
    "data":[]
}"""


def cleanName(string):
    newString = ""
    for c in string:
        if c.isalnum():
            newString+=c
        else:
            newString+="_"
    if newString[0].isnumeric():
        newString ="_"+newString
    return newString

def genDataTypeComment(templateData):
    dataTypeComment = ""
    for p in sorted(templateData):
        dataTypeComment += addDataTypeComment.format(p, templateData[p])
    return dataTypeComment

def genFunctionDef(className,templateData):
    functionDef = "    def {}(".format(className)
    for p in sorted(templateData):
        functionDef += p+"=None, "
    return functionDef.rstrip(", ")+"):\n"

def genFunctionBody(groupName,className,templateData):
    pyAddString  = '        trace()\n'
    pyAddString += '        log("Adding Evidence:",str_good("'+className+'"))\n'
    pyAddString += '        objStr = "<{}_{}>"\n'.format(groupName,className)
    for p in sorted(templateData):
        pyAddString += '        objStr += objectDataString("{propName}", {propName})\n'.replace('{propName}',p)
    pyAddString += '        objStr += "</{}_{}>"\n'.format(groupName,className)
    pyAddString += '        addEvidenceObject(etree.fromstring(objStr))\n\n'    
    return pyAddString
    
def genXsdProperties(templateData):
    xsdProperties = ""
    for p in sorted(templateData):
        xsdProperties += xmlProperty.format(p, templateData[p])
    return xsdProperties

def generateFiles(orgDict):
    directoryPath = os.path.dirname(os.path.realpath(__file__))
    
    # Generate File Paths
    pyAddFilePath  = os.path.realpath(os.path.join(directoryPath, "..","Evidence","Add.py"))
    XsdFilePath        = os.path.realpath(os.path.join(directoryPath, "..","Evidence","RACK-DATA.xsd"))
    pyConstantsFilePath = os.path.realpath(os.path.join(directoryPath, "..","Evidence","CONSTANTS.py"))
    
    # Create File objects 
    pyAddFile = open(pyAddFilePath,"w")
    pyConstantsFile = open(pyConstantsFilePath,"w")
    xsdFile = open(XsdFilePath,"w")
    
    # Write Headers to files
    pyAddFile.write(addFileHeader)
    pyConstantsFile.write('nodegroupMapping = {\n')
    xsdFile.write(xmlHeader)
    
    pyConstantsString = ""
    
    # Loop for Namespace Groups
    for o in sorted(orgDict):
        groupName = cleanName(o)
        print("Generating for {}...".format(groupName))
        pyAddFile.write("class {}:\n".format(groupName))
        
        # Loop for Classes
        for c in sorted(orgDict[o]):
            print("    {}...".format(orgDict[o][c]))
            className = cleanName(c)
            
            # add a Line to the Constants string
            pyConstantsString += '    "{}_{}":"{}",\n'.format(groupName,className,orgDict[o][c])            
            
            # Query semtk to get the Ingestion Template information
            templateData = getTemplate(orgDict[o][c])
            
            # Write XSD 
            xsdFile.write(xmlElementHeader.format(groupName,className))
            xsdFile.write(genXsdProperties(templateData))
            xsdFile.write(xmlElementFooter)

            # write Add Function
            pyAddFile.write(addFunctionComment.format(orgDict[o][c],"Add.{}.{}()".format(groupName,className), genDataTypeComment(templateData)))
            pyAddFile.write(genFunctionDef(className, templateData))
            pyAddFile.write(genFunctionBody(groupName,className, templateData))
    
    # Write Footers        
    pyConstantsFile.write(pyConstantsString.rstrip(",\n") + '}')
    xsdFile.write(xmlFooter)
    
    # Close open file objects
    pyAddFile.close()
    pyConstantsFile.close()
    xsdFile.close()

def getTemplate(classUri):
    nodeProperties = {}    
    (ng, col_names_str, col_types_str) = semtk3.get_class_template_and_csv(classUri, id_regex="identifier")
    col_names = col_names_str.rstrip().split(",")
    col_types = col_types_str.rstrip().split(",")
    
    for cn in col_names:
        nodeProperties[cn] = col_types[col_names.index(cn)]
    return nodeProperties

def organizeClasses(classList):
    orgDict = {}
    for c in classList:
        className = c.split("#")[-1]
        groupName = c.split("#")[0].split("/")[-1]
        if groupName not in orgDict:
            orgDict[groupName] = {}
        orgDict[groupName][className] = c
    return orgDict

if __name__ == "__main__":
    # First Connect to semTK    
    semtk3.set_connection_override(connString)
    all_ok = semtk3.check_services();
    if not all_ok: 
        raise Exception("Semtk services are not properly running on localhost")
    
    classList = semtk3.get_class_names()    
    orgDict = organizeClasses(classList)
    generateFiles(orgDict)

