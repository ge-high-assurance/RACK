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

import os
import os.path
import sys
import csv
from datetime import datetime
from Logging import *
from lxml import etree

__EvidenceDir__ = None
__Evidence__ = None

__ingestionIdentifier__ = None

def getXsd():
    rackData = {}
    AutoGenerationDir = os.path.split(__file__)[0]
    xsd = etree.parse(os.path.join(AutoGenerationDir, "RACK-DATA.xsd"))
    root = xsd.getroot()
    for c in list(root):
        things = c.find('{http://www.w3.org/2001/XMLSchema}complexType')\
              .find('{http://www.w3.org/2001/XMLSchema}sequence')
        for d in list(things):
            properties = d.find('{http://www.w3.org/2001/XMLSchema}complexType')\
              .find('{http://www.w3.org/2001/XMLSchema}sequence')
            headers = list()
            for e in list(properties):
                headers.append(e.attrib['name'])
            rackData[d.attrib['name']]=headers
    return rackData

def createCDR(dataGraph="http://rack001/data"):
    '''
    Creating the CDR is done in two phases the first CDR only has the identifiers so that it will create all the objects
    the second phase has all the data, by splitting this into two phase ths allows the lookups to succeed regardless of load order
    '''
    global __EvidenceDir__, __Evidence__, __ingestionIdentifier__

    Add.ACTIVITY(identifier=__ingestionIdentifier__,endedAtTime=str(datetime.now()).split(".")[0])

    __Evidence__.write(__EvidenceDir__, 
               pretty_print=True,
               xml_declaration=True,
               encoding='UTF-8')

    cdrFiles = list()
    xsdSpec = getXsd()
    outputDir = __EvidenceDir__.replace(".xml","")
    if not os.path.exists(outputDir):
        os.makedirs(outputDir)
    for thing in xsdSpec:
        headers = xsdSpec[thing]
        usedHeaders = list(["identifier", "dataInsertedBy_identifier"]) # all CDRs will have at least these columns
        data = etree.parse(__EvidenceDir__)
        root = data.getroot()
        if root.find(thing)!=None:
            cdrFiles.append(thing)
            with open(os.path.join(outputDir,thing+"1.csv"), 'w', newline='', encoding="utf-8") as outFile:
                outwriter = csv.writer(outFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                outwriter.writerow(["identifier"])
                loaded = list()

                for c in root.iter(thing):
                    if c.find("identifier") is not None:
                        if c.find("identifier").text not in loaded:
                            outwriter.writerow([c.find("identifier").text])
                            loaded.append(c.find("identifier").text)
                        # Check to see if the header item 
                        for k in headers:
                            if c.find(k) is not None:
                                if k not in usedHeaders:
                                    usedHeaders.append(k)

                    else:
                        log("Identifier not found.")

            print("Found the following data for "+thing+":", str(usedHeaders))
            with open(os.path.join(outputDir,thing+"2.csv"), 'w', newline='', encoding="utf-8") as outFile:
                outwriter = csv.writer(outFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                outwriter.writerow(usedHeaders)

                for c in root.iter(thing):
                    rowData = list()
                    thingData = {}
                    for k in usedHeaders:
                        thingData[k] = c.find(k)
                    dataString = ""        
                    for k in usedHeaders:
                        if thingData[k] is not None:
                            rowData.append(thingData[k].text)
                        elif k == "dataInsertedBy_identifier":
                            rowData.append(__ingestionIdentifier__)

                        else:
                            rowData.append("")
                    outwriter.writerow(rowData)
          
    with open(os.path.join(outputDir,"import.yaml"), 'w', encoding="utf-8") as outFile:
        outFile.write('''data-graph: "{{DataGraph}}"
ingestion-steps:
#Phase1: Identifiers Only
'''.replace("{{DataGraph}}",dataGraph))
        for cdr in cdrFiles:
            outFile.write('- {nodegroup: "ingest_{{THING}}", csv: "{{THING}}1.csv"}\n'.replace("{{THING}}",cdr))
        outFile.write("\n#Phase2: All Evidence\n")
        for cdr in cdrFiles:
            outFile.write('- {nodegroup: "ingest_{{THING}}", csv: "{{THING}}2.csv"}\n'.replace("{{THING}}",cdr))
    return os.path.join(outputDir,"import.yaml")

def createEvidenceFile(ingestionTitle="ScrapingToolKitIngestion", ingestionDescription="Data that was ingested using the ARCOS Scraping Tool Kit.", filePath="RACK-DATA.xml"):   
    trace()
    global __EvidenceDir__, __ingestionIdentifier__, __Evidence__
    __Evidence__ = None
    __EvidenceDir__ = filePath
    log("Created Evidence File:", str_highlight(__EvidenceDir__))
    with open(__EvidenceDir__, "w") as eFile:
        eFile.write('<?xml version="1.0" encoding="UTF-8"?>')
        eFile.write('<RACK-DATA>')
        eFile.write('</RACK-DATA>')

    __ingestionIdentifier__ = ingestionTitle

    Add.ACTIVITY(identifier=__ingestionIdentifier__,description = ingestionDescription,
                 title= ingestionTitle,
                 startedAtTime=str(datetime.now()).split(".")[0])

def addEvidenceObject(eObject):   
    trace()
    global __EvidenceDir__, __Evidence__
    # Currently this function and really the whole module is not very efficient in that 
    #  it write to the hard drive every call, this could be improved by storing the xml data in 
    #  memory until a flush call or something similar.  This works well for now as you can 
    #  as it will log the data up to the point where a exception occurs
    if not os.path.exists(__EvidenceDir__):
        createEvidenceFile(__EvidenceDir__)
    if __Evidence__ is None:
        __Evidence__ = etree.parse(__EvidenceDir__, 
                                   etree.XMLParser(remove_blank_text=True))
    __Evidence__.getroot().append(eObject)



def objectDataString(typeStr, value):
    trace()
    rtnStr = ""
    if value != None:
        rtnStr+='<{type}>{value}</{type}>'\
            .replace("{type}",typeStr)\
            .replace("{value}", value)
    return rtnStr
