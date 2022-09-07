#!/usr/bin/env python3
import ResolutionEngine
import semtk3
from difflib import SequenceMatcher
import time
from lxml import etree
import json
import os.path
import time
data = {}
entities = {}
DEBUG = False
def Debug(*args):
    if DEBUG:
        print(*args)
#####################################
# Queries
#####################################

#####################################
# helper Functions
#####################################
def cleanString(string):
    cleanString = ""
    for c in string:
        if c.isalnum():
            cleanString+=c
        else:
            cleanString += " "
    while cleanString.find("  ")!=-1:
        cleanString = cleanString.replace("  "," ")

    return cleanString.upper().rstrip(" ").lstrip(" ")
#####################################
# Rules Definitions
#####################################
def getDescription(e):
    guid = e.split("#")[-1]
    data = cacheData(e)
    if "@graph" not in data: # No Graph tag means there is a single element at the root.
        baseElement = data
        if 'PROV_S:description' in data:
            return data['PROV_S:description']
        else:
            return None
    for el in data["@graph"]:
        if el["@id"].split(":")[-1] == guid:
            if 'PROV_S:description' in el:
                return el['PROV_S:description']
            else:
                return None
            
def fuzzyDescriptionCompare(e1,e2):
    Debug("descriptionCompare")
    if getDescription(e1) != None and getDescription(e2) != None:
        t1 = cleanString(getDescription(e1))
        t2 = cleanString(getDescription(e2))
        matcher = SequenceMatcher(None, t1, t2)
        return True, matcher.ratio()*2
    else:
        return False, 1.0
def fuzzyIdentifierCompare(e1,e2):
    Debug("fuzzyIdentifierCompare")
    global data
    t1 = cleanString(getIdentifier(e1))
    t2 = cleanString(getIdentifier(e2))
    matcher = SequenceMatcher(None, t1, t2)
    return True, matcher.ratio() * 2
def getIdentifier(e):
    guid = e.split("#")[-1]
    data = cacheData(e)
    if "@graph" not in data: # No Graph tag means there is a single element at the root.
        baseElement = data
        return data['PROV_S:identifier']
    ## Create a Hash based on the GUID and find the base 
    baseElement = None
    for el in data["@graph"]:
        if el["@id"].split(":")[-1] == guid:
            return el['PROV_S:identifier']
            
def identifierCompare(e1,e2):
    Debug("identifierCompare")
    global data
    t1 = cleanString(getIdentifier(e1))
    t2 = cleanString(getIdentifier(e2))

    if t1 == t2:
        return True, 5.0
    else:
        return False, 5.0
        
def getDataInsertedBy(e):
    guid = e.split("#")[-1]
    data = cacheData(e)
    ## Create a Hash based on the GUID and find the base Element
    elements={}
    baseElement = None
    if "@graph" not in data: # No Graph tag means there is a single element at the root.
        baseElement = data
        return list()

    for el in data["@graph"]:
        elements[el["@id"].split(":")[-1]] = el
        if el["@id"].split(":")[-1] == guid:
            baseElement = el
    dataInsertedByIdentifiers = []   
    ## Get dataInsertedBy Elements and collect the identifiers
    if type(baseElement['PROV_S:dataInsertedBy']) is dict:
        dataInsertedByIdentifiers.append(elements[baseElement['PROV_S:dataInsertedBy']['@id'].split(":")[-1]]['PROV_S:identifier'])
    elif type(baseElement['PROV_S:dataInsertedBy']) is list:
        for i in baseElement['PROV_S:dataInsertedBy']:
            dataInsertedByIdentifiers.append(elements[i['@id'].split(":")[-1]]['PROV_S:identifier'])
    else:
        print("***** ERRROR 1 *****")
    return dataInsertedByIdentifiers
    
    
def dataInsertedByCheck(e1,e2):
    Debug("dataInsertedByCheck")
    global data
    t1 = getDataInsertedBy(e1)
    t2 = getDataInsertedBy(e2)

    # If the requirements were inserted by the same ingestion then assum the entities are different
    for i1 in t1:
        for i2 in t2:
            if i1 == i2:
                return True, 0.0
    
    return False, 1.0

def cacheData(e):
    guid = e.split("#")[-1]
    if not os.path.exists("cache/"+guid+".json"):
        print("Caching Data for:",e, flush=True)
    
        with open("cache/"+guid+".json", "w") as outFile:
            constraint = semtk3.build_constraint("REQUIREMENT", semtk3.OP_MATCHES, [e] )
            res = semtk3.query_by_id("GetRequirementData", runtime_constraints= [constraint])
            json.dump(res, outFile)
        return res
    else:
        while os.path.getsize("cache/"+guid+".json") == 0:
            time.sleep(0.5)
        with open("cache/"+guid+".json", "r") as outFile:
            return json.load(outFile)
#####################################
# Get Entities
#####################################        
def getEntities(connString):
    queryString = """prefix REQUIREMENTS:<http://arcos.rack/REQUIREMENTS#>
prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix rdfs:<http://www.w3.org/2000/01/rdf-schema#>
prefix SRI:<http://arcos.descert/SRI#>
prefix XMLSchema:<http://www.w3.org/2001/XMLSchema#>
prefix generated:<http://semtk.research.ge.com/generated#>
prefix SRI_SS:<http://arcos.descert/SRI-SS#>
prefix Boeing:<http://arcos.AH-64D/Boeing#>
select distinct ?super ?directSub 
		FROM <http://rack001/model>
WHERE { ?directSub rdfs:subClassOf ?super .
VALUES ?super {REQUIREMENTS:REQUIREMENT} .
}"""

    
    tab = semtk3.query(queryString,connString)
    classes = []
    for c in tab.get_column("directSub"):
        superName =tab.get_column("super")[tab.get_column("directSub").index(c)]
        if superName not in classes:
            classes.append(superName)
        if c not in classes:
            classes.append(c)

    queryString = """prefix REQUIREMENTS:<http://arcos.rack/REQUIREMENTS#>
prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix rdfs:<http://www.w3.org/2000/01/rdf-schema#>
prefix SRI:<http://arcos.descert/SRI#>

prefix XMLSchema:<http://www.w3.org/2001/XMLSchema#>
prefix generated:<http://semtk.research.ge.com/generated#>
prefix SRI_SS:<http://arcos.descert/SRI-SS#>
prefix Boeing:<http://arcos.AH-64D/Boeing#>
select distinct ?REQUIREMENT

		FROM <http://rack001/ResolvedData>
		FROM <http://rack001/mitre-cwe>
		FROM <http://rack001/nist-800-53>
		FROM <http://rack001/model>
 where {

	VALUES ?REQUIREMENT_type { REQUIREMENTS:REQUIREMENT <{ReqType}>  } .
	?REQUIREMENT a ?REQUIREMENT_type .
}"""

    ReqDict = {}
    ReqList = []
    for ReqType in classes:
        tab =semtk3.query(queryString.replace("{ReqType}",ReqType),connString)
        ReqDict[ReqType] =tab.get_column("REQUIREMENT")
        for r in ReqDict[ReqType]:
            ReqList.append((r,ReqType))
    for r in ReqDict:
        print(r, len(ReqDict[r]))
    
    temp = {}
    
    for r in ReqList:
        reqType = r[1]
        temp[r[0]] = []
        for t in ReqDict[reqType]:
            if t != r[0]:
                temp[r[0]].append(t)
    return temp
    
def run(reset):
    global entities
    # Connect to semTK
    reqEngine = ResolutionEngine.ResolutionEngine(copy=reset)
    semtk3.set_connection_override(reqEngine.resolvedConnection)
    all_ok = semtk3.check_services();
    if not all_ok: 
        raise Exception("Semtk services are not properly running on localhost")

    reqEngine.addEntities(getEntities(reqEngine.resolvedConnection))
    reqEngine.addAbsoluteRule(dataInsertedByCheck) 
    reqEngine.addAbsoluteRule(identifierCompare)
    reqEngine.addRelativeRule(fuzzyIdentifierCompare)
    reqEngine.addRelativeRule(fuzzyDescriptionCompare)
    
    reqEngine.runAllAnalysis()
    
if __name__ == "__main__":
    run(True)
