#!/usr/bin/env python3
import DataAccess as da
import ResolutionEngine as re
from difflib import SequenceMatcher
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
    data = da.getData(e)
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
    data = da.getData(e)
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
    data = da.getData(e)
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
    
def run(entities,  reset=True):
    resEngine = re.ResolutionEngine(copy=reset)
    resEngine.addEntities(entities)
    resEngine.addAbsoluteRule(dataInsertedByCheck) 
    resEngine.addAbsoluteRule(identifierCompare)
    resEngine.addRelativeRule(fuzzyIdentifierCompare)
    resEngine.addRelativeRule(fuzzyDescriptionCompare)
    
    resEngine.runAllAnalysis()
    
if __name__ == "__main__":
    run(True)
