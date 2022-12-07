#!/usr/bin/env python3
import os
import json
import semtk3
import os.path
import RACK_CONSTANTS as rc
def cacheData(e):
    guid = e.split("#")[-1]
    nodegroup_json_str = ""
    with open("getData.json", "r") as jsonFile:
        nodegroup_json_str = jsonFile.read()
    constraint = semtk3.build_constraint("THING", semtk3.OP_MATCHES, [e] )
    res = semtk3.query_by_nodegroup(nodegroup_json_str, runtime_constraints= [constraint])
    with open("cache/"+guid+".json", "w") as dataFile:
        json.dump(res, dataFile, indent = 4)


def getType(e):
    guid = e.split("#")[-1]
    data = None
    typeStr = ""
    if not os.path.exists("cache/"+guid+".json"):
        cacheData(e)
    with open("cache/"+guid+".json", "r") as dataFile:
        data = json.load(dataFile)
    if "@graph" not in data: # No Graph tag means there is a single element at the root.
        typeStr = data['@type']
    else:
        for el in data["@graph"]:
            if el["@id"].split(":")[-1] == guid:
                typeStr = el['@type']
                break
    context = data["@context"]
    mod,  t = typeStr.split(":")
    return context[mod]+t
    
def getIdentifier(e):
    guid = e.split("#")[-1]
    data = None
    if not os.path.exists("cache/"+guid+".json"):
        cacheData(e)
    with open("cache/"+guid+".json", "r") as dataFile:
        data = json.load(dataFile)
    if "@graph" not in data: # No Graph tag means there is a single element at the root.
        #baseElement = data
        return data['PROV_S:identifier']
    ## Create a Hash based on the GUID and find the base 
    #baseElement = None
    for el in data["@graph"]:
        if el["@id"].split(":")[-1] == guid:
            return el['PROV_S:identifier']
            
def getData(e):
    guid = e.split("#")[-1]
    data = None
    if not os.path.exists("cache/"+guid+".json"):
        cacheData(e)
    with open("cache/"+guid+".json", "r") as dataFile:
        data = json.load(dataFile)
    return data
    
if __name__ == "__main__":
    semtk3.upload_owl("Model.owl", rc.connString2, model_or_data=semtk3.SEMTK3_CONN_DATA, conn_index = 0)
