#!/usr/bin/env python3
import os
import json
import semtk3
import os.path
import time
import RACK_CONSTANTS as rc
def cacheData(e):
    guid = e.split("#")[-1]
    graph = "http://rack001/Data"
    res = semtk3.query_raw_sparql(rc.dataQuery\
                                                        .replace("{{GUID}}",guid) \
                                                        .replace("{{GRAPH}}",graph),\
                                                        result_type=semtk3.RESULT_TYPE_GRAPH_JSONLD)
    with open("cache/"+guid+".json", "w") as dataFile:
        json.dump(res, dataFile, indent = 4)

def getRelationships(e):
    relationships = []
    guid = e.split("#")[-1]
    data = getData(e)["@graph"]
    if type(data) == list:
        identData = {}
        for el in data:
            identData[el['@id']] = el['PROV_S:identifier']
        for el in data:
            if el['@id'][6:] == guid:
                for p in el:
                    if type(el[p]) == dict:
                        relationships.append((p,  identData[el[p]['@id']],  "Outgoing"))
            else:
                for p in el:
                    if type(el[p]) == dict:
                        relationships.append((p,  el['PROV_S:identifier'],    "Incoming"))
    return relationships
    
def getDataProperties(e):
    dataProperties = []
    guid = e.split("#")[-1]
    data = getData(e)["@graph"]
    if type(data) == list:
        for el in data:
            if el['@id'][6:] == guid:
                for p in el:
                    if type(el[p]) != dict:
                        dataProperties.append((p,  el[p]))
                break
    else:
        for p in data:
            if type(data[p]) != dict:
                dataProperties.append((p,  el[p]))  
    return dataProperties

def getDescription(e):
    guid = e.split("#")[-1]
    data = getData(e)["@graph"]
    if type(data) == list:
        for el in data:
            if el['@id'][6:] == guid:
                if 'PROV_S:description' in el:
                    return el['PROV_S:description']
                else:
                    return None
    else:
        if 'PROV_S:description' in data:
            return data['PROV_S:description']
        else:
            return None

def getType(e):
    guid = e.split("#")[-1]
    data = getData(e)["@graph"]
    context = None
    if "@context" in getData(e):
        context = getData(e)["@context"]
    elif "@context" in data:
        context = data['@context']
    else:
        print("ERROR: Could not find context from data graph!!!")
        print("{}".format())
    if type(data) == list:
        for el in data:
            if el['@id'][6:] == guid:
                if '@type' in el:
                    ns, _type = el['@type'].split(":")
                    return context[ns]+_type
                else:
                    return None
    else:
        if '@type' in data:
            ns, _type = data['@type'].split(":")
            return context[ns]+_type
        else:
            return None
    
def getIdentifier(e):
    guid = e.split("#")[-1]
    data = getData(e)["@graph"]
    if type(data) == list:
        for el in data:            
            if el['@id'][6:] == guid:
                if 'PROV_S:identifier' in el:
                    return el['PROV_S:identifier']
                else:
                    return None
    else:
        if 'PROV_S:identifier' in data:
            return data['PROV_S:identifier']
        else:
            return None
            
def getData(e):
    guid = e.split("#")[-1]
    data = None
    if not os.path.exists("cache/"+guid+".json"):
        cacheData(e)
    #This is to handle the case with multiprocssing where one thread has created the file but not yet populated with data
    while os.path.getsize("cache/"+guid+".json") ==0:
        time.sleep(0.1)
    with open("cache/"+guid+".json", "r") as dataFile:
        data = json.load(dataFile)
    if "@graph" not in data:
        data = {"@graph":data}
    return data
    
if __name__ == "__main__":
    semtk3.upload_owl("Model.owl", rc.connStringSource2, model_or_data=semtk3.SEMTK3_CONN_DATA, conn_index = 0)
