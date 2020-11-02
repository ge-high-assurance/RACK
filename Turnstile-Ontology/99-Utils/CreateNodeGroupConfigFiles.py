# Copyright (c) 2020, General Electric Company, Galois, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).
#

import os
import os.path
import json

csvHeader = "ID,comments,creator,jsonFile"

yamlHeader = '''# This file is intended to be used using the rack.py script found
# in RACK-Ontology/scripts/
#
# Script documentation is available in RACK-Ontology/scripts/README.md
data-graph: "http://rack001/data"
ingestion-steps:'''

sadlHeader ='''
/* Copyright (c) 2020, General Electric Company, Galois, Inc.
 *
 * All Rights Reserved
 *
 * This material is based upon work supported by the Defense Advanced Research
 * Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
 *
 * Any opinions, findings and conclusions or recommendations expressed in this
 * material are those of the author(s) and do not necessarily reflect the views
 * of the Defense Advanced Research Projects Agency (DARPA).
 */
 uri "http://Turnstile/DataQueries".

import "http://Turnstile/All".
'''

NodeTemplate = '''>**<<SparqlID>>** : <<NodeName>>

>><<Node Notes>>

'''
propertyTemplate = '''>>**<<SparqlID>>** : <<ValueType>>
    
>>><<Prop Notes>>

'''
edgeTemplate = '''>**<<SparqlID>>** - <<KeyName>> -> **<<SnodeSparqlIDs>>**

'''

def CreateNodeGroupIngestionTemplate(nodeGroupPath):

    # first load the existing json file to a local version for editing.
    # This way if an error is encountered the original node group will not be corrupted.
    nodeGroup = None
    with open(nodeGroupPath, "r") as nodeGroupJson:
        nodeGroup = json.load(nodeGroupJson)
    
    nodeReport = "" # working string for the node and property descriptions
    edgeReport = "" # working string for the edge descriptions
    optionalNodes = list() # This will record the Nodes that are the target of optional edge
    
    for node in nodeGroup["sNodeGroup"]["sNodeList"]:
        nodeReport += NodeTemplate.replace("<<SparqlID>>",node["SparqlID"].replace("?","")) \
                                  .replace("<<NodeName>>", node["NodeName"]) \
                                  .replace("<<Node Notes>>", "Node Notes...") 
        for p in node["propList"]:
            # This is improving the SparqlID for properties from the default "<type>_#" format by replacing with 
            #  the property type (KeyName), underscore and then the Node SparqlID. This should continue to be unique 
            #  as the Node SparqlID should be unique globally, and the property KeyName should be unique locally for the Node.
            p["SparqlID"] = "?"+p["KeyName"]+"_"+node["SparqlID"].replace("?","")
            nodeReport += propertyTemplate.replace("<<SparqlID>>",p["SparqlID"].replace("?","")) \
                                  .replace("<<ValueType>>", p["ValueType"]) \
                                  .replace("<<Prop Notes>>", "Prop Notes...") 
        for v in node["nodeList"]:
            i = 0
            while i < len(v["SnodeSparqlIDs"]):
                # if the edge is optional then add the target sparqlID to the list of optional Nodes,
                # This assumes that you only have one edge per node, or at least all edges are 
                # either required or optional together. 
                if v["OptionalMinus"][i]=="1":
                    optionalNodes.append(v["SnodeSparqlIDs"][i]) 
                edgeReport += edgeTemplate.replace("<<SparqlID>>",node["SparqlID"].replace("?","")) \
                                          .replace("<<KeyName>>",v["KeyName"]) \
                                          .replace("<<SnodeSparqlIDs>>",v["SnodeSparqlIDs"][i].replace("?",""))
                i+=1

    formatDescription = "# "+nodeGroupPath+"\n"
    formatDescription += "## Nodes\n\n"
    formatDescription +=nodeReport
    formatDescription += "## Edges\n\n"
    formatDescription +=edgeReport    
    formatDescription += "## CSV\n\n"
    formatDescription += "Column Name | Description |Optional\n"
    formatDescription += "------------|-------------|---\n"
    
    nodesList = list()
    columnsList = list() 
    csvHeader = ""

    for node in nodeGroup["sNodeGroup"]["sNodeList"]:
        propList = list()
        for prop in node["propList"]:
            if prop["isReturned"]:
                
                # If the len of the propList is one then it can be assumed that this is the primary key.
                primaryKey = len(node["propList"])==1
                
                # Hack to just assume that all identifiers are the primary key for look up purposes.
                if prop["UriRelationship"] == "http://arcos.rack/PROV-S#identifier":
                    primaryKey = True 
                    
                colId = "col_"+str(len(columnsList))
                
                # if the property is optional or it is on an optional node the CSV template table should indicate that the column is optional
                optional = "No"
                if prop["optMinus"] == 1 or node["SparqlID"] in optionalNodes:
                    optional = "Yes"
                
                
                colName = prop["SparqlID"].replace("?","")
                
                columnsList.append({"colId":colId,"colName":colName})
                csvHeader += colName+", "
                if primaryKey:
                    formatDescription += colName +"|"+ " primaryKey Key for "+node["SparqlID"].replace("?","")+ " | "+optional+"\n"
                    propList.append({
                            "URIRelation": prop["UriRelationship"],
                            "URILookup": [ node["SparqlID"] ],
                            "mapping": [
                                {
                                    "colId": colId,
                                    "transformList": [
                                        "trans_0"
                                    ]
                                }
                            ]
                        })
                else:
                    formatDescription += colName +"| | "+ optional+ "\n"
                    propList.append({
                            "URIRelation": prop["UriRelationship"],
                            "mapping": [
                                {
                                    "colId": colId,
                                    "transformList": [
                                        "trans_0"
                                    ]
                                }
                            ]
                        })
        nodesList.append({"sparqlID":node["SparqlID"],
                          "type":node["fullURIName"],
                          "URILookupMode": "createIfMissing",
                          "mapping": [],
                          "props": propList})
    importSpec = {
        "version": "1",
        "baseURI": "",
        "columns": columnsList,
        "dataValidator": [],
        "texts": [],
        "transforms": [
            {
                "transId": "trans_0",
                "name": "RemoveNulls",
                "transType": "replaceAll",
                "arg1": "null",
                "arg2": ""
            }
        ],
        "nodes": nodesList
    }
    nodeGroup["importSpec"] = importSpec
    
    # Since we got this far with out an exceptions it is assumed that all the 
    #  updates worked so write the node group back to the original json file
    #  and write out the template csv and the md description 
    with open(nodeGroupPath.replace(".json",".json"), "w") as outFile:
        json.dump(nodeGroup, outFile, indent='\t')
    with open(nodeGroupPath.replace(".json",".csv").replace("NodeGroups","CsvTemplates"), "w") as outFile:
        outFile.write(csvHeader)
    with open(nodeGroupPath.replace(".json",".md").replace("NodeGroups","CsvSpecs"), "w") as outFile:
        outFile.write(formatDescription)
    
def getSparqlFromNodeGroup(nodeGroupPath):
    
    print("  Proccessing :" + nodeGroupPath)
    CreateNodeGroupIngestionTemplate(nodeGroupPath)


    nodeGroup = None
    sparqlIds = list()

    propStrings = {}
    nodeString = {}
    whereString = ""
    #Create a Sparql string from a node group json
    with open(nodeGroupPath, "r") as nodeGroupJson:
        nodeGroup = json.load(nodeGroupJson)
    for node in nodeGroup["sNodeGroup"]["sNodeList"]:
        propStrings[node["SparqlID"]] = list()
        if(node["isReturned"]):
            sparqlIds.append(node["SparqlID"])
        
        nodeString[node["SparqlID"]] = node["SparqlID"]+" a <"+ node["fullURIName"]+">"
        for prop in node["propList"]:
            if prop["isReturned"]:
                sparqlIds.append(prop["SparqlID"]) 
            if prop["optMinus"] == 0:
                propStrings[node["SparqlID"]].append(node["SparqlID"] + " <" + prop["UriRelationship"] + "> " + prop["SparqlID"])
            else: 
                propStrings[node["SparqlID"]].append("optional{ "+node["SparqlID"] + " <" + prop["UriRelationship"] + "> " + prop["SparqlID"]+"}")
        
    for node in nodeGroup["sNodeGroup"]["sNodeList"]:
        if len(node["nodeList"]) >0:
            whereString += "    " + nodeString[node["SparqlID"]] + " . \n"
            for p in propStrings[node["SparqlID"]]:
                whereString += "    " + p + " . \n"
            
            for e in node["nodeList"]:
                i=0
                while i<len(e["SnodeSparqlIDs"]):
                    if e["OptionalMinus"][i] == "1":
                        whereString+= "    optional{" +"\n\n"
                    
                    whereString += "    "+node["SparqlID"]+" <"+e["UriConnectBy"]+"> "+e["SnodeSparqlIDs"][i] + " .\n"
                    whereString += "        "+nodeString[e["SnodeSparqlIDs"][i]] + " . \n"
                    for p in propStrings[e["SnodeSparqlIDs"][i]]:
                        whereString += "        " + p+ " . \n"
                    if e["OptionalMinus"][i] == "1":
                        whereString+= "    }\n"
                    i+=1
             

    selectString = "select distinct"
    for i in sparqlIds:
        selectString +=  " "+i 
    
    return selectString+"\n"+"where {\n" +whereString +"\n}"   
        
csvFiles = list()
with open("NodeGroups/store_data.csv", "w") as outfile:
    outfile.write(csvHeader+"\n")
    for f in os.listdir(os.path.curdir+"/NodeGroups"):
        
        if f.upper().endswith(".JSON"):
            id = f.split(".")[0]
            comments = "Node group to "+ id.split("-")[0].lower() +" "+id.split("-")[1] +" data for the RACK-in-a-Box Turnstile example."
            outfile.write(id +","+ comments+","+"Turnstile"+ ","+ f+"\n")
            csvFiles.append(id)

with open("Data/Model.yaml", "w") as outfile:
    outfile.write(yamlHeader+"\n")
    for c in csvFiles:
        outfile.write('- {nodegroup: "'+c+'", csv: "'+c.split("-")[1]+'.csv"}\n')

with open("DataQueries.sadl", "w") as outfile:
    outfile.write(sadlHeader+"\n")
    for c in csvFiles:

        if c.startswith("Ingest"):
            outfile.write('Write: data {Ask '+c.split("-")[1]+'Csv:"\n\n')
            outfile.write(getSparqlFromNodeGroup("NodeGroups/"+c+".json"))
            outfile.write('".} to "99-Utils/Data/'+c.split("-")[1]+'.csv".\n\n')
    
            