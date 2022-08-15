#!/usr/bin/env python3
import os
import json
from difflib import SequenceMatcher
import csv
from colorama import Fore, Back, Style
import semtk3

DEBUG = False
REPORT_TEMPLATE = """{}{}:
      Primary Identifier : {}
    Secondary Identifier : {}
       Description Ratio : {}"""
def Debug(*args):
    if DEBUG:
        print(*args)

    
class RequirementResolution:
    requirementData = {}
    resolutionData  = {}

    def __init__(self):
        # Loading raw requirement data
        allDescriptions = ""
        with open(os.path.join(".", "rawRequirements.csv")) as csvFile:
            for row in csv.DictReader(csvFile):
                if row["REQUIREMENT"] not in self.requirementData:
                    self.requirementData[row["REQUIREMENT"]] = {}
                    self.requirementData[row["REQUIREMENT"]]["identifier"]=row["identifier"]
                    self.requirementData[row["REQUIREMENT"]]["description"]=row["description"]
                    allDescriptions += row["identifier"]+"\n"
                    self.requirementData[row["REQUIREMENT"]]["REQUIREMENT_type"]=row["REQUIREMENT_type"]
                    self.requirementData[row["REQUIREMENT"]]["dataInsertedBy_identifier"]=list()
                self.requirementData[row["REQUIREMENT"]]["dataInsertedBy_identifier"].append(row["dataInsertedBy_identifier"])
        # Loading existing resolution data
        self.bagOfWords ={}
        for w in self.cleanString(allDescriptions).split(" "):
            if w not in self.bagOfWords:
                self.bagOfWords[w] = 0
            self.bagOfWords[w] += 1
            
        self.identifierStopWords = list()
        for w in sorted(self.bagOfWords, key=self.bagOfWords.get, reverse=True):
            #print(w, self.bagOfWords[w])
            if self.bagOfWords[w] / len(self.requirementData) > 0.05 and not w.isnumeric():                
                self.identifierStopWords.append(w)
        
        print(self.identifierStopWords)    
             
        with open(os.path.join(".","resolutionData.csv")) as csvFile:
            for row in csv.DictReader(csvFile):
                if row["primaryEntity"] not in self.resolutionData:
                    self.resolutionData[row["primaryEntity"]] = list()    
                self.resolutionData[row["primaryEntity"]].append([row["EntityResolution_type"],row["secondaryEntity"]])
        self.resolutionData ={}
        
    def cleanString(self, string):
        cleanString = ""
        for c in string:
            if c.isalnum():
                cleanString+=c
            else:
                cleanString += " "
        while cleanString.find("  ")!=-1:
            cleanString = cleanString.replace("  "," ")
        return cleanString.upper().rstrip(" ").lstrip(" ")
        
    def deepCleanString(self, string):
        string =self.cleanString(string)
        NewString = ""
        for t in string.split(" "):
            if t not in self.identifierStopWords:
                NewString+= t + " "
        
        return NewString.lstrip()
    
    ######################################################################
    # Function check to see if the descriptions have discriminator words
    ######################################################################    
    def checkForDiscriminators(self, reqP, reqS)
        descriptionP = self.cleanString(self.requirementData[reqP]["description"]).rstrip()
        descriptionS = self.cleanString(self.requirementData[reqS]["description"]).rstrip()
                
        added = list()
        removed = list()
        matched = list()
        stopped = list()
        for p in descriptionP.split(" "):
            if p not in descriptionS.split(" "):
                if p not in self.identifierStopWords:
                    removed.append(p)
                else:
                    stopped.append(p)
            else:
                if p not in self.identifierStopWords:
                    matched.append(p)
                else:
                    stopped.append(p)
        for s in descriptionS.split(" "):
            if s not in descriptionP.split(" "):
                if p not in self.identifierStopWords:
                    added.append(s)
                else:
                    stopped.append(s)
        
        
        discriminators == [["GPS1","GPS2"],["IRS1","IRS2"]]
        
        
        return 
    ######################################################################
    # Function does a final categorization of potential matches
    ######################################################################    
    def FinalCategorization(self, reqP, reqS):
        identifierP = self.deepCleanString(self.requirementData[reqP]["identifier"])
        identifierS = self.deepCleanString(self.requirementData[reqS]["identifier"])
        descriptionP = self.cleanString(self.requirementData[reqP]["description"])
        descriptionS = self.cleanString(self.requirementData[reqS]["description"])
        print(identifierP,"<->",identifierS)
        if identifierP == identifierS:
            print(REPORT_TEMPLATE.format(Fore.GREEN, "AssumedSameAs", self.requirementData[reqP]["identifier"], self.requirementData[reqS]["identifier"],"N/A"))
            if reqP not in self.resolutionData:
                self.resolutionData[reqP] = list()
            self.resolutionData[reqP].append(["AssumedSameAs",reqS])
            return
            
        simRatio = 0.0
        if descriptionP !="" and descriptionS != "":
            matcher = SequenceMatcher(None, descriptionP, descriptionS)
            simRatio = matcher.ratio()
            
        if simRatio > 0.9:
            print(REPORT_TEMPLATE.format(Fore.GREEN, "AssumedSameAs", self.requirementData[reqP]["identifier"], self.requirementData[reqS]["identifier"],simRatio))
            
            if reqP not in self.resolutionData:
                self.resolutionData[reqP] = list()
            self.resolutionData[reqP].append(["AssumedSameAs",reqS])
            
        elif simRatio < 0.5 and simRatio!=0.0:
            print(REPORT_TEMPLATE.format(Fore.RED, "AssumedDifferent", self.requirementData[reqP]["identifier"], self.requirementData[reqS]["identifier"],simRatio))
            
            if reqP not in self.resolutionData:
                self.resolutionData[reqP] = list()
            self.resolutionData[reqP].append(["AssumedDifferent",reqS])
        else:
            print(REPORT_TEMPLATE.format(Fore.YELLOW, "PossibleSameAs", self.requirementData[reqP]["identifier"], self.requirementData[reqS]["identifier"],simRatio))
            
            if reqP not in self.resolutionData:
                self.resolutionData[reqP] = list()
            self.resolutionData[reqP].append(["PossibleSameAs",reqS])
    ######################################################################
    # Function returns True if two requirements are part of the same data ingestion
    ######################################################################               
    def checkDataInsertedBy(self, reqP, reqS):
        dataInsertedByP = self.requirementData[reqP]["dataInsertedBy_identifier"]
        dataInsertedByS = self.requirementData[reqS]["dataInsertedBy_identifier"]
        for i in dataInsertedByP:
            if i in dataInsertedByS:
                return True
        return False

    ######################################################################
    # Function returns True if two requirements have disimilar identifiers
    ######################################################################   
    def checkIdentifier(self, reqP, reqS):
        identifierP = self.cleanString(self.requirementData[reqP]["identifier"]).rstrip()
        identifierS = self.cleanString(self.requirementData[reqS]["identifier"]).rstrip()
        
        #Requirements have the same identifier following cleaning so they are a match
        if identifierP == identifierS:
            return False
        matcher = SequenceMatcher(None, identifierP, identifierS)
        
        #Sequence matcher ratio is less than 0.8 so it can be assumed that they disimilar
        ratio = matcher.real_quick_ratio() 
        if ratio< 0.8:
            return True
        
        added = list()
        removed = list()
        matched = list()
        stopped = list()
        for p in identifierP.split(" "):
            if p not in identifierS.split(" "):
                if p not in self.identifierStopWords:
                    removed.append(p)
                else:
                    stopped.append(p)
            else:
                if p not in self.identifierStopWords:
                    matched.append(p)
                else:
                    stopped.append(p)
        for s in identifierS.split(" "):
            if s not in identifierP.split(" "):
                if p not in self.identifierStopWords:
                    added.append(s)
                else:
                    stopped.append(s)
        # Matched minus stop words
        if len(added) == 0  and len(removed) == 0:
            return False
        return True
      
    ######################################################################
    # Function returns True if two requirements are not of compatible types
    ######################################################################    
    def checkForValidTypes(self, reqP, reqS):
        typeP = self.requirementData[reqP]["REQUIREMENT_type"]
        typeS = self.requirementData[reqS]["REQUIREMENT_type"]
        if typeP == typeS and reqP>reqS:
            return False
        elif typeP != typeS:
            if typeS == "http://arcos.rack/REQUIREMENTS#REQUIREMENT":
                return False
            else:
                return True
                
    ######################################################################
    # Function returns True if two requirements already have resolution data
    ###################################################################### 
    def checkForResolutionData(self, reqP, reqS):
        return False 

    def findPossibleSameAs(self):
        i = 0
        for reqP in self.requirementData:
            i+=1
            print(Style.RESET_ALL)
            print("===========================")
            print("{} {}/{} :: Found {}".format(self.requirementData[reqP]["identifier"], i, len(self.requirementData), len(self.resolutionData)))
            for reqS in self.requirementData:
                # Check 
                if reqP == reqS: # Same entitiy so move to the next
                    Debug(Fore.YELLOW, "Requirements are the Same")
                elif self.checkForResolutionData(reqP, reqS):
                    Debug(Fore.YELLOW, "Requirements already have Resolution Data")
                elif self.checkForValidTypes(reqP, reqS):
                    Debug(Fore.YELLOW, "Requirements have in compatible types.")
                elif self.checkDataInsertedBy(reqP, reqS):
                    Debug(Fore.YELLOW, "Requirements are from the same data ingestion")
                elif self.checkIdentifier(reqP, reqS):
                    Debug(Fore.YELLOW, "Requirements have disimiliar identifiers")
                else:
                    self.FinalCategorization(reqP, reqS)
                    
    def loadResolutionData(self):
        with open(os.path.join(".", "resolutionData.json"), "r") as jsonFile:
            self.resolutionData = json.load(jsonFile)   
            
            
    def writeResolutionData(self):
        # Loading raw requirement data
        with open(os.path.join(".", "resolutionData.json"), "w") as jsonFile:
            json_object = json.dumps(self.resolutionData, indent = 4)
            jsonFile.write(json_object)
        with open(os.path.join(".", "resolutionData.csv"), "w") as csvFile:
            writer = csv.DictWriter(csvFile,fieldnames=["EntityResolution_type","primaryEntity","secondaryEntity","primaryIdentifier","secondaryIdentifier","primaryDescription","secondaryDescription"])
            writer.writeheader()
            for req in self.resolutionData:
                #self.resolutionData[reqP].append(["AssumedAssumedSameAs",reqS])
                for res in self.resolutionData[req]:
                    reqP = self.requirementData[req]
                    reqS = self.requirementData[res[1]]
                    rowDict = {"EntityResolution_type":res[0],
                               "primaryEntity":req,
                               "secondaryEntity":res[1],
                               "primaryIdentifier":reqP["identifier"],
                               "secondaryIdentifier":reqS["identifier"],
                               "primaryDescription":reqP["description"],
                               "secondaryDescription":reqS["description"]}
                    writer.writerow(rowDict)

    def resolveGraph(self):
        print(len(self.requirementData))
        conn_str = '''{"name":"RACK local fuseki Apache Phase 2","domain":"","enableOwlImports":false,"model":[{"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/model"}],"data":[{"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/data"},{"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/mitre-cwe"},{"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/nist-800-53"}]}
{"name":"RACK","domain":"","enableOwlImports":false,"model":[{"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/model"}],"data":[{"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/data"},{"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/turnstiledata"}]}
{"name":"RACK1","domain":"","enableOwlImports":false,"model":[{"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/model"}],"data":[{"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/data"}]}'''
        semtk3.set_connection_override(conn_str)
        for req in self.resolutionData:
            for res in self.resolutionData[req]:
                if res[0] == "AssumedSameAs":
                    primaryEntity = req
                    secondaryEntity = res[1]
                    print(Fore.GREEN, "semtk3.combine_entities(",self.requirementData[secondaryEntity]["REQUIREMENT_type"],",",secondaryEntity,",",primaryEntity,")",Style.RESET_ALL)
                    semtk3.combine_entities(self.requirementData[secondaryEntity]["REQUIREMENT_type"], primaryEntity, secondaryEntity,None, None)

if __name__ =="__main__":
    T = RequirementResolution()
    T.findPossibleSameAs()

    #T.loadResolutionData()
    
    T.writeResolutionData()
    #T.resolveGraph()
    
