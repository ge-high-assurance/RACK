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
###############################  Read PDF file #####################################
# exec(open('TurnstileIngestion_Requirements.py').read())
import PyPDF2
from PyPDF2 import PdfFileReader
#from PIL import Image
import operator, functools
from itertools import product 

#create file object variables
#opening method will be rb
pdf1='RequirementsDocument/Version1.pdf'
pdf2='RequirementsDocument/Version2.pdf'
def pdffileobj(pdf):
    return open(pdf,'rb')
 #create reader variable that will read the pdffileobj
def pdfreader(pdf):
    return PyPDF2.PdfFileReader(pdffileobj(pdf))
 #This will store the number of pages of this pdf file
#x=pdfreader.numPages
 #create a variable that will select the selected number of pages
def pageobj(x,pdf): 
    return pdfreader(pdf).getPage(x)
#create text variable which will store all text datafrom pdf file
def text(x,pdf):
    return pageobj(x,pdf).extractText()
############################### Main list information #################################### 
def cleanPage (page,pdf):
    return list(filter(lambda x:x!=" ",text(page,pdf).split("\n")))
#numPages = pdfreader.numPages    
 
def composeCleanPages(pdf):
    listOfCleanPages= list(map(lambda x: cleanPage(x,pdf),list(range(0,pdfreader(pdf).numPages))))
    return list(functools.reduce(lambda a,b:a+b,listOfCleanPages,[]))
##list that have all the claen pages of the pdf file    
mainList1 = composeCleanPages (pdf1)
mainList2 = composeCleanPages (pdf2)
#######################################Keys for sections,Requirements and requirements fields#############################
# requirements keys 1) ,2) etc
def numReqKeys_Aux (x):
    numList = x.split(")")
    return  len(numList)==2 and str.isdigit(numList[0])
#sections keys 1., 2. , 3. etc 
def sectionsKeys_Aux(x):
    listCandidate = x.split(".")
    return len(listCandidate)==2 and str.isdigit(listCandidate[0]) and listCandidate[1]==""
#  req fields a. , b. etc
def reqFieldsKeys_aux (x):
    alphabet =list(map(lambda x: chr(x) , list(range(97,123))))
    listCandidate = x.split(".")
    return len(listCandidate)==2 and listCandidate[0] in alphabet and listCandidate[1]==""
 
def getKeysFromMainList (boolFunc,mList):
    return list(filter(boolFunc,mList))
## get keys section 1. 2. etc and cleaning using uppercase section title
def getKeysSections(mList):
    keys= getKeysFromMainList(sectionsKeys_Aux,mList)
    checkUpperCase = lambda x:mList[mList.index(x)+1].isupper() 
    return list(filter(checkUpperCase ,keys))
## get keys for requirements 1), 2), etc
def keysReq(reqList): 
    return getKeysFromMainList(numReqKeys_Aux,reqList)   
## get keys for requirements fields a. , b. , etc
def reqFieldsKeys(req):
    return getKeysFromMainList(reqFieldsKeys_aux, req)
 
def getSubList(key1,key2,mList):
   return mList[mList.index(key1):mList.index(key2)] 
 
#####################################
##Get sections
def getKeysSection(mList): 
    return list(zip(getKeysSections(mList),getKeysSections(mList)[1:]))
def lastSection(mList):
    return mList[mList.index(getKeysSections(mList)[len(getKeysSections(mList))-1] ):len(mList)]
 
def getSections(mainList): 
    return list(map(lambda x:getSubList(x[0],x[1],mainList),getKeysSection(mainList)))+[lastSection(mainList)]
########################################################
##Get Requirements
def clean(l):
    return list(filter(lambda x: x!=None,l))
########################################################################
#search and find System, High level and Low level requirements
def listToString(l):
    return functools.reduce(lambda a,b: a+b,l,"").replace(" ","")
def cleanSpace(l):
    return list(filter(lambda x: x!=" ",l))          
def findReq_aux(keySection,listReq):
    if "1)" in listReq and keySection in listReq: 
     return (listToString(getSubList(keySection,"1)",listReq)[1:]),listReq) 
    else:list()
def findReq(mainList,keySection):
    return clean(list(map(lambda x: findReq_aux(keySection,x),getSections(mainList))))
 
def listOfReqs_aux(mainList):
    return list(filter(lambda x:clean(x),list(map(lambda x: findReq(mainList,x),getKeysSections(mainList)))))
def listOfReqs(mainList) :
    return  [item for sublist in listOfReqs_aux(mainList) for item in sublist] 
####################################################
##dictionary of "SYSTEMREQUIREMENTS","HIGHLEVELREQUIREMENTS","LOWLEVELREQUIREMENTS"
def dictReqs(mainList): 
    return dict(listOfReqs(mainList))
def getKeysReq(listReq):
    return list(zip(keysReq(listReq),keysReq(listReq)[1:]))
 
def lastsReq(listReq):
    keys = keysReq(listReq)
    return listReq[listReq.index(keys[len(keys)-1]):]
 
def getReq(listReq):
    return list(map(lambda x: getSubList(x[0],x[1],listReq),getKeysReq(listReq)))+[lastsReq(listReq)]
 
def sys_Req(mainList):
    return getReq (dictReqs(mainList)["SYSTEMREQUIREMENTS"])
def hl_Req(mainList):
    return getReq(dictReqs(mainList)["HIGHLEVELREQUIREMENTS"])
def ll_Req(mainList):
    return getReq(dictReqs(mainList)["LOWLEVELREQUIREMENTS"]) 
#############################
def toString(l):
    return functools.reduce(lambda a,b: a+b,l,"")
def getKeysField(req):
    return list(zip(reqFieldsKeys(req),reqFieldsKeys(req)[1:])) 
def getReqFieldLast(req):
    keys= reqFieldsKeys(req)
    return req[req.index(keys[len(keys)-1]):]
def getReqFieldFirst(req):
    return req[:req.index("a.")]    
def getReqField(Req):
    firstField = [toString(getReqFieldFirst(Req))]
    middle = list(map(lambda x:[toString(x)],list(map(lambda x: getSubList(x[0],x[1],Req),getKeysField(Req))) ))
    lastField =[toString(getReqFieldLast(Req))]
    return [firstField]+ middle+ [lastField]
    
def getReqs(reqType):
    listReqAux = list(range(0,len(reqType)))
    return list(map(lambda x: getReqField(reqType[x]),listReqAux))
 
def reqDict_aux(req):
    return dict(list(map(lambda x: (listToString((x[0].split(":",1)[0]))[2:],x[0].split(":",1)[1]),req)))
 
def reqsDict(reqs):
    return list(map(lambda x: reqDict_aux(x),getReqs(reqs)))
def checkKeyExist (req,key): 
    if key in req.keys(): return req[key]
    else : return ""
#only for requirementidentification key
def keybiggerThan10 (k10,k1,req):
    if k10 in req.keys():return k10
    else: return k1    
####################
###Get revision history table
def table(pdf):
    return cleanPage(1,pdf)[:cleanPage(1,pdf).index("INTRODUCTION")]
def numColon_aux(pdf,ele):
    l = ele.split(":") 
    if len(l)==2 and l[1]=="" and str.isdigit(l[0]):return l[0]+":"
    else: return None
def numColon(pdf):
    return list(filter(lambda x: x!=None,list(map(lambda x: numColon_aux(pdf,x),table(pdf)))))
def listNumColon(pdf):
    return list(zip(numColon(pdf),numColon(pdf)[1:]))+[(numColon(pdf)[len(numColon(pdf))-1],"1.") ] 
def getReqModi(key,pdf):
    l= toString(table(pdf)[table(pdf).index(key[0]):table(pdf).index(key[1])]) 
    if "High-level" in l: return l.split("High-level",1)[0].replace(key[0],"")
    if "Low-level" in l: return l.split("Low-level",1)[0].replace(key[0],"")
    if "Sys-level" in l: return l.split("Sys-level",1)[0].replace(key[0],"")
    else: return None
    #else: return None  
def reqModified(pdf): 
    return list(map(lambda x: getReqModi(x,pdf),listNumColon(pdf)))
 
# def getDate(date,pdf):
#       l= date.split("/")
#       if len(l)==3: return  [x for x,z in enumerate(table(pdf)) if z == date] 
#       else : None
# def indexDate_aux(pdf):
#     return list(filter(lambda x: x!=None,list(map(lambda x: getDate(x,pdf),table(pdf))))) 
    
# def indexDate(pdf):
#     return sorted(set([item for sublist in indexDate_aux(pdf) for item in sublist]))      
# def getReqModifiedFirst(index1,pdf):
#     return table(pdf)[table(pdf).index("DESCRIPTION")+1:index1]
# def getReqModifiedRest(index1,index2,pdf):
#     return table(pdf)[index1+1:index2]
# def indexZip(index):
#     return list(zip(index,index[1:]))     
# def reqModified_aux(pdf):
#     return list(map(lambda x: getReqModifiedRest(x[0],x[1],pdf),indexZip(indexDate(pdf)))) + [getReqModifiedFirst(indexDate(pdf)[0],pdf)]
 
#def reqModified(pdf):
#    if indexDate(pdf) ==[]: return None
#    else: return list(map(lambda x: listToString(x) ,reqModified_aux(pdf))) 
###########################################Version update
# 
l="Requirementidentification"
l1=")Requirementidentification"
def Nospace(l):
    return list(map(lambda x: x.replace(" ",""),l))
def reqsIds(reqType,mainList):
    return list(Nospace(map(lambda x: x[keybiggerThan10(l1,l,x)],reqsDict(reqType(mainList)))))
    
def isInList(a,l):
    if a in l: return a
    else: return None
def reqTypeModified (reqType,mainList,pdf):
    reqTypeList = reqsIds(reqType,mainList)
    reqModifieList= reqModified(pdf)
    return list(clean(map(lambda x: isInList(x,reqTypeList),reqModifieList)))
sysReqsModified_Ids = reqTypeModified(sys_Req,mainList1,pdf2)
hlReqsModified_Ids = reqTypeModified(hl_Req,mainList1,pdf2) 
llReqsModified_Ids = reqTypeModified(ll_Req,mainList1,pdf2) 
 
def reqUpdate(reqModi,reqsDictList):
    return list(filter(lambda x: x[keybiggerThan10(l1,l,x)].replace(" ","")==reqModi,reqsDictList))
def reqsUpdate(reqsDictList,reqsModified):
    return list(map(lambda x: reqUpdate(x, reqsDictList),reqsModified)) 
def flatten (listoflist):
    return [item for sublist in listoflist for item in sublist]
    
# hlReqsModified=flatten(reqsUpdate(reqsDict(hl_Req(mainList2)),hlReqsModified_Ids))
# llReqsModified=flatten(reqsUpdate(makeDicts(mainListReq(mainList2,ll_Req)),llReqsModified_Ids))
# sysReqsModified=flatten(reqsUpdate(reqsDict(sys_Req(mainList2)),sysReqsModified_Ids)) 
 
########################################
def aux(req):
    keys =["Description", "Requirementidentification",")Requirementidentification"]
    if req[0] in keys: return req
    else: return (req[0],req[1].split(","))
def splitFieldsReq(req):
     return list(map(lambda x: aux(x),req))  
def key_value(keyValue):
    if type(keyValue[1])==list: return list(map(lambda x:(keyValue[0],x),keyValue[1]))
    else : return [keyValue]
def reqSplit(req):
    return list(map(lambda x: key_value(x),req))    
a= list(reqsDict(ll_Req(mainList1))[4].items()) 
def reqclean_aux(req):
    return list(map(lambda x: (list(map(lambda y: y,x))),reqSplit(splitFieldsReq(req)))) #str(y)
def listReqTuples(mainList,reqType):
    return list(map(lambda x: list(x.items()),list(reqsDict(reqType(mainList)))))
 
def reqClean(req):
    return list(product(*reqclean_aux(req))) 
 
def getNoSpace_aux(req):
    return list(map(lambda x: (x[0],x[1].strip()),req))
def getNoSpace(Reqs):
    return list(map(lambda x: getNoSpace_aux(x),Reqs))
def mainListReq (mainList,reqType):   
    return getNoSpace(flatten(list(map(lambda x: reqClean(x),listReqTuples(mainList,reqType))))) 
##cleaning ")Requirementsidentification"
def changeKey_aux(req,key):
    if key in req[0][0]: return [(key,req[0][1])]+req[req.index(req[1]):]
    else : return req
def changeKey(reqs,key):
    return list(map(lambda req: changeKey_aux(req,key),reqs)) 
 ##maing dictionary of reqs and cleaning keys "Requirementsidentification"         
def makeDicts (reqs):
    return list(map(lambda x: dict(x),changeKey(reqs,"Requirementidentification"))) 
#####
hlReqsModified=flatten(reqsUpdate(makeDicts(mainListReq(mainList2,hl_Req)),hlReqsModified_Ids))
llReqsModified=flatten(reqsUpdate(makeDicts(mainListReq(mainList2,ll_Req)),llReqsModified_Ids))
sysReqsModified=flatten(reqsUpdate(makeDicts(mainListReq(mainList2,sys_Req)),sysReqsModified_Ids)) 
##Reqs from version 1
hl= makeDicts(mainListReq(mainList1,hl_Req)) 
ll= makeDicts(mainListReq(mainList1,ll_Req))
sys= makeDicts(mainListReq(mainList1,sys_Req))
#### update requirement changed in version2 in dictionaries sys,hl,ll
def updateVerReq_aux(req,key,reqModiId):
   if req[key] in reqModiId : return req.update({key:req[key]+":v1"}) 
   else :req 
def updateVerReq (reqs,reqModified):
    return list(map(lambda x: updateVerReq_aux(x,"Requirementidentification",reqModified),reqs)) 
def updateVersionValue_aux(req,key,reqModified):
    if checkKeyExist(req,key) in reqModified: return req.update({key:req[key]+":v1"})
    else :req
def updateVersionValue(Oldreqs,key,reqModified):
    return list(map(lambda x: updateVersionValue_aux(x,key,reqModified),Oldreqs))           
updateVerReq(hl,hlReqsModified_Ids)
updateVerReq(ll,llReqsModified_Ids)
updateVerReq(sys,sysReqsModified_Ids) 
updateVersionValue(ll,"Satisfies",hlReqsModified_Ids)
### new requirements added in version 2
def newReqs_aux(reqType,newList,oldList):
    return list(set(reqsIds(reqType,newList))-set(reqsIds(reqType,oldList)))
def newReqs(reqType,newList,oldList):
    newListDict= makeDicts(mainListReq(newList,reqType))
    return list(filter(lambda x: x["Requirementidentification"] in newReqs_aux(reqType,newList,oldList) ,newListDict))
newSysReq= newReqs(sys_Req,mainList2,mainList1)
newHLReq= newReqs(hl_Req,mainList2,mainList1)
newLLReq= newReqs(ll_Req,mainList2,mainList1)    
###########################################Get list of fields separated by ","
#def getlistOfFields(reqsDict,key):
#    clean = lambda x: filter(lambda y:y[0]!=[""],x)
#    return list(clean(list(map(lambda x: (checkKeyExist(x,key).split(","),x),reqsDict))))
#getlistOfFields(reqsDict(ll_Req(mainList1)),"Satisfies")
#list(product([1],[2],[3],["a","b","c"],[10,20]))
#list(product(["(1,2)"],["(2,3)"],["(3,a)","(3,b)"],["(5,a)"],["(4,1)","(4,2)","(4,3)"]))
#def splitDictBylistofFields_aux(fieldDict):
#    return list(map(lambda x: (x,fieldDict[1]),fieldDict[0])) 
#def splitDictBylistofFields(reqsDict):
#    return  list(map(lambda x: splitDictBylistofFields_aux(x),reqsDict)) 
#Gives the requirements with the list of fields 
#def reqTypeListOfFields(reqType,mainList,key):
#    return flatten(splitDictBylistofFields(getlistOfFields(reqsDict(reqType(mainList)),key)))
#Requirements updated the "Satisfies" with the  new hl requirement version 
#reqTypeListOfFields(ll_Req,mainList1,"Satisfies") 
#def newDict(req,key):
#    return list(map(lambda x: {l:x[1][keybiggerThan10(l1,l,x[1])],"Satisfies": x[0]}, req))
#satisfiesToBeSplited = reqTypeListOfFields(ll_Req,mainList1,"Satisfies")      
#reqSatisfiesSplit(satisfiesToBeSplited)              
##########################################
from Evidence import createEvidenceFile, createCDR
import Evidence.Add as Add
import shutil
import os.path
input_path = "RequirementsDocument"
def CreateCdrs():
################################################
    #    System Requirements
################################################
    createEvidenceFile(ingestionTitle="TurnstileIngestion-System Requirements", ingestionDescription="Ingestion of Turnstile System Requirements using Scraping Tool Kit")
    
    def sysReqToIngestAux (req,version,fileSource):
        l1="Requirementidentification"
        l2="Description"
        l3="Governs"
        return [Add.GE.SystemRequirement(identifier = req[l1]+version, 
                                               governs_identifier = checkKeyExist(req,l3),
                                               description = req[l2],
                                               definedIn_identifier=fileSource),
                Add.SYSTEM.SYSTEM(identifier = checkKeyExist(req,l3))]                                
    #sys has an update using the version 2
    list(map(lambda x: sysReqToIngestAux(x,"","SYS Doc:v1"),sys))
    #list(map(lambda x: sysReqToIngestAux(x),reqsDict(sys_Req(mainList1))))
    #list(map(lambda x: hlReqToIngestAux(x,":v2"),sysReqsModified))
    ##system  level Req version update
    list(map(lambda x: sysReqToIngestAux(x,":v2","SYS Doc:v1"),sysReqsModified))
    ##New Req version 2
    list(map(lambda x: sysReqToIngestAux(x,"","SYS Doc:v1"), newSysReq))
    #------------ Document Files ------------    
    Add.FILE.FILE(identifier="SYS Doc:v1",entityURL="https://github.com/ge-high-assurance/RACK/blob/master/Turnstile-Example/RequirementsDocument/Sys_Req.txt")
    createCDR("http://rack001/turnstiledata")
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemRequirements"))
 
################################################
    #    High-level Requirements Version 1
################################################        
    createEvidenceFile(ingestionTitle="TurnstileIngestion-High Level Requirements", ingestionDescription="Manual ingestion of Turnstile High Level Requirements")
    
    def hlReqToIngestAux (req,reqVersion,revision,version,fileSource):
        l1="Requirementidentification"
        l2="Description"
        l3="Governs"
        l4="Satisfies"
        l5="Mitigates"
        l6="wasGeneratedBy"
        return [Add.GE.HighLevelRequirement(identifier = req[l1]+reqVersion, 
                                                  governs_identifier = checkKeyExist(req,l3),
                                                  description = req[l2],
                                                  satisfies_identifier = checkKeyExist(req,l4),
                                                  mitigates_identifier = checkKeyExist(req,l5),
                                                  wasGeneratedBy_identifier= checkKeyExist(req,l6),
                                                  wasRevisionOf_identifier=revision+version,
                                                  definedIn_identifier=fileSource),
               Add.GE.SystemRequirement(identifier=checkKeyExist(req,l4)),
               Add.HAZARD.HAZARD(identifier=checkKeyExist(req,l5)),
               Add.GE.SystemComponent(identifier=checkKeyExist(req,l3))]                                  
 
    
    list(map(lambda x: hlReqToIngestAux(x,"","","","HLR Doc:v1"),hl))
    #list(map(lambda x: hlReqToIngestAux(x,":v1","",""),reqsDict(hl_Req(mainList1))))
    ##High level Req version update
    list(map(lambda x: hlReqToIngestAux(x,":v2",x[keybiggerThan10(l1,l,x)],":v1","HLR Doc:v2"),hlReqsModified))
    ##New Req version 2
    list(map(lambda x: hlReqToIngestAux(x,"","","","HLR Doc:v2"), newHLReq))
    #------------ Document Files ------------    
    Add.FILE.FILE(identifier="HLR Doc:v1",entityURL="file://{{BASEDIR}}/RequirementsDocument/Version1.pdf")
    Add.FILE.FILE(identifier="HLR Doc:v2",entityURL="file://{{BASEDIR}}/RequirementsDocument/Version2.pdf")
    #------------ 125569538 ------------
    Add.GE.Engineer(identifier = "125569538",
                 title = "Doe, John",
                 emailAddress = "john.doe@ge.com",
                 employedBy_identifier = "General_Electric")
    Add.AGENTS.ORGANIZATION(identifier = "General_Electric")
    
    #------------ HlrDev1 ------------
    Add.GE.SoftwareRequirementsDefinition(identifier = "HlrDev1",
                                       endedAtTime = "2020-07-15 10:56:38",
                                       wasAssociatedWith_identifier = "125569538",
                                       referenced_identifier = "RQ-STD:v1")
    Add.DOCUMENT.DOCUMENT(identifier = "RQ-STD:v1")
    #------------ inflowEvent ------------                                
    Add.GE.DataDictionary(identifier = "inflowEvent",
                       description = "Signal indicating that a person has passed through the ingate",
                       wasGeneratedBy_identifier = "HlrDev1")                       
    Add.GE.DataDictionary(identifier = "inflowEvent",
                       providedBy_identifier = "inflow")
    Add.GE.SystemInterfaceDefinition (identifier="inflow")
    Add.GE.DataDictionary(identifier = "inflowEvent",
                       consumedBy_identifier = "HLR-1:v1")
                 
    #------------ outflowEvent ------------  
    Add.GE.DataDictionary(identifier = "outflowEvent",
                       description = "Signal indicating that a person has passed through the outgate",
                       wasGeneratedBy_identifier = "HlrDev1")
    Add.GE.DataDictionary(identifier = "outflowEvent",
                       providedBy_identifier = "outflow")
    Add.GE.SystemInterfaceDefinition (identifier="outflow")
    Add.GE.DataDictionary(identifier = "outflowEvent",
                       consumedBy_identifier = "HLR-2:v1")
	
	#------------ counter ------------ 
    Add.GE.DataDictionary(identifier = "counter",
                       description = "running total people in the park.",
                       wasGeneratedBy_identifier = "HlrDev1")
    Add.GE.DataDictionary(identifier = "counter",
                       providedBy_identifier = "HLR-1:v1")
    Add.GE.DataDictionary(identifier = "counter",
                       providedBy_identifier = "HLR-2:v1")
    Add.GE.DataDictionary(identifier = "counter",
                       consumedBy_identifier = "HLR-3:v1")	

    #------------ display ------------ 
    Add.GE.DataDictionary(identifier = "display",
                       wasGeneratedBy_identifier = "HlrDev1")
    Add.GE.DataDictionary(identifier = "display",
                       providedBy_identifier = "HLR-3:v1")
    Add.GE.DataDictionary(identifier = "display",
                       consumedBy_identifier = "census")
    Add.GE.SystemInterfaceDefinition (identifier="census")

    #########version2 
     #------------ HlrDev2 ------------
    Add.GE.SoftwareRequirementsDefinition(identifier = "HlrDev2",
                                       endedAtTime = "2020-07-25 10:53:38",
                                       wasAssociatedWith_identifier = "125569538",
                                       referenced_identifier = "RQ-STD:v1")
    Add.DOCUMENT.DOCUMENT(identifier = "RQ-STD:v1")
    #------------ inflowEvent ------------
    Add.GE.DataDictionary(identifier = "inflowEvent",
                       consumedBy_identifier = "HLR-1:v2")

    #------------ outflowEvent ------------
    Add.GE.DataDictionary(identifier = "outflowEvent",
                       consumedBy_identifier = "HLR-2:v2")

    #------------ counter ------------
    Add.GE.DataDictionary(identifier = "counter",
                       providedBy_identifier = "HLR-1:v2")                   
    Add.GE.DataDictionary(identifier = "counter",
                       providedBy_identifier = "HLR-2:v2")

    createCDR("http://rack001/turnstiledata")
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileHighLevelRequirements"))
################################################
    #    Low-level Requirements 
################################################   
    createEvidenceFile(ingestionTitle="TurnstileIngestion-Low Level Requirements", ingestionDescription="Manual ingestion of Turnstile Low Level Requirements")
 
    def llReqToIngestAux (req,version):
        l1="Requirementidentification"
        #l1_1=")Requirementidentification"
        l2="Description"
        l3="Governs"
        l4= "wasGeneratedBy"
        l5="Satisfies"
        return [Add.GE.LowLevelRequirement(#identifier = req[keybiggerThan10(l1_1,l1,req)], 
                                                 identifier = req[l1]+version,
                                                 governs_identifier = checkKeyExist(req,l3),
                                                 wasGeneratedBy_identifier= checkKeyExist(req,l4),
                                                 description = req[l2],
                                                 satisfies_identifier =checkKeyExist(req,l5)
                                                 ),
                Add.GE.HighLevelRequirement(identifier=checkKeyExist(req,l5)) ]

    # def llReqToIngestAux1 (req):
    #     l1="Requirementidentification"
    #     l1_1=")Requirementidentification"
    #     l5="Satisfies"
    #     return Add.GE.LowLevelRequirement( identifier = req[keybiggerThan10(l1_1,l1,req)],
    #                                             satisfies_identifier =checkKeyExist(req,l5))
    
    
    list(map(lambda x: llReqToIngestAux(x,""),ll))
    #list(map(lambda x: llReqToIngestAux1(x),newDict(satisfiesToBeSplited,"Satisfies")))    
    #list(map(lambda x: llReqToIngestAux(x),reqsDict(ll_Req(mainList1))))
    list(map(lambda x: llReqToIngestAux(x,":v2"),llReqsModified))
    ##New Req version 2
    list(map(lambda x: llReqToIngestAux(x,""), newLLReq))

    ###
    #------------ 2125895152 ------------
    Add.GE.Engineer(identifier = "2125895152",
                           title = "Doe, Jane",
                           emailAddress = "jane.doe@ge.com",
                           employedBy_identifier = "General_Electric")
    Add.AGENTS.ORGANIZATION(identifier = "General_Electric")
    #------------ LlrDev1 ------------
    Add.GE.SoftwareDesign(identifier = "LlrDev1",
                                 endedAtTime = "2020-07-19 11:48:38",
                                 wasAssociatedWith_identifier = "2125895152",
                                 referenced_identifier = "SW-STD:v1")
    Add.DOCUMENT.DOCUMENT(identifier = "SW-STD:v1")
    #------------ SwDesign ------------
    Add.GE.SoftwareDesign(identifier = "SwDesign",
                                 endedAtTime = "2020-07-23 09:52:38",
                                 wasAssociatedWith_identifier = "2125895152",
                                 referenced_identifier = "SW-STD:v1")
    Add.DOCUMENT.DOCUMENT(identifier = "SW-STD:v1")
    #------------ InputThread ------------
    Add.SYSTEM.SYSTEM_DEVELOPMENT(identifier="SysThreadDesign")
    
    Add.GE.SoftwareThread(identifier = "InputThread",
                                 partOf_identifier = "CounterApplication",
		                         wasGeneratedBy_identifier = "SysThreadDesign")
    #------------ OutputThread ------------
    Add.GE.SoftwareThread(identifier = "OutputThread",
                                 partOf_identifier = "CounterApplication",
		                         wasGeneratedBy_identifier = "SysThreadDesign")
    #------------ ExecutiveThread ------------
    Add.GE.SoftwareThread(identifier = "ExecutiveThread",
                                 partOf_identifier = "CounterApplication",
		                         wasGeneratedBy_identifier = "SysThreadDesign")
    #------------ DCC-1 ------------	
    Add.GE.DataAndControlCouple(identifier = "DCC-1",
                                       description = "PowerUp",
                                       wasGeneratedBy_identifier = "LlrDev1")
    Add.GE.DataAndControlCouple(identifier = "DCC-1",
                                       consumedBy_identifier = "EXE-LLR-1")
    Add.GE.DataAndControlCouple(identifier = "DCC-1",
                                       consumedBy_identifier = "EXE-LLR-2")
    Add.GE.DataAndControlCouple(identifier = "DCC-1",
                                       consumedBy_identifier = "IN-LLR-1")
    Add.GE.DataAndControlCouple(identifier = "DCC-1",
                                       consumedBy_identifier = "OUT-LLR-1")

    #------------ DCC-2 ------------	
    Add.GE.DataAndControlCouple(identifier = "DCC-2", 
                                       description = "incoming UDP message",
                                       wasGeneratedBy_identifier = "LlrDev1")
    Add.GE.DataAndControlCouple(identifier = "DCC-2",
                                       consumedBy_identifier = "IN-LLR-2:v1")
    Add.GE.DataAndControlCouple(identifier = "DCC-2",
                                       consumedBy_identifier = "IN-LLR-2:v2")
    Add.GE.DataAndControlCouple(identifier = "DCC-2",
                                       consumedBy_identifier = "IN-LLR-3:v1")
    Add.GE.DataAndControlCouple(identifier = "DCC-2",
                                       consumedBy_identifier = "IN-LLR-3:v2")
    Add.GE.DataAndControlCouple(identifier = "DCC-2",
                                       consumedBy_identifier = "IN-LLR-5")
    Add.GE.DataAndControlCouple(identifier = "DCC-2",
                                       consumedBy_identifier = "IN-LLR-6")

		
    #------------ DCC-3 ------------	
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       description = "input_park_count",
                                       wasGeneratedBy_identifier = "LlrDev1")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "IN-LLR-2:v1")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "IN-LLR-2:v2")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "IN-LLR-3:v1")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "IN-LLR-3:v2")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "IN-LLR-4")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "IN-LLR-5")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "IN-LLR-6")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       providedBy_identifier = "IN-LLR-1")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       providedBy_identifier = "IN-LLR-4")

		
    #------------ DCC-4 ------------	
    Add.GE.DataAndControlCouple(identifier = "DCC-4",
                                       description = "output_park_count",
                                       wasGeneratedBy_identifier = "LlrDev1")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "OUT-LLR-2:v1")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       consumedBy_identifier = "OUT-LLR-2:v2")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       providedBy_identifier = "OUT-LLR-1")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       providedBy_identifier = "IN-LLR-3:v1")
    Add.GE.DataAndControlCouple(identifier = "DCC-3",
                                       providedBy_identifier = "IN-LLR-3:v2")

	
    #------------ DCC-5 ------------	
    Add.GE.DataAndControlCouple(identifier = "DCC-5",
                                       description = "outgoing UDP message",
                                       wasGeneratedBy_identifier = "LlrDev1")
    Add.GE.DataAndControlCouple(identifier = "DCC-5",
                                       providedBy_identifier = "OUT-LLR-2:v1")
    Add.GE.DataAndControlCouple(identifier = "DCC-5",
                                       providedBy_identifier = "OUT-LLR-2:v2")

		
    #------------ DCC-6 ------------	
    Add.GE.DataAndControlCouple(identifier = "DCC-6",  
                                       description = "console",
                                       wasGeneratedBy_identifier = "LlrDev1")
    Add.GE.DataAndControlCouple(identifier = "DCC-6",
                                       providedBy_identifier = "EXE-LLR-3")
    Add.GE.DataAndControlCouple(identifier = "DCC-6",
                                       providedBy_identifier = "IN-LLR-5")
    Add.GE.DataAndControlCouple(identifier = "DCC-6",
                                       providedBy_identifier = "IN-LLR-6")
    createCDR("http://rack001/turnstiledata")
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileLowLevelRequirements")) 

################################################
#    Baselines
################################################
    createEvidenceFile(ingestionTitle="TurnstileIngestion-Baselines", ingestionDescription="Ingestion of Turnstile Requirements Baselines using Scraping Tool Kit")

    reqBase1 = "RequirementsPackage:v1"
    reqBase2 = "RequirementsPackage:v2"
    swBase1 = "SoftwareDesignAndCode:v1"
    testBase1 = "TestSuite:v1"
    turnstile1 = "Turnstile:v1"
    turnstile1_1 = "Turnstile:v1.1"

    # Requirements packages
    for req in hlReqsModified_Ids:
        Add.BASELINE.BASELINE(identifier=reqBase1, content_identifier=req+":v1")
        Add.BASELINE.BASELINE(identifier=reqBase2, content_identifier=req+":v2")
    for req in llReqsModified_Ids:
        Add.BASELINE.BASELINE(identifier=reqBase1, content_identifier=req+":v1")
        Add.BASELINE.BASELINE(identifier=reqBase2, content_identifier=req+":v2")
    for req in sys:
        Add.BASELINE.BASELINE(identifier=reqBase1, content_identifier=req[l])
        Add.BASELINE.BASELINE(identifier=reqBase2, content_identifier=req[l])
    Add.BASELINE.BASELINE(identifier=reqBase2, wasRevisionOf_identifier=reqBase1)

    # Software baseline
    Add.BASELINE.BASELINE(identifier=swBase1, content_identifier="OutputThread")
    Add.BASELINE.BASELINE(identifier=swBase1, content_identifier="InputThread")
    Add.BASELINE.BASELINE(identifier=swBase1, content_identifier="ExecutiveThread")

    # Testsuite release
    Add.BASELINE.BASELINE(identifier=testBase1, content_identifier="TC-1-1")
    Add.BASELINE.BASELINE(identifier=testBase1, content_identifier="TC-1-2")
    Add.BASELINE.BASELINE(identifier=testBase1, content_identifier="TC-1-3")
    Add.BASELINE.BASELINE(identifier=testBase1, content_identifier="TC-1-4")

    # Complete release baselines
    Add.BASELINE.BASELINE(identifier=turnstile1, content_identifier=reqBase1)
    Add.BASELINE.BASELINE(identifier=turnstile1, content_identifier=swBase1)
    Add.BASELINE.BASELINE(identifier=turnstile1, content_identifier=testBase1)

    Add.BASELINE.BASELINE(identifier=turnstile1_1, wasRevisionOf_identifier=turnstile1)
    Add.BASELINE.BASELINE(identifier=turnstile1_1, content_identifier=reqBase2)
    Add.BASELINE.BASELINE(identifier=turnstile1_1, content_identifier=swBase1)
    # A lack of a test suite targetting v2 requirements is a known omission

    createCDR("http://rack001/turnstiledata")
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileBaselines"))

########################################################RequirementsDocument
if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/RequirementsDocument")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/RequirementsDocument"))
    shutil.copytree(os.path.join(".","RequirementsDocument"), os.path.join(".","Turnstile-IngestionPackage/RequirementsDocument"))
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemRequirements")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileSystemRequirements"))
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileHighLevelRequirements")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileHighLevelRequirements"))
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileLowLevelRequirements")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileLowLevelRequirements"))
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileBaselines")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileBaselines"))
    CreateCdrs()
