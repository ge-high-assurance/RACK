#!/bin/python3.8
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

from tkinter import *
from tkinter import ttk
from tkinter import filedialog
import NLUtils as nlu
import Logger
import json
import uuid
from ExtractRackData import createCSV
import subprocess
import os

class DocTree(Frame):

    def __init__(self, parent, *args, **kwargs):
        '''DocTree is a customized which for managing '''
        Frame.__init__(self, parent)
        self.columnconfigure(1,weight=1)
        self.rowconfigure(4,weight=1)

        #URI Label
        Label(self, text= "Model URI :", justify="left")\
                    .grid(row = 1, column = 1,\
                             columnspan= 1, rowspan = 1,\
                             sticky="NSEW")
        #URI Entry
        self.uri = StringVar(self)
        self.uriEntry = Entry(self,textvariable=self.uri)
        self.uriEntry.grid(row = 1, column = 2,\
                              columnspan= 2, rowspan = 1,\
                              sticky="NSEW")
        #Doc Name Label
        Label(self, text= "Doc Name :", justify="left")\
                    .grid(row = 2, column = 1,\
                             columnspan= 1, rowspan = 1,\
                             sticky="NSEW")
        #Doc Name Entry
        self.DocName = StringVar(self)
        self.DocNameEntry = Entry(self, textvariable=self.DocName)
        self.DocNameEntry.grid(row = 2, column = 2,\
                              columnspan= 2, rowspan = 1,\
                              sticky="NSEW")
        #Doc Type Label
        Label(self, text= "Doc Type :", justify="left")\
                    .grid(row = 3, column = 1,\
                             columnspan= 1, rowspan = 1,\
                             sticky="NSEW")
        #Doc Type Menu
        self.DocType = StringVar(self)
        self.DocTypeMenu = OptionMenu(self, self.DocType,\
                                       "DESCRIPTION",\
                                       "PLAN",\
                                       "PROCEDURE",\
                                       "REPORT",\
                                       "REQUEST",\
                                       "SPECIFICATION",\
                                       "SECTION")
        self.DocTypeMenu.grid(row = 3, column = 2,\
                              columnspan= 2, rowspan = 1,\
                              sticky="NSEW")


        #Doc Treeview
        self.DocTree = ttk.Treeview(self, selectmode='browse')
        self.DocTree.grid(row = 4, column = 1,\
                             columnspan= 2, rowspan = 1,\
                             sticky="NSEW")

        self.scroll = ttk.Scrollbar(self,orient="vertical",command=self.DocTree.yview)
        self.scroll.grid(row = 4, column = 3,\
                            columnspan= 1, rowspan = 1,\
                            sticky="NSEW")
        self.DocTree.configure(yscrollcommand=self.scroll.set)

        #Export to SADL Button
        Button(self, command = self.loadToRACK, text="Load To RACK")\
                     .grid(row = 5, column = 1,\
                           columnspan= 3, rowspan = 1,\
                           sticky="NSEW")
        #Export to JSON Button
        Button(self, command = self.exportToJson, text="Export to JSON")\
                     .grid(row = 6, column = 1,\
                           columnspan= 3, rowspan = 1,\
                           sticky="NSEW")
        #Import from JSON Button
        Button(self, command = self.importFromJson, text="Import from JSON")\
                     .grid(row = 7, column = 1,\
                           columnspan= 3, rowspan = 1,\
                           sticky="NSEW")
        
        self.popup_menu = Menu(parent, title="Actions")
        
        
        
        self.popup_menu.add_command(label="Add SECTION", command=self.addSection)
        self.popup_menu.add_command(label="Add title", command=self.addTitle)  
        
        self.popup_menu.add_separator()        
        self.popup_menu.add_command(label="Add REQUIREMENT", command=self.addRequirement)
        self.popup_menu.add_command(label="Add text", command=self.addText)
        self.popup_menu.add_command(label="Add satisfies", command=self.addSatisfies)                
        self.popup_menu.add_command(label="Add satisfiedBy", command=self.addSatisfiedBy)
        self.popup_menu.add_command(label="Add verifiedBy", command=self.addVerifiedBy)
        
        self.popup_menu.add_separator()  
        self.popup_menu.add_command(label="Add OBJECTIVE", command=self.addObjective)
        self.popup_menu.add_command(label="Add description", command=self.addDescription)
        
        self.popup_menu.add_separator()
        self.popup_menu.add_command(label="Add ACTIVITY", command=self.addActivity)
        self.popup_menu.add_command(label="Add wasInformedBy", command=self.addWasInformedBy)
        
        self.popup_menu.add_separator()
        self.popup_menu.add_command(label="Add TEST", command=self.addTestCase)
        self.popup_menu.add_command(label="Add text", command=self.addText)
        self.popup_menu.add_command(label="Add verifies", command=self.addVerifies)        
        self.popup_menu.add_command(label="Add comfirmedBy", command=self.addConfirmedBy)

        self.popup_menu.add_separator()
        self.popup_menu.add_command(label="Add TEST_RESULT", command=self.addTestResult)
        self.popup_menu.add_command(label="Add confirms", command=self.addConfirms)  
        
        self.popup_menu.add_separator()
        self.popup_menu.add_command(label="Append Object", command=self.appendObject)
        
        self.popup_menu.add_separator()
        self.popup_menu.add_command(label="Move Up", command=self.moveUp)
        self.popup_menu.add_command(label="Move Down", command=self.moveDown)
        self.popup_menu.add_command(label="Level Up", command=self.levelUp)
        self.popup_menu.add_command(label="Level Down", command=self.levelDown)
        self.popup_menu.add_separator()
        self.popup_menu.add_command(label="Remove", command=self.remove)
        self.popup_menu.tk_popup(0,0)
        
        self.DocTree.bind("<<TreeviewSelect>>", self.selection)
        self.DocTree.bind("<Button-3>", self.popup)
        self.DocTree.bind("<Delete>", self.remove)

        self.selectedText = None
        self.entityTypes = {}
    def exportToSadl(self, event=None):
        Logger.write("DocTree.exportToSadl")
        sadlFileName = filedialog.asksaveasfilename(initialdir = "../",\
                                                    title = "Select file name",\
                                                    filetypes=(("SADL file", "*.sadl"),\
                                                               ("All files", "*.*")))
        Logger.write("Writing to sadl File to :", sadlFileName)
        with open(sadlFileName, "w") as sadlFile:
            sadlFile.write('uri "'+self.uri.get()+'/'+self.DocName.get()+'" alias '+self.DocName.get()+'.\n')
            sadlFile.write(self.getImportString()+"\n") 
            for a in self.DocTree.get_children():
                Logger.write(a, self.entityTypes[a])
                sadlFile.write(self.createSadlString(a))
    def loadToRACK(self, event=None):
        Logger.write("DocTree.loadToRACK")
        if not os.path.exists("./Temp"):
            os.makedirs("./Temp")
        xmlFileName ="./Temp/RACK-DATA.xml" 
        Logger.write("Writing to xml File to :", xmlFileName)
        with open(xmlFileName, "w") as xmlFile:
            xmlFile.write('<?xml version="1.0" encoding="UTF-8"?>\n')
            xmlFile.write('<RACK-DATA>\n')
            for a in self.DocTree.get_children():
                Logger.write(a, self.entityTypes[a])
                front, back = self.createXmlString(a)
                xmlFile.write(back+front)
            xmlFile.write('</RACK-DATA>\n')   
        createCSV(xmlFileName)
        subprocess.call(['bash', './IngestData.sh']) 
    def getChildEntities(self, child):
        temp = list()
        for i in self.DocTree.get_children(child):
            temp.append(self.entityTypes[i])
            temp+=self.getChildEntities(i)
        return list(set(temp))
        
    def getImportString(self):

        imports = {"TEST":'import "http://arcos.rack/TESTING".\n',
                   "REQUIREMENT":'import "http://arcos.rack/REQUIREMENTS".\n',
                   "OBJECTIVE":'import "http://arcos.rack/PROCESS".\n',
                   "SECTION":'import "http://arcos.rack/DOCUMENT".\n',
                   "DOCUMENT":'import "http://arcos.rack/DOCUMENT".\n'}
        
        entitys = list()
        for a in self.DocTree.get_children():
                entitys.append(self.entityTypes[a])
                entitys+=self.getChildEntities(a)
        importString = ""
        for a in set(entitys):
            if a in imports:
                importString += imports[a]
        return importString
                
                
    def exportToJson(self, event=None):
        Logger.write("DocTree.exportToJson")
        jsonFileName = filedialog.asksaveasfilename(initialdir = "../",\
                                                    title = "Select file name",\
                                                    filetypes=(("JSON file", "*.json"),\
                                                               ("All files", "*.*")))
        Logger.write("Writing to json File to :", jsonFileName)
        with open(jsonFileName, "w") as jsonFile:
            structure = {}
            structure["DocName"] = self.DocName.get()
            structure["URI"] = self.uri.get()
            structure["DocType"] = self.DocType.get()
            
            data = list()
            for a in self.DocTree.get_children():
                data.append(self.createDictionary(a))
            structure["data"] = data
            json.dump(structure, jsonFile)

    def importFromJsonOld(self, event=None):
        Logger.write("DocTree.importFromJson")
        jsonFileName = filedialog.askopenfilename(initialdir = "../",\
                                                    title = "Select file",\
                                                    filetypes=(("JSON file", "*.json"),\
                                                               ("All files", "*.*")))
        Logger.write("Writing to json File to :", jsonFileName)
        data = None
        with open(jsonFileName, "r") as jsonFile:
            data = json.load(jsonFile)
        Logger.write(data)
        self.DocTree.delete(*self.DocTree.get_children())
        self.entityTypes = {}
        for i in data:
            self.loadDictionary("",i)

    def importFromJson(self, event=None):
        Logger.write("DocTree.importFromJson")
        jsonFileName = filedialog.askopenfilename(initialdir = "../",\
                                                    title = "Select file",\
                                                    filetypes=(("JSON file", "*.json"),\
                                                               ("All files", "*.*")))
        Logger.write("Reading from json File to :", jsonFileName)
        structure = None
        with open(jsonFileName, "r") as jsonFile:
            structure = json.load(jsonFile)
        self.DocTree.delete(*self.DocTree.get_children())

        self.DocName.set(structure["DocName"])
        self.uri.set(structure["URI"])
        self.DocType.set(structure["DocType"])
        
        self.entityTypes = {}
        for i in structure["data"]:
            self.loadDictionary("",i)
            
    def loadDictionary(self,parent, itemDict):
        self.entityTypes[itemDict["ItemKey"]] = itemDict["entityType"]
        self.DocTree.insert(parent,END, itemDict["ItemKey"], text=itemDict["text"])
        for c in itemDict["Children"]:
            self.loadDictionary(itemDict["ItemKey"],c)

    def createDictionary(self, item):        
        text = self.DocTree.item(item)["text"]
        Logger.write("text",text)
        entityType = self.entityTypes[item]
        children = list()
        for a in self.DocTree.get_children(item):
                children.append(self.createDictionary(a))
        return {"ItemKey":item,"text":text, "entityType":entityType, "Children":children}
    def createXmlString(self, item, string="", appendString=""):
        Logger.write("DocTree.createXmlString")
        entityTypeList = ["SECTION",
                          "SYSTEM",
                          "REQUIREMENT",
                          "TEST",
                          "TEST_RESULT",
                          "OBJECTIVE","ACTIVITY"]
        relationshipList  = ["satisfies" , 
                             "satisfiedBy",  
                             "verifiedBy", 
                             "verifies", 
                             "wasInformedBy",
                             "confirmedBy", 
                             "confirms"]
        primaryRelationshipList  = ["satisfies" , 
                                     "verifies", 
                                     "wasInformedBy",
                                     "confirms"]
        InverseDictionary = {"satisfiedBy":"satisfies",
                             "verifiedBy":"verifies",
                             "confirmedBy":"confirms"}
        indent = "    "
        objectString = ""
        if item in self.entityTypes:
            objectString +='    <'+self.entityTypes[item].upper()+'>\n'
            objectString +='        <identifier>'+self.entityTypes[item].upper()+"-"+item+'</identifier>\n'
            
            
            string += objectString
            string +='        <title>'+self.DocTree.item(item)["text"]+'</title>\n'
            string +='    </'+self.entityTypes[item].upper()+'>\n'
            
            for c in self.DocTree.get_children(item):
                if self.entityTypes[c].upper() in entityTypeList:
                    appendString +=objectString
                    appendString +='        <content>'+self.entityTypes[c]+"-"+c+'</content>\n'
                    appendString +='    </'+self.entityTypes[item].upper()+'>\n'
                    string, appendString = self.createXmlString(c, string, appendString)
                
                elif self.entityTypes[c] in relationshipList:
                    for gc in self.DocTree.get_children(c):                    
                        string, appendString = self.createXmlString(gc, string, appendString)
                        if self.entityTypes[c] in primaryRelationshipList:
                            string +=objectString
                            string +='        <'+self.entityTypes[gc]+'>'+self.entityTypes[item].upper()+"-"+item+'</'+self.entityTypes[gc]+'>\n'
                            string +='    </'+self.entityTypes[item].upper()+'>\n'
                        else:

                            string +='    <'+self.entityTypes[gc].upper()+'>\n'
                            string +='        <identifier>'+self.entityTypes[gc].upper()+"-"+gc+'</identifier>\n'
                            string +='        <'+InverseDictionary[self.entityTypes[c]]+'>'+self.entityTypes[item].upper()+"-"+item+'</'+InverseDictionary[self.entityTypes[c]]+'>\n'
                            string +='    </'+self.entityTypes[gc].upper()+'>\n'
                else:
                    string +=objectString
                    string +='        <'+self.entityTypes[c]+'>'+self.DocTree.item(c)["text"]+'</'+self.entityTypes[c]+'>\n'
                    string +='    </'+self.entityTypes[item].upper()+'>\n'
        return string, appendString  
                               
    def createSadlString(self, item, indent = ""):
        Logger.write("DocTree.createSadlString")
        string = ""
        if item in self.entityTypes:            
            objTxt = self.getUuid(self.DocTree.item(item)["text"])
            parText = self.getUuid(self.DocTree.item(self.DocTree.parent(item))["text"])

            Logger.write("Obj Text", objTxt)
            Logger.write("Par Text", parText)
            Logger.write("Entity Type", self.entityTypes[item])
            ## Process Document Object (Should Only be one)
            if self.entityTypes[item].upper() == "DOCUMENT":
                string += "\n"+indent + objTxt +" is a "+self.DocType.get()+", \n"
                string += indent + '    has uniqueIdentifier "'+self.DocTree.item(item)["text"]+'"'
                for c in self.DocTree.get_children(item):
                    if self.entityTypes[c].upper() == "SECTION" or\
                       self.entityTypes[c].upper() == "REQUIREMENT" or \
                       self.entityTypes[c].upper() == "TEST" or \
                       self.entityTypes[c].upper() == "TEST_RESULT" or \
                       self.entityTypes[c].upper() == "OBJECTIVE" or \
                       self.entityTypes[c].upper() == "ACTIVITY":
                        childText = self.getUuid(self.DocTree.item(c)["text"])
                        string +=  ",\n" + indent + "    has content "+childText
                string+= "."

            ## Process Section Object    
            elif self.entityTypes[item].upper() == "SECTION":
                string += "\n"+indent + objTxt +" is a SECTION,\n"
                string += indent + '    has uniqueIdentifier "'+self.DocTree.item(item)["text"]+'"'

                for c in self.DocTree.get_children(item):
                    if self.entityTypes[c].upper() == "SECTION" or\
                       self.entityTypes[c].upper() == "REQUIREMENT" or \
                       self.entityTypes[c].upper() == "TEST" or \
                       self.entityTypes[c].upper() == "TEST_RESULT" or \
                       self.entityTypes[c].upper() == "OBJECTIVE" or \
                       self.entityTypes[c].upper() == "ACTIVITY":
                        childText = self.getUuid(self.DocTree.item(c)["text"])
                        string +=  ",\n" + indent + "    has content "+childText
                    elif self.entityTypes[c].lower() == "title" or\
                       self.entityTypes[c].lower() == "text":
                        childText = self.getUuid(self.DocTree.item(c)["text"])
                        string +=  ",\n" + indent + '    has '+self.entityTypes[c].lower()+' "'+self.DocTree.item(c)["text"]+'"'
                string+= "."
                        

            ## Process Requirement, TestCase and Objective Object
            elif self.entityTypes[item].upper() == "REQUIREMENT" or \
               self.entityTypes[item].upper() == "TEST" or \
               self.entityTypes[item].upper() == "TEST_RESULT" or \
               self.entityTypes[item].upper() == "OBJECTIVE" or \
               self.entityTypes[item].upper() == "ACTIVITY":
                string += "\n" + indent + objTxt +" is a "+self.entityTypes[item].upper()+ ","
                string += "\n" + indent + '    has uniqueIdentifier "'+self.DocTree.item(item)["text"]+'"'
                for c in self.DocTree.get_children(item):
                    if self.entityTypes[c].lower() == "title" or\
                       self.entityTypes[c].lower() == "description" or\
                       self.entityTypes[c].lower() == "text":
                        childText = self.getUuid(self.DocTree.item(c)["text"])
                        string +=  ",\n" + indent + '    has '+self.entityTypes[c]+' "'+self.DocTree.item(c)["text"]+'"'
                    elif self.entityTypes[c] == "confirms" or \
                         self.entityTypes[c] == "verifies" or \
                         self.entityTypes[c] == "satisfies" or \
                         self.entityTypes[c] == "wasInformedBy":
                        for d in self.DocTree.get_children(c):
                            childText = self.getUuid(self.DocTree.item(d)["text"])
                            string +=  ",\n" + indent + '    has '+self.entityTypes[c]+' '+childText+''
                string += "."
                
            ## Process Other Object
            elif self.entityTypes[item] == "confirmedBy" or\
                  self.entityTypes[item] == "verifiedBy" or\
                  self.entityTypes[item] == "satisfiedBy":
                stringDictionary = {"confirmedBy":"confirms",
                                    "verifiedBy":"verifies",
                                    "satisfiedBy":"satisfies"}
                for c in self.DocTree.get_children(item):
                    childText = self.getUuid(self.DocTree.item(c)["text"])
                    string +=  "\n" +indent + "  " +childText +" has "+stringDictionary[self.entityTypes[item]]+" "+ parText+"."   

            for c in self.DocTree.get_children(item):
                string += "\n" + self.createSadlString(c, indent + "    ")
        return string
                
    def getUuid(self,txt):
        Logger.write("DocTree.getUuid")
        # Create a string from the URI, DocName and the Text to create a UUID.
        a = self.uri.get() +"/"+ self.DocName.get()+"#"+txt
        return "uuid_"+str(uuid.uuid3(uuid.NAMESPACE_URL, a))
    
    
    def selection(self, event):
        Logger.write("DocTree.selection")
        t = self.DocTree.selection()[0]
        Logger.write(t)
        self.currentObject= t
        self.parentObject = self.DocTree.parent(self.currentObject)
        
    def updateSelection(self, item):
        Logger.write("DocTree.updateSelection")
        self.DocTree.see(item)
        self.DocTree.selection_set(item)
        

        
    def remove (self, event=None):
        '''function to move object one level down'''
        Logger.write("DocTree.remove")
        t = self.currentObject
        if self.DocTree.index(self.currentObject)==0:            
            self.updateSelection(self.parentObject)
        else:
            self.updateSelection(self.DocTree.prev(self.currentObject))
        if t!="":
            self.DocTree.delete(t)

    def moveUp(self):
        '''function to move object one level down'''
        Logger.write("DocTree.moveUp")
        self.DocTree.move(self.currentObject,self.DocTree.parent(self.currentObject),self.DocTree.index(self.currentObject)-1)
        self.updateSelection(self.currentObject)                    

    def moveDown(self):
        '''function to move object one level down'''
        Logger.write("DocTree.moveDown")
        self.DocTree.move(self.currentObject,self.DocTree.parent(self.currentObject),self.DocTree.index(self.currentObject)+1)
        self.updateSelection(self.currentObject)

    def levelDown(self):
        '''function to move object one level down'''
        Logger.write("DocTree.moveDown")
        if self.DocTree.prev(self.currentObject) != "":
            self.DocTree.move(self.currentObject,self.DocTree.prev(self.currentObject),END)
            self.updateSelection(self.currentObject)
                    
    def levelUp(self):
        '''function to move object one level up'''
        Logger.write("DocTree.moveUp")
        self.DocTree.move(self.currentObject,self.DocTree.parent(self.parentObject),END)
        self.updateSelection(self.currentObject)
                
    def createObjectBelow(self, objType):
        ''' function to added a new object into the document tree.'''
        Logger.write("DocTree.createObjectBelow")
        textStrings = ["New "+objType]
        if self.selectedText != None:
            textStrings = self.selectedText
        t=self.currentObject
        for s in textStrings:
            t = self.DocTree.insert(self.currentObject,\
                                        END,\
                                        text=nlu.normalizeString(s))
            
            self.entityTypes[t] = objType
        self.updateSelection(t)
                
    def appendObject(self):
        ''' function to append data to the tree.'''
        Logger.write("DocTree.appendObject")
        textStrings = ["ADD"]
        if self.selectedText != None:
            textStrings = self.selectedText
        for s in textStrings:
            newString = nlu.normalizeString(self.DocTree.item(self.currentObject)["text"] + " " +s)
            Logger.write(newString)
            self.DocTree.item(self.currentObject, text=newString)
            
                
    def createObjectAfter(self, objType):        
        Logger.write("DocTree.createObjectAfter")
        textStrings = ["New "+objType]
        if self.selectedText != None:
            textStrings = self.selectedText
        t = self.currentObject
        for s in textStrings:
            t = self.DocTree.insert(self.parentObject,\
                                        self.DocTree.index(self.currentObject)+1,\
                                        text=nlu.normalizeString(s))
            self.entityTypes[t] = objType
            self.currentObject = t
            self.updateSelection(t)
                
    def addRequirement(self):        
        Logger.write("DocTree.addTestResult")
        if self.entityTypes[self.currentObject] == "REQUIREMENT":
            self.createObjectAfter("REQUIREMENT")
        else:
            self.createObjectBelow("REQUIREMENT")

    def addTestCase(self):        
        Logger.write("DocTree.addTestCase")
        if self.entityTypes[self.currentObject] == "verifiedBy" or \
        self.entityTypes[self.currentObject] == "verifies" :
            self.createObjectBelow("TEST")
        else:
            self.createObjectAfter("TEST")
    def addTestResult(self):        
        Logger.write("DocTree.addTestResult")
        if self.entityTypes[self.currentObject] == "confirmedBy" or \
        self.entityTypes[self.currentObject] == "confirms" :
            self.createObjectBelow("TEST_RESULT")
        else:
            self.createObjectAfter("TEST_RESULT")
    def addObjective(self):        
        Logger.write("DocTree.addObjective")
        if self.entityTypes[self.currentObject] == "SECTION":
            self.createObjectBelow("OBJECTIVE")
        else:
            self.createObjectAfter("OBJECTIVE")
    def addActivity(self):        
        Logger.write("DocTree.addActivity")
        if self.entityTypes[self.currentObject] == "SECTION":
            self.createObjectBelow("ACTIVITY")
        else:
            self.createObjectAfter("ACTIVITY")                
    def addSection(self):        
        Logger.write("DocTree.addSection")
        if self.entityTypes[self.currentObject] == "DOCUMENT":
            self.createObjectBelow("SECTION")
        else:
            self.createObjectAfter("SECTION") 

    def addText(self):        
        Logger.write("DocTree.addText")
        if self.entityTypes[self.currentObject] == "REQUIREMENT":
            self.createObjectBelow("text")
        else:
            self.createObjectAfter("text") 
    
    def addDescription(self):        
        Logger.write("DocTree.addDescription")
        if self.entityTypes[self.currentObject] == "OBJECTIVE":
            self.createObjectBelow("description")
        else:
            self.createObjectAfter("description") 
    def addTitle(self):        
        Logger.write("DocTree.addTitle")
        if self.entityTypes[self.currentObject] == "SECTION":
            self.createObjectBelow("title")
        else:
            self.createObjectAfter("title")                

    def addWasInformedBy(self):        
        Logger.write("DocTree.addWasInformedBy")
        self.selectedText = ["wasInformedBy"]
        if self.entityTypes[self.currentObject] == "ACTIVITY":
            self.createObjectBelow("wasInformedBy")
        else:
            self.createObjectAfter("wasInformedBy")  

    def addConfirms(self):        
        Logger.write("DocTree.addConfirms")
        self.selectedText = ["confirms"]
        if self.entityTypes[self.currentObject] == "TEST_RESULT":
            self.createObjectBelow("confirms")
        else:
            self.createObjectAfter("confirms")  

    def addConfirmedBy(self):        
        Logger.write("DocTree.addConfirmedBy")
        self.selectedText = ["confirmedBy"]
        if self.entityTypes[self.currentObject] == "TEST":
            self.createObjectBelow("confirmedBy")
        else:
            self.createObjectAfter("confirmedBy")  
        
    def addSatisfies(self):        
        Logger.write("DocTree.addSatisfies")
        self.selectedText = ["satisfies"]
        if self.entityTypes[self.currentObject] == "REQUIREMENT":
            self.createObjectBelow("satisfies")
        else:
            self.createObjectAfter("satisfies")  
        
    def addSatisfiedBy(self):        
        Logger.write("DocTree.addSatisfiedBy")
        self.selectedText = ["satisfiedBy"]
        if self.entityTypes[self.currentObject] == "REQUIREMENT":
            self.createObjectBelow("satisfiedBy")
        else:
            self.createObjectAfter("satisfiedBy") 
        
    def addVerifies(self):        
        Logger.write("DocTree.addVerifies")
        self.selectedText = ["verifies"]
        if self.entityTypes[self.currentObject] == "TEST":
            self.createObjectBelow("verifies")
        else:
            self.createObjectAfter("verifies")

    def addVerifiedBy(self):        
        Logger.write("DocTree.addVerifiedBy")
        self.selectedText = ["verifiedBy"]
        if self.entityTypes[self.currentObject] == "REQUIREMENT":
            self.createObjectBelow("verifiedBy")
        else:
            self.createObjectAfter("verifiedBy")

        
    def initializeDoc(self, name="New Document"):
        Logger.write("DocTree.initializeDoc")
        self.DocTree.delete(*self.DocTree.get_children())
        t = self.DocTree.insert("",END, text=name)
        self.DocTree.see(t)
        self.DocTree.selection_set(t)
        self.entityTypes[t] = "DOCUMENT"
        self.currentObject = t
        self.parentObject = self.DocTree.parent(t)
                
    def popup(self, event):
        '''Event Handler for creating the popup menu'''
        Logger.write("DocTree.popup")
        try:
            self.popup_menu.tk_popup(event.x, event.y, 0)
        finally:
            self.popup_menu.grab_release()



if __name__ == '__main__':
    Test = Tk()
    Test.title("DocTree Test")
    #rd = Frame(Test)
    DT = DocTree(Test)
    DT.grid(row = 1, column = 7)
    DT.initializeDoc()
    Test.mainloop()
                      
