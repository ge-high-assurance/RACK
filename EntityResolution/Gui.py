#!/usr/bin/env python3
import os
import json
from difflib import SequenceMatcher
import csv
from colorama import Fore, Back, Style
import tkinter as tk
from tkinter import ttk
from tkinter.messagebox import askyesno
import semtk3
import json
import os.path

connString = """
{   "name":"RACK local fuseki Apache Phase 2 Resolved",
    "domain":"",
    "enableOwlImports":false,
    "model":[
        {"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/model"}
        ],
    "data":[
        {"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/ResolvedData"}
        ]
}"""
DEBUG = False
def getIdentifier(e):
    guid = e.split("#")[-1]
    data = None
    if not os.path.exists("cache/"+guid+".json"):
        cacheData(e)
    with open("cache/"+guid+".json", "r") as dataFile:
        data = json.load(dataFile)
    if "@graph" not in data: # No Graph tag means there is a single element at the root.
        baseElement = data
        return data['PROV_S:identifier']
    ## Create a Hash based on the GUID and find the base 
    baseElement = None
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
    
class Entity(tk.Frame):
    
    uri = None
    def __init__(self, updateCallback):
        super().__init__()
        self.updateCallback = updateCallback
        self.propertyString = ''

        
        
        self.properties = ttk.Treeview(self, selectmode='browse')
        self.properties["columns"]=["Property","Value"]
        self.properties["show"]="headings"
        self.properties.heading("Property", text="Property")
        self.properties.heading("Value", text="Value")
        self.properties.column("Property", width=200, stretch=tk.NO)
        self.properties.bind('<ButtonRelease-1>', self.selectProperty)
        self.properties.pack(fill=tk.X, expand=True)

        self.relationships = ttk.Treeview(self, selectmode='none')
        self.relationships["columns"]=["Relationship","Identifier","Direction"]
        self.relationships["show"]="headings"
        self.relationships.heading("Identifier", text="Identifier")
        self.relationships.heading("Relationship", text="Relationship")
        self.relationships.heading("Direction", text="Direction")
        self.relationships.column("Relationship", width=200, stretch=tk.NO)
        self.relationships.pack(fill=tk.X, expand=True)
    '''===================================================
            Callback for selecting property for an Entity
       ===================================================''' 
    def selectProperty(self,a):
        currItem = self.properties.focus()
        self.propertyString = self.properties.item(currItem)['values'][1]
        self.updateCallback()   

    def update(self, e): 
        
        self.propertyString = ''
        # Clear ListView
        for item in self.relationships.get_children():
            self.relationships.delete(item)
        for item in self.properties.get_children():
            self.properties.delete(item)
        print(e)
        if e !=None:
            guid = e.split("#")[-1]
            data = getData(e)
            elements={}
            baseElement = None
            if "@graph" in data: # Cache has a graph tag so there is multiple nodes
                for el in data["@graph"]:
                    elements[el["@id"]] = el
                    if el["@id"].split(":")[-1] == guid:
                        baseElement = el
            else:
                baseElement = data
                
        
            ## Update Relationships and Properties
            for k in baseElement:
                if type(baseElement[k]) is str:
                    self.properties.insert("", 'end', values =(k, baseElement[k]))
                elif type(baseElement[k]) is dict:
                    if '@id' in baseElement[k]:
                        self.relationships.insert("", 'end', values =(k, elements[baseElement[k]['@id']]["PROV_S:identifier"],"Outgoing"))
                elif type(baseElement[k]) is list:
                    for i in baseElement[k]:
                        # get id
                        print(elements[i['@id']])
                        self.relationships.insert("", 'end', values =(k, elements[i['@id']]["PROV_S:identifier"],"Outgoing"))

            for k in elements:
               if k != baseElement:
                   if type(elements[k]) is dict:
                       for p in elements[k]:
                           if type(elements[k][p]) is dict:
                               if elements[k][p]['@id'].split(":")[-1] == guid: 
                                   self.relationships.insert("", 'end', values =(p, elements[k]["PROV_S:identifier"],"Incoming"))
                                
                        
                    

       
class MainWindow(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title('RACK Entity Resolution Tool')
        self.primary = ''
        
        ## Menu
        self.menubar = tk.Menu(self)
        self.fileMenu = tk.Menu(self.menubar)
        self.fileMenu.add_command(label="Load Data...", command=self.loadData)
        self.fileMenu.add_command(label="Save Data", command=self.saveData)
        self.fileMenu.add_separator()
        self.fileMenu.add_command(label="Exit",command=self.close)
        self.menubar.add_cascade(label="File", menu=self.fileMenu)
        
        self.rackMenu = tk.Menu(self)
        self.rackMenu.add_command(label="Push Resolutions...", command=self.push)
        self.rackMenu.add_command(label="Pull Data...", command=self.pull)
        self.menubar.add_cascade(label="RACK", menu=self.rackMenu)
        
        
        self.config(menu=self.menubar)
        
        ## Primary Frame
        self.primaryFrame = tk.Frame()
        self.primaryFrame.grid(column=0, row=0, rowspan=2,sticky='ew', padx=10,pady=10)
        
        
        ## Secondary Frame
        self.secondaryFrame = tk.Frame()
        self.secondaryFrame.grid(column=2, row=0,rowspan=2,sticky='ew', padx=10,pady=10)
        
        ## Primary Treeview
        self.primaryTree = ttk.Treeview(self.primaryFrame, selectmode="browse", height=10)        
        self.primaryTree["columns"]=["Identifier","Score"]
        self.primaryTree["show"]="headings"
        self.primaryTree.heading("Identifier", text="Primary")
        self.primaryTree.heading("Score", text="Best Score")
        self.primaryTree.column("Score", width=100, stretch=tk.NO)
        self.primaryTree.bind('<ButtonRelease-1>', self.selectPrimary)
        self.primaryTree.tag_configure('confirmedCombined', background="red")
        self.primaryTree.tag_configure('assumedCombined', background="#E38699")
        self.primaryTree.tag_configure('confirmedRemaining', background="green")
        self.primaryTree.tag_configure('assumedRemaining', background="#89E0A8")
        self.primaryTree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        
        ## Primary Scrollbar
        self.primaryScrollbar = tk.Scrollbar(self.primaryFrame, orient=tk.VERTICAL)
        self.primaryScrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        
        self.primaryTree.config(yscrollcommand=self.primaryScrollbar.set)
        self.primaryScrollbar.config(command=self.primaryTree.yview)
        
        ## Secondary Treeview        
        self.secondaryTree = ttk.Treeview(self.secondaryFrame, selectmode="browse", height=10)
        self.secondaryTree["columns"]=["Identifier","Score"]
        self.secondaryTree["show"]="headings"
        self.secondaryTree.heading("Identifier", text="Secondary")
        self.secondaryTree.heading("Score", text="Score")
        self.secondaryTree.column("Score", width=100, stretch=tk.NO)
        self.secondaryTree.bind('<ButtonRelease-1>', self.selectSecondary)
        self.secondaryTree.tag_configure('confirmedSameAs', background="green")
        self.secondaryTree.tag_configure('confirmedDifferent', background="red")
        self.secondaryTree.tag_configure('assumedSameAs', background="#89E0A8")
        self.secondaryTree.tag_configure('assumedDifferent', background="#E38699")
        self.secondaryTree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)

        ## Compare Text
        self.compareText = tk.Text(self, height=5)
        self.compareText.grid(row=2,column=0, columnspan=3,sticky='ew', padx=10,pady=10)

        ## Secondary Scrollbar
        self.secondaryScrollbar = tk.Scrollbar(self.secondaryFrame, orient=tk.VERTICAL)
        self.secondaryScrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        
        self.secondaryTree.config(yscrollcommand=self.secondaryScrollbar.set)
        self.secondaryScrollbar.config(command=self.secondaryTree.yview)
        
        ## Primary Entity
        self.primaryEntity = Entity(self.updateCompare)
        self.primaryEntity.grid(row=4,column=0,sticky='ew', padx=10,pady=10)
        
        ## Secondary Entity       
        self.secondaryEntity = Entity(self.updateCompare)
        self.secondaryEntity.grid(row=4,column=2,sticky='ew', padx=10,pady=10)
        
        ## Different Button
        self.differentButton = ttk.Button(self, text="Confirmed Different", command=self.confirmDifferent)
        self.differentButton.grid(row=5, column=0,sticky='ew', padx=10,pady=10)
        
        ## Same as Button
        self.sameAsButton = ttk.Button(self, text="Confirmed Same As", command=self.confirmSameAs)
        self.sameAsButton.grid(row=5, column=2,sticky='ew', padx=10,pady=10)
        
        ## Assumed SameAs Threshold
        self.sameAsLabel = ttk.Label(self, text="Assumed\nSame As\nThreshold", justify=tk.CENTER)
        self.sameAsLabel.grid(column=4, row=0, padx=10,pady=10)
        self.sameAsScale= tk.Scale(self, resolution=-1)
        self.sameAsScale.grid(column=4, row=1, rowspan=4, sticky='ns')
        
        ## Different Threshold
        self.differentLabel = ttk.Label(self, text="Assumed\nDifferent\nThreshold", justify=tk.CENTER)
        self.differentLabel.grid(column=3, row=0, padx=10,pady=10)
        self.differentScale = tk.Scale(self, resolution=-1)
        self.differentScale.grid(column=3, row=1, rowspan=4, sticky='ns')
        
        ## Update Button
        self.updateButton = ttk.Button(self, text="Update", command=self.assumptions)
        self.updateButton.grid(row=5, column=3, columnspan=2,sticky='ew', padx=10,pady=10)

        self.grid_columnconfigure(0,weight=1)
        self.grid_columnconfigure(1,weight=0)
        self.grid_columnconfigure(2,weight=1)
    def push(self):
        # Connect to semTK
        semtk3.set_connection_override(connString)
        all_ok = semtk3.check_services();
        if not all_ok: 
            print("Semtk services are not properly running on localhost")
            return
        count = 0
        for p in self.decisions:
            if self.decisions[p] != "confirmedCombined" and self.decisions[p] != "assumedCombined":
                for s in self.decisions[p]:
                    if self.decisions[p][s] == "assumedSameAs" or self.decisions[p][s] == "confirmedSameAs":
                        print("Combining",p,s)
                        semtk3.combine_entities(p,s, None, ["http://arcos.rack/PROV-s#identifier","http://arcos.rack/PROV-s#title","http://arcos.rack/PROV-s#description"]) 
                        count+=1
        print(count,"entities combined")
            
        
    def pull(self):
        semtk3.set_connection_override(connString)
        all_ok = semtk3.check_services();
        if not all_ok: 
            print("Semtk services are not properly running on localhost")
            return
            
        # Get THINGS, Returns a list of all 
        typeQuery = '''prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix PROV_S:<http://arcos.rack/PROV-S#>
prefix rdfs:<http://www.w3.org/2000/01/rdf-schema#>
select distinct ?directSub
		FROM <http://rack001/model>
 where { ?directSub rdfs:subClassOf ?super.
 values ?super{PROV_S:ENTITY} .}
'''
        tab = semtk3.query(typeQuery,connString)
        classes = []
        for c in tab.get_column("directSub"):
            if c not in classes:
                print(c)
                classes.append(c)

        quit()
        import ResolveRequirements
        
        
        reset = askyesno("Reset Resolutions", "Do you want to reset any existings resolutions?")
        ResolveRequirements.run(reset)
        self.loadData()
    '''===================================================
            Callback for selecting close menu button
       ===================================================''' 
    def close(self):
        print("Close")
        del(self)
    '''===================================================
            Callback for selecting save data menu button
       ===================================================''' 
    def saveData(self):
        with open("Decisions.json","w") as decisionFile:
            json.dump(self.decisions, decisionFile, indent=4)
    '''===================================================
            Callback for selecting load data menu button
       ===================================================''' 
    def loadData(self):
        self.summary ={}
        self.decisions = {}
        self.maxScore = 0.0
        
        with open("Resolutions/Summary.csv","r") as summaryFile:
            reader = csv.DictReader(summaryFile)
            for row in reader:
                self.summary[row["Primary"]] = float(row["Score"])
        if os.path.exists("Decisions.json"):
            if askyesno("Load Existing Decisions", "Existing decisions file was found. Do you want to load previous decisions?"):
                with open("Decisions.json","r") as decisionFile:
                    self.decisions = json.load(decisionFile)
        ## Clear Primary Tree    
        for item in self.primaryTree.get_children():
            self.primaryTree.delete(item)
        for p in sorted(self.summary.items(), key=lambda x:x[1],reverse=True):
            bestMatch = p[1]
            if bestMatch > self.maxScore:
                self.maxScore = bestMatch
            identifier = getIdentifier(p[0])
            self.primaryTree.insert("", 'end', text=p[0], values =(identifier, "{:.3f}".format(bestMatch)))

        self.sameAsScale.configure(to=self.maxScore)
        self.sameAsScale.set(self.maxScore)

        self.differentScale.set(0)
        self.differentScale.configure(to=self.maxScore)
        self.updatePrimary()
        self.updateSecondary()
        self.updateCompare()
    '''===================================================
            Callback for selecting a primary entity
       ==================================================='''    
    def selectPrimary(self, a):
        currItem = self.primaryTree.focus()
        self.primary = self.primaryTree.item(currItem)['text']
        self.updateSecondary()
        self.updateCompare()
    '''===================================================
            Callback for selecting a secondary entity
       ==================================================='''  
    def selectSecondary(self,a):
        currItem = self.secondaryTree.focus()
        secondary = self.secondaryTree.item(currItem)['text']
        if secondary != '':
            self.secondaryEntity.update(secondary)   
        else:
            self.secondaryEntity.update(None)   
        self.updateCompare()
        
    '''===================================================
            Callback for updating the compare Text box
       ==================================================='''  
    def updateCompare(self):
    
        self.compareText.delete("1.0","end")   
        primary = self.primaryEntity.propertyString
        secondary = self.secondaryEntity.propertyString
        s = SequenceMatcher(None, primary,secondary)
        for code in s.get_opcodes():
            if code[0] == "equal": 
                self.compareText.insert("end", primary[code[1]:code[2]],('equal'))

            elif code[0] == "delete":
                self.compareText.insert("end", primary[code[1]:code[2]],('delete'))
            elif code[0] == "insert":
                self.compareText.insert("end", secondary[code[3]:code[4]],('insert'))
            elif code[0] == "replace":
                self.compareText.insert("end", primary[code[1]:code[2]],('delete'))
                
                self.compareText.insert("end", secondary[code[3]:code[4]],('insert'))

             
        self.compareText.tag_config("equal", background="white", foreground="black")
        self.compareText.tag_config("delete", background="white", foreground="red")
        self.compareText.tag_config("insert", background="white", foreground="green")
        
    def updatePrimary(self):
        for item in self.primaryTree.get_children():
            p = self.primaryTree.item(item)['text']
            if p in self.decisions and self.decisions[p] != None:
                if self.decisions[p] == "assumedCombined":
                    tags = ("assumedCombined",)
                elif self.decisions[p] == "confirmedCombined":
                    tags = ("confirmedCombined",)
                else:
                    for s in self.decisions[p]:
                        if self.decisions[p][s] == "confirmedSameAs":
                            tags = ("confirmedRemaining",)
                            break
                    tags = ("assumedRemaining",)
                self.primaryTree.item(item, tags = tags)
    
    def updateSecondary(self):
        ## Clear secondary Tree    
        for item in self.secondaryTree.get_children():
            self.secondaryTree.delete(item)
        if self.primary != '':
            self.resolution = {}
            with open("Resolutions/"+self.primary.split("#")[-1]+".json","r") as resFile:
                self.resolution = json.load(resFile)
                
            for p in sorted(self.resolution.items(), key=lambda x:x[1],reverse=True):
                identifier = getIdentifier(p[0])
                tags = ()
                if self.primary in self.decisions and self.decisions[self.primary]!= None and p[0] in self.decisions[self.primary]:
                    tags = (self.decisions[self.primary][p[0]],)
                self.secondaryTree.insert("",'end', text=p[0], values=(identifier, "{:.3f}".format(p[1])), tags = tags)

            self.primaryEntity.update(self.primary)
            self.secondaryEntity.update(None)
        else:
            self.primaryEntity.update(None)
            self.secondaryEntity.update(None)
                
    '''===================================================
            Callback for selecting a the confirmed same as button
       ===================================================''' 
    def confirmSameAs(self):
        currItem = self.primaryTree.focus()
        primary = self.primaryTree.item(currItem)['text']
        
        currItem = self.secondaryTree.focus()
        secondary = self.secondaryTree.item(currItem)['text']
        
        if primary not in self.decisions:
            self.decisions[primary] = {}
        self.decisions[primary][secondary] = "confirmedSameAs"
        self.decisions[secondary] = "confirmedCombined"
        self.updatePrimary()   
        self.updateSecondary()
        self.updateCompare()
        
    '''===================================================
            Callback for selecting a the confirmed different button
       ==================================================='''  
    def confirmDifferent(self):
        currItem = self.primaryTree.focus()
        primary = self.primaryTree.item(currItem)['text']
        
        currItem = self.secondaryTree.focus()
        secondary = self.secondaryTree.item(currItem)['text']
        
        if primary not in self.decisions:
            self.decisions[primary] = {}
        self.decisions[primary][secondary] = "confirmedDifferent"
        self.updatePrimary()   
        self.updateSecondary()
        self.updateCompare()
    '''===================================================
            Callback for selecting a the Update Assumptions button
       ==================================================='''  
    def assumptions(self):
        i = 0
        for p in self.summary:
            print(i, "/", len(self.summary))
            i+=1
            print(p)
            temp = {}
            with open("Resolutions/"+p.split("#")[-1]+".json","r") as resFile:
                temp = json.load(resFile)

            ## Check if there is some confirmed
            if p not in self.decisions:
                self.decisions[p] = {}
            for s in temp:                
                if s in self.decisions[p]:
                    if self.decisions[p][s] == "confirmedDifferent" or self.decisions[p][s] == "confirmedSameAs":
                        continue
                    else:
                        del self.decisions[p][s]
                if temp[s] < self.differentScale.get():
                    self.decisions[p][s] = "assumedDifferent"
                elif temp[s] > self.sameAsScale.get():
                    self.decisions[p][s] = "assumedSameAs"
        for p in self.decisions:
            if type(self.decisions[p]) is dict:
                for s in self.decisions[p]:
                    if self.decisions[p][s] == "assumedSameAs":
                        self.decisions[s]= "assumedCombined"
                    elif self.decisions[p][s] == "confirmedSameAs":
                        self.decisions[s]= "confirmedCombined"
        self.updatePrimary()
        self.updateSecondary()


if __name__ =="__main__":
    mw = MainWindow()
    mw.mainloop()
