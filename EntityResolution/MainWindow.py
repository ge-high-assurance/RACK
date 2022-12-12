#!/usr/bin/env python3
import DataAccess as da
import os
import json
from difflib import SequenceMatcher
import csv
import tkinter as tk
from tkinter import ttk
from tkinter.messagebox import askyesno
import semtk3
import os.path
import RACK_CONSTANTS as rc
import CreateIngestion as ci
from SelectClassWindow import SelectClass
from Entity import *

CONFIRMED_DIFFERENT =0
ASSUMED_DIFFERENT = 1
ASSUMED_SAME_AS=2
CONFIRMED_SAME_AS = 3
CONFIRMED_COMBINED = 4
ASSUMED_COMBINED =5
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
        self.rackMenu.add_command(label="Create Ingestion Data", command=self.push)
        self.rackMenu.add_command(label="Start Resolution...", command=self.pull)
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
        ci.createIngestion(self.decisions)
            
        
    def pull(self):
        semtk3.set_connection_override(rc.connString)
        all_ok = semtk3.check_services();
        if not all_ok: 
            print("Semtk services are not properly running on localhost")
            return
        # Get User selected PROV-S subclass to perform entity resolution on
        classes = SelectClass()
        print("Selected:", classes)
        classStr = ""
        for c in classes:
            classStr += "<"+c+"> "
            
        tab = semtk3.query(rc.instanceQuery.replace("<{{Types}}>", classStr),rc.connString)
        instances = {}
        for r in tab.get_rows():
            if r[1] not in instances:
                instances[r[1]] = []
            instances[r[1]].append(r[0])
        relations = {}
        for i in instances:
            relations[i] = []
            tab = semtk3.query(rc.subClassQuery.replace("{{Type}}", i),rc.connString)
            for c in tab.get_column("super"):
                if c not in relations[i]:
                    relations[i].append(c)
        secondaryDict = {}
        for r in relations:
            secondaryDict [r] = list(instances[r])
            for i in relations[r]:
                if i in instances:
                    secondaryDict[r] += list(instances[i])
                else:
                    print('No instances of {} found.'.format(i))
        for k in instances:
            print("count:", k, len(instances[k]))

        for k in secondaryDict:
            print('possible matches:', k, len(secondaryDict[k]))
        primaryDict = {}
        for k in instances:
            for i in instances[k]:
                primaryDict[i] = secondaryDict[k]
        
        #for i in tab.get_column("instance"):
        #   if i not in instances:
        #        instances.append(i)
        #c=0
        #for i in instances:
        #    c+=1
        #    print("Caching:{}     {}/{}".format( i,  c,  len(instances)))
        #    da.cacheData(i)
        import ResolveThings
        reset = askyesno("Reset Resolutions", "Do you want to reset any existings resolutions?")
        ResolveThings.run(primaryDict, reset=reset)
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
            identifier = da.getIdentifier(p[0])
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
                if self.decisions[p] == ASSUMED_COMBINED:
                    tags = ("assumedCombined",)
                elif self.decisions[p] == CONFIRMED_COMBINED:
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
                identifier = da.getIdentifier(p[0])
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
                    if self.decisions[p][s] == CONFIRMED_DIFFERENT or self.decisions[p][s] == CONFIRMED_SAME_AS:
                        continue
                    else:
                        del self.decisions[p][s]
                if temp[s] < self.differentScale.get():
                    self.decisions[p][s] = ASSUMED_DIFFERENT
                elif temp[s] > self.sameAsScale.get():
                    self.decisions[p][s] = ASSUMED_SAME_AS
        for p in self.decisions:
            if type(self.decisions[p]) is dict:
                for s in self.decisions[p]:
                    if self.decisions[p][s] == ASSUMED_SAME_AS:
                        self.decisions[s]= ASSUMED_COMBINED
                    elif self.decisions[p][s] == CONFIRMED_SAME_AS:
                        self.decisions[s]= CONFIRMED_COMBINED
        self.updatePrimary()
        self.updateSecondary()


if __name__ =="__main__":
    mw = MainWindow()
    mw.mainloop()
