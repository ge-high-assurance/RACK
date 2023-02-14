#!/usr/bin/env python3
#import os

from CheckBar import Checkbar
import tkinter as tk
from tkinter import ttk
#from tkinter.messagebox import askyesno
import semtk3
#import os.path
import RACK_CONSTANTS as rc
#from Entity import *
results = []
class ClassWindow(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title('Select Classes')
        self.superClassSelections = Checkbar(self, picks=['Activity',  'Agent', 'Entity'],  command=self.update)
        self.superClassSelections.grid(row=0,  column=1)
        
        
        self.classesTree = ttk.Treeview(self, selectmode=None, height=40)        
        self.classesTree["columns"]=["Class","Parent"]
        self.classesTree["show"]="headings"
        self.sortOrder = {}
        for h in self.classesTree["columns"]:
            self.classesTree.heading(h, text=h,  command=lambda h=h: self.treeview_sort_column(h))
            self.sortOrder[h] = True
        self.classesTree.grid(row=1,  column=0,  columnspan=3, sticky='nsew')
    
        self.updateButton = ttk.Button(self, text="Done", command=self.done)
        self.updateButton.grid(row=2,  column=1)
        
        self.grid_columnconfigure(0,weight=1)
        self.grid_columnconfigure(1,weight=0)
        self.grid_columnconfigure(2,weight=1)
        
        self.grid_rowconfigure(0,weight=0)
        self.grid_rowconfigure(1,weight=1)
        self.grid_rowconfigure(2,weight=0)
        self.queryRack()
        self.update()
        
    def treeview_sort_column(self, col):
        tv = self.classesTree
        l = [(tv.set(k, col), k) for k in tv.get_children('')]
        l.sort(reverse=self.sortOrder[col] )

        # rearrange items in sorted positions
        for index, (val, k) in enumerate(l):
            tv.move(k, '', index)
        
        # reverse sort next time
        self.sortOrder[col] = not self.sortOrder[col] 

                   
    def done(self):
        global results
        results = []
        selections = []
        for s in self.classesTree.selection():
            selections.append(self.classesTree.item(s)['text'])
        results = getSubclass(selections)
        self.destroy()
    def queryRack(self):
        all_ok = semtk3.check_services();
        if not all_ok: 
            print("Semtk services are not properly running on localhost")
            return        
        self.classes = {'Entity':[], 'Activity':[], 'Agent':[], }        
        
        tab = semtk3.query_raw_sparql(rc.entityTypeQuery)        
        for c in tab.get_column("directSub"):
            if c not in self.classes['Entity']:
                self.classes['Entity'].append(c)

        tab = semtk3.query_raw_sparql(rc.agentTypeQuery)        
        for c in tab.get_column("directSub"):
            if c not in self.classes['Agent']:
                self.classes['Agent'].append(c)
                
        tab = semtk3.query_raw_sparql(rc.activityTypeQuery)        
        for c in tab.get_column("directSub"):
            if c not in self.classes['Activity']:
                self.classes['Activity'].append(c)
        
    def update(self):                    
        for item in self.classesTree.get_children():
            self.classesTree.delete(item)
            
        s = self.superClassSelections.state()
        print(s)
        if s['Entity']:
            for c in self.classes['Entity']:
                self.classesTree.insert("",'end', text=c, values=(c, 'Entity' ))
        if s['Activity']:
            for c in self.classes['Activity']:
                self.classesTree.insert("",'end', text=c, values=(c, 'Activity' ))
        if s['Agent']:
            for c in self.classes['Agent']:
                self.classesTree.insert("",'end', text=c, values=(c, 'Agent' ))
                    
def getSubclass(classes):
    all_ok = semtk3.check_services();
    if not all_ok: 
        print("Semtk services are not properly running on localhost")
        return
    subclasses = []
    for s in classes:
        subclasses.append(s)
        tab = semtk3.query_raw_sparql(rc.classQuery.replace("{{Type}}", s))
        for c in tab.get_column("directSub"):
            if c not in classes:
                subclasses.append(c)
    return subclasses
            

def SelectClass():
    c = ClassWindow()
    c.wait_window()
    return results
if __name__ == "__main__":
    print(SelectClass())
        
