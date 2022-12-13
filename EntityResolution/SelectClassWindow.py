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
results = None
class ClassWindow(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title('Select Classes')
        self.superClassSelections = Checkbar(self, picks=['Activity',  'Agent', 'Entity'],  command=self.update)
        self.superClassSelections.pack()
        
        
        self.classesTree = ttk.Treeview(self, selectmode=None, height=40)        
        self.classesTree["columns"]=["Class","Parent"]
        self.classesTree["show"]="headings"
        self.classesTree.heading("Class", text="Class")
        self.classesTree.heading("Parent", text="Parent")
        self.classesTree.pack()
    
        self.updateButton = ttk.Button(self, text="Done", command=self.done)
        self.updateButton.pack()
        self.update()
        
    def done(self):
        global results
        results = []
        selections = []
        for s in self.classesTree.selection():
            selections.append(self.classesTree.item(s)['text'])
        results = getSubclass(selections)
        self.destroy()
        
    def update(self):        
        semtk3.set_connection_override(rc.connString)
        all_ok = semtk3.check_services();
        if not all_ok: 
            print("Semtk services are not properly running on localhost")
            return
            
        for item in self.classesTree.get_children():
            self.classesTree.delete(item)
            
        s = self.superClassSelections.state()
        if s['Entity']:
            # Get THINGS, Returns a list of all 
            tab = semtk3.query(rc.entityTypeQuery,rc.connString)
            classes = []
            for c in tab.get_column("directSub"):
                if c not in classes:
                    self.classesTree.insert("",'end', text=c, values=(c, 'Entity' ))
        if s['Activity']:
            # Get THINGS, Returns a list of all 
            tab = semtk3.query(rc.activityTypeQuery,rc.connString)
            classes = []
            for c in tab.get_column("directSub"):
                if c not in classes:
                    self.classesTree.insert("",'end', text=c, values=(c, 'Activity' ))
        if s['Agent']:
            # Get THINGS, Returns a list of all 
            tab = semtk3.query(rc.agentTypeQuery,rc.connString)
            classes = []
            for c in tab.get_column("directSub"):
                if c not in classes:
                    self.classesTree.insert("",'end', text=c, values=(c, 'Agent' ))
def getSubclass(classes):
    semtk3.set_connection_override(rc.connString)
    all_ok = semtk3.check_services();
    if not all_ok: 
        print("Semtk services are not properly running on localhost")
        return
    subclasses = []
    for s in classes:
        subclasses.append(s)
        tab = semtk3.query(rc.classQuery.replace("{{Type}}", s),rc.connString)
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
        
