#!/usr/bin/env python3
import DataAccess as da
import tkinter as tk
from tkinter import ttk
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
            properties = da.getDataProperties(e)
            relationships = da.getRelationships(e)
            ## Update Relationships and Properties
            for k in properties:
                self.properties.insert("", 'end',  values=k)
            for k in relationships:
                self.relationships.insert("", 'end',  values=k)
#                if type(properties[k]) is str:
#                    self.properties.insert("", 'end', values =(k, baseElement[k]))
#                elif type(baseElement[k]) is dict:
#                    if '@id' in baseElement[k]:
#                        self.relationships.insert("", 'end', values =(k, elements[baseElement[k]['@id']]["PROV_S:identifier"],"Outgoing"))
#                elif type(baseElement[k]) is list:
#                    for i in baseElement[k]:
#                        # get id
#                        print(elements[i['@id']])
#                        self.relationships.insert("", 'end', values =(k, elements[i['@id']]["PROV_S:identifier"],"Outgoing"))
#
#            for k in elements:
#               if k != baseElement:
#                   if type(elements[k]) is dict:
#                       for p in elements[k]:
#                           if type(elements[k][p]) is dict:
#                               if elements[k][p]['@id'].split(":")[-1] == guid: 
#                                   self.relationships.insert("", 'end', values =(p, elements[k]["PROV_S:identifier"],"Incoming"))
