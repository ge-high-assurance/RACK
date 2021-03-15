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

from Evidence.Add import SYSTEM, INTERFACE, REQUIREMENT
from Logging import *
from lxml import etree

__xmlroot__ = None
__xmlpath__ = None
handlers = None
ignoredTags = None
    
def getAttribute(e, attr ,ns=None):
    trace()
    # helper function to get the attribute, if a namespace is provided this name space will be looked up from the xml
    if ns is not None:
        return e.get("{<<NS>>}<<attr>>"
                     .replace("<<NS>>", e.nsmap[ns])
                     .replace("<<attr>>", attr))
    else:
        return e.get(attr)

def getBase(xmiId):
    trace()
    global __xmlroot__
    baseClass = None
    for e in __xmlroot__.iter("packagedElement"):
        if getAttribute(e,"id", ns="xmi") == xmiId:
            return e

def sysml_Requirement(e):
    trace()    
    baseClass = getBase(getAttribute(e,"base_Class"))
    if baseClass is not None:
        debug("Found SysML:Requirement")
        REQUIREMENT(identifier = getAttribute(baseClass,"id", ns="xmi"), 
               title = getAttribute(baseClass,'name'),
               text = getAttribute(e,'Text'))   
    else:
        log(str_bad("Unable to process SysML:Requirement"),": xmi:id=", getAttribute(e,"id"))
def sysml_functionalRequirement(e):
    trace()
    
    baseClass = getBase(getAttribute(e,"base_Class"))
    if baseClass is not None:
        debug("Found SysML:functionalRequirement")
        REQUIREMENT(identifier = getAttribute(baseClass,"id", ns="xmi"), 
               title = getAttribute(baseClass,'name'),
               text = getAttribute(e,'Text'))        
    else:
        log(str_bad("Unable to process SysML:_functionalRequirement"),": xmi:id=", getAttribute(e,"id"))
def sysml_DeriveReqt(e):
    trace()
    
    baseAbstraction = getBase(getAttribute(e,"base_Abstraction"))
    source = baseAbstraction.find("client")
    destination = baseAbstraction.find("supplier")
    
    if baseAbstraction is not None:
        debug("Found SysML:DeriveReqt")
        REQUIREMENT(identifier=getAttribute(source,"idref","xmi"), 
                    satisfies_identifier = getAttribute(destination,"idref","xmi"))
    else:
        log(str_bad("Unable to process SysML:DeriveReqt"),": xmi:id=", getAttribute(e,"id"))
def sysml_itemflow(e):
    trace()
    
    baseInformationFlow = getBase(getAttribute(e,"base_InformationFlow"))
    source = baseInformationFlow.find("informationSource")
    destination = baseInformationFlow.find("informationTarget")
    if baseInformationFlow is not None and source is not None and destination is not None:
        debug("Found SysML:Itemflow")
        debug(getAttribute(baseInformationFlow,"id", ns="xmi"))
        debug(getAttribute(destination,"idref",ns="xmi"))
        debug(getAttribute(source,"idref",ns="xmi"))
        INTERFACE(identifier=getAttribute(baseInformationFlow,"id", ns="xmi"), 
                  title = getAttribute(baseInformationFlow,"name"),
                  destination_identifier = getAttribute(destination,"idref",ns="xmi"),
                  source_identifier =getAttribute(source,"idref",ns="xmi"))
    else:
        log(str_bad("Unable to process SysML:Itemflow"),": xmi:id=", getAttribute(e,"id"))
def sysml_Block(e):
    trace()
    # Identify the basic SYSTEM 
    # sysmlBlock -> baseClass
    baseClass = getBase(getAttribute(e,"base_Class"))
    if baseClass is not None:
        debug("Found SysML:Block")
        SYSTEM(identifier = getAttribute(baseClass,"id", ns="xmi"), 
               title = getAttribute(baseClass, 'name'))
    else:
        log(str_bad("Unable to process SysML:Block"),": xmi:id=", getAttribute(e,"id"))
    # Identify the SYSTEM parts
    # baseClass -> ownedAttribute(type=uml:property)-type
    for oa in baseClass.findall("ownedAttribute"):
        if getAttribute(oa,"type",ns="xmi") =="uml:Property":
             childSystemId = getAttribute(oa,"type")
             if childSystemId is not None:
                 debug("  Found Composite Part")
                 SYSTEM(identifier = childSystemId ,
                        partOf_identifier = getAttribute(baseClass,"id", ns="xmi"))


def getQualifiedTag(tag ,ns=None, e=__xmlroot__):
    '''
    Helper function to get a fully qualified tag name,
    if the optional ns attribute is provided then a name space is included in the string
    the namespace is it is looked from the element "e" if provide, otherwise the xml root is used. 
    '''
    global __xmlroot__
    if ns is not None:
        return "{<<NS>>}<<tag>>".replace("<<NS>>", __xmlroot__.nsmap[ns]).replace("<<tag>>", tag)
    else:
        return tag
        
def getroot():
    global __xmlroot__
    return __xmlroot__
    
def initialize(xmlPath):
    global __xmlroot__, handlers, __xmlpath__
    __xmlpath__ = xmlPath
    __xmlroot__ = etree.parse(xmlPath).getroot()    
    # Initialize the tag handlers.
    handlers = {getQualifiedTag("Block",ns="sysml"):sysml_Block,
                getQualifiedTag("ItemFlow",ns="sysml"):sysml_itemflow,
                getQualifiedTag("functionalRequirement",ns="sysml"):sysml_functionalRequirement,
                getQualifiedTag("Requirement",ns="sysml"):sysml_Requirement,
                getQualifiedTag("DeriveReqt",ns="sysml"):sysml_DeriveReqt}
    
    
