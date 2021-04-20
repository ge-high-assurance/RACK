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

from Evidence.Add import COMPONENT, FILE
from Logging import *
from lxml import etree

__xmlroot__ = None
__xmlpath__ = None
__sourceType__ = None
__fileIdentifier__=None

handlers = None
    
def getAttribute(e, attr ,ns=None):
    trace()
    # helper function to get the attribute, if a namespace is provided this name space will be looked up from the xml
    if ns is not None:
        return e.get("{<<NS>>}<<attr>>"
                     .replace("<<NS>>", e.nsmap[ns])
                     .replace("<<attr>>", attr))
    else:
        return e.get(attr)

def getQualifiedTag(tag ,ns=None, e=__xmlroot__):
    '''
    Helper function to get a fully qualified tag name,
    if the optional ns attribute is provided then a name space is included in the string
    the namespace is it is looked from the element "e" if provide, otherwise the xml root is used. 
    '''
    global __xmlroot__
    if ns in __xmlroot__.nsmap:
        return "{<<NS>>}<<tag>>".replace("<<NS>>", __xmlroot__.nsmap[ns]).replace("<<tag>>", tag)
    else:
        return tag
        
def getParentNamespace(e): 
    parent = None   
    if e !=None:
        parent = e.getparent()
        while parent.tag != getQualifiedTag("namespace"):
            parent = parent.getparent()
            if parent == None:
                break
    return parent

def getFQName(e):
    name = e.find(getQualifiedTag("name"))
    parent = getParentNamespace(e)
    if parent != None:
        if getFQName(parent) != None:
            s = getFQName(parent) + "."+str(name.text)
            return s
        else:
            return name.text
    else:
        return name.text
def getFQPackage():
    global __xmlroot__
    package = __xmlroot__.find(getQualifiedTag("package"))
    name = package.find(getQualifiedTag("name"))
    nameStr = str(etree.tostring(name).decode('utf-8'))\
                  .replace("<name>","")\
                  .replace("</name>","")\
                  .replace("<operator>","")\
                  .replace("</operator>","")\
                  .replace('<name xmlns="http://www.srcML.org/srcML/src">',"")\
                  .replace(';',"")

    return nameStr
def packageJava(e):
    subcomponentOf_identifierStr = None
    lastDot = getFQPackage().rfind(".")
    if lastDot !=-1:
        subcomponentOf_identifierStr ="Source:"+getFQPackage()[0:lastDot]
    COMPONENT(fileParent_identifier = __fileIdentifier__,
              identifier="Source:"+getFQPackage(),
              title=getFQPackage(),
              subcomponentOf_identifier = subcomponentOf_identifierStr,
              componentType_identifier = e.tag.split("}")[-1])

def classJava(e):
    global __fileIdentifier__
    subcomponentOf_identifierStr = "Source:"+getFQPackage()
    name = e.find(getQualifiedTag("name"))
    #log(str(etree.tostring(e,pretty_print=True).decode('utf-8')))
    if name is not None:
        if name.text is not None:
            COMPONENT(fileParent_identifier = __fileIdentifier__,
                      identifier="Source:"+getFQPackage()+"."+name.text,
                      title=getFQPackage()+"."+name.text,
                      subcomponentOf_identifier = subcomponentOf_identifierStr,
                      componentType_identifier = e.tag.split("}")[-1])

def componentCpp(e):
    global __fileIdentifier__
    subcomponentOf_identifierStr = None
    if getParentNamespace(e) is not None:
        subcomponentOf_identifierStr = "Source:"+getFQName(getParentNamespace(e))
    COMPONENT(fileParent_identifier = __fileIdentifier__,
              identifier="Source:"+getFQName(e),
              title=getFQName(e),
              subcomponentOf_identifier = subcomponentOf_identifierStr,
              componentType_identifier = e.tag.split("}")[-1])
def getroot():
    global __xmlroot__
    return __xmlroot__
    
def initialize(xmlPath):
    global __xmlroot__, handlers, __xmlpath__, __sourceType__, __fileIdentifier__ 
    __xmlpath__ = xmlPath
    __xmlroot__ = etree.parse(xmlPath).getroot()
    __sourceType__ = getAttribute(__xmlroot__,"language")  
    __fileIdentifier__ =  getAttribute(__xmlroot__,"filename")
    FILE(identifier = __fileIdentifier__)
    # Initialize the tag handlers.
    if __sourceType__ == "C++":
        handlers = {getQualifiedTag("namespace"):componentCpp,
                    getQualifiedTag("class"):componentCpp}
    elif __sourceType__ == "Java":
        handlers = {getQualifiedTag("class"):classJava,
                    getQualifiedTag("package"):packageJava,}
    else:
        log(str_bad("Handlers for "+__sourceType__ + " not defined."))
        handlers = {}

    
    
