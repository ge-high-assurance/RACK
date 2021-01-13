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

import os.path
from Logging import *
from lxml import etree


def genericHandler(e, Config):
    '''
    Recursively loops through the root element looking for handlers defined in the Config Module
    
    '''
    trace()
    
    # Next check to see if this Tag has a Handler to call
    if e.tag in Config.handlers:
        Config.handlers[e.tag](e)
    
    # Now call the generic handler for each child
    for ce in e:
        genericHandler(ce,Config)
    return

def scrap(xmlPath, Config):
    '''
    Scraps an XML file based on the Config module provided
    
    '''
    trace()
    if not os.path.exists(xmlPath):
        return "XML file not found"    
    
    tree = etree.parse(xmlPath)        
    Config.initialize(tree.getroot())
    genericHandler(tree.getroot(),Config)
    
    return None
    