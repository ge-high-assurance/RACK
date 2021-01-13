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

import sys
import colorama
from colorama import Fore, Style

__LogFile__ = None
DEBUG = False
TRACE = False

def str_good(s: str) -> str:
    return Fore.GREEN + s + Style.RESET_ALL

def str_bad(s: str) -> str:
    return Fore.RED + s + Style.RESET_ALL

def str_highlight(s: str) -> str:
    return Fore.MAGENTA + s + Style.RESET_ALL

def startLog(path, append=False):
    global __LogFile__
    appendStr = {True:"a",False:"w"}
    __LogFile__ = open(path, appendStr[append])

def closeLog():
    global __LogFile__
    __LogFile__.close()
    
def trace(l=0):
    global __LogFile__
    highlight = {0:Style.RESET_ALL, 1:Fore.GREEN, 2:Fore.MAGENTA, 3:Fore.RED}
    if TRACE:
        print(highlight[l]+"TRACE:",Style.RESET_ALL, sys._getframe(1).f_code.co_filename, "Line:",sys._getframe(1).f_lineno )
        if __LogFile__ != None:
            print("TRACE:", sys._getframe(1).f_code.co_name, file=__LogFile__)   

def log(*args):
    global __LogFile__
    print("LOG:",*args)
    if __LogFile__ != None:
        print(*args, file=__LogFile__)

def debug(*args):
    global __LogFile__
    if DEBUG:
        print("DEBUG:", *args)
        if __LogFile__ != None:
            print("DEBUG:", *args, file=__LogFile__)        