#!/usr/bin/env python3
import os
import json
from colorama import Fore,  Style
import multiprocessing
import os.path
DEBUG = False
def Debug(*args):
    if DEBUG:
        print(*args)


######################################
# 0.0 == confirmedDifferent
# 0.0 < assumedDifferent <= 0.5
# 0.5 < possibleSameAs <= 0.9
# 0.8 < assumedSameAs < 1.0
# 1.0 == confirmedSameAs    
######################################

reportString = """{} / {} - {}
  Best Match:{}
  Score:{}{}{}
------------------------------------------------------------------------"""

class ResolutionEngine:
    entityList = None
    ruleList = None
    resolutions = None
    processed = 0
    sourceConnection = None
    resolvedConnection = None
    logString = ""
    
    def __init__(self, copy=True):
        self.entityList = list()
        self.ruleList = list()
      
    def __runRules__(self, eP, eS):
        Score = 1
        for ruleType, rule in self.ruleList:
            applicable, level = rule(eP, eS)
            if applicable:
                if ruleType == "Absolute":
                    return level
                else:
                    Score += level
        return Score
        
    def addEntities(self, entityUriList):
        self.entityList = entityUriList
    
    def addAbsoluteRule(self, ruleFunction):
        self.ruleList.append(["Absolute", ruleFunction])
    
    def addRelativeRule(self, ruleFunction):
        self.ruleList.append(["Relative", ruleFunction])

    def work(self, eP):
        print("Running Analysis on {}".format(eP))
        maxScore = 0
        bestMatch = None
        resolutions = {}
        Score = 0.0
        for eS in self.entityList[eP]:
            if eS!=eP:
                Score = self.__runRules__(eP,eS)
                resolutions[eS] = Score
                if Score > maxScore:
                    maxScore = Score
                    bestMatch = eS
        color = Fore.WHITE
        if Score> 2:
            color = Fore.YELLOW
        elif Score > 4:
            color = Fore.GREEN

        with open("Resolutions/"+eP.split("#")[-1]+".json", "w") as out:
            json.dump(resolutions, out, indent=4)
            
        with open("Resolutions/Summary.csv", "a") as out:
            out.write("{},{},{}\n".format(eP,bestMatch ,maxScore))
            
        print(reportString.format(len(os.listdir("Resolutions")), len(self.entityList), eP, bestMatch, color,str(maxScore),Style.RESET_ALL))

    def runAllAnalysis(self):
        
        ######################################################################
        print("Intializing Resolution Dictionary..")
        for f in os.listdir("Resolutions"):
            os.remove(os.path.join("Resolutions",f))
        with open("Resolutions/Summary.csv", "w") as out:
            out.write("Primary,Best Match,Score\n")
        print("  Initialization Done.")
        ######################################################################
        
        ######################################################################
        print("Running Analysis..")
        #for k in self.entityList.keys():
        #    self.work(k)
        print("  analyzing {} things for commonality.".format(len(self.entityList)))
        with multiprocessing.Pool() as pool:
            pool.map(self.work, self.entityList.keys())
        print("  Analysis Complete.")
        ######################################################################
        
    
