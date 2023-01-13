#!/usr/bin/env python3
import DataAccess as da
import tkinter.filedialog as fd
import shutil
import os
import os.path
DEBUG = False
def Debug(*args):
    if DEBUG:
        print(*args)
#####################################
# Queries
#####################################

#####################################
# helper Functions
#####################################
def createIngestion(decisions):
    saveLocation = fd.asksaveasfilename(filetypes=[("Manifest File","*.zip")], defaultextension =".zip")
    print("Saving Manifest File to {}".format(saveLocation))
    tempFolder = os.path.splitext(saveLocation)[0] 
    shutil.copytree("manifest_template", tempFolder)    
    
    with open(os.path.join(tempFolder, "resolutions","SAME_AS.csv"),  "w") as outfile:
        outfile.write("primary_identifier,primary_THING_type,secondary_identifier, secondary_THING_type\n")
        for p in decisions:
            #print(decisions[p] )
            if decisions[p] != 4 and decisions[p] != 5:
                for s in decisions[p]:
                    if decisions[p][s] == 2 or decisions[p][s] == 3:
                        print("Primary:{}".format(p))
                        print("Secondary:{}".format(s))
                        outfile.write('"{}","{}!","{}","{}!"\n'.format(da.getIdentifier(p), da.getType(p), da.getIdentifier(s), da.getType(s)))
    shutil.make_archive(tempFolder, 'zip', tempFolder)
    
