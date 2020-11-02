import os

sadlCopyrightString = '''/* Copyright (c) 2020, General Electric Company, Galois, Inc.
 *
 * All Rights Reserved
 *
 * This material is based upon work supported by the Defense Advanced Research
 * Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
 *
 * Any opinions, findings and conclusions or recommendations expressed in this
 * material are those of the author(s) and do not necessarily reflect the views
 * of the Defense Advanced Research Projects Agency (DARPA).
 */
 '''


for dir, folders, files in os.walk(".."):
    #print(dir, folders, files)
    for file in files:
        if file.upper().endswith(".SADL"):
            existingString = ""
            with open(dir+"/"+file, "r") as inFile:
                existingString = inFile.read()
            if not existingString.startswith(sadlCopyrightString):
                existingString = sadlCopyrightString + existingString
            try:
                with open(dir+"/"+file, "w") as outFile:
                    outFile.write(existingString)
                    print("added copyright to:", dir+"/"+file, )
            except:
                print("unable to add copyright to:", dir+"/"+file, )