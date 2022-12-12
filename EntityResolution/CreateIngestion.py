#!/usr/bin/env python3
import DataAccess as da
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
    with open("ingest.csv",  "w") as outfile:
        outfile.write("primary_identifier,primary_THING_type,secondary_identifier, secondary_THING_type\n")
        for p in decisions:
            #print(decisions[p] )
            if decisions[p] != 4 and decisions[p] != 5:
                for s in decisions[p]:
                    if decisions[p][s] == 2 or decisions[p][s] == 3:
                        outfile.write('"{}","{}!","{}","{}!"\n'.format(da.getIdentifier(p), da.getType(p), da.getIdentifier(s), da.getType(s)))
