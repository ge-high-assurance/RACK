# Overview
The Scraping Tool Kit (STK) is a series of python packages that provide utilities to make is easier to extract assurance evidence from existing files.  STK set up to allow a adaptable interface for quickly generating Common Data Representation (CDR) files for ingestion into RACK. The STK allows users to simply "add" evidence as it is found while processing documents, and all the tooling to curate and orgainize the information into CDR files for ingestion into RACK.  

# Installation


# Usage

After installing, the STK there are two modules that need to be imported into your python script in order use.

## Evidence Module
`Evidence` - Provides two critical function:

### createEvidenceFile
`Evidence.createEvidenceFile()` - Initializes an new RACK-DATA.xml for the collecting of data when the `Add` functions are called

### createCDR  
`Evidence.createCDR()` - Curates the data collected in RACK-DATA.xml and generates CDR CSV files as well as the RACK CLI import.yaml for ingesting the data.

## Evidence.Add Module
`Evidence.Add` - This module is provided all the `Add` functions for adding data the the RACK-DATA.xml.  For each class from the ontology there is a function that allows you to add a evidence record to the RACK-DATA.xml.  

### <CLASS>
  `Evidence.Add.<CLASS>` - Each function has a series of option arguments that correspond to the properties of the class. Every property is optional and are defaulted to `None`, for example:
    `def FILE(createBy_identifier=None, fileFormat_identifier=None, fileHash_identifier=None, fileParent_identifier=None, filename=None, satisfies_identifier=None, dataInsertedBy_identifier=None, description=None, generatedAtTime=None, identifier=None, invalidatedAtTime=None, title=None, wasAttributedTo_identifier=None, wasDerivedFrom_identifier=None, wasGeneratedBy_identifier=None, wasImpactedBy_identifier=None, wasRevisionOf_identifier=None)`

When this function is call the RACK-DATA.xml is populated with a Evidence Record based on the data provided.

Note: Currently there is no error checking for this but the `identifier` argument should not be treated as optional as this would generate CDR files that cannot be ingested into rack as every piece of evidence needs an `identifier`. Currently while there is no restriction in that this `identifier` is unique throughout to project, the current CDR ingestion will run into issues while looking up items during ingestion if the same identifier is used for items of different class. Therefore it is typlically best to make the identifier unique. For example of this issue if you have a `REQUIREMENT:identifier` of `R-1` and a `REVIEW:identifier` of `R-1`, if you are ingesting a `TEST` that `verifies` the `R-1` (`REQUIREMENT`), the CDR `verified_identifier` column will have issues looking up `R-1` as it is ambiguous as to if it is the `REQUIREMENT` or `REVIEW` that has the `identifier` of `R-1`.

If a `identifier` is referenced that is not defined in greater detail else where it will be ingested as the simpliest class possible, which typically will be the PROV-S classes of `ACTIVITY`, `ENTITY`, or `AGENT`.  This definition can be done before or after the reference but must be done prior to creating the CDR. For Example:

```
import Evidence
import Evidence.Add

Evidence.createEvidenceFile()
Evidence.Add.REQUIREMENT(identifier = "R-1", description = "System shall do something.", satisfies_identifier = "Parent-1")
Evidence.Add.FILE(identifier = "SourceCodeFile", satisfies_identifier = "R-1")
Evidence.Add.REQUIREMENT(identifier = "R-2", description = "System shall do something else.", satisfies_identifier = "Parent-1")
Evidence.createCDR()
```
This will create a `ENTITY` for `PARENT-1`.

```
import Evidence
import Evidence.Add

Evidence.createEvidenceFile()
Evidence.Add.REQUIREMENT(identifier = "R-1", description = "System shall do something.", satisfies_identifier = "Parent-1")
Evidence.Add.FILE(identifier = "SourceCodeFile", satisfies_identifier = "R-1")
Evidence.Add.REQUIREMENT(identifier = "R-2", description = "System shall do something else.", satisfies_identifier = "Parent-1")
Evidence.Add.REQUIREMENT(identifier = "Parent-1")
Evidence.createCDR()
```
This will create `PARENT-1` as a `REQUIREMENT`.

```
import Evidence
import Evidence.Add

Evidence.createEvidenceFile()
Evidence.Add.REQUIREMENT(identifier = "R-1", description = "System shall do something.", satisfies_identifier = "Parent-1")
Evidence.Add.FILE(identifier = "SourceCodeFile", satisfies_identifier = "R-1")
Evidence.Add.REQUIREMENT(identifier = "R-2", description = "System shall do something else.", satisfies_identifier = "Parent-1")
Evidence.Add.SPECIFICATION(identifier = "Parent-1")
Evidence.createCDR()
```
This will create `PARENT-1` as a `SPECIFICATION`.

```
import Evidence
import Evidence.Add

Evidence.createEvidenceFile()
Evidence.Add.REQUIREMENT(identifier = "R-1", description = "System shall do something.", satisfies_identifier = "Parent-1")
Evidence.Add.FILE(identifier = "SourceCodeFile", satisfies_identifier = "R-1")
Evidence.Add.REQUIREMENT(identifier = "R-2", description = "System shall do something else.", satisfies_identifier = "Parent-1")
Evidence.Add.SPECIFICATION(identifier = "Parent-1")
Evidence.Add.REQUIREMENT(identifier = "Parent-1")
Evidence.createCDR()
```
This will result in an ingestion error `PARENT-1` for the `satisfies_identifier` could be either `SPECIFICATION` or `REQUIREMENT`.

# Example Plain Text Files

Ingesting of Test Files is entirely up to the user on how the data if formatted and how they are processing it.  This example is created just to show the how the STK can be used, it is not the only way.  
Example File REQs.txt: 
```
[REQ-1] - System shall do something.
ParentRequirement:{Parent-1}
SourceCode:{SourceCodeFile1,SourceCodeFile2}

[REQ-2] - System shall do something else.
ParentRequirement:{Parent-1,Parent-2}
SourceCode:{SourceCodeFile}
```



```
import Evidence
import Evidence.Add

def ingest(filePath):
  with open(filePath, "r") as txtFile:
    lastReqId = None
    for l in txtFile.readlines():
      if l.startwith("["):
        # Square bracket undicates a requirement
        reqId, reqDesc = l.split("-")
        reqId = reqId.lstrip().rstrip()
        reqDesc = reqDesc.lstrip().rstrip()
        Evidence.Add.REQUIREMENT(identifier = reqId, description = reqDesc)
        lastReqId = reqId
      elif l.startswith("ParentRequirement"):
        # Found Parent Requirement List
        start = line.find("{")
        end = line.rfind("}")
        parIds = line[start,end].split(",")
        for pId in parIds:
          Evidence.Add.REQUIREMENT(identifier = lastReqId, satisfies_identifier = pId)
          Evidence.Add.REQUIREMENT(identifier = pId)
      elif l.startswith("SourceCode"):
        # Found Parent Requirement List
        start = line.find("{")
        end = line.rfind("}")
        sourceIds = line[start,end].split(",")
        for sId in sourceIds:
          Evidence.Add.FILE(identifier = sId, satisfies_identifier = lastReqId)        

if __name__=="__main__":
  Evidence.createEvidenceFile()
  ingest("REQs.txt")
  Evidence.createCDR()
```



# Example XMLs

# Ingesting Resulting Data

# Updating STK
