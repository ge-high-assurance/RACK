# Overview
The Scraping Tool Kit (STK) is a series of python packages that provide utilities to make it easier to extract assurance evidence from existing files.  STK is set up to allow an adaptable interface for quickly generating Common Data Representation (CDR) files for ingestion into RACK. The STK allows users to simply "add" evidence as it is found while processing documents, and all the tooling to curate and orgainize the information into CDR files for ingestion into RACK.  

# Installation

STK is installed by downloading the files to your local machine.  This can either be from a git clone of the whole RACK repo, or it can be just the ScrapingToolKit folder. Once the source files are downloaded installation is as simple entering `pip install .` from a command window in your `ScrapingToolKit` folder.

Dependencies:
`pip install lxml`

Note: Sometimes for linux machines that have multiple versions of Python installed the command will be `pip3 install .`

TODO: May need to add more instructions to make sure any missing dependencies are installed as well; possibly set up as a virtualenv

# Usage

After installing, the STK there are two modules that need to be imported into your python script in order to use.

## Evidence Module
`Evidence` - Provides two critical function:

### createEvidenceFile
`Evidence.createEvidenceFile()` - Initializes a new RACK-DATA.xml for the collecting of data when the `Add` functions are called.

### createCDR  
`Evidence.createCDR()` - Curates the data collected in RACK-DATA.xml and generates CDR CSV files as well as the RACK CLI import.yaml for ingesting the data.

## Evidence.Add Module
`Evidence.Add` - This module is provides all the `Add` functions for adding data in the RACK-DATA.xml.  For each class from the ontology there is a function that allows you to add an evidence record to the RACK-DATA.xml.  

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
This will result in an ingestion error. `PARENT-1` for the `satisfies_identifier` could be either `SPECIFICATION` or `REQUIREMENT`.

# Examples
## Example Plain Text

Ingesting of Text Files is entirely up to the user on how the data is formatted and how they are processing it.  This example is created just to show how the STK can be used, it is not the only way. Any processing begins by examining the data to be ingested. For this short tutorial we are going to look at a simple text file "REQs.txt"

Example File REQs.txt: 
```
[REQ1] - System shall do something.
ParentRequirement:{Parent-1}
SourceCode:{SourceCodeFile1,SourceCodeFile2}

[REQ2] - System shall do something else.
ParentRequirement:{Parent-1,Parent-2}
SourceCode:{SourceCodeFile}
```
First thing to do is to identify how the text can be processed. For a more complex file the use of RegEx might be easier, but for this example each line starts with a token that can be used to identify the data that is in the line. So this can be accomplished by simply reading through the file line by line and processing the data.  The only complication is that a record of the last requirement line identifier needs to be maintained. 

```
import Evidence
import Evidence.Add

def ingest(filePath):
  with open(filePath, "r") as txtFile:
    lastReqId = None
    for l in txtFile.readlines():
      if l.startswith("["):
        # Square bracket indicates a requirement
        reqId, reqDesc = l.split("-")
        reqId = reqId.lstrip().rstrip()
        reqDesc = reqDesc.lstrip().rstrip()
        Evidence.Add.REQUIREMENT(identifier = reqId, description = reqDesc)
        lastReqId = reqId
      elif l.startswith("ParentRequirement"):
        # Found Parent Requirement List
        start = l.find("{")
        end = l.rfind("}")
        parIds = l[start+1:end].split(",")
        for pId in parIds:
          Evidence.Add.REQUIREMENT(identifier = lastReqId, satisfies_identifier = pId)
          Evidence.Add.REQUIREMENT(identifier = pId)
      elif l.startswith("SourceCode"):
        # Found Source Code List
        start = l.find("{")
        end = l.rfind("}")
        sourceIds = l[start+1:end].split(",")
        for sId in sourceIds:
          Evidence.Add.FILE(identifier = sId, satisfies_identifier = lastReqId)        

if __name__=="__main__":
  Evidence.createEvidenceFile()
  ingest("REQs.txt")
  Evidence.createCDR()
```

## Example XMLs

STK contains additional module for helping to process XML source files.  While one could implement their own processing just as is done for other documents, the XML utilies provide a simple framework for processing XML files.

The approach in the XML is to recursively loop through all elements in the source file and check if a handlers function has been identified for that element's tag; if one has been then that handler is called; if not, processing continues through all the child elements.  To use as a example consider the xml file defined below (`reqs.xml`):

```
<Reqs>
  <Req id="R-1>
    <ParentReq id="P-1">
    <ParentReq id="P-2">
    <Source id="file1.ada">
  </Req>
  <Req id="R-2>
    <ParentReq id="P-1">
    <ParentReq id="P-2">
    <Source id="file2.ada">
  </Req>
</Reqs>
```

Customizations for the handling of XML data is created by making a python module that encasulates the uniqueness of the XML file format being processed.  Below is an example of this:
```
#!/usr/bin/env python3
import Evidence.Add as Add
from lxml import etree

__xmlroot__ = None
__xmlpath__ = None
handlers = None

def req(e):
    reqId = e.attrib["id"]
    Add.REQUIREMENT(identifier=reqId)
    for p in e.iter("ParentReq"):
      parentId = p.attrib["id"]
      Add.REQUIREMENT(identifier=parentId)
      Add.REQUIREMENT(identifier=reqId, satisfies_identifier=parentId)
    for f in e.iter("Source"):
      fileId = f.attrib["id"]
      Add.FILE(identifier=fileId)
      Add.FILE(identifier=fileId, satisfies_identifier=reqId)

def initialize(xmlPath):
    global __xmlroot__, handlers, __xmlpath__
    Add.FILE(identifier=xmlPath)
    # Initialize the tag handlers.
    handlers = {"Req", req}

```
The key parts to this are the initialize function that is called to start processing an XML, and the handlers dictionary that is initialized as part of this function. These handlers identify the tag and the handler function that should be called when that tag is encountered.

# Ingesting Resulting Data
STK ultimately produces two sets of CDR files and an import.yaml files when the `Evidence.createCDR()` function is called.  The first set of CDR files are named `<Class>1.csv` and are simply a list of all the identifiers for each class. The second set of CDR files are named `<Class>2.csv` which include all the detailed properties that were identified in the scraping process. The final file generated is an `import.yaml` for RACK CLI. This file will ingest the CDR files in the correct order.

Resulting data can be ingested into RACK by using the CLI command:
`rack data import <path to generated data>/RACK-DATA/import.yaml`

# Updating STK

STK has the ability to update itself to changes or additions to the ontology. This can be done by downloading a data dictionary from the load tab in SemTK:

Once this csv file is downloaded (`table.csv`) it should be moved to `./ScrapingToolKit/AutoGeneration`. Running `GenFile.py` will generate updated `\Evidence\Add.py` and `\Evidence\RACK-DATA.xsd`.  To use these updated file simply re-install the STK using `pip install .` as described in the installation section.

Note: this is dependent on the autogenerated CDR files and this functionality is not yet included in the SemTK but will be in the future so right now the CDR nodegroups would mis-match and will have compatiblity issues. This means that the user may have to manually create the CDR nodegroups that match the generated CSVs.
