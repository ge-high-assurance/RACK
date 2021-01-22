# Overview
The Scraping Tool Kit (STK) is a series of python packages that provide utilities to make is easier to extract assurance evidence from existing files.  STK set up to allow a adaptable interface for quickly generating Common Data Representation (CDR) files for ingestion into RACK. The STK allows users to simply "add" evidence as it is found while processing documents, and all the tooling to curate and orgainize the information into CDR files for ingestion into RACK.  

# Installation


# Usage

```
from Evidence import createEvidenceFile, createCDR
import Evidence.Add as Add

createEvidenceFile()
Add.REQUIREMENT(identifier = "R-1", description = "System shall do something.", satisfies_identifier = "Parent-1")
Add.FILE(identifier = "SourceCodeFile", satisfies_identifier = "R-1")
Add.REQUIREMENT(identifier = "R-2", description = "System shall do something else.", satisfies_identifier = "Parent-1")
createCDR()
```

# Example Plain Text Files

# Example XMLs

# Ingesting Resulting Data
