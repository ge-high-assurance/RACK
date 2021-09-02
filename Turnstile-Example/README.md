# Turnstile-Example Directory Explained
The purpose this directory is to demonstrate the following using our [Turnstile example](https://github.com/ge-high-assurance/RACK/wiki/Turnstile-Example):
* ontology overlay 
* CDR usage and instance data
* data ingestion package along with a Load-TurnstileData.sh script
* ingestion via owl (i.e., sadl instance file)

## Ontology Overlay
The Turnstile overlay is created in a file called [GE.sadl](https://github.com/ge-high-assurance/RACK/blob/TurnstileUpdate/GE-Ontology/ontology/GE.sadl). For the ARCOS program, overlays are created by individual performers and are named after the prime. For example, the turnstile ontology is created by the GE-Galois team. The overlay file is located in a directory called [GE-Ontology](https://github.com/ge-high-assurance/RACK/blob/TurnstileUpdate/GE-Ontology/) (where GE is the prime). The structure and content of this directory is as follows:
```text
GE-Ontology/
|-- ImplicitModel/
|   |-- SadlBuiltinFunctions.sadl
|   |-- SadlImplicitModel.sadl
|-- ontology/
|   |-- GE.sadl
|-- OwlModels/
|   |-- GE.owl
|   |-- import.yaml
|-- setup-GE.sh
```
This structure is suited for using the [SADL IDE](https://github.com/SemanticApplicationDesignLanguage/sadl) to create the ontology. There are 3 subdirectories:
1) ImplicitModel - contains defintion of built-in functions and classes used in SADL
2) ontology - contains the .sadl file which is the overlay
3) OwlModels - contains the .owl file which is automatically translated from the .sadl overlay file. Also contains the import.yaml file which lists what .owl file to load. (This will become clearer in later steps.)

Let's take a look at the [GE.sadl](https://github.com/ge-high-assurance/RACK/blob/TurnstileUpdate/GE-Ontology/ontology/GE.sadl) file.
```
 1 uri "http://arcos.turnstile/GE" alias turnstile.
 2 import "http://arcos.rack/DOCUMENT".
 3 import "http://arcos.rack/SYSTEM".
 4 import "http://arcos.rack/SOFTWARE".
 5 import "http://arcos.rack/REQUIREMENTS".
 6 import "http://arcos.rack/REVIEW".
 7 import "http://arcos.rack/TESTING".
 8 import "http://arcos.rack/HAZARD".
 9 import "http://arcos.rack/ANALYSIS".
10 import "http://arcos.rack/PROCESS".
11 import "http://arcos.rack/AGENTS".
12 import "http://common/DO-178C".
13
14 //Model Location: DevelopmentPlan.02-SystemDevelopement.01-SystemRequirements.SystemRequirementsDefinition
15 SystemRequirementsDefinition is a type of REQUIREMENT_DEVELOPMENT.
16  turnstile:wasInformedBy of SystemRequirementsDefinition has values of type DevelopSystemArchitecture.
```
In line 1, the convension for the file uri is "http://arcos.projectName/primeName". The alias can be any abbrevated name you want. Lines 2 to 11 imports RACK's core ontology. Line 12 imports the objectives from the DO-178C standard, which we use in our Turnstile example.

Lines 15 to 16 are examples of the overlay. Notice how `SystemRequirementsDefinition` is the name of the lass we chose to use in our Turnstile overlay, which is a type of `REQUIREMENT_DEVELOPMENT` from the core ontology. The property `wasInformedBy` (qualified with `turnstile:`) describes `SystemRequirementsDefinition` with values of type `DevelopSystemArchitecture`, which is defined further down in the GE.sadl file. Note: not all properties need to be qualified. The SADL IDE can help identify these.

Now let's look at the [setup-GE.sh](https://github.com/ge-high-assurance/RACK/blob/TurnstileUpdate/GE-Ontology/setup-GE.sh) script.
```sh
23 # Turnstile model & auto-generated nodegroups
24 rack model import "$BASEDIR"/OwlModels/import.yaml
25 rack nodegroups import "$BASEDIR"/../nodegroups/ingestion/arcos.turnstile
```
Line 24 uses the [CLI](https://github.com/ge-high-assurance/RACK/tree/master/cli) to import the owl model specified in import.yaml. In this case, it lists GE.owl. Line 25 uses the imports the ingestion nodegroups based on the GE-Ontology overlay. Note the name of the ingestion nodegroup directory is taken from the uri of the ontology overlay file: `arcos.projectName`.

## CDR usage and instance data
RACK ships with a set of nodegroups and CSV templates, which we call Common Data Representation (CDR), that covers each data type in the ontology. Data providers collect evidence, matches it to the schema by using the [CDR template](https://github.com/ge-high-assurance/RACK/tree/TurnstileUpdate/nodegroups/CDR), and the associated [nodegroup](https://github.com/ge-high-assurance/RACK/tree/TurnstileUpdate/nodegroups/ingestion/arcos.rack), and ingests the evidence. The Turnstile-IngestionPackage contains CSV files that follow the CDR templates. For example, the HazardAssessment directory contains files with hazard instance data. This would be used with the nodegroup ingest_HAZARD that maps the columns and the properties in the nodegroup. Notice that if an object in the nodegroup is optional then the entry can be left blank (or an entire column can be eliminated).

|identifier|dataInsertedBy_identifier|definition|source_identifier|wasDerivedFrom_identifier|
|---|---|---|---|---|
|H-1|TurnstileIngestion-HazardAssessment|System Crash|Turnstile||	
|H-1.1|TurnstileIngestion-HazardAssessment|Integer Under Flow||H-1|
|H-1.2|TurnstileIngestion-HazardAssessment|Integer Over Flow||H-1|
|H-2|TurnstileIngestion-HazardAssessment|Park Exceeds Capacity|Turnstile||	

Note that in the CDR directory, templates based on the core ontology are named ingest_CLASS.csv; templates from ontology overlays are named ingest_projectName_Class.csv. Note also the ingestion nodegroup directory has subdirectories for each performer overlay named as arcos.performerProjName.

Back in the HazardAssessment directory, notice there are CSV file names *1.csv, and *2.csv. The files named *1.csv are ingested first and defines the object in RACK with just the identifier. The files named *2.csv are ingested second and defines the rest of the data. Ingesting it this order prevents circularity and ensures that the ingested objects are properly referred to. Below is the [import.yaml](Turnstile-IngestionPackage/HazardAssessment/import.yaml) file for HazardAssessment.
```sh
 1 data-graph: "http://rack001/turnstiledata"
 2 ingestion-steps:
 3 #Phase1: Identifiers Only
 4 - {nodegroup: "ingest_ACTIVITY", csv: "ACTIVITY1.csv"}
 5 - {nodegroup: "ingest_HAZARD", csv: "HAZARD1.csv"}
 6 - {nodegroup: "ingest_SYSTEM", csv: "SYSTEM1.csv"}
 7
 8 #Phase2: All Evidence
 9 - {nodegroup: "ingest_ACTIVITY", csv: "ACTIVITY2.csv"}
10 - {nodegroup: "ingest_HAZARD", csv: "HAZARD2.csv"}
11 - {nodegroup: "ingest_SYSTEM", csv: "SYSTEM2.csv"}
```
Notice the ordering of the ingestion steps. Notice also in line 1 that the Turnstile data is being imported into the data graph http://rack001/turnstiledata.

## data ingestion package
It is convenient to create an [ingestion package](https://github.com/ge-high-assurance/RACK/wiki#preparing-your-own-data) that can be shared for others to load into RACK. [The Scraping Tool Kit](https://github.com/ge-high-assurance/RACK/tree/master/ScrapingToolKit) conveniently generates such ingestion packages. The subdirectories in Turnstile-IngestionPackage are generated using the Scraping Tool Kit. For example, the script TurnstileIngestion_HazardAssessment.py generates data in the HazardAssessment subdirectory. The shell script, Load-TurnstileData.sh, collectively loads all the data provided in Turnstile-IngestionPackage.

## ingestion via owl 
It is possible to ingest raw triples into RACK (although it is not recommended because this will bypass many of the verification checks that happen when ingesting via nodegroups). We provide a small example as part of Turnstile to demonstrate the capability. The directory CounterApplicationUnitTesting contains a structure suited for using the [SADL IDE](https://github.com/SemanticApplicationDesignLanguage/sadl) to create the instance data via owl. The instance data is in the file `ontology/CounterApplicationUnitTesting.sadl`.
