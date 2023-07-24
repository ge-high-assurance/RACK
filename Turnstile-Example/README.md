# Turnstile-Example Explained

The purpose of
[Turnstile-Example](https://github.com/ge-high-assurance/RACK/wiki/Turnstile-Example)
is to demonstrate the following:

* ontology overlay
* CDR usage and instance data
* data ingestion package
* ingestion via owl (i.e., sadl instance file)

The subdirectory
[Turnstile-IngestionPackage](Turnstile-IngestionPackage) contains all
the Turnstile data and is ready to be loaded into RACK.  Step by step
instructions are in the
[README](Turnstile-IngestionPackage/README.md).

## Ontology Overlay

The Turnstile overlay is created in a file called
[GE.sadl](../overlays/GE-Ontology/ontology/GE.sadl).  For the ARCOS
program, overlays are created by individual performers and are named
after the prime.  For example, the Turnstile ontology is created by
the GE-Galois team.  The overlay file is located in a directory called
[GE-Ontology](../overlays/GE-Ontology/) (where GE is the prime).  The
structure and content of the directory is as follows:

```text
GE-Ontology
├── ImplicitModel
│   ├── SadlBuiltinFunctions.sadl
│   └── SadlImplicitModel.sadl
├── OwlModels
│   ├── CPS.owl
│   ├── GE.owl
│   ├── SadlBaseModel.owl
│   ├── SadlBuiltinFunctions.owl
│   ├── SadlImplicitModel.owl
│   ├── SadlListModel.owl
│   ├── configuration.rdf
│   ├── model.yaml
│   └── ont-policy.rdf
├── README.md
├── manifest.yaml
└── ontology
    ├── CPS.sadl
    └── GE.sadl
```

This structure is suited for using the [SADL
IDE](https://github.com/SemanticApplicationDesignLanguage/sadl) to
create the ontology.  There are 3 subdirectories:

1. ImplicitModel - contains defintion of built-in functions and
   classes used in SADL
2. ontology - contains the .sadl files which comprise the overlay
3. OwlModels - contains the .owl files which are automatically
   translated from the .sadl overlay files.  Also contains the
   model.yaml file which lists which .owl files to load.  (This will
   become clearer in later steps.)

Let's take a look at the
[GE.sadl](../overlays/GE-Ontology/ontology/GE.sadl) file.

```text
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
12
13 //Model Location: DevelopmentPlan.02-SystemDevelopement.01-SystemRequirements.SystemRequirementsDefinition
14 SystemRequirementsDefinition is a type of REQUIREMENT_DEVELOPMENT.
15  wasInformedBy of SystemRequirementsDefinition only has values of type DevelopSystemArchitecture.
```

In line 1, the convension for the file uri is
"http://arcos.projectName/primeName".  The alias can be any abbrevated
name you want.  Lines 2 to 11 imports RACK's core ontology.

Lines 13 to 15 are examples of the overlay.  Notice how
`SystemRequirementsDefinition` is the name of the class we chose to
use in our Turnstile overlay, which is a type of
`REQUIREMENT_DEVELOPMENT` from the core ontology.  The property
`wasInformedBy` describes `SystemRequirementsDefinition` with values
of type `DevelopSystemArchitecture`, which is defined further down in
the GE.sadl file.

Now let's look at the
[manifest.yaml](../overlays/GE-Ontology/manifest.yaml) for the
overlay.

```yaml
name: GE-Ontology
footprint:
  model-graphs:
  - http://rack001/model
steps:
- manifest: ../../RACK-Ontology/manifest.yaml
- model: OwlModels/model.yaml
```

This file indexes all the components of the overlay.  In this case the
overlay is just an extension to the model and a dependency on the core
RACK ontology.  Other overlays might also contain additional data or
nodegroups.

## CDR usage and instance data

RACK ships with a set of nodegroups and CSV templates, which we call
Common Data Representation (CDR), that covers each data type in the
ontology.  Data providers collect evidence, matches it to the schema
by using the CDR template and the associated nodegroup, and ingests
the evidence.  CDR templates and nodegroups need to be copied from the
RACK-in-a-Box to your local drive using
[setup-owl.sh](../cli/setup-owl.sh).  The Turnstile-IngestionPackage
contains CSV files that follow the CDR templates.  For example, the
HazardAssessment directory contains files with hazard instance data.
This would be used with the nodegroup ingest\_HAZARD that maps the
columns and the properties in the nodegroup.  Notice that if an object
in the nodegroup is optional then the entry can be left blank (or an
entire column can be eliminated).

|identifier|dataInsertedBy\_identifier|definition|source\_identifier|wasDerivedFrom\_identifier|
|---|---|---|---|---|
|H-1|TurnstileIngestion-HazardAssessment|System Crash|Turnstile||
|H-1.1|TurnstileIngestion-HazardAssessment|Integer Under Flow||H-1|
|H-1.2|TurnstileIngestion-HazardAssessment|Integer Over Flow||H-1|
|H-2|TurnstileIngestion-HazardAssessment|Park Exceeds Capacity|Turnstile||

Note that in the CDR directory, templates based on the core ontology
are named ingest\_CLASS.csv; templates from ontology overlays are
named ingest\_projectName\_Class.csv.  Note also the ingestion
nodegroup directory has subdirectories for each performer overlay
named as arcos.performerProjName.

Back in the HazardAssessment directory, notice there are CSV file
names \*1.csv, and \*2.csv.  The files named \*1.csv are ingested
first and defines the object in RACK with just the identifier.  The
files named \*2.csv are ingested second and defines the rest of the
data.  Ingesting it this order prevents circularity and ensures that
the ingested objects are properly referred to.  Below is the
[data.yaml](Turnstile-IngestionPackage/HazardAssessment/data.yaml)
file for HazardAssessment.

```sh
 1 data-graph: "http://rack001/turnstiledata"
 2 ingestion-steps:
 3 #Phase1: Identifiers Only
 4 - {class: "http://arcos.rack/HAZARD#HAZARD", csv: "HAZARD_HAZARD1.csv"}
 5 - {class: "http://arcos.rack/PROV-S#ACTIVITY", csv: "PROV_S_ACTIVITY1.csv"}
 6 - {class: "http://arcos.rack/SYSTEM#SYSTEM", csv: "SYSTEM_SYSTEM1.csv"}
 7
 8 #Phase2: All Evidence
 9 - {class: "http://arcos.rack/HAZARD#HAZARD", csv: "HAZARD_HAZARD2.csv"}
10 - {class: "http://arcos.rack/PROV-S#ACTIVITY", csv: "PROV_S_ACTIVITY2.csv"}
11 - {class: "http://arcos.rack/SYSTEM#SYSTEM", csv: "SYSTEM_SYSTEM2.csv"}
```

Notice the ordering of the ingestion steps.  Notice also in line 1
that the Turnstile data is being imported into the data graph
http://rack001/turnstiledata.

## Data Ingestion Package

It is convenient to create an [ingestion
package](https://github.com/ge-high-assurance/RACK/wiki#preparing-your-own-data)
that can be shared for others to load into RACK.  [The Scraping Tool
Kit](../ScrapingToolKit) conveniently generates such ingestion
packages.  The subdirectories in
[Turnstile-IngestionPackage](Turnstile-IngestionPackage) are generated
using the Scraping Tool Kit via the TurnstileDataCreation\*.py
scripts.  For example, the script
TurnstileIngestion\_HazardAssessment.py generates data in the
HazardAssessment subdirectory.

The [Turnstile-IngestionPackage](Turnstile-IngestionPackage) contains
all the Turnstile data and is ready to be loaded into RACK.  Step by
step instructions are in the
[README](Turnstile-IngestionPackage/README.md).

## Ingestion via Owl

It is possible to ingest raw triples into RACK (although it is not
recommended because this will bypass many of the verification checks
that happen when ingesting via nodegroups).  We provide a small
example as part of Turnstile to demonstrate the capability.  The
directory CounterApplicationUnitTesting contains a structure suited
for using the [SADL
IDE](https://github.com/SemanticApplicationDesignLanguage/sadl) to
create the instance data via owl.  The instance data is in the file
`ontology/CounterApplicationUnitTesting.sadl`.
