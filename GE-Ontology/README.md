# GE's Ontology Overlay used in the Turnstile model
The Turnstile overlay is created in a file called [GE.sadl](ontology/GE.sadl). For the ARCOS program, overlays are created by individual performers and are named after the prime. For example, the turnstile ontology is created by the GE-Galois team. The structure and content of this directory is as follows:
```text
GE-Ontology/
|
|-- ImplicitModel/
|   |-- SadlBuiltinFunctions.sadl
|   |-- SadlImplicitModel.sadl
|
|-- ontology/
|   |-- GE.sadl
|
|-- OwlModels/
|   |-- GE.owl
|   |-- import.yaml
|
|-- setup-GE.sh
```
This structure is suited for using the [SADL IDE](https://github.com/SemanticApplicationDesignLanguage/sadl) to create the ontology. There are 3 subdirectories:
1) ImplicitModel - contains defintion of built-in functions and classes used in SADL
2) ontology - contains the .sadl file which is the overlay
3) OwlModels - contains the .owl file which is automatically translated from the .sadl overlay file. Also contains the import.yaml file which lists what .owl file to load. (This will become clearer in later steps.)

Let's take a look at the [GE.sadl](ontology/GE.sadl) file.
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

Now let's look at the [setup-GE.sh](setup-GE.sh) script.
```sh
23 # Turnstile model & auto-generated nodegroups
24 rack model import "$BASEDIR"/OwlModels/import.yaml
25 rack nodegroups import "$BASEDIR"/../nodegroups/ingestion/arcos.turnstile
```
Line 24 uses the [CLI](../cli) to import the owl model specified in import.yaml. In this case, it lists GE.owl. Line 25 uses the imports the ingestion nodegroups based on the GE-Ontology overlay. Note the name of the ingestion nodegroup directory is taken from the uri of the ontology overlay file: `arcos.projectName`.