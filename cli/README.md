# RACK CLI

<!--
NOTE: Copy the file between here and copyright to the wiki before each release:
https://github.com/ge-high-assurance/RACK/wiki/RACK-CLI
-->

<!-- markdownlint-disable first-line-heading -->
<!-- markdownlint-disable line-length -->

The RACK command-line interface (the `rack` program) can

- [initialize](#initialize-rack-in-a-box) RACK-in-a-Box
- [import data](#import-data) into RACK-in-a-Box from CSV files
- [export data](#export-data) from RACK-in-a-Box to CSV files

To do its work, the rack program uses the Node Execution Group REST
API which is documented in the [REST API Swagger
Demo](https://github.com/ge-high-assurance/RACK/wiki/REST-API-Swagger-Demo)
and [REST
cookbook](https://github.com/ge-semtk/semtk/wiki/REST-API) wiki
pages.

## Install dependencies

The rack program requires the
[semtk-python3](https://github.com/ge-semtk/semtk-python3) package and
other requirements listed in `requirements.txt`.

We recommend installing these dependencies in an isolated virtual
environment to ensure reproducibility of results.

<!--
Note for documentation authors: These instructions should be kept in sync with
the RACK-Box README.
-->

```shell
python -m venv venv
source venv/bin/activate
pip install --force -r requirements.txt
python3 setup.py install
```

If running on Windows, [GitBash](https://gitforwindows.org/) can be used with the following commands.

```shell
virtualenv venv
source venv/Scripts/activate
pip install --force -r requirements.txt
python setup.py install
```

## Initialize RACK-in-a-Box

We have written a shell script called `setup-rack.sh` which will call
the `rack` program to initialize a RACK-in-a-Box instance with the
RACK core ontology model.  It is assumed that you are
still in the isolated Python virtual environment and running a
RACK-in-a-Box instance in a Docker container on `localhost`.  Here is
how to run `setup-rack.sh` and what its output may look like:

```shell
(venv) $ ./setup-rack.sh
Clearing graph
Success Update succeeded
Ingesting ../RACK-Ontology/OwlModels/AGENTS.owl...               OK
Ingesting ../RACK-Ontology/OwlModels/ANALYSIS.owl...             OK
Ingesting ../RACK-Ontology/OwlModels/CONFIDENCE.owl...           OK
Ingesting ../RACK-Ontology/OwlModels/DOCUMENT.owl...             OK
Ingesting ../RACK-Ontology/OwlModels/FILE.owl...                 OK
Ingesting ../RACK-Ontology/OwlModels/HAZARD.owl...               OK
Ingesting ../RACK-Ontology/OwlModels/MODEL.owl...                OK
Ingesting ../RACK-Ontology/OwlModels/PROCESS.owl...              OK
Ingesting ../RACK-Ontology/OwlModels/PROV-S.owl...               OK
Ingesting ../RACK-Ontology/OwlModels/REQUIREMENTS.owl...         OK
Ingesting ../RACK-Ontology/OwlModels/REVIEW.owl...               OK
Ingesting ../RACK-Ontology/OwlModels/SOFTWARE.owl...             OK
Ingesting ../RACK-Ontology/OwlModels/SYSTEM.owl...               OK
Ingesting ../RACK-Ontology/OwlModels/TESTING.owl...              OK
Storing nodegroups...                                       OK
Storing nodegroups...                                       OK
```
We have written a separate shell script called `setup-arcos.sh` which will call
the `rack` program to load RACK-in-a-Box instance with the
ARCOS program ontology overlays. Here is what its output may look like:

```shell
(venv) $ ./setup-arcos.sh
Ingesting ../Boeing-Ontology/OwlModels/Boeing.owl...             OK
Storing nodegroups...                                            OK
Ingesting ../GrammaTech-Ontology/OwlModels/GrammaTech.owl...     OK
Storing nodegroups...                                            OK
Ingesting ../LM-Ontology/OwlModels/LM.owl...                     OK
Storing nodegroups...                                            OK
Ingesting ../SRI-Ontology/OwlModels/SRI.owl...                   OK
Storing nodegroups...                                            OK
Ingesting ../STR-Ontology/OwlModels/STR.owl...                   OK
Storing nodegroups...                                            OK
```

## How to use the rack program

The rack program accepts the following command line arguments:

```text
usage: rack [-h] [--base-url BASE_URL] [--triple-store TRIPLE_STORE] [--log-level LOG_LEVEL] {data,model,nodegroups} ...

RACK in a Box toolkit

positional arguments:
  {data,model,nodegroups}
    data                Import or export CSV data
    model               Interact with SemTK model
    nodegroups          Interact with SemTK nodegroups

optional arguments:
  -h, --help            show this help message and exit
  --base-url BASE_URL   Base SemTK instance URL
  --triple-store TRIPLE_STORE
                        Override Fuseki URL
  --log-level LOG_LEVEL
                        Assign logger severity level
```

The `rack` command is split into three subcommands: `data`, `model`,
and `nodegroups`. Each of these subcommands offers its own help
listing. For example try `rack data --help` for more information about
the flags available when interacting with the data store.

The `model` subcommand is used to load the ontology itself. This is
typically only done at the initial configuration step for RACK.

The `nodegroups` subcommand is used to import and export nodegroups
from SemTK. This is useful both for initial configuration and also for
persisting your generated nodegroups across RACK instances.

The `data` subcommand is used to import CSV and OWL data files using
the RACK ontology as well as exporting CSV files using nodegroups
stored in SemTK.

## Data Ingestion Configuration file format

The import configuration files are YAML files that specify the target
`data-graph` and `ingestion-steps`. These files will be the argument
to a `rack data import` command.

`data-graph` is a graph identifier URL. This URL allows multiple
datasets to be stored in the same triple store.

`ingestion-steps` takes an ordered list of CSV and OWL file imports
used to populate the graph.

CSV files can be ingested by adding list elements containing a `csv`
path and a `nodegroup` that lists an existing nodegroup ID in
RACK. CSV paths are resolved relative to the configuration file.

OWL files can be ingested by adding list elements containing an `owl`
path to the OWL file.

```text
data-model: "http://rack001/data"
ingestion-steps:
- {nodegroup: "ingest_SYSTEM",    csv: "SYSTEM.csv"}
- {nodegroup: "ingest_INTERFACE", csv: "INTERFACE.csv"}
- {owl: "example.owl"}
```

## Overriding default RACK URLs

Connecting to RACK requires knowledge of the SemTK and Fuseki
URLs. The RACK cli assumes that these URLs will be at their default
locations on `localhost`. These URLs can be overridden using either
command-line flags or environment variables.

### SemTK URL

- Default value: `http://localhost`
- Override flag: `--base-url`
- Override environment variable: `BASE_URL`

### Fuseki URL

- Default value: *Base URL* + `:3030/RACK`
- Override flags: `--triple-store`
- Override environment variable: `TRIPLE_STORE`

## Example invocations

These examples uses the virtual environment as defined in the
*Installing Dependencies* section above.

### Import data

This example populates the *Turnstile* example into a RACK-in-a-Box
instance running in a Docker container on `localhost`:

```shell
$ source venv/bin/activate
(venv) $ rack model import ../Turnstile-Ontology/99-Utils/import.yaml
Ingesting ../Turnstile-Ontology/99-Utils/../OwlModels/DevelopmentPlan.owl...   OK

(venv) $ rack nodegroups import ../Turnstile-Ontology/99-Utils/NodeGroups
Storing nodegroups...

(venv) $ rack data import --clear ../RACK-Ontology/OwlModels/DO-178C.yaml
Clearing graph
Success Update succeeded
Ingesting DO-178C.owl                     OK

(venv) $ rack data import --clear ../Turnstile-Ontology/99-Utils/Data/Model.yaml
Clearing graph
Success Update succeeded
Loading Ingest-DataAndControlCouple...             OK Records: 24
Loading Ingest-DataDictionary...                   OK Records: 5
Loading Ingest-Engineer...                         OK Records: 3
Loading Ingest-HAZARD...                           OK Records: 4
Loading Ingest-HighLevelRequirements...            OK Records: 3
Loading Ingest-LowLevelRequirements...             OK Records: 13
Loading Ingest-Objective...                        OK Records: 99
Loading Ingest-SoftwareComponentTest...            OK Records: 4
Loading Ingest-SoftwareComponentTestExecution...   OK Records: 2
Loading Ingest-SoftwareComponentTestResult...      OK Records: 8
Loading Ingest-SoftwareDesign...                   OK Records: 2
Loading Ingest-SoftwareDesignReview...             OK Records: 11
Loading Ingest-SoftwareDesignReviewArtifacts...    OK Records: 11
Loading Ingest-SoftwareRequirementsDefinition...   OK Records: 1
Loading Ingest-SoftwareRequirementsReview...       OK Records: 9
Loading Ingest-SoftwareRequirementsReviewArtifacts...OK Records: 3
Loading Ingest-SoftwareThread...                   OK Records: 6
Loading Ingest-SoftwareUnitTest...                 OK Records: 4
Loading Ingest-SoftwareUnitTestExecution...        OK Records: 2
Loading Ingest-SoftwareUnitTestResult...           OK Records: 8
Loading Ingest-SystemComponent...                  OK Records: 4
Loading Ingest-SystemInterfaceDefinition...        OK Records: 7
Loading Ingest-SystemRequirement...                OK Records: 3
Loading Ingest-SoftwareComponent...                OK Records: 3
```

### Export data

This example exports instances of the `SYSTEM` class from the
*Turnstile* example from a Rack-in-a-Box instance running in a Docker
container on `localhost`:

```shell
$ source venv/bin/activate
(venv) $ rack data export --data-graph http://rack001/data "Ingest-SystemComponent"
identifier_SystemComponent    identifier_SYSTEM
----------------------------  -------------------
Counter Application           Turnstile
Display                       Turnstile
Out Gate                      Turnstile
In Gate                       Turnstile
```

See `rack data export --help` for options, including different export
formats (such as CSV), emitting to a file, and omitting the header
row.

Runtime constraints can be specified with the `--constraint` flag.
We support the following constraint operations:

- `=` matches
- `<` less-than
- `<=` less-than or equal-to
- `>` greater-than
- `<=` greater-than or equal-to
- `~` regular expression
- `:...<>...` between
- `:...<=>...` inclusive-between

To specify a constraint you'll pass a string containing the: constraint ID,
operator, and value. Multiple constraint variables can be specified at the same time,
but you should only provide one constraint per variable.

The constraint syntax is very limited. Please do not add extra whitespace or
operators.

Examples:

```shell
# Example using exact matches
rack data export "query Requirements decomposition" \
  --data-graph http://rack001/data \
  --constraint req=HLR-1 \
  --constraint decomposition=IN-LLR-2

# Example using regular expressions
rack data export "query Requirements decomposition" \
  --data-graph http://rack001/data \
  --constraint "req~^HLR-.$" \
  --constraint "decomposition~^IN-"
```

### Count result rows

The number of results a nodegroup would generate can be obtained
using the `count` sub-command.

```shell
(venv) $ data count --data-graph http://rack001/data "query Requirements with Tests"
8
```

### Clear data graph

Data can be cleared from RACK by graph name. Use `--data-graph` to override the
default `http://rack001/data`. Multiple data graphs can be specified.

```shell
(venv) $ rack data clear
(venv) $ rack data clear --data-graph http://rack001/Example
(venv) $ rack data clear --data-graph http://rack001/ex1 --data-graph http://rack001/ex2
```

### Nodegroups

The script can automate loading a directory full of nodegroups
indexed by a `store_data.csv` file.

```shell
(venv) $ rack nodegroups import ../nodegroups/ingestion/arcos.rack
Storing nodegroups...                                       OK
```

It can also export nodegroups matching a regular expression
into a directory alongside its `store_data.csv` file for future
loads.

```shell
(venv) $ mkdir outdir
(venv) $ rack nodegroups export ^query outdir
Retrieving nodegroups...                                    OK
(venv) $ ls outdir
query Compilation Inputs.json                   query Requirements without Tests.json
query Control Flow From Function.json           query System Structure.json
...
```

The tool can also list the currently loaded nodegroups.

```shell
(venv) $ rack nodegroups list
Listing nodegroups...                                       OK
ID                                           comments                [...]
-------------------------------------------  ------------------------[...]
Ingest-SoftwareComponent                     Node group to ingest COM[...]
Ingest-SoftwareComponentTestResult           Node group to ingest Sof[...]
[...]
```

## Hacking

See [dev/README.md](https://github.com/ge-high-assurance/RACK/tree/master/cli/dev).

<!--
Don't copy below to wiki; wiki already has copyright in _Footer.md
-->

---
Copyright (c) 2021, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
