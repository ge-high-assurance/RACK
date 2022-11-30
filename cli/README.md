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

To do its work, the `rack` program uses the Node Execution Group REST
API which is documented in the [REST API Swagger
Demo](https://github.com/ge-high-assurance/RACK/wiki/REST-API-Swagger-Demo)
and [REST cookbook](https://github.com/ge-semtk/semtk/wiki/REST-API)
wiki pages.

## Install dependencies

The `rack` program requires the
[semtk-python3](https://github.com/ge-semtk/semtk-python3) package and
other requirements listed in `requirements.txt`.

> **_NOTE_** these programs and dependencies are already installed in
> the RACK Box container and virtual machine images; no further
> installation is necessary for those environments, which can be used
> directly (e.g. `docker exec -it CONTAINER-ID-OR-NAME /bin/bash`).
>
> In general, RACK is accessed via network-based operations, so
> running the tools against a RACK Box or against the user's local
> environment should be equivalent.

We recommend installing these dependencies in an isolated virtual
environment to ensure reproducibility of results.

<!--
Note for documentation authors: These instructions should be kept in sync with
the RACK-Box README.
-->

```shell
python3 -m venv venv
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

RACK-in-a-Box comes setup with the core ontology. From time to time,
you may want to revert to the initial RACK state. You will need to
setup your local drive with the necessary files. We have written a
shell script called `setup-owl.sh` which will copy the OWL files,
ingestion nodegroups, and CDR (common data representation .csv
templates) to your local drive. It is assumed that you already have a
docker container running.  Here is what the output may look like:

```shell
$ ./setup-owl.sh
Found container d8140c00ce3
Copying OwlModels
Copying nodegroups
```

We have written a shell script called `setup-rack.sh` which will call
the `rack` program to initialize a RACK-in-a-Box instance with the
RACK core ontology model.  It is assumed that you are
still in the isolated Python virtual environment and running a
RACK-in-a-Box instance in a Linux container on `localhost`.  Here is
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
usage: rack [-h] [--base-url BASE_URL] [--triple-store TRIPLE_STORE] [--triple-store-type TRIPLE_STORE_TYPE] [--log-level LOG_LEVEL] {data,model,nodegroups} ...

RACK in a Box toolkit

positional arguments:
  {manifest,data,model,nodegroups}
    manifest            Ingestion package automation
    data                Import or export CSV data
    model               Interact with SemTK model
    nodegroups          Interact with SemTK nodegroups

optional arguments:
  -h, --help            show this help message and exit
  --base-url BASE_URL   Base SemTK instance URL
  --triple-store TRIPLE_STORE
                        Override Fuseki URL
  --triple-store-type TRIPLE_STORE_TYPE
                        Override triplestore type (default: fuseki)
  --log-level LOG_LEVEL
                        Assign logger severity level
```

The `rack` command is split into four subcommands: `manifest`, `data`, `model`,
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

The `manifest` subcommand is used to import a complete set of CSV and OWL data
from multiple files as specified by a single top-level manifest file.  This
subcommand subsumes the `data`, `nodegroups`, and `model` subcommands and is the
recommended way to initialize a RACK instance for use.

The following options default to their matching ENVIRONMENT variables if they exist:

- --base-url : $BASE_URL
- --triple-store : $TRIPLE_STORE
- --log-level : $LOG_LEVEL

For example, **ingestion warnings can be suppressed** by either using
`rack --log-level ERROR data import...` or by executing this command
in a bash script before calling 'rack':

`export LOG_LEVEL=ERROR`

## Data Ingestion Configuration file format

The import configuration files are YAML files that specify the target
`data-graph` and `ingestion-steps`. These files will be the argument
to a `rack data import` command.

`data-graph` is a graph identifier URL. A graph is a distinct namespace
within the triplestore in order to allow multiple datasets to be stored
without overwriting each other.  Data will be ingested into this graph.

`extra-data-graphs` is a list of additional graph identifiers that are
used for URI lookups.  While `data-graph` will be used to store the
ingested data, `extra-data-graphs` will be used only to look up data.

`ingestion-steps` takes an ordered list of CSV and OWL file imports
used to populate the graph.

CSV files can be ingested by adding list elements containing a `csv`
path and a `nodegroup` that lists an existing nodegroup ID in
RACK. CSV paths are resolved relative to the configuration file.

OWL files can be ingested by adding list elements containing an `owl`
path to the OWL file.

```yaml
data-graph: "http://rack001/data"
extra-data-graphs:
- "http://rack001/otherdata"
- "http://rack001/somedata"
ingestion-steps:
- {nodegroup: "ingest_SYSTEM",    csv: "SYSTEM.csv"}
- {nodegroup: "ingest_INTERFACE", csv: "INTERFACE.csv"}
- {class: "http://arcos.rack/HAZARD#HAZARD", csv: "HAZARD.csv"}
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

### Triplestore URL

- Default value: *Base URL* + `:3030/RACK`
- Override flags: `--triple-store`
- Override environment variable: `TRIPLE_STORE`

### Triplestore Type

- Default value: `fuseki`
- Override flags: `--triple-store-type`
- Override environment variable: `TRIPLE_STORE_TYPE`

## Example invocations

These examples uses the virtual environment as defined in the
*Installing Dependencies* section above.

### Import data

See the `setup-turnstile.sh` as an example for populating the model
and nodegroups for the *Turnstile* example into RACK-in-a-Box.

See the
`../Turnstile-Example/Turnstile-IngestionPackage/Load-TurnstileData.sh`
as an example for populating data for the *Turnstile* example into
Rack-in-a-Box.

### Export data

This example exports instances of the `SYSTEM` class from the
*Turnstile* example from a Rack-in-a-Box instance running in a Linux
container on `localhost`:

```shell
$ source venv/bin/activate
(venv) $ rack data export --data-graph http://rack001/turnstiledata ingest_turnstile_SystemComponent
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
  --data-graph http://rack001/turnstiledata \
  --constraint req=HLR-1 \
  --constraint decomposition=IN-LLR-2

# Example using regular expressions
rack data export "query Requirements decomposition" \
  --data-graph http://rack001/turnstiledata \
  --constraint "req~^HLR-.$" \
  --constraint "decomposition~^IN-"
```

### Count result rows

The number of results a nodegroup would generate can be obtained
using the `count` sub-command.

```shell
(venv) $ rack data count --data-graph http://rack001/data "query Requirements with Tests"
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

## Ingestion Packages (manifest)

The bulk ingestion of multiple models, nodegrounds, and data can be
automated using a manifest file.

```yaml
name: "short name"
description: "optional long package description"
footprint:
    model-graphs:
      - "http://rack001/model"
    data-graphs:
      - "http://rack001/data"
steps:
  - manifest: another.yaml
  - model: model-manifest.yaml
  - data: data-manifest.yaml
  - copygraph:
      from-graph: 'http://rack001/data'
      to-graph:   'uri://DefaultGraph'
```

The `name` and `description` fields are informational and are used to
provide a nicer UI for users loading an ingestion package.

The `footprint` section is optional. When it is provided it allows
the ingestion UI to automatically populate a connection string. In
addition these graph URIs will be cleared if the manifest is loaded
using the `--clear` flag.

The `steps` section is required. It describes the sequential process
of loading this ingestion package. This section must be a list of singleton
maps. There are currently 4 kinds of step you can use in a manifest:

- `manifest` steps take a relative path argument and recursively
  import that manifest file.
- `model` steps take a relative path argument and invoke
  `rack model import` on that file.
- `nodegroups` steps take a relative path argument and invoke
  `rack nodegroups import` on that directory.
- `data` steps take a relative path argument and invoke
  `rack data import` on that directory.
- `copygraph` steps take a dictionary specifying a `from-graph` URI
  and a `to-graph` URI and perform a merge copying triples from the
  from graph into the to graph.

All file paths are resolved relative to the location of the manifest
YAML file.

### CLI support

```text
usage: rack manifest import [-h] [--clear] [--default-graph] manifest

positional arguments:
  manifest         Manifest YAML file

optional arguments:
  -h, --help       show this help message and exit
  --clear          Clear footprint before import
  --default-graph  Load whole manifest into default graph
```

Manifests can be loaded using `rack manifest import`.

To clear all graphs mentioned in the `footprint` use `--clear`. For example:
`rack manifest import --clear my-manifest.yaml`

Fuseki happens to run faster when data is stored in the *default graph*.
To load a complete ingestion manifest into the default graph use
`--default-graph`. For example:
`rack manifest import --default-graph my-manifest.yaml`

## Hacking

See [dev/README.md](https://github.com/ge-high-assurance/RACK/tree/master/cli/dev).

<!--
Don't copy below to wiki; wiki already has copyright in _Footer.md
-->

---
Copyright (c) 2021-2022, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
