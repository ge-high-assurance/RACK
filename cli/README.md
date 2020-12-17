<!---
NOTE: This file should be copied to this wiki page before each release:

https://github.com/ge-high-assurance/RACK/wiki/RACK-CLI
-->

# RACK CLI

The RACK command-line interface (the `rack` program) can

- [initialize](#initialize-rack-in-a-box) RACK-in-a-Box
- [import data](#import-data) into RACK-in-a-Box from CSV files
- [export data](#export-data) from RACK-in-a-Box to CSV files

To do its work, the rack program uses the Node Execution Group REST
API which is documented in the [REST API Swagger
Demo](https://github.com/ge-high-assurance/RACK/wiki/REST-API-Swagger-Demo)
and [REST
cookbook](https://github.com/ge-semtk/semtk/wiki/REST-cookbook) wiki
pages.

## Install dependencies

The rack program requires the
[semtk-python3](https://github.com/ge-semtk/semtk-python3) package and
other requirements listed in `requirements.txt`.

We recommend installing these dependencies in an isolated virtual
environment using [virtualenv](https://pypi.org/project/virtualenv/)
to ensure reproducibility of results.

<!--
Note for documentation authors: These instructions should be kept in sync with
the RACK-Box README.
-->

```shell
virtualenv venv
source venv/bin/activate
pip install --force -r requirements.txt
python3 setup.py install
```

If running on Windows, GitBash can be used with the following commands.

```shell
virtualenv venv
source venv/Scripts/activate
pip install --force -r requirements.txt
python setup.py install
```

## Initialize RACK-in-a-Box

We have written a shell script called `setup-rack.sh` which will call
the `rack` program to initialize a RACK-in-a-Box instance with the
RACK ontology model and some default data.  It is assumed that you are
still in the isolated Python virtual environment and running a
RACK-in-a-Box instance in a Docker container on `localhost`.  Here is
how to run `setup-rack.sh` and what its output may look like:

```shell
(venv) $ ./setup-rack.sh
Clearing graph
Success Update succeeded
Ingesting ../OwlModels/AGENTS.owl...               OK
Ingesting ../OwlModels/ANALYSIS.owl...             OK
Ingesting ../OwlModels/DOCUMENT.owl...             OK
Ingesting ../OwlModels/HAZARD.owl...               OK
Ingesting ../OwlModels/PROCESS.owl...              OK
Ingesting ../OwlModels/PROV-S.owl...               OK
Ingesting ../OwlModels/REQUIREMENTS.owl...         OK
Ingesting ../OwlModels/REVIEW.owl...               OK
Ingesting ../OwlModels/SACM-S.owl...               OK
Ingesting ../OwlModels/SOFTWARE.owl...             OK
Ingesting ../OwlModels/SYSTEM.owl...               OK
Ingesting ../OwlModels/TESTING.owl...              OK
Storing nodegroups...                                       OK
Storing nodegroups...                                       OK
Clearing graph
Success Update succeeded
Loading ingest01 system...                         OK Records: 8       Failures: 0
Loading ingest02 interface...                      OK Records: 4       Failures: 0
Loading ingest03 hazard...                         OK Records: 4       Failures: 0
Loading ingest04 requirement...                    OK Records: 22      Failures: 0
Loading ingest05 data dict...                      OK Records: 29      Failures: 0
Loading ingest06 test...                           OK Records: 8       Failures: 0
Loading ingest07 test results...                   OK Records: 16      Failures: 0
Loading ingest08 agent...                          OK Records: 1       Failures: 0
Loading ingest09 package...                        OK Records: 3       Failures: 0
Loading ingest10 compile...                        OK Records: 14      Failures: 0
Loading ingest11 format...                         OK Records: 6       Failures: 0
Loading ingest12 file...                           OK Records: 19      Failures: 0
Loading ingest13 component...                      OK Records: 4       Failures: 0
Ingesting ARP-4754A.owl                   OK
Ingesting DO-178C.owl                     OK
Ingesting DO-330.owl                      OK
Ingesting MIL-STD-881D.owl                OK
Ingesting MIL-STD-881D-AppxB.owl          OK
Ingesting MIL-STD-881D-AppxD.owl          OK
Ingesting MIL-STD-881D-AppxA.owl          OK
Ingesting MIL-STD-881D-AppxC.owl          OK
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
- {nodegroup: "ingest01 system",    csv: "SYSTEM.csv"}
- {nodegroup: "ingest02 interface", csv: "INTERFACE.csv"}
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
(venv) $ rack data import --clear ../models/TurnstileSystem/Data/import.yaml
Clearing graph
Success Update succeeded
Loading ingest01 system...                         OK Records: 8       Failures: 0
Loading ingest02 interface...                      OK Records: 4       Failures: 0
Loading ingest03 hazard...                         OK Records: 4       Failures: 0
Loading ingest04 requirement...                    OK Records: 22      Failures: 0
Loading ingest05 data dict...                      OK Records: 29      Failures: 0
Loading ingest06 test...                           OK Records: 8       Failures: 0
Loading ingest07 test results...                   OK Records: 16      Failures: 0
Loading ingest08 agent...                          OK Records: 1       Failures: 0
Loading ingest09 package...                        OK Records: 3       Failures: 0
Loading ingest10 compile...                        OK Records: 14      Failures: 0
Loading ingest11 format...                         OK Records: 6       Failures: 0
Loading ingest12 file...                           OK Records: 19      Failures: 0
Loading ingest13 component...                      OK Records: 4       Failures: 0
Loading ingest14 confidence...                     OK Records: 2       Failures: 0
```

### Export data

This example exports instances of the `SYSTEM` class from the
*Turnstile* example from a Rack-in-a-Box instance running in a Docker
container on `localhost`:

```shell
$ source venv/bin/activate
(venv) $ rack data export "ingest01 system" http://rack001/data

identifier     identifier_parent
-------------------  -------------------------
TurnStileSystem
Counter Application  TurnStileSystem
Display              TurnStileSystem
ExecutiveThread      Counter Application
In Gate              TurnStileSystem
InputThread          Counter Application
Out Gate             TurnStileSystem
OutputThread         Counter Application
```

See `rack data export --help` for options, including different export
formats (such as CSV), emitting to a file, and omitting the header
row.

### Count result rows

The number of results a nodegroup would generate can be obtained
using the `count` sub-command.

```shell
(venv) $ rack data count "ingest07 test results" "http://rack001/data"
16
```

### Update Nodegroups

The script can automate loading a directory full of nodegroups
indexed by a `store_data.csv` file.

```shell
(venv) $ rack nodegroups import ../../nodegroups/ingestion
Storing nodegroups...                                       OK
```

It can also export nodegroups matching a regular expression
into a directory alongside its `store_data.csv` file for future
loads.

```shell
(venv) $ mkdir outdir
(venv) $ rack nodegroups export ^queries outdir
Retrieving nodegroups...                                    OK
(venv) $ ls outdir
query Compilation Inputs.json                   query Requirements without Tests.json
query Control Flow From Function.json           query System Structure.json
...
```

## Hacking

See [dev/README.md](dev/README.md).

---
Copyright (c) 2020, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
