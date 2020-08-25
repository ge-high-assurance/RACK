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
to ensure reproducability of results.

```shell
virtualenv venv
source venv/bin/activate
pip install -r requirements.txt
python3 setup.py install
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

The *BASE_URL* argument overrides the default URL used to find the
locally running RACK-in-a-Box instance; it allows the rack program to
contact a remote RACK-in-a-Box instance instead.

The *TRIPLE_STORE* URL argument overrides the default URL used to find
the RACK-in-a-Box's triple-store.

## Configuration file format

The import configuration files are YAML files that specify the target
*data-graph* and *ingestion-steps*.

*data-graph* is a graph identifier URL. This URL allows multiple
datasets to be stored in the same triple store.

*ingestion-steps* is a list of pairs of *nodegroup id* and *CSV path*.

CSV paths are resolved relative to the configuration file.

```text
data-model: "http://rack001/data"
ingestion-steps:
- {nodegroup: "ingest01 system",    csv: "SYSTEM.csv"}
- {nodegroup: "ingest02 interface", csv: "INTERFACE.csv"}
```

## Example invocation

These examples uses the virtual environment as defined in the
*Installing Dependencies* section above.

### Import data

This example populates the *Turnstile* example into a RACK-in-a-Box
instance running in a Docker container on `localhost`:

```shell
$ source venv/bin/activate
(venv) $ rack data import ../models/TurnstileSystem/Data/import.yaml
Clearing graph
Success Update succeeded
Loading ingest01 system                 OK Records: 8       Failures: 0
Loading ingest02 interface              OK Records: 4       Failures: 0
Loading ingest03 hazard                 OK Records: 4       Failures: 0
Loading ingest04 requirement            OK Records: 22      Failures: 0
Loading ingest05 data dict              OK Records: 29      Failures: 0
Loading ingest06 test                   OK Records: 8       Failures: 0
Loading ingest07 test results           OK Records: 16      Failures: 0
Loading ingest08 language               OK Records: 1       Failures: 0
Loading ingest09 compiler               OK Records: 1       Failures: 0
Loading ingest10 packager               OK Records: 1       Failures: 0
Loading ingest11 agent                  OK Records: 3       Failures: 0
Loading ingest12 code file              OK Records: 10      Failures: 0
Loading ingest13 object file            OK Records: 3       Failures: 0
Loading ingest14 library                OK Records: 1       Failures: 0
Loading ingest15 executable             OK Records: 1       Failures: 0
Loading ingest16 config file            OK Records: 1       Failures: 0
Loading ingest17 package                OK Records: 6       Failures: 0
Loading ingest18 package file           OK Records: 1       Failures: 0
Loading ingest19 compile                OK Records: 27      Failures: 0
```

### Export data

This example exports instances of the `SYSTEM` class from the
*Turnstile* example from a Rack-in-a-Box instance running in a Docker
container on `localhost`:

```shell
$ source venv/bin/activate
(venv) $ rack data export "ingest01 system" http://rack001/data

uniqueIdentifier     uniqueIdentifier_parent
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

### Update nodegroups

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
(venv) $ rack nodegroups export ^ingest outdir
Retrieving nodegroups...                                    OK
```

## Hacking

See [dev/README.md](dev/README.md).
