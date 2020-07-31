<!---
NOTE: This document should be copied verbatim to this wiki page before every
release:

https://github.com/ge-high-assurance/RACK/wiki/Command-line-script-rack.py
-->

# Ingestion Script

This python script uses the Node Execution Group REST API
to populate data into RACK in a Box via CSV files.

More documentation on the REST API is available on the
[REST API Swagger Demo](https://github.com/ge-high-assurance/RACK/wiki/REST-API-Swagger-Demo)
and [REST cookbook](https://github.com/ge-semtk/semtk/wiki/REST-cookbook) wiki pages.

## Installing dependencies

This script requires the [semtk-python3](https://github.com/ge-semtk/semtk-python3) package
among other requirements listed in `requirements.txt`.

We recommend installing these dependencies in an isolated virtual environment
like [virtualenv](https://pypi.org/project/virtualenv/) to ensure reproducability
of results.

```
$ virtualenv venv
$ source venv/bin/activate
$ pip install -r requirements.txt
```

## Usage

```
usage: rack.py [-h] [--base-url BASE_URL] [--triple-store TRIPLE_STORE]
               [--log-level LOG_LEVEL]
               {data,plumbing} ...

RACK in a Box toolkit

positional arguments:
  {data,plumbing}
    data                Import or export CSV data
    plumbing            Tools for RACK developers

optional arguments:
  -h, --help            show this help message and exit
  --base-url BASE_URL   Base SemTK instance URL
  --triple-store TRIPLE_STORE
                        Override Fuseki URL
  --log-level LOG_LEVEL
                        Assign logger severity level
```

The *base-url* overrides a localhost installation of RACK in a Box.

The *triple-store* URL argument overrides the default RACK in a Box triple-store URL.

The *data-graph* URL argument overrides the data-set identifier specified in the configuration file.

## Configuration file format

The import configuration files are YAML files that specify the target *data-graph*
and *ingestion-steps*.

*data-graph* is a graph identifier URL. This URL allows multiple datasets to be stored
in the same triple store.

*ingestion-steps* is a list of pairs of *nodegroup id* and *CSV path*.

CSV paths are resolved relative to the configuration file.

```
data-model: "http://rack001/data"
ingestion-steps:
- {nodegroup: "ingest01 system",    csv: "SYSTEM.csv"}
- {nodegroup: "ingest02 interface", csv: "INTERFACE.csv"}
```

## Example invocation

These examples uses the virtual environment as defined in the *Installing Dependencies*
section above.

### Import

This example populates the *Turnstile* example into a RACK in the Box instance
running on `localhost`

```
$ source venv/bin/activate
(venv) $ ./rack.py data import ../models/TurnstileSystem/Data/import.yaml
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

### Export

This example exports instances of the `SYSTEM` class from the *Turnstile*
example from a Rack in the Box instance running on `localhost`:

```
$ source venv/bin/activate
(venv) $ ./rack.py data export "ingest01 system" http://rack001/data

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
See `./rack.py data export --help` for options, including different export formats (such as CSV), emitting to a file, and omitting the header row.

### Updating nodegroups

The script can automate loading a directory full of nodegroups
indexed by a `store_data.csv` file.

```
(venv) $ ./rack.py plumbing store-nodegroups ../../nodegroups/ingestion
Storing nodegroups...                                       OK
```

It can also export nodegroups matching a regular expression
into a directory alongside its `store_data.csv` file for future
loads.

```
(venv) $ mkdir outdir
(venv) $ ./rack.py $CONFIG plumbing retrieve-nodegroups ^ingest outdir
Retrieving nodegroups...                                    OK
```

## Hacking

The following documentation is only useful for developers of this script.

### Mypy

This script has [Mypy](http://mypy-lang.org)-compliant type annotations which
are used to statically type-check the code. Usage is simple:
```bash
$ source venv/bin/activate
$ pip install mypy
$ mypy .
```

### Tests

You can run the tests with `pytest`:
```bash
$ source venv/bin/activate
$ pip install pytest
$ pytest
```
