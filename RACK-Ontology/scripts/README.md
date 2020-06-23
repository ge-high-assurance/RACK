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
usage: load_data.py [-h] [--data-graph DATA_GRAPH]
                    [--triple-store TRIPLE_STORE]
                    config URL

Load CSV data into RACK in a Box

positional arguments:
  config                Configuration YAML file
  URL                   Base SemTK instance URL

optional arguments:
  -h, --help            show this help message and exit
  --data-graph DATA_GRAPH
                        Override data graph URL
  --triple-store TRIPLE_STORE
                        Override Fuseki URL
```

The *config* argument is a file path pointing to a data import configuration file.

The *URL* argument is expected to point to the base of a RACK in a Box instance.

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
- ["TA1 ingest1 system",        "SYSTEM.csv"]
- ["TA1 ingest2 interface",     "INTERFACE.csv"]
```

## Example invocation

This example populates the *Turnstile* example into a RACK in the Box instance
running on `localhost`

This example uses the virtual environment as defined in the *Installing Dependencies*
section above.

```
$ source venv/bin/activate
(venv) $ ./load_data.py ../models/TurnstileSystem/Data/import.yaml http://localhost
INFO:semtk3:Percent complete:  80%
INFO:semtk3:Percent complete:  100%
Clear graph:  Success Update succeeded 
Loading [TA1 ingest1 system]
Records: 8	Failures: 0
Loading [TA1 ingest2 interface]
Records: 4	Failures: 0
Loading [TA1 ingest1 system]
Records: 8	Failures: 0
Loading [TA1 ingest2 interface]
Records: 4	Failures: 0
Loading [TA1 ingest3 hazard]
Records: 4	Failures: 0
Loading [TA1 ingest4 requirement]
Records: 19	Failures: 0
Loading [TA1 ingest5 data dict]
Records: 29	Failures: 0
Loading [TA1 ingest6 test]
Records: 4	Failures: 0
Loading [TA1 ingest7 test results]
Records: 8	Failures: 0
```
