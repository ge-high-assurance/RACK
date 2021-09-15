# Instructions

## Run CLI in a virtual environment:
Refer to the cli [README](RACK/cli/README.md) for installation instructions.
```sh
$ cd RACK/cli
$ . venv/bin/activate
```
If running on Windows, GitBash can be used with the following commands.
```sh
$ cd RACK/cli
$ . venv/Scripts/activate
```

## Run the setup scripts:
Note that the dataset uses the GE ontology overlay. 
```sh
(venv) $ ./setup-rack.sh
(venv) $ cd ../GE-Ontology
(venv) $ ./setup-GE.sh
```

## Ingest the turnstile data into RACK:
Browse to the dataset location and load the data via the provided shell script.
```sh
(venv) $ cd ../Turnstile-Example/Turnstile-IngestionPackage
(venv) $ ./Load-TurnstileData.sh
```

## Export the data from RACK
Use cli to export some data from RACK using one of the query nodegroups. Note that the Turnstile data is in the data graph http://rack001/turnstiledata.
```sh
(venv) $ rack data export "query Requirements with Tests" --data-graph http://rack001/turnstiledata
```
