# Instructions

The following instructions assume that the user is operating from a
checked-out (from github) version of RACK or one of the
RACK-in-the-Box Docker or VM environments.  For either of these
configurations, the following assumes that the `RACK` environment
variable is set to the top of the RACK tree (`/home/ubuntu/RACK` for
the Docker or VM environments).

## Copy nodegroups and owl files. Setup a virtual environment:
Note: this step is not required when running in the RACK Docker image: the python environment is already setup and available in that image.
Refer to the cli [README](../../cli/README.md) for installation instructions.
The script [setup-owl.sh](../../cli/setup-owl.sh) copies out of the RACK-in-the-Box the nodegroups and owl files onto your local drive. 
```sh
$ cd ${RACK}/cli
$ ./setup-owl.sh
$ . venv/bin/activate
```
If running on Windows, GitBash can be used with the following commands.
```sh
$ cd ${RACK}/cli
$ ./setup-owl.sh
$ . venv/Scripts/activate
```

## Run the setup script:
The script [setup-turnstile.sh](../../cli/setup-turnstile.sh) loads the [GE ontology overlay](../../GE-Ontology/ontology/GE.sadl). 
```sh
(venv) $ cd ${RACK}/cli
(venv) $ ./setup-owl.sh
(venv) $ ./setup-turnstile.sh
```

## Ingest the turnstile data into RACK:
Browse to the dataset location and load the data via the provided shell script.
```sh
(venv) $ cd ${RACK}/Turnstile-Example/Turnstile-IngestionPackage
(venv) $ ./Load-TurnstileData.sh
```

## Export the data from RACK
Use cli to export some data from RACK using one of the query nodegroups. Note that the Turnstile data is in the data graph http://rack001/turnstiledata.
```sh
(venv) $ rack data export "query Requirements with Tests" --data-graph http://rack001/turnstiledata
```
