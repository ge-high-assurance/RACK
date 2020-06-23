#!/usr/bin/env python3
"""
This script makes it easy to populate your RACK instance with the initial
Turnstile model data.

This simple process can be adapted to import other data into RACK for experimentation.
"""

import json
import requests

# URLS

# This URL must point to your RACK-in-a-box installation
URL_BASE = "http://eic-60-182.galois.com"

FUSEKI_DB_URL = URL_BASE + ":8080/fuseki/SemTK"

# Node Group Execution Rest Controller
# http://localhost:12058/swagger-ui.html
NODEGROUP_EXECUTION_URL = URL_BASE + ":12058/nodeGroupExecution"
INGESTION_ENDPOINT = NODEGROUP_EXECUTION_URL + "/ingestFromCsvStringsById"
CLEAR_GRAPH_ENDPOINT = NODEGROUP_EXECUTION_URL + "/dispatchClearGraph"

# Directory containing CSV files
DATA_PATH = "../models/TurnstileSystem/Data"

# Graph identifiers

# This URL identifies the ontology. This script does not modify this data.
MODEL_GRAPH = "http://arcos.rack/ontology"

# This URL identifies the data graph. It will be cleared and repopulated.
DATA_GRAPH = "http://arcos.rack/data"

# Connection string
SPARQL_CONNECTION = {
    "name": "%NODEGROUP%",
    "model": [{"type": "fuseki", "url": FUSEKI_DB_URL, "graph": MODEL_GRAPH}],
    "data":  [{"type": "fuseki", "url": FUSEKI_DB_URL, "graph": DATA_GRAPH}],
    }

def clear_data():
    """Clear all the existing data in DATA_GRAPH"""

    params = {"serverType": "fuseki", "serverAndPort": FUSEKI_DB_URL, "graph": DATA_GRAPH}
    response = requests.post(CLEAR_GRAPH_ENDPOINT, json=params)
    status = response.json()['status']
    print(f"Clear graph: {status}")

def ingest(nodegroup, csv_name):
    """Ingest a CSV file using the named nodegroup into DATA_GRAPH."""

    with open(DATA_PATH + "/" + csv_name, "r") as csv_file:
        csv = csv_file.read()

    params = {
        "csvContent": csv,
        "sparqlConnection": json.dumps(SPARQL_CONNECTION),
        "templateId": nodegroup
        }
    response = requests.post(INGESTION_ENDPOINT, json=params)
    results = response.json()['recordProcessResults']
    print(f'{nodegroup}:\tprocessed {results["recordsProcessed"]}\tfailures: {results["failuresEncountered"]}')

if __name__ == "__main__":
    clear_data()
    ingest("TA1 ingest1 system",        "SYSTEM.csv")
    ingest("TA1 ingest2 interface",     "INTERFACE.csv")
    ingest("TA1 ingest3 hazard",        "HAZARD.csv")
    ingest("TA1 ingest4 requirement",   "REQUIREMENT.csv")
    ingest("TA1 ingest5 data dict",     "DATA_DICTIONARY_TERM.csv")
    ingest("TA1 ingest6 test",          "TEST.csv")
    ingest("TA1 ingest7 test results",  "TEST_RESULTS.csv")
    ingest("TA1 ingest8 language",      "LANGUAGE.csv")
    ingest("TA1 ingest9 compiler",      "COMPILER.csv")
    ingest("TA1 ingest10 packager",     "PACKAGER.csv")
    ingest("TA1 ingest11 agent",        "AGENT.csv")
    ingest("TA1 ingest12 code file",    "CODE_FILE.csv")
    ingest("TA1 ingest13 compile",      "COMPILE.csv")
    ingest("TA1 ingest14 object file",  "OBJECT_FILE.csv")
    ingest("TA1 ingest15 library",      "LIBRARY.csv")
    ingest("TA1 ingest16 executable",   "EXECUTABLE.csv")
    ingest("TA1 ingest17 config file",  "CONFIG_FILE.csv")
    ingest("TA1 ingest18 package file", "PACKAGE_FILE.csv")
    ingest("TA1 ingest19 package",      "PACKAGE.csv")
