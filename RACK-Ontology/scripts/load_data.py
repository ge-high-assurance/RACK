#!/usr/bin/env python3
"""Loads CSV data into RACK in a Box

This simple process can be adapted to import other data into RACK for experimentation.
"""
import argparse
import json
import logging
import os
import requests
import semtk3
import sys
import yaml

__author__ = "Eric Mertens"
__email__ = "emertens@galois.com"

MODEL_GRAPH = "http://arcos.rack/ontology"

def sparql_connection(base_url, data_graph, triple_store):
    """Generate a SPARQL connection value."""

    # Default to RACK in a Box triple-store location
    triple_store = triple_store or (base_url + ":3030/RACK")

    return json.dumps({
        "name": "%NODEGROUP%",
        "model": [{"type": "fuseki", "url": triple_store, "graph": MODEL_GRAPH}],
        "data":  [{"type": "fuseki", "url": triple_store, "graph": data_graph}],
        })

def clear_data(conn):
    """Clear all the existing data in DATA_GRAPH"""
    try:
        result = semtk3.clear_graph(conn, 'data', 0)
    except requests.ConnectionError as exc:
        logging.error('Failed to clear data graph: %s', exc)
        raise
    except semtk3.restclient.RestException as exc:
        logging.error('Failed to clear data graph: %s', exc)
        raise

    print('Clear graph: ' + result)

def ingest(conn, nodegroup, csv_name):
    """Ingest a CSV file using the named nodegroup."""
    print(f'Loading [{nodegroup}]')

    try:
        with open(csv_name, "r") as csv_file:
            csv = csv_file.read()
    except OSError as exc:
        logging.error("Failed to read CSV file: %s", exc)
        raise

    try:
        result = semtk3.ingest_by_id(nodegroup, csv, conn)
    except requests.ConnectionError as exc:
        logging.error('Failed to ingest CSV file: %s', exc)
        raise
    except semtk3.restclient.RestException as exc:
        logging.error('Failed to ingest CSV file: %s', exc)
        raise

    print(f'Records: {result["recordsProcessed"]}\tFailures: {result["failuresEncountered"]}')

def driver(config_path, base_url, data_graph, triple_store):

    try:
        with open(config_path, 'r') as config_file:
            config = yaml.safe_load(config_file)
    except FileNotFoundError as exc:
        logging.error('Failed to open YAML configuration file: %s', exc)
        raise
    except yaml.YAMLError as exc:
        logging.error('Failed to load YAML configuration file (%s): %s', config_path, exc)
        raise

    steps = config['ingestion-steps']
    data_graph = data_graph or config['data-model']
    base_path = os.path.dirname(config_path)

    conn = sparql_connection(base_url, data_graph, triple_store)

    semtk3.set_host(base_url)

    clear_data(conn)
    for step in steps:
        ingest(conn, step[0], os.path.join(base_path, step[1]))

def main():
    """Main function"""
    parser = argparse.ArgumentParser(description="Load CSV data into RACK in a Box")
    parser.add_argument('config', type=str, help='Configuration YAML file')
    parser.add_argument('URL', type=str, help='Base SemTK instance URL')
    parser.add_argument('--data-graph', type=str, help='Override data graph URL')
    parser.add_argument('--triple-store', type=str, help='Override Fuseki URL')
    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO)

    try:
        driver(args.config, args.URL, args.data_graph, args.triple_store)
    except:
        logging.error("Ingestion failed.")
        sys.exit(1)

if __name__ == "__main__":
    main()
