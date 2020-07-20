#!/usr/bin/env python3
"""Loads CSV data into RACK in a Box

This simple process can be adapted to import other data into RACK for experimentation.
"""
import argparse
import json
import logging
import os
import sys
from pathlib import Path

from jsonschema import ValidationError, validate
import requests
import semtk3
import yaml

__author__ = "Eric Mertens"
__email__ = "emertens@galois.com"

DEFAULT_BASE_URL = "http://localhost"

MODEL_GRAPH = "http://rack001/model"

INGEST_CSV_CONFIG_SCHEMA = {
    'type' : 'object',
    'additionalProperties': False,
    'properties': {
        'ingestion-steps': {
            'type': 'array',
            'contains': {'type': 'array', 'items': [{'type': 'string'}, {'type': 'string'}]}},
        'data-graph': {'type': 'string'}
    }
}

INGEST_OWL_CONFIG_SCHEMA = {
    'type' : 'object',
    'additionalProperties': False,
    'properties': {
        'files': {
            'type': 'array',
            'contains': {'type': 'string'}},
        'data-graph': {'type': 'string'}
    }
}

def sparql_connection(base_url, data_graph, triple_store):
    """Generate a SPARQL connection value."""

    # Default to RACK in a Box triple-store location
    triple_store = triple_store or (base_url + ":3030/RACK")
    conn = {
        "name": "%NODEGROUP%",
        "model": [{"type": "fuseki", "url": triple_store, "graph": MODEL_GRAPH}],
        "data": []
        }
    if data_graph is not None:
        conn["data"].append({"type": "fuseki", "url": triple_store, "graph": data_graph})
    return json.dumps(conn)

def clear_graph(conn, which_graph='data'):
    """Clear all the existing data in the data or model graph"""
    print('Clearing graph')
    result = semtk3.clear_graph(conn, which_graph, 0)
    print(result)

def ingest_csv(conn, nodegroup, csv_name):
    """Ingest a CSV file using the named nodegroup."""
    print(f'Loading [{nodegroup}]')

    with open(csv_name, "r") as csv_file:
        csv = csv_file.read()

    result = semtk3.ingest_by_id(nodegroup, csv, conn)

    print(f' Records: {result["recordsProcessed"]}\tFailures: {result["failuresEncountered"]}')

def ingest_owl(conn, owl_file):
    print(f'Ingesting [{owl_file}]', end="")
    semtk3.upload_owl(owl_file, conn, "rack", "rack")
    print('\tOK')

def ingest_csv_driver(config_path, base_url, data_graph, triple_store):

    with open(config_path, 'r') as config_file:
        config = yaml.safe_load(config_file)
        validate(config, INGEST_CSV_CONFIG_SCHEMA)

    steps = config['ingestion-steps']
    data_graph = data_graph or config['data-graph']
    base_path = os.path.dirname(config_path)

    conn = sparql_connection(base_url, data_graph, triple_store)

    semtk3.set_host(base_url)

    clear_graph(conn)
    for step in steps:
        ingest_csv(conn, step[0], os.path.join(base_path, step[1]))

def ingest_owl_driver(config_path, base_url, triple_store):
    with open(config_path, 'r') as config_file:
        config = yaml.safe_load(config_file)
        validate(config, INGEST_OWL_CONFIG_SCHEMA)

    files = config['files']
    base_path = Path(config_path).parent

    conn = sparql_connection(base_url, None, triple_store)

    semtk3.set_host(base_url)

    clear_graph(conn, which_graph='model')
    for f in files:
        ingest_owl(conn, base_path / f)

def dispatch_data_import(args):
    ingest_csv_driver(args.config, args.base_url, args.data_graph, args.triple_store)

def dispatch_plumbing_model(args):
    ingest_owl_driver(args.config, args.base_url, args.triple_store)

def main():
    """Main function"""
    parser = argparse.ArgumentParser(description='RACK in a Box toolkit')
    parser.add_argument('--base-url', type=str, default=DEFAULT_BASE_URL, help='Base SemTK instance URL')
    parser.add_argument('--triple-store', type=str, help='Override Fuseki URL')

    subparsers = parser.add_subparsers(dest='command')

    data_parser = subparsers.add_parser('data', help='Import or export CSV data')
    data_subparsers = data_parser.add_subparsers(dest='command', required=True)
    data_import_parser = data_subparsers.add_parser('import', help='Import CSV data')
    plumbing_parser = subparsers.add_parser('plumbing', help='Tools for RACK developers')
    plumbing_subparsers = plumbing_parser.add_subparsers(dest='command', required=True)
    plumbing_model_parser = plumbing_subparsers.add_parser('model', help='Modify the data model')

    data_import_parser.add_argument('config', type=str, help='Configuration YAML file')
    data_import_parser.add_argument('--data-graph', type=str, help='Override data graph URL')
    data_import_parser.set_defaults(func=dispatch_data_import)

    plumbing_model_parser.add_argument('config', type=str, help='Configuration YAML file')
    plumbing_model_parser.set_defaults(func=dispatch_plumbing_model)


    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO)

    try:
        if args.command is None:
            logging.error('Subcommand required (use --help to see options)')
            sys.exit(1)
        try:
            func = args.func
        except AttributeError:
            logging.error('Unknown subcommand: %s', args.command)
            sys.exit(1)
        func(args)
    except requests.ConnectionError as exc:
        logging.error('Connection failure\n%s', exc)
        sys.exit(1)
    except semtk3.restclient.RestException as exc:
        logging.error('REST Endpoint Failure\n%s', exc)
        sys.exit(1)
    except FileNotFoundError as exc:
        logging.error('File not found\n%s', exc)
        sys.exit(1)
    except yaml.YAMLError as exc:
        logging.error('Failed to load YAML configuration file: %s\n%s', args.config, exc)
        sys.exit(1)
    except ValidationError as exc:
        logging.error('Bad configuration file: %s\n%s', args.config, exc)
        sys.exit(1)

if __name__ == "__main__":
    main()
