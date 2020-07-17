#!/usr/bin/env python3
"""Loads CSV data into RACK in a Box

This simple process can be adapted to import other data into RACK for experimentation.
"""
import argparse
import json
import logging
import os
import sys

from jsonschema import ValidationError, validate
import requests
import semtk3
import yaml

__author__ = "Eric Mertens"
__email__ = "emertens@galois.com"

MODEL_GRAPH = "http://rack001/model"

CONFIG_SCHEMA = {
    'type' : 'object',
    'additionalProperties': False,
    'properties': {
        'ingestion-steps': {
            'type': 'array',
            'contains': {'type': 'array', 'items': [{'type': 'string'}, {'type': 'string'}]}},
        'data-graph': {'type': 'string'}
    }
}

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
    print('Clearing graph')
    result = semtk3.clear_graph(conn, 'data', 0)
    print(result)

def ingest_csv(conn, nodegroup, csv_name):
    """Ingest a CSV file using the named nodegroup."""
    print(f'Loading [{nodegroup}]')

    with open(csv_name, "r") as csv_file:
        csv = csv_file.read()

    result = semtk3.ingest_by_id(nodegroup, csv, conn)

    print(f' Records: {result["recordsProcessed"]}\tFailures: {result["failuresEncountered"]}')

def ingest_csv_driver(config_path, base_url, data_graph, triple_store):

    with open(config_path, 'r') as config_file:
        config = yaml.safe_load(config_file)
        validate(config, CONFIG_SCHEMA)

    steps = config['ingestion-steps']
    data_graph = data_graph or config['data-graph']
    base_path = os.path.dirname(config_path)

    conn = sparql_connection(base_url, data_graph, triple_store)

    semtk3.set_host(base_url)

    clear_data(conn)
    for step in steps:
        ingest_csv(conn, step[0], os.path.join(base_path, step[1]))


def dispatch_data_import(args):
    ingest_csv_driver(args.config, args.URL, args.data_graph, args.triple_store)

def main():
    """Main function"""
    parser = argparse.ArgumentParser(description='Load CSV data into RACK in a Box')
    subparsers = parser.add_subparsers(dest='command')

    data_parser = subparsers.add_parser('data', help='Import or export CSV data')
    data_subparsers = data_parser.add_subparsers(dest='command', required=True)
    data_import_parser = data_subparsers.add_parser('import', help='Import CSV data')
    plumbing_parser = subparsers.add_parser('plumbing', help='Tools for RACK developers')
    plumbing_subparsers = plumbing_parser.add_subparsers(dest='command', required=True)
    plumbing_model_parser = plumbing_subparsers.add_parser('model', help='Modify the data model')

    data_import_parser.add_argument('config', type=str, help='Configuration YAML file')
    data_import_parser.add_argument('URL', type=str, help='Base SemTK instance URL')
    data_import_parser.add_argument('--data-graph', type=str, help='Override data graph URL')
    data_import_parser.add_argument('--triple-store', type=str, help='Override Fuseki URL')
    data_import_parser.set_defaults(func=dispatch_data_import)

    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO)

    try:
        if args.command is None:
            logging.error(f'Subcommand required (use --help to see options)')
            sys.exit(1)
        try:
            args.func(args)
        except AttributeError:
            logging.error(f'Unknown subcommand: "{args.command}"')
            sys.exit(1)
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
