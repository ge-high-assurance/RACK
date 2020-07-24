#!/usr/bin/env python3
"""Loads CSV data into RACK in a Box

This simple process can be adapted to import other data into RACK for experimentation.
"""
import argparse
import colorama
import json
import logging
import sys
from colorama import Fore, Back, Style
from enum import Enum
from pathlib import Path
from typing import Any, Dict, Optional, NewType
from types import SimpleNamespace

from jsonschema import ValidationError, validate
from tabulate import tabulate
import requests
import semtk3
import yaml

__author__ = "Eric Mertens"
__email__ = "emertens@galois.com"

# NOTE: Do **not** use the root logger via `logging.command(...)`, instead use `logger.command(...)`
logger = logging.getLogger(__name__)

Connection = NewType('Connection', str)
Url = NewType('Url', str)

class Graph(Enum):
    """Enumeration of SemTK graph types"""
    DATA = "data"
    MODEL = "model"

DEFAULT_BASE_URL: Url = Url("http://localhost")

MODEL_GRAPH: Url = Url("http://rack001/model")

INGEST_CSV_CONFIG_SCHEMA: Dict[str, Any] = {
    'type' : 'object',
    'additionalProperties': False,
    'properties': {
        'ingestion-steps': {
            'type': 'array',
            'contains': {'type': 'array', 'items': [{'type': 'string'}, {'type': 'string'}]}},
        'data-graph': {'type': 'string'}
    }
}

INGEST_OWL_CONFIG_SCHEMA: Dict[str, Any] = {
    'type' : 'object',
    'additionalProperties': False,
    'properties': {
        'files': {
            'type': 'array',
            'contains': {'type': 'string'}},
        'data-graph': {'type': 'string'}
    }
}

class CustomFormatter(logging.Formatter):
    """Add custom styles to our log"""

    # NOTE: 8 is the longest levelname (CRITICAL)
    format = "[%(asctime)s - %(filename)s:%(lineno)d] %(levelname)8s: %(message)s"

    FORMATS = {
        logging.DEBUG: Fore.WHITE + format + Style.RESET_ALL,
        logging.INFO: Fore.GREEN + format + Style.RESET_ALL,
        logging.WARNING: Fore.YELLOW + format + Style.RESET_ALL,
        logging.ERROR: Fore.RED + format + Style.RESET_ALL,
        logging.CRITICAL: Style.BRIGHT + Fore.RED + format + Style.RESET_ALL
    }

    def format(self, record):
        which_format = self.FORMATS.get(record.levelno)
        formatter = logging.Formatter(which_format, "%H:%M:%S")
        return formatter.format(record)

def sparql_connection(base_url: Url, data_graph: Optional[Url], triple_store: Optional[Url]) -> Connection:
    """Generate a SPARQL connection value."""

    semtk3.set_host(base_url)
    # Default to RACK in a Box triple-store location
    triple_store = triple_store or Url(base_url + ":3030/RACK")
    conn: Dict[str, Any] = {
        "name": "%NODEGROUP%",
        "model": [{"type": "fuseki", "url": triple_store, "graph": MODEL_GRAPH}],
        "data": []
        }
    if data_graph is not None:
        conn["data"].append({"type": "fuseki", "url": triple_store, "graph": data_graph})
    return Connection(json.dumps(conn))

def clear_graph(conn: Connection, which_graph: Graph = Graph.DATA) -> None:
    """Clear all the existing data in the data or model graph"""
    print('Clearing graph')
    result = semtk3.clear_graph(conn, which_graph.value, 0)
    print(result)

def run_query(conn: Connection, nodegroup: str) -> None:
    semtk3.SEMTK3_CONN_OVERRIDE = conn
    semtk_table = semtk3.select_by_id(nodegroup)
    print()
    print(tabulate(semtk_table.get_rows(), headers=semtk_table.get_column_names()))

def ingest_csv(conn: Connection, nodegroup: str, csv_name: Path) -> None:
    """Ingest a CSV file using the named nodegroup."""
    print(f'Loading [{nodegroup}]')

    with open(csv_name, "r") as csv_file:
        csv = csv_file.read()

    result = semtk3.ingest_by_id(nodegroup, csv, conn)

    print(f' Records: {result["recordsProcessed"]}\tFailures: {result["failuresEncountered"]}')

def ingest_owl(conn: Connection, owl_file: Path) -> None:
    """Upload an OWL file into the model graph."""
    print(f'Ingesting [{owl_file}]', end="")
    semtk3.upload_owl(owl_file, conn, "rack", "rack")
    print('\tOK')

def ingest_csv_driver(config_path: Path, base_url: Url, data_graph: Optional[Url], triple_store: Optional[Url]) -> None:
    """Use an import.yaml file to ingest multiple CSV files into the data graph."""
    with open(config_path, 'r') as config_file:
        config = yaml.safe_load(config_file)
        validate(config, INGEST_CSV_CONFIG_SCHEMA)

    steps = config['ingestion-steps']
    data_graph = data_graph or config['data-graph']
    base_path = config_path.parent

    conn = sparql_connection(base_url, data_graph, triple_store)

    clear_graph(conn)
    for step in steps:
        ingest_csv(conn, step[0], base_path / step[1])

def ingest_owl_driver(config_path: Path, base_url: Url, triple_store: Optional[Url]) -> None:
    """Use an import.yaml file to ingest multiple OWL files into the model graph."""
    with open(config_path, 'r') as config_file:
        config = yaml.safe_load(config_file)
        validate(config, INGEST_OWL_CONFIG_SCHEMA)

    files = config['files']
    base_path = config_path.parent

    conn = sparql_connection(base_url, None, triple_store)

    clear_graph(conn, which_graph=Graph.MODEL)
    for file in files:
        ingest_owl(conn, base_path / file)

def store_nodegroups_driver(directory: Path, base_url: Url) -> None:
    print("Storing nodegroups: ", end="", flush=True)
    sparql_connection(base_url, None, None)
    semtk3.store_nodegroups(directory)
    print("OK")

def retrieve_nodegroups_driver(regexp: str, directory: Path, base_url: Url) -> None:
    print("Retrieving nodegroups: ", end="", flush=True)
    sparql_connection(base_url, None, None)
    semtk3.retrieve_from_store(regexp, directory)
    print("OK")

def dispatch_data_export(args: SimpleNamespace) -> None:
    conn = sparql_connection(args.base_url, args.data_graph, args.triple_store)
    run_query(conn, args.nodegroup)

def dispatch_data_import(args: SimpleNamespace) -> None:
    """Implementation of the data import subcommand"""
    ingest_csv_driver(Path(args.config), args.base_url, args.data_graph, args.triple_store)

def dispatch_plumbing_model(args: SimpleNamespace) -> None:
    """Implementation of the plumbing model subcommand"""
    ingest_owl_driver(Path(args.config), args.base_url, args.triple_store)

def dispatch_plumbing_storenodegroups(args: SimpleNamespace) -> None:
    store_nodegroups_driver(args.directory, args.base_url)

def dispatch_plumbing_retrievenodegroups(args: SimpleNamespace) -> None:
    retrieve_nodegroups_driver(args.regexp, args.directory, args.base_url)

def main() -> None:
    """Main function"""

    # Sets up colors for Windows users
    colorama.init()

    # Register our custom color formatter for our logger
    ch = logging.StreamHandler()
    ch.setFormatter(CustomFormatter())
    logger.addHandler(ch)

    parser = argparse.ArgumentParser(description='RACK in a Box toolkit')
    parser.add_argument('--base-url', type=str, default=DEFAULT_BASE_URL, help='Base SemTK instance URL')
    parser.add_argument('--triple-store', type=str, help='Override Fuseki URL')

    subparsers = parser.add_subparsers(dest='command')

    data_parser = subparsers.add_parser('data', help='Import or export CSV data')
    data_subparsers = data_parser.add_subparsers(dest='command', required=True)
    data_import_parser = data_subparsers.add_parser('import', help='Import CSV data')
    data_export_parser = data_subparsers.add_parser('export', help='Export query results')

    plumbing_parser = subparsers.add_parser('plumbing', help='Tools for RACK developers')
    plumbing_subparsers = plumbing_parser.add_subparsers(dest='command', required=True)
    plumbing_model_parser = plumbing_subparsers.add_parser('model', help='Modify the data model')
    plumbing_storenodegroups_parser = plumbing_subparsers.add_parser('store-nodegroups', help='Store nodegroups into RACK')
    plumbing_retrievenodegroups_parser = plumbing_subparsers.add_parser('retrieve-nodegroups', help='Retrieve nodegroups from RACK')

    data_import_parser.add_argument('config', type=str, help='Configuration YAML file')
    data_import_parser.add_argument('--data-graph', type=str, help='Override data graph URL')
    data_import_parser.set_defaults(func=dispatch_data_import)

    data_export_parser.add_argument('nodegroup', type=str, help='ID of nodegroup')
    data_export_parser.add_argument('data_graph', type=str, help='Data graph URL')
    data_export_parser.set_defaults(func=dispatch_data_export)

    plumbing_model_parser.add_argument('config', type=str, help='Configuration YAML file')
    plumbing_model_parser.set_defaults(func=dispatch_plumbing_model)

    plumbing_storenodegroups_parser.add_argument('directory', type=str, help='Nodegroup directory')
    plumbing_storenodegroups_parser.set_defaults(func=dispatch_plumbing_storenodegroups)

    plumbing_retrievenodegroups_parser.add_argument('regexp', type=str, help='Nodegroup selection regular expression')
    plumbing_retrievenodegroups_parser.add_argument('directory', type=str, help='Nodegroup directory')
    plumbing_retrievenodegroups_parser.set_defaults(func=dispatch_plumbing_retrievenodegroups)

    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO)

    if args.base_url.endswith('/'):
        logging.warning('Trimming the final \'/\' from your base_url')
        args.base_url = args.base_url[:-1]

    try:
        if args.command is None:
            logger.error('Subcommand required (use --help to see options)')
            sys.exit(1)
        try:
            func = args.func
        except AttributeError:
            logger.error('Unknown subcommand: %s', args.command)
            sys.exit(1)
        func(args)
    except requests.ConnectionError as exc:
        logger.error('Connection failure\n%s', exc)
        sys.exit(1)
    except semtk3.restclient.RestException as exc:
        logger.error(f'REST Endpoint Failure\n{exc}')
        sys.exit(1)
    except FileNotFoundError as exc:
        logger.error('File not found\n%s', exc)
        sys.exit(1)
    except yaml.YAMLError as exc:
        logger.error('Failed to load YAML configuration file: %s\n%s', args.config, exc)
        sys.exit(1)
    except ValidationError as exc:
        logger.error('Bad configuration file: %s\n%s', args.config, exc)
        sys.exit(1)

if __name__ == "__main__":
    main()
