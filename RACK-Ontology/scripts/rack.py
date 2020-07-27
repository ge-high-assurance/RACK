#!/usr/bin/env python3
"""Loads CSV data into RACK in a Box

This simple process can be adapted to import other data into RACK for experimentation.
"""
# standard imports
import argparse
import csv
from enum import Enum, unique
import json
import logging
from pathlib import Path
import sys
from typing import Any, Callable, Dict, Optional, NewType, TypeVar, cast
from types import SimpleNamespace

# library imports
import colorama
from colorama import Fore, Style
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

@unique
class ExportFormat(Enum):
    """Enumeration of data export formats"""
    TEXT = "text"  # plain text
    CSV = "csv"  # comma-separated values

    def __str__(self) -> str:
        """For inclusion in --help"""
        return self.value

DEFAULT_BASE_URL: Url = Url("http://localhost")

MODEL_GRAPH: Url = Url("http://rack001/model")
DEFAULT_DATA_GRAPH = Url("http://rack001/data")

INGEST_CSV_CONFIG_SCHEMA: Dict[str, Any] = {
    'type': 'object',
    'additionalProperties': False,
    'required': ['ingestion-steps'],
    'properties': {
        'ingestion-steps': {
            'type': 'array',
            'items': {
                'oneOf': [
                    {
                        'type': 'object',
                        'additionalProperties': False,
                        'required': ['nodegroup', 'csv'],
                        'properties': {
                            'nodegroup': {'type': 'string'},
                            'csv': {'type': 'string'}
                        }
                    },
                    {
                        'type': 'object',
                        'addtionalProperties': False,
                        'required': ['owl'],
                        'properties': {'owl': {'type': 'string'}}
                    }
                ]
            }
        },
        'data-graph': {'type': 'string'}
    }
}

INGEST_OWL_CONFIG_SCHEMA: Dict[str, Any] = {
    'type' : 'object',
    'additionalProperties': False,
    'required': ['files'],
    'properties': {
        'files': {
            'type': 'array',
            'contains': {'type': 'string'}}
    }
}

def str_good(s: str) -> str:
    return Fore.GREEN + s + Style.RESET_ALL

def str_bad(s: str) -> str:
    return Fore.RED + s + Style.RESET_ALL

def str_highlight(s: str) -> str:
    return Fore.MAGENTA + s + Style.RESET_ALL

class CustomFormatter(logging.Formatter):
    """Add custom styles to our log"""

    format_string = "[%(filename)s:%(lineno)d] %(levelname)s: %(message)s"

    FORMATS = {
        logging.DEBUG: format_string,
        logging.INFO: Fore.CYAN + format_string + Style.RESET_ALL,
        logging.WARNING: Fore.YELLOW + format_string + Style.RESET_ALL,
        logging.ERROR: str_bad(format_string),
        logging.CRITICAL: Style.BRIGHT + Fore.RED + format_string + Style.RESET_ALL
    }

    def format(self, record: logging.LogRecord) -> str:
        which_format = self.FORMATS.get(record.levelno)
        formatter = logging.Formatter(which_format, "%H:%M:%S")
        return formatter.format(record)

Decoratee = TypeVar('Decoratee', bound=Callable[..., Any])

def with_status(prefix: str, suffix: Callable[[Any], str] = lambda _ : '') -> Callable[[Decoratee], Decoratee]:
    """This decorator writes the prefix, followed by three dots, then runs the
    decorated function.  Upon success, it appends OK, upon failure, it appends
    FAIL.  If suffix is set, the result of the computation is passed to suffix,
    and the resulting string is appended after OK."""
    def decorator(func: Decoratee) -> Decoratee:
        def wrapper(*args, **kwargs):
            nonlocal prefix
            prefix += '...'
            print(f'{prefix: <60}', end='', flush=True)
            try:
                result = func(*args, **kwargs)
            except Exception as e:
                print(str_bad('FAIL'))
                raise e
            print(str_good('OK') + suffix(result))
            return result
        return cast(Decoratee, wrapper)
    return decorator

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
    print(result.lstrip())

def run_query(conn: Connection, nodegroup: str, format: ExportFormat = ExportFormat.TEXT) -> None:
    semtk3.SEMTK3_CONN_OVERRIDE = conn
    semtk_table = semtk3.select_by_id(nodegroup)
    print()
    if format == ExportFormat.TEXT:
        print(tabulate(semtk_table.get_rows(), headers=semtk_table.get_column_names()))
    elif format == ExportFormat.CSV:
        writer = csv.writer(sys.stdout)
        for row in semtk_table.get_rows():
            writer.writerow(row)
    else:
        print(f"Internal error: incomplete implementation of run_query for '{format}'")
        sys.exit(1)

def ingest_csv(conn: Connection, nodegroup: str, csv_name: Path) -> None:
    """Ingest a CSV file using the named nodegroup."""

    def suffix(result: dict) -> str:
        return f' Records: {result["recordsProcessed"]: <7} Failures: {result["failuresEncountered"]}'

    @with_status(f'Loading {str_highlight(nodegroup)}', suffix)
    def go() -> dict:
        with open(csv_name, "r") as csv_file:
            csv = csv_file.read()
        return semtk3.ingest_by_id(nodegroup, csv, conn)

    go()

def ingest_owl(conn: Connection, owl_file: Path) -> None:
    """Upload an OWL file into the model graph."""
    @with_status(f'Ingesting {str_highlight(str(owl_file))}')
    def go() -> None:
        return semtk3.upload_owl(owl_file, conn, "rack", "rack")
    go()

def ingest_data_driver(config_path: Path, base_url: Url, data_graph: Optional[Url], triple_store: Optional[Url]) -> None:
    """Use an import.yaml file to ingest multiple CSV files into the data graph."""
    with open(config_path, 'r') as config_file:
        config = yaml.safe_load(config_file)
        validate(config, INGEST_CSV_CONFIG_SCHEMA)

    steps = config['ingestion-steps']
    data_graph = data_graph or config['data-graph']
    base_path = config_path.parent

    if data_graph is None:
        logger.warning("Defaulting data-graph to %s", DEFAULT_DATA_GRAPH)
        data_graph = DEFAULT_DATA_GRAPH

    conn = sparql_connection(base_url, data_graph, triple_store)

    clear_graph(conn)
    for step in steps:
        if 'owl' in step:
            owl_file = step['owl']
            print(f'Ingesting {str_highlight(str(owl_file)): <40}', end="")
            try:
                semtk3.upload_owl(base_path / owl_file, conn, "rack", "rack", semtk3.SEMTK3_CONN_DATA)
            except Exception as e:
                print(str_bad(' FAIL'))
                raise e
            print(str_good(' OK'))
        elif 'csv' in step:
            ingest_csv(conn, step['nodegroup'], base_path / step['csv'])

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

@with_status('Storing nodegroups')
def store_nodegroups_driver(directory: Path, base_url: Url) -> None:
    sparql_connection(base_url, None, None)
    semtk3.store_nodegroups(directory)

@with_status('Retrieving nodegroups')
def retrieve_nodegroups_driver(regexp: str, directory: Path, base_url: Url) -> None:
    sparql_connection(base_url, None, None)
    semtk3.retrieve_from_store(regexp, directory)

def dispatch_data_export(args: SimpleNamespace) -> None:
    conn = sparql_connection(args.base_url, args.data_graph, args.triple_store)
    run_query(conn, args.nodegroup, format=args.format)

def dispatch_data_import(args: SimpleNamespace) -> None:
    """Implementation of the data import subcommand"""
    ingest_data_driver(Path(args.config), args.base_url, args.data_graph, args.triple_store)

def dispatch_plumbing_model(args: SimpleNamespace) -> None:
    """Implementation of the plumbing model subcommand"""
    ingest_owl_driver(Path(args.config), args.base_url, args.triple_store)

def dispatch_plumbing_storenodegroups(args: SimpleNamespace) -> None:
    store_nodegroups_driver(args.directory, args.base_url)

def dispatch_plumbing_retrievenodegroups(args: SimpleNamespace) -> None:
    retrieve_nodegroups_driver(args.regexp, args.directory, args.base_url)

def get_argument_parser() -> argparse.ArgumentParser:

    parser = argparse.ArgumentParser(description='RACK in a Box toolkit')
    parser.add_argument('--base-url', type=str, default=DEFAULT_BASE_URL, help='Base SemTK instance URL')
    parser.add_argument('--triple-store', type=str, help='Override Fuseki URL')
    parser.add_argument('--log-level', type=str, default='WARNING', help='Assign logger severity level')

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
    data_export_parser.add_argument('--format', type=ExportFormat, help='Export format', choices=list(ExportFormat), default=ExportFormat.TEXT)
    data_export_parser.set_defaults(func=dispatch_data_export)

    plumbing_model_parser.add_argument('config', type=str, help='Configuration YAML file')
    plumbing_model_parser.set_defaults(func=dispatch_plumbing_model)

    plumbing_storenodegroups_parser.add_argument('directory', type=str, help='Nodegroup directory')
    plumbing_storenodegroups_parser.set_defaults(func=dispatch_plumbing_storenodegroups)

    plumbing_retrievenodegroups_parser.add_argument('regexp', type=str, help='Nodegroup selection regular expression')
    plumbing_retrievenodegroups_parser.add_argument('directory', type=str, help='Nodegroup directory')
    plumbing_retrievenodegroups_parser.set_defaults(func=dispatch_plumbing_retrievenodegroups)

    return parser

def main() -> None:
    """Main function"""

    # Sets up colors for Windows users
    colorama.init()

    # Register our custom color formatter for our logger
    stream_handler = logging.StreamHandler()
    stream_handler.setFormatter(CustomFormatter())
    logger.propagate = False
    logger.addHandler(stream_handler)
    semtk3_logger = logging.getLogger("semtk3")
    semtk3_logger.handlers = []
    semtk3_logger.propagate = False
    semtk3_logger.addHandler(stream_handler)

    args = get_argument_parser().parse_args()

    try:
        logging.basicConfig(level=args.log_level)
    except ValueError:
        logger.error('Bad log level specified')
        sys.exit(1)

    if args.base_url.endswith('/'):
        logger.warning('Trimming the final \'/\' from your base_url')
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
        logger.error('REST Endpoint Failure\n%s', exc)
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
