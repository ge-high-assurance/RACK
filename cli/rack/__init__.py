#!/usr/bin/env python3
#
# Copyright (c) 2020, Galois, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

"""Loads CSV data into RACK in a Box

This simple process can be adapted to import other data into RACK for experimentation.
"""
# standard imports
import argparse
import csv
from enum import Enum, unique
from io import StringIO
import json
import logging
from os import environ
from pathlib import Path
import re
import sys
from typing import Any, Callable, Dict, List, Optional, NewType, TypeVar, cast
from types import SimpleNamespace

# library imports
import colorama
from colorama import Fore, Style
from jsonschema import ValidationError, validate
from tabulate import tabulate
import requests
import semtk3
from semtk3.semtktable import SemtkTable
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

class CLIMethod(Enum):
    """Enumeration of the CLI methods (for context in error reporting)"""
    DATA_IMPORT = "data import"
    MODEL_IMPORT = "model import"
    OTHER_CLI_METHOD = "..."

# In the absence of overwrite, this will be the default
cliMethod = CLIMethod.OTHER_CLI_METHOD

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

# Bug https://github.com/PyCQA/pylint/issues/1953
# pylint: disable=unused-argument
def with_status(prefix: str, suffix: Callable[[Any], str] = lambda _ : '') -> Callable[[Decoratee], Decoratee]:
    """This decorator writes the prefix, followed by three dots, then runs the
    decorated function.  Upon success, it appends OK, upon failure, it appends
    FAIL.  If suffix is set, the result of the computation is passed to suffix,
    and the resulting string is appended after OK."""
    def decorator(func: Decoratee) -> Decoratee:
        def wrapper(*args: Any, **kwargs: Any) -> Any:
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
# pylint: enable=unused-argument

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

def format_semtk_table(semtk_table: SemtkTable, export_format: ExportFormat = ExportFormat.TEXT, headers: bool = True) -> str:

    if export_format == ExportFormat.TEXT:
        if headers is True:
            return tabulate(semtk_table.get_rows(), headers=semtk_table.get_column_names())
        else:
            return tabulate(semtk_table.get_rows())

    elif export_format == ExportFormat.CSV:
        output = StringIO()
        writer = csv.writer(output)
        if headers is True:
            writer.writerow(semtk_table.get_column_names())
        for row in semtk_table.get_rows():
            writer.writerow(row)
        return output.getvalue()

    print(str_bad(f"Internal error: incomplete implementation of run_query for '{export_format}'"))
    sys.exit(1)

def run_query(conn: Connection, nodegroup: str, export_format: ExportFormat = ExportFormat.TEXT, headers: bool = True, path: Optional[Path] = None) -> None:
    semtk3.SEMTK3_CONN_OVERRIDE = conn
    semtk_table = semtk3.select_by_id(nodegroup)
    formatted_table = format_semtk_table(semtk_table, export_format=export_format, headers=headers)
    if path is None:
        print()
        print(formatted_table)
    else:
        with open(path, mode="w") as f:
            print(formatted_table, file=f)

def run_count_query(conn: Connection, nodegroup: str) -> None:
    semtk3.SEMTK3_CONN_OVERRIDE = conn
    semtk_table = semtk3.count_by_id(nodegroup)
    print(semtk_table.get_rows()[0][0])

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

def ingest_data_driver(config_path: Path, base_url: Url, data_graph: Optional[Url], triple_store: Optional[Url], clear: bool) -> None:
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

    if clear:
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

def ingest_owl_driver(config_path: Path, base_url: Url, triple_store: Optional[Url], clear: bool) -> None:
    """Use an import.yaml file to ingest multiple OWL files into the model graph."""
    with open(config_path, 'r') as config_file:
        config = yaml.safe_load(config_file)
        validate(config, INGEST_OWL_CONFIG_SCHEMA)

    files = config['files']
    base_path = config_path.parent

    conn = sparql_connection(base_url, None, triple_store)

    if clear:
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

def list_nodegroups_driver(base_url: Url) -> None:

    @with_status('Listing nodegroups')
    def list_nodegroups() -> SemtkTable:
        sparql_connection(base_url, None, None)
        return semtk3.get_nodegroup_store_data()

    print(format_semtk_table(list_nodegroups()))

def confirm(on_confirmed: Callable[[], None], yes: bool = False) -> None:
    if yes:
        on_confirmed()
        return
    print(f'{Fore.CYAN}Confirm [y/N]')
    if input().lower() == 'y':
        print(str_good('Confirmed.'))
        on_confirmed()
    else:
        print(str_bad('Aborted.'))

def delete_nodegroup(nodegroup: str) -> None:
    @with_status(f'Deleting {str_highlight(nodegroup)}')
    def delete() -> None:
        semtk3.delete_nodegroup_from_store(nodegroup)
    delete()

def delete_nodegroups_driver(nodegroups: List[str], ignore_nonexistent: bool, yes: bool, use_regexp: bool, base_url: Url) -> None:
    if not nodegroups:
        print('No nodegroups specified for deletion: doing nothing.')
        return

    sparql_connection(base_url, None, None)
    allIDs = semtk3.get_nodegroup_store_data().get_column('ID')

    if use_regexp:
        regexps = [re.compile(regex_str) for regex_str in nodegroups]
        nonexistent = [r.pattern for r in regexps if not any(r.search(i) for i in allIDs)]
        to_delete = [i for i in allIDs if any(r.search(i) for r in regexps)]
    else:
        nonexistent = [n for n in nodegroups if n not in allIDs]
        to_delete = [n for n in nodegroups if n in allIDs]

    if nonexistent:
        print('The following nodegroups do not exist: {}'.format(', '.join(str_highlight(s) for s in nonexistent)))
        if not ignore_nonexistent:
            print(str_bad('Aborting. Remove the nonexistent IDs or use --ignore-nonexistent.'))
            sys.exit(1)

    if not yes:
        print('The following nodegroups would be removed: {}'.format(', '.join(str_highlight(s) for s in to_delete)))

    def on_confirmed() -> None:
        for nodegroup in to_delete:
            delete_nodegroup(nodegroup)
    confirm(on_confirmed, yes)

def delete_all_nodegroups_driver(yes: bool, base_url: Url) -> None:
    sparql_connection(base_url, None, None)
    allIDs = semtk3.get_nodegroup_store_data().get_column('ID')

    if not yes:
        print('The following nodegroups would be removed: {}'.format(' '.join(str_highlight(s) for s in allIDs)))

    def on_confirmed() -> None:
        for nodegroup in allIDs:
            delete_nodegroup(nodegroup)
    confirm(on_confirmed, yes)

def dispatch_data_export(args: SimpleNamespace) -> None:
    conn = sparql_connection(args.base_url, args.data_graph, args.triple_store)
    run_query(conn, args.nodegroup, export_format=args.format, headers=not args.no_headers, path=args.file)

def dispatch_data_count(args: SimpleNamespace) -> None:
    conn = sparql_connection(args.base_url, args.data_graph, args.triple_store)
    run_count_query(conn, args.nodegroup)

def dispatch_data_import(args: SimpleNamespace) -> None:
    """Implementation of the data import subcommand"""
    cliMethod = CLIMethod.DATA_IMPORT
    ingest_data_driver(Path(args.config), args.base_url, args.data_graph, args.triple_store, args.clear)

def dispatch_model_import(args: SimpleNamespace) -> None:
    """Implementation of the plumbing model subcommand"""
    cliMethod = CLIMethod.MODEL_IMPORT
    ingest_owl_driver(Path(args.config), args.base_url, args.triple_store, args.clear)

def dispatch_nodegroups_import(args: SimpleNamespace) -> None:
    store_nodegroups_driver(args.directory, args.base_url)

def dispatch_nodegroups_export(args: SimpleNamespace) -> None:
    retrieve_nodegroups_driver(args.regexp, args.directory, args.base_url)

def dispatch_nodegroups_list(args: SimpleNamespace) -> None:
    list_nodegroups_driver(args.base_url)

def dispatch_nodegroups_delete(args: SimpleNamespace) -> None:
    delete_nodegroups_driver(args.nodegroups, args.ignore_nonexistent, args.yes, args.regexp, args.base_url)

def dispatch_nodegroups_deleteall(args: SimpleNamespace) -> None:
    delete_all_nodegroups_driver(args.yes, args.base_url)

def get_argument_parser() -> argparse.ArgumentParser:

    parser = argparse.ArgumentParser(description='RACK in a Box toolkit')
    parser.add_argument('--base-url', type=str, default=environ.get('BASE_URL') or DEFAULT_BASE_URL, help='Base SemTK instance URL')
    parser.add_argument('--triple-store', type=str, default=environ.get('TRIPLE_STORE'), help='Override Fuseki URL')
    parser.add_argument('--log-level', type=str, default='WARNING', help='Assign logger severity level')

    subparsers = parser.add_subparsers(dest='command')

    data_parser = subparsers.add_parser('data', help='Import or export CSV data')
    data_subparsers = data_parser.add_subparsers(dest='command')
    data_import_parser = data_subparsers.add_parser('import', help='Import CSV data')
    data_export_parser = data_subparsers.add_parser('export', help='Export query results')
    data_count_parser = data_subparsers.add_parser('count', help='Count matched query rows')

    model_parser = subparsers.add_parser('model', help='Interact with SemTK model')
    model_subparsers = model_parser.add_subparsers(dest='command')
    model_import_parser = model_subparsers.add_parser('import', help='Modify the data model')

    nodegroups_parser = subparsers.add_parser('nodegroups', help='Interact with SemTK nodegroups')
    nodegroups_subparsers = nodegroups_parser.add_subparsers(dest='command')
    nodegroups_import_parser = nodegroups_subparsers.add_parser('import', help='Store nodegroups into RACK')
    nodegroups_export_parser = nodegroups_subparsers.add_parser('export', help='Retrieve nodegroups from RACK')
    nodegroups_list_parser = nodegroups_subparsers.add_parser('list', help='List nodegroups from RACK')
    nodegroups_delete_parser = nodegroups_subparsers.add_parser('delete', help='Delete some nodegroups from RACK')
    nodegroups_deleteall_parser = nodegroups_subparsers.add_parser('delete-all', help='Delete all nodegroups from RACK')

    data_import_parser.add_argument('config', type=str, help='Configuration YAML file')
    data_import_parser.add_argument('--data-graph', type=str, help='Override data graph URL')
    data_import_parser.add_argument('--clear', action='store_true', help='Clear data graph before import')
    data_import_parser.set_defaults(func=dispatch_data_import)

    data_export_parser.add_argument('nodegroup', type=str, help='ID of nodegroup')
    data_export_parser.add_argument('data_graph', type=str, help='Data graph URL')
    data_export_parser.add_argument('--format', type=ExportFormat, help='Export format', choices=list(ExportFormat), default=ExportFormat.TEXT)
    data_export_parser.add_argument('--no-headers', action='store_true', help='Omit header row')
    data_export_parser.add_argument('--file', type=Path, help='Output to file')
    data_export_parser.set_defaults(func=dispatch_data_export)

    data_count_parser.add_argument('nodegroup', type=str, help='ID of nodegroup')
    data_count_parser.add_argument('data_graph', type=str, help='Data graph URL')
    data_count_parser.set_defaults(func=dispatch_data_count)

    model_import_parser.add_argument('config', type=str, help='Configuration YAML file')
    model_import_parser.set_defaults(func=dispatch_model_import)
    model_import_parser.add_argument('--clear', action='store_true', help='Clear model graph before import')

    nodegroups_import_parser.add_argument('directory', type=str, help='Nodegroup directory')
    nodegroups_import_parser.set_defaults(func=dispatch_nodegroups_import)

    nodegroups_export_parser.add_argument('regexp', type=str, help='Nodegroup selection regular expression')
    nodegroups_export_parser.add_argument('directory', type=str, help='Nodegroup directory')
    nodegroups_export_parser.set_defaults(func=dispatch_nodegroups_export)

    nodegroups_list_parser.set_defaults(func=dispatch_nodegroups_list)

    nodegroups_delete_parser.add_argument('nodegroups', type=str, nargs='+', help='IDs of nodegroups to be removed')
    nodegroups_delete_parser.add_argument('--ignore-nonexistent', action='store_true', help='Ignore nonexistent IDs')
    nodegroups_delete_parser.add_argument('--yes', action='store_true', help='Automatically confirm deletion')
    nodegroups_delete_parser.add_argument('--regexp', action='store_true', help='Match nodegroup ID with regular expression')
    nodegroups_delete_parser.set_defaults(func=dispatch_nodegroups_delete)

    nodegroups_deleteall_parser.add_argument('--yes', action='store_true', help='Automatically confirm deletion')
    nodegroups_deleteall_parser.set_defaults(func=dispatch_nodegroups_deleteall)

    return parser
