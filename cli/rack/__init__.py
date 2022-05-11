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
                        'additionalProperties': False,
                        'required': ['owl'],
                        'properties': {'owl': {'type': 'string'}}
                    },
                    {
                        'type': 'object',
                        'additionalProperties': False,
                        'required': ['name', 'creator', 'nodegroup_json'],
                        'properties': {
                            'name': {'type': 'string'},
                            'creator': {'type': 'string'},
                            'comment': {'type': 'string'},
                            'nodegroup_json': {'type': 'string'}
                        }
                    },
                    {
                        'type': 'object',
                        'additionalProperties': False,
                        'required': ['count', 'nodegroup'],
                        'properties': {
                            'count': {'type': 'number'},
                            'nodegroup': {'type': 'string'},
                            'constraints': {'type': 'array', 'items': { 'type': 'string'} }
                        }
                    }
                ]
            }
        },
        'data-graph': {'type': 'string'},
        'extra-data-graphs': {
            'type': 'array',
            'contains': {'type': 'string'}}
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
    prefix += '...'
    def decorator(func: Decoratee) -> Decoratee:
        def wrapper(*args: Any, **kwargs: Any) -> Any:
            nonlocal prefix
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

def sparql_connection(base_url: Url, data_graph: Optional[Url], extra_data_graphs: List[Url], triple_store: Optional[Url]) -> Connection:
    """Generate a SPARQL connection value."""

    semtk3.set_host(base_url)
    # Default to RACK in a Box triple-store location
    triple_store = triple_store or Url("http://localhost:3030/RACK")
    conn: Dict[str, Any] = {
        "name": "%NODEGROUP%",
        "model": [{"type": "fuseki", "url": triple_store, "graph": MODEL_GRAPH}],
        "data": []
        }
    if data_graph is not None:
        conn["data"].append({"type": "fuseki", "url": triple_store, "graph": data_graph})
    for graph in extra_data_graphs:
        conn["data"].append({"type": "fuseki", "url": triple_store, "graph": graph})
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

def generate_constraints(constraint_texts: List[str]) -> List[Any]:
    constraint_re = re.compile(r'^(?P<var>[^<>=~:]*)((:(?P<val1>.*)(?P<op2><=?>)(?P<val2>.*))|(?P<op>~|<=|>=|=|<|>)(?P<val>.*))$')
    operators = {
        '~': semtk3.OP_REGEX,
        '=': semtk3.OP_MATCHES,
        '>': semtk3.OP_GREATERTHAN,
        '>=': semtk3.OP_GREATERTHANOREQUALS,
        '<': semtk3.OP_LESSTHAN,
        '<=': semtk3.OP_LESSTHANOREQUALS,
        '<>': semtk3.OP_VALUEBETWEEN,
        '<=>': semtk3.OP_VALUEBETWEENUNINCLUSIVE
    }

    result = []
    for constraint_text in constraint_texts:
        match = constraint_re.match(constraint_text)
        if match is None:
            print(str_bad('Unsupported constraint: ' + constraint_text))
            sys.exit(1)
        
        if match['op'] is not None:
            c = semtk3.build_constraint(match['var'], operators[match['op']], [match['val']])
        else:
            c = semtk3.build_constraint(match['var'], operators[match['op2']], [match['val1'], match['val2']])
        result.append(c)

    return result

def run_query(conn: Connection, nodegroup: str, export_format: ExportFormat = ExportFormat.TEXT, headers: bool = True, path: Optional[Path] = None, constraints: Optional[List[str]] = None) -> None:
    semtk3.SEMTK3_CONN_OVERRIDE = conn

    runtime_constraints = generate_constraints(constraints or [])

    semtk_table = semtk3.select_by_id(nodegroup, runtime_constraints=runtime_constraints)
    formatted_table = format_semtk_table(semtk_table, export_format=export_format, headers=headers)
    if path is None:
        print(formatted_table)
    else:
        with open(path, mode='w', encoding='utf-8') as f:
            print(formatted_table, file=f)

def run_count_query(conn: Connection, nodegroup: str, constraints: Optional[List[str]] = None) -> None:
    semtk3.SEMTK3_CONN_OVERRIDE = conn

    runtime_constraints = generate_constraints(constraints or [])

    semtk_table = semtk3.count_by_id(nodegroup, runtime_constraints=runtime_constraints)
    print(semtk_table.get_rows()[0][0])

def ingest_csv(conn: Connection, nodegroup: str, csv_name: Path) -> None:
    """Ingest a CSV file using the named nodegroup."""

    def suffix(result: dict) -> str:
        return f' Records: {result}'

    @with_status(f'Loading {str_highlight(nodegroup)}', suffix)
    def go() -> dict:
        with open(csv_name, mode='r', encoding='utf-8-sig') as csv_file:
            csv = csv_file.read()

        return semtk3.ingest_by_id(nodegroup, csv, conn)

    go()

def ingest_owl(conn: Connection, owl_file: Path) -> None:
    """Upload an OWL file into the model graph."""
    @with_status(f'Ingesting {str_highlight(str(owl_file))}')
    def go() -> None:
        return semtk3.upload_owl(owl_file, conn, "rack", "rack")
    go()

def ingest_data_driver(config_path: Path, base_url: Url, data_graphs: Optional[List[Url]], triple_store: Optional[Url], clear: bool) -> None:
    """Use an import.yaml file to ingest multiple CSV files into the data graph."""
    with open(config_path, mode='r', encoding='utf-8-sig') as config_file:
        config = yaml.safe_load(config_file)
        validate(config, INGEST_CSV_CONFIG_SCHEMA)

    steps = config['ingestion-steps']
    
    if data_graphs is not None:
        data_graph = data_graphs[0]
    elif 'data-graph' in config:
        data_graph = config['data-graph']
    else:
        logger.warning("Defaulting data-graph to %s", DEFAULT_DATA_GRAPH)
        data_graph = DEFAULT_DATA_GRAPH

    base_path = config_path.parent

    if data_graphs is not None:
        extra_data_graphs = data_graphs[1:]
    elif 'extra-data-graphs' in config:
        extra_data_graphs = config['extra-data-graphs']
    else:
        extra_data_graphs = []

    conn = sparql_connection(base_url, data_graph, extra_data_graphs, triple_store)

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

        elif 'nodegroup_json' in step:
            with open(base_path / step['nodegroup_json']) as f:
                nodegroup_json_str = f.read()
            name = step['name']
            comment = step.get('comment', '')
            creator = step['creator']
            print(f'Adding nodegroup {str_highlight(name): <40}', end="")
            try:
                semtk3.delete_nodegroup_from_store(name) # succeeds even if not found
                semtk3.store_nodegroup(name, comment, creator, nodegroup_json_str)
            except Exception as e:
                print(str_bad(' FAIL'))
                raise e
            print(str_good(' OK'))
        
        elif 'count' in step:
            expected = step['count']
            name = step['nodegroup']
            runtime_constraints = generate_constraints(step.get('constraints', []))
            
            print(f'Counting nodegroup {str_highlight(name): <40}', end="")

            semtk_table = semtk3.count_by_id(name, runtime_constraints=runtime_constraints)
            got = int(semtk_table.get_rows()[0][0])
            if got == expected:
                print(str_good(' OK'))
            else:
                print(str_bad(f' FAIL got:{got} expected:{expected}'))


def ingest_owl_driver(config_path: Path, base_url: Url, triple_store: Optional[Url], clear: bool) -> None:
    """Use an import.yaml file to ingest multiple OWL files into the model graph."""
    with open(config_path, mode='r', encoding='utf-8-sig') as config_file:
        config = yaml.safe_load(config_file)
        validate(config, INGEST_OWL_CONFIG_SCHEMA)

    files = config['files']
    base_path = config_path.parent

    conn = sparql_connection(base_url, None, [], triple_store)

    if clear:
        clear_graph(conn, which_graph=Graph.MODEL)

    for file in files:
        ingest_owl(conn, base_path / file)

@with_status('Clearing')
def clear_driver(base_url: Url, data_graphs: Optional[List[Url]], triple_store: Optional[Url], graph: Graph) -> None:
    """Clear the given data graphs"""
    if data_graphs is None:
        conn = sparql_connection(base_url, DEFAULT_DATA_GRAPH, [], triple_store)
        clear_graph(conn, which_graph=graph)
    else:
        for data_graph in data_graphs:
            conn = sparql_connection(base_url, data_graph, [], triple_store)
            clear_graph(conn, which_graph=graph)

@with_status('Storing nodegroups')
def store_nodegroups_driver(directory: Path, base_url: Url) -> None:
    sparql_connection(base_url, None, [], None)
    semtk3.store_nodegroups(directory)

@with_status('Storing nodegroup')
def store_nodegroup_driver(name: str, creator: str, filename: str, comment: Optional[str], base_url: Url, kind: str) -> None:
    sparql_connection(base_url, None, [], None)

    with open(filename) as f:
        nodegroup_json_str = f.read()

    if kind == 'nodegroup':
        item_type = semtk3.STORE_ITEM_TYPE_NODEGROUP
    elif kind == 'report':
        item_type = semtk3.STORE_ITEM_TYPE_REPORT

    semtk3.delete_item_from_store(name, item_type) # succeeds even if not found
    semtk3.store_item(name, comment or '', creator, nodegroup_json_str, item_type)

@with_status('Converting nodegroup to SPARQL')
def sparql_nodegroup_driver(base_url: Url, filename: str) -> None:
    with open(filename) as f:
        nodegroup_json_str = f.read()
    from urllib.parse import urljoin, urlparse
    urlorig = urlparse(base_url)
    urlbase = urlorig._replace(netloc=':'.join(
                                    urlorig.netloc.split(':')[:1] + ['12059'])
                               ).geturl()
    rsp = requests.post(urljoin(urlbase, 'nodeGroup/generateSelect'),
                        json={'jsonRenderedNodeGroup': nodegroup_json_str})
    rsp.raise_for_status()
    print(rsp.json()['simpleresults']['SparqlQuery'])

@with_status('Retrieving nodegroups')
def retrieve_nodegroups_driver(regexp: str, directory: Path, base_url: Url, kind: str) -> None:
    sparql_connection(base_url, None, [], None)
    if kind == 'nodegroup':
        item_type = semtk3.STORE_ITEM_TYPE_NODEGROUP
    elif kind == 'report':
        item_type = semtk3.STORE_ITEM_TYPE_REPORT
    else:
        item_type = semtk3.STORE_ITEM_TYPE_ALL
    semtk3.retrieve_items_from_store(regexp, directory, item_type)

def list_nodegroups_driver(base_url: Url) -> None:

    @with_status('Listing nodegroups')
    def list_nodegroups() -> SemtkTable:
        sparql_connection(base_url, None, [], None)
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

def delete_store_item(id: str, item_type: str) -> None:
    @with_status(f'Deleting {str_highlight(id)}')
    def delete() -> None:
        semtk3.delete_store_item(id, item_type)
    delete()

def delete_nodegroups_driver(nodegroups: List[str], ignore_nonexistent: bool, yes: bool, use_regexp: bool, base_url: Url) -> None:
    if not nodegroups:
        print('No nodegroups specified for deletion: doing nothing.')
        return

    sparql_connection(base_url, None, [], None)
    allIDs = semtk3.get_store_table().get_column('ID')

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
    sparql_connection(base_url, None, [], None)

    table = semtk3.get_store_table()
    id_col = table.get_column_index('ID')
    type_col = table.get_column_index('itemType')

    if not yes:
        print('The following nodegroups would be removed: {}'.format(' '.join(str_highlight(s) for s in table.get_column(id_col))))

    def on_confirmed() -> None:

        for r in table.get_rows():
            semtk3.delete_item_from_store(r[id_col], r[type_col].split("#")[-1])

    confirm(on_confirmed, yes)

def dispatch_data_export(args: SimpleNamespace) -> None:
    conn = sparql_connection(args.base_url, args.data_graph[0], args.data_graph[1:], args.triple_store)
    run_query(conn, args.nodegroup, export_format=args.format, headers=not args.no_headers, path=args.file, constraints=args.constraint)

def dispatch_data_count(args: SimpleNamespace) -> None:
    conn = sparql_connection(args.base_url, args.data_graph[0], args.data_graph[1:], args.triple_store)
    run_count_query(conn, args.nodegroup, constraints=args.constraint)

def dispatch_data_import(args: SimpleNamespace) -> None:
    """Implementation of the data import subcommand"""
    cliMethod = CLIMethod.DATA_IMPORT
    ingest_data_driver(Path(args.config), args.base_url, args.data_graph, args.triple_store, args.clear)

def dispatch_model_import(args: SimpleNamespace) -> None:
    """Implementation of the plumbing model subcommand"""
    cliMethod = CLIMethod.MODEL_IMPORT
    ingest_owl_driver(Path(args.config), args.base_url, args.triple_store, args.clear)

def dispatch_data_clear(args: SimpleNamespace) -> None:
    """Implementation of the data clear subcommand"""
    clear_driver(args.base_url, args.data_graph, args.triple_store, Graph.DATA)

def dispatch_model_clear(args: SimpleNamespace) -> None:
    """Implementation of the model clear subcommand"""
    clear_driver(args.base_url, None, args.triple_store, Graph.MODEL)

def dispatch_nodegroups_import(args: SimpleNamespace) -> None:
    store_nodegroups_driver(args.directory, args.base_url)

def dispatch_nodegroups_store(args: SimpleNamespace) -> None:
    store_nodegroup_driver(args.name, args.creator, args.filename, args.comment, args.base_url, args.kind)

def dispatch_nodegroups_export(args: SimpleNamespace) -> None:
    retrieve_nodegroups_driver(args.regexp, args.directory, args.base_url, args.kind)

def dispatch_nodegroups_list(args: SimpleNamespace) -> None:
    list_nodegroups_driver(args.base_url)

def dispatch_nodegroups_delete(args: SimpleNamespace) -> None:
    delete_nodegroups_driver(args.nodegroups, args.ignore_nonexistent, args.yes, args.regexp, args.base_url)

def dispatch_nodegroups_deleteall(args: SimpleNamespace) -> None:
    delete_all_nodegroups_driver(args.yes, args.base_url)

def dispatch_nodegroups_sparql(args: SimpleNamespace) -> None:
    sparql_nodegroup_driver(args.base_url, args.filename)

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
    data_clear_parser = data_subparsers.add_parser('clear', help='Clear data graph')

    model_parser = subparsers.add_parser('model', help='Interact with SemTK model')
    model_subparsers = model_parser.add_subparsers(dest='command')
    model_import_parser = model_subparsers.add_parser('import', help='Modify the data model')
    model_clear_parser = model_subparsers.add_parser('clear', help='Clear model graph')

    nodegroups_parser = subparsers.add_parser('nodegroups', help='Interact with SemTK nodegroups')
    nodegroups_subparsers = nodegroups_parser.add_subparsers(dest='command')
    nodegroups_import_parser = nodegroups_subparsers.add_parser('import', help='Store nodegroups directory into RACK')
    nodegroups_export_parser = nodegroups_subparsers.add_parser('export', help='Retrieve nodegroups from RACK')
    nodegroups_store_parser = nodegroups_subparsers.add_parser('store', help='Store single nodegroup into RACK')
    nodegroups_list_parser = nodegroups_subparsers.add_parser('list', help='List nodegroups from RACK')
    nodegroups_delete_parser = nodegroups_subparsers.add_parser('delete', help='Delete some nodegroups from RACK')
    nodegroups_deleteall_parser = nodegroups_subparsers.add_parser('delete-all', help='Delete all nodegroups from RACK')
    nodegroups_sparql_parser = nodegroups_subparsers.add_parser('sparql', help='Show SPARQL query for nodegroup')

    data_import_parser.add_argument('config', type=str, help='Configuration YAML file')
    data_import_parser.add_argument('--data-graph', type=str, action='append', help='Data graph URL')
    data_import_parser.add_argument('--clear', action='store_true', help='Clear data graph before import')
    data_import_parser.set_defaults(func=dispatch_data_import)

    data_export_parser.add_argument('nodegroup', type=str, help='ID of nodegroup')
    data_export_parser.add_argument('--data-graph', type=str, required=True, action='append', help='Data graph URL')
    data_export_parser.add_argument('--format', type=ExportFormat, help='Export format', choices=list(ExportFormat), default=ExportFormat.TEXT)
    data_export_parser.add_argument('--no-headers', action='store_true', help='Omit header row')
    data_export_parser.add_argument('--file', type=Path, help='Output to file')
    data_export_parser.add_argument('--constraint', type=str, action='append', help='Runtime constraint: key=value')
    data_export_parser.set_defaults(func=dispatch_data_export)

    data_count_parser.add_argument('nodegroup', type=str, help='ID of nodegroup')
    data_count_parser.add_argument('--data-graph', type=str, required=True, action='append', help='Data graph URL')
    data_count_parser.add_argument('--constraint', type=str, action='append', help='Runtime constraint: key=value')
    data_count_parser.set_defaults(func=dispatch_data_count)

    data_clear_parser.add_argument('--data-graph', type=str, action='append', help='Data graph URL')
    data_clear_parser.set_defaults(func=dispatch_data_clear)

    model_import_parser.add_argument('config', type=str, help='Configuration YAML file')
    model_import_parser.set_defaults(func=dispatch_model_import)
    model_import_parser.add_argument('--clear', action='store_true', help='Clear model graph before import')

    model_clear_parser.set_defaults(func=dispatch_model_clear)

    nodegroups_import_parser.add_argument('directory', type=str, help='Nodegroup directory')
    nodegroups_import_parser.set_defaults(func=dispatch_nodegroups_import)

    nodegroups_store_parser.add_argument('--comment', type=str, help="Nodegroup description")
    nodegroups_store_parser.add_argument('name', type=str, help="Nodegroup identifier")
    nodegroups_store_parser.add_argument('creator', type=str, help="Nodegroup author name")
    nodegroups_store_parser.add_argument('filename', type=str, help='Nodegroup JSON filename')
    nodegroups_store_parser.add_argument('--kind', type=str, default='nodegroup', choices=['nodegroup', 'report'], help='Specify kind of object to store')
    nodegroups_store_parser.set_defaults(func=dispatch_nodegroups_store)

    nodegroups_sparql_parser.add_argument('filename', type=str, help='Nodegroup (JSON) filename')
    nodegroups_sparql_parser.set_defaults(func=dispatch_nodegroups_sparql)

    nodegroups_export_parser.add_argument('regexp', type=str, help='Nodegroup selection regular expression')
    nodegroups_export_parser.add_argument('directory', type=str, help='Nodegroup directory')
    nodegroups_export_parser.add_argument('--kind', type=str, default='all', choices=['all', 'nodegroup', 'report'], help='Specify kind of object to export')
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
