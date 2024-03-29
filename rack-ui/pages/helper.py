
""" Helper functions """

import yaml
import tempfile
import traceback
import dash
import re
import os
import uuid
import semtk3
import rack
import subprocess

# configuration
SPARQLGRAPH_BASE_URL = "http://localhost:8080"
CONFIG_FILE_DEFAULT = "config/config-default.yml"
CONFIG_FILE_OVERRIDE = "config/config-override.yml"

def get_config(key) -> str:
    """ Load default config, override it with custom config if present """
    # TODO read config once, store for subsequent use
    with open(CONFIG_FILE_DEFAULT) as f:
        config_obj = yaml.safe_load(f)
    try:
        with open(CONFIG_FILE_OVERRIDE) as f:
            config_custom = yaml.safe_load(f)
        config_obj.update(config_custom)  # override
    except FileNotFoundError:
        pass
    return config_obj.get(key)

def get_temp_dir() -> str:
    """ Get a temp dir """
    return tempfile.gettempdir()

def get_temp_dir_unique(prefix) -> str:
    """ Get a unique subdirectory within the temp dir, e.g. /tmp/ingest_9d40551e-f31f-4530-8c90-ca3e0acc4257"""
    return os.path.join(get_temp_dir(), f"{prefix}_{uuid.uuid4()}")

def get_error_trace(e) -> str:
    """ Get error trace string """
    trace = traceback.format_exception(None, e, e.__traceback__)
    return trace[-1]

def get_error_message(e) -> str:
    """ Get error message """
    return str(e)

def get_trigger():
    """
    Get the input that triggered a callback
    Not for use with @dash.callback (in local Windows environment, gives dash.exceptions.MissingCallbackContextException)
    """
    return dash.callback_context.triggered[0]['prop_id']

def clean_for_display(s):
    """ Cleans process output to be displayed to user """
    ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')  # remove ANSI escape sequences (e.g. ESC[32m, ESC[0m) from command output
    return ansi_escape.sub('', s)

def get_graph_info():
    """ Gets list of graphs in the triple store, with their triple counts """
    conn_str = rack.sparql_connection(rack.DEFAULT_BASE_URL, None, None, [], rack.DEFAULT_TRIPLE_STORE, rack.DEFAULT_TRIPLE_STORE_TYPE)
    graph_info_table = semtk3.get_graph_info(conn_str, True, False)  # True to exclude internal SemTK graphs, False to get counts too
    return graph_info_table

def run_subprocess(command, status_filepath=None):
    """ Launch a process using a given command.  Pipe output to file if provided """
    if status_filepath is not None:
        command = f"{command} > {status_filepath} 2>&1"
    completed_process = subprocess.run(command, shell=True, capture_output=True)
    print(completed_process)    # useful to see exit code
    return completed_process