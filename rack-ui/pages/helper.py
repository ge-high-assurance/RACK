
""" Helper functions """

import tempfile
import traceback
import dash
import re
import os
import uuid

def get_temp_dir() -> str:
    """ Get a temp dir """
    return tempfile.gettempdir()

def get_temp_dir_unique(prefix) -> str:
    """ Get a unique subdirectory within the temp dir, e.g. /tmp/ingest_9d40551e-f31f-4530-8c90-ca3e0acc4257"""
    return os.path.join(get_temp_dir(), prefix + "_" + str(uuid.uuid4()))

def get_error_trace(e) -> str:
    """ Get error trace string """
    trace = traceback.format_exception(None, e, e.__traceback__)
    return trace[-1]

def get_trigger():
    """ Get the input that triggered a callback (for @app.callback only, not @dash.callback) """
    return dash.callback_context.triggered[0]['prop_id']

def clean_for_display(s):
    """ Cleans process output to be displayed to user """
    ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')  # remove ANSI escape sequences (e.g. ESC[32m, ESC[0m) from command output
    return ansi_escape.sub('', s)