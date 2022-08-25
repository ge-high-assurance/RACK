import io
import base64
import glob
import time
import os
import re
import uuid
import diskcache
import traceback
from pathlib import Path
from zipfile import ZipFile
from contextlib import redirect_stdout
import urllib
from urllib.parse import urlparse
import tempfile

import dash
from dash import DiskcacheManager, Input, Output, html, dcc, State
import dash_bootstrap_components as dbc

import rack
from rack import Graph, Manifest

TEMP_DIR = tempfile.gettempdir()

BASE_URL = "http://localhost"
TRIPLE_STORE = "http://localhost:3030/RACK"
TRIPLE_STORE_TYPE = "fuseki"

# name of default manifest file within ingestion package
MANIFEST_FILE_NAME = "manifest.yaml"

# style for sidebar
SIDEBAR_STYLE = {
    "position": "fixed",
    "top": 0,
    "left": 0,
    "bottom": 0,
    "width": "16rem",
    "padding": "2rem 1rem",
    "background-color": "#f8f9fa",
}

# style for main content
CONTENT_STYLE = {
    "margin-left": "18rem",
    "margin-right": "2rem",
    "padding": "2rem 1rem",
}

# style for buttons
BUTTON_STYLE = {
    "font-size": "16px",
    "width": "200px",
    "display": "inline-block",
    "margin-bottom": "10px",
    "margin-right": "5px",
    "height":"25px",
    "border-radius": "5px",
    "padding": "0px",
    "background-color": "DeepSkyBlue",
    "border": "none",
}

# style for scrolling status
SCROLLING_STATUS_STYLE = {
    "margin-top": "100px",
    "white-space": "pre-wrap",
    "border-style": "none",
    "height": 500,
    "width": 1200, "overflow-y": "auto",
    "display": "flex",
    "flex-direction": "column-reverse"
}

# diskcache for non-production apps when developing locally (fine for our Docker application).  Needed for @dash.callback with background=True
cache = diskcache.Cache(TEMP_DIR + "/cache")
background_callback_manager = DiskcacheManager(cache)

app = dash.Dash(external_stylesheets=[dbc.themes.BOOTSTRAP], background_callback_manager=background_callback_manager)
app.title = 'RACK UI'

# menu
sidebar = html.Div(
    [
        html.Table([
            html.Tr([
                html.Td( html.Img(src=app.get_asset_url('RACK_cartoon.jpg'), height="90px")),
                html.Td([
                    dcc.Markdown("## RACK\n\nin-a-box\n\n_System manager_")
                ])
            ])
        ]),
    ],
    style=SIDEBAR_STYLE,
)

# dialog confirming action done (or showing error)
done_dialog = dbc.Modal(
    [
        dbc.ModalBody("MESSAGE", id="done-dialog-body"),
        dbc.ModalFooter(dbc.Button("Close", id="done-dialog-button", className="ms-auto", n_clicks=0)),
    ],
    id="done-dialog",
    is_open=False,
    backdrop=False,
)

content = html.Div(
    [   
        dcc.Markdown("Welcome to RACK."),
        html.Div([dcc.Upload( html.Button(id="run-button", children="Load ingestion package", style=BUTTON_STYLE), id='run-button-upload', accept=".zip", multiple=False)]),  # upload button
        html.Div(id="div-status", style=SCROLLING_STATUS_STYLE),                                # displays ingestion status
        done_dialog,
        dcc.Store("status-store"),                                                              # stores the ingestion status for display
        dcc.Interval(id='interval-component', interval=0.5*1000, n_intervals=0, disabled=True), # triggers updating the status display
    ],
    style=CONTENT_STYLE,
)

app.layout = html.Div([dcc.Location(id="url"), sidebar, content])


@app.callback(Output("status-store", "data"),                       # create store, which will trigger the ingest callback
              Input("run-button-upload", "contents"),               # triggered by user selecting an upload file
              prevent_initial_call=True
              )
def run_button(file_contents):
    """
    When an upload file is selected, generate a status store filename which will trigger run_ingest
    """
    status_store_filename = os.path.join(TEMP_DIR, "output_" + str(uuid.uuid4()))
    return status_store_filename

# note:  @dash.callback, not @app.callback.   Requires dash 2.6.1
@dash.callback(
    output=[Output("done-dialog-body", "children"), Output("run-button-upload", "contents")], # 2nd output is to reset uploaded file, otherwise system ignores re-upload of same file
    inputs=Input("status-store", "data"),                           # triggered by creating the store
    state=[State("status-store", "data"), State('run-button-upload', 'contents')],
    background=True,                                                # background callback
    running=[
        (Output("run-button", "disabled"), True, False),            # disable the run button while running
        (Output("interval-component", "disabled"), False, True)     # enable the interval component while running
    ],
    prevent_initial_call=True
)
def run_ingest(status_store, status_store2, zip_file_contents):
    """
    Extract the selected zip file and ingest it
    """
    try:
        f = open(status_store, "a")
        with redirect_stdout(f):    # send command output to temporary file
            tmp_dir = TEMP_DIR + "/ingest_" + str(uuid.uuid4())  # temp directory to store the unzipped package
            zip_str = io.BytesIO(base64.b64decode(zip_file_contents.split(',')[1]))
            zip_obj = ZipFile(zip_str, 'r')
            zip_obj.extractall(path=tmp_dir)  # unzip the package
            manifest_paths = glob.glob(tmp_dir + '/**/' + MANIFEST_FILE_NAME, recursive=True)
            if len(manifest_paths) == 0:
                raise Exception("Cannot load ingestion package: does not contain manifest file " + MANIFEST_FILE_NAME)
            if len(manifest_paths) > 1:
                raise Exception("Cannot load ingestion package: contains multiple default manifest files: " + str(manifests))
            manifest_path = manifest_paths[0]

            rack.ingest_manifest_driver(Path(manifest_path), BASE_URL, TRIPLE_STORE, TRIPLE_STORE_TYPE, True, False)  # process the manifest

            # get connection from manifest, construct SPARQLGraph URL
            with open(manifest_path, mode='r', encoding='utf-8-sig') as manifest_file:
                manifest = Manifest.fromYAML(manifest_file)
            conn = manifest.getConnection()
            sparqlgraph_url_str = "http://localhost:8080/sparqlGraph/main-oss/sparqlGraph.html?conn=" + urllib.parse.quote(conn, safe="")

        time.sleep(1)
    except Exception as e:
        return get_error_trace(e), None  # show done dialog with error

    return [dcc.Markdown("Loaded ingestion package."), html.A("Open in SPARQLGraph UI", href=sparqlgraph_url_str, target="_blank", style={"margin-top": "100px"})], None


@app.callback(Output("div-status", "children"),
              Input("interval-component", "n_intervals"),   # triggered at regular interval
              State("status-store", "data"),
              prevent_initial_call=True)
def update_status(n, status_store_filename):
    """
        Update the displayed status
    """
    print("update_status") # show it's running
    status = ""
    try:
        with open(status_store_filename, "r") as file:
            status = file.read()
        ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')  # remove ANSI escape sequences (e.g. ESC[32m, ESC[0m) from command output

        return ansi_escape.sub('', status)
    except:
        return ""


@app.callback(Output("done-dialog", "is_open"),
              Input("done-dialog-body", "children"),
              Input("done-dialog-button", "n_clicks"),
              prevent_initial_call=True
              )
def manage_done_dialog(children, n_clicks):
    """ 
    Show or hide the done dialog
    """
    if (get_trigger() == "done-dialog-button.n_clicks"):
        return False    # button pressed, hide the dialog
    else:
        return True     # child added, show the dialog


def get_trigger():
    """ Get the input that triggered a callback (for @app.callback only, not @dash.callback) """
    return dash.callback_context.triggered[0]['prop_id']

def get_error_trace(e) -> str:
    """ Get error trace string """
    trace = traceback.format_exception(None, e, e.__traceback__)
    return trace[-1]

if __name__ == "__main__":
    app.run_server(host="0.0.0.0", debug=False)
