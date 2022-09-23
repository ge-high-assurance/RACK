""" Content for the "load data" page """

import time
import io
import base64
import glob
from contextlib import redirect_stdout
from urllib.parse import urlparse
from pathlib import Path
from zipfile import ZipFile
import dash
from dash import html, dcc, callback, Input, Output, State
import dash_bootstrap_components as dbc
import rack
from rack import Manifest
import semtk3
from .helper import *

BASE_URL = "http://localhost"
TRIPLE_STORE = "http://localhost:3030/RACK"
TRIPLE_STORE_TYPE = "fuseki"

# name of default manifest file within ingestion package
MANIFEST_FILE_NAME = "manifest.yaml"

# div showing load details/options and load/cancel buttons
load_div = html.Div(
    [
        dcc.Markdown("", id="load-div-message"),
        dcc.RadioItems([], value="manifest-graphs", id="load-graph-radio", labelStyle={'display': 'block'}, inputStyle={"margin-right": "10px"}),   # choose to load to manifest-specified or default graphs
        html.Button("Load", id="load-button", n_clicks=0),                  # load button
        html.Button("Cancel", id="cancel-load-button", n_clicks=0)          # cancel button
    ],
    id="load-div",
    hidden=True,
    style={"margin-top": "50px"},
)

# dialog indicating unzip error (e.g. no manifest)
unzip_error_dialog = dbc.Modal(
    [
        dbc.ModalBody("UNZIP ERROR PLACEHOLDER", id="unzip-error-dialog-body"),             # message
        dbc.ModalFooter(html.Button("Close", id="unzip-error-dialog-button", n_clicks=0)),  # close button
    ],
    id="unzip-error-dialog",
    is_open=False,
    backdrop=False,
)

# dialog confirming load done
done_dialog = dbc.Modal(
    [
        dbc.ModalBody("MESSAGE PLACEHOLDER", id="done-dialog-body"),                    # message
        dbc.ModalFooter(html.Button("Close", id="done-dialog-button", n_clicks=0)),     # close button
    ],
    id="done-dialog",
    is_open=False,
    backdrop=False,
)

# page elements
layout = html.Div([
        html.H2("Load data"),
        dcc.Markdown("_Load data into RACK_"),
        html.Div([html.Button(id="turnstile-button", children="Load Turnstile data")]),  # button to load turnstile
        dbc.Tooltip("Load the Turnstile sample data provided with RACK", target="turnstile-button"),
        html.Div([dcc.Upload( html.Button(id="select-button", children="Load ingestion package"), id='select-button-upload', accept=".zip", multiple=False)]),  # button to show upload dialog to pick ingestion package
        dbc.Tooltip("Load an ingestion package (in .zip format) from your local machine", target="select-button"),
        load_div,
        html.Div(id="status-div", className="scrollarea"),      # displays ingestion status
        unzip_error_dialog,
        done_dialog,
        dcc.Store("status-filepath"),           # stores the filename of the temp file containing status
        dcc.Store("manifest-filepath"),         # stores the path to the manifest file
        dcc.Interval(id='status-interval', interval=0.5*1000, n_intervals=0, disabled=True), # triggers updating the status display
    ])

####### callbacks #######

@dash.callback(
    output=[
            Output("load-div-message", "children"),
            Output("load-graph-radio", "options"),
            Output("manifest-filepath", "data"), 
            Output("unzip-error-dialog-body", "children"),
            Output("status-filepath", "data"),                      # store a status file path
            Output("select-button-upload", "contents")],            # set to None after extracting, else callback ignores re-uploaded file
    inputs=[
            Input("select-button-upload", "contents"),              # triggered by user selecting an upload file
            Input("turnstile-button", "n_clicks")],                 # triggered by turnstile button
    background=True,                                                # background callback
    running=[
        (Output("select-button", "disabled"), True, False),         # disable the button while running
        (Output("turnstile-button", "disabled"), True, False),      # disable the button while running
    ],
    prevent_initial_call=True
)
def run_unzip(zip_file_contents, turnstile_clicks):
    """
    Extract the selected zip file
    """
    try:
        if zip_file_contents != None:
            tmp_dir = get_temp_dir_unique("ingest")   # temp directory to store the unzipped package
            zip_str = io.BytesIO(base64.b64decode(zip_file_contents.split(',')[1]))
            zip_obj = ZipFile(zip_str, 'r')
            zip_obj.extractall(path=tmp_dir)  # unzip the package
            manifest_paths = glob.glob(tmp_dir + '/**/' + MANIFEST_FILE_NAME, recursive=True)
            if len(manifest_paths) == 0:
                raise Exception("Cannot load ingestion package: does not contain manifest file " + MANIFEST_FILE_NAME)
            if len(manifest_paths) > 1:
                raise Exception("Cannot load ingestion package: contains multiple default manifest files: " + str(manifests))
            manifest_path = manifest_paths[0]
        else:
            manifest_path = "../Turnstile-Example/Turnstile-IngestionPackage/manifest.yaml"

        manifest = get_manifest(manifest_path)
        manifest_graphs_option = "Load to " + str(manifest.getModelgraphsFootprint()) + " " + str(manifest.getDatagraphsFootprint())
        radio_choices = [{'label': manifest_graphs_option, 'value': 'manifest-graphs'}, {'label': 'Load to default graph (for optimized performance)', 'value': 'default-graph'}]

        # generate a file in which to capture the ingestion status
        status_filepath = get_temp_dir_unique("output")

        selected_message = "You have selected package '" + manifest.getName() + "'"
        if manifest.getDescription() != None and manifest.getDescription().strip() != "":
            selected_message = selected_message + " (" + manifest.getDescription() + ")"

    except Exception as e:
        return "", [], None, get_error_trace(e), None, None
    return selected_message, radio_choices, manifest_path, None, status_filepath, None


@dash.callback(
    output=Output("done-dialog-body", "children"),
    inputs=Input("load-button", "n_clicks"),                        # triggered by user clicking load button
    state=[
        State("load-graph-radio", "value"),                         # load to manifest or default graphs
        State("status-filepath", "data"),
        State("manifest-filepath", "data")],
    background=True,                                                # background callback
    running=[
        (Output("select-button", "disabled"), True, False),         # disable button while running
        (Output("turnstile-button", "disabled"), True, False),      # disable button while running
        (Output("status-interval", "disabled"), False, True)        # enable the interval component while running
    ],
    prevent_initial_call=True
)
def run_ingest(load_button_clicks, manifest_or_default_graphs, status_filepath, manifest_filepath):
    """
    Ingest the selected zip file
    """
    try:
        use_default_graph = (manifest_or_default_graphs == "default-graph")

        f = open(status_filepath, "a")
        with redirect_stdout(f):    # send command output to temporary file
            rack.ingest_manifest_driver(Path(manifest_filepath), BASE_URL, TRIPLE_STORE, TRIPLE_STORE_TYPE, True, use_default_graph)  # process the manifest

        # get connection from manifest, construct SPARQLGraph URL
        manifest = get_manifest(manifest_filepath)
        if use_default_graph:
            conn_str = manifest.getDefaultGraphConnection()
        else:
            conn_str = manifest.getConnection()
        sparqlgraph_url_str = semtk3.get_sparqlgraph_url("http://localhost:8080", conn_json_str=conn_str)

        time.sleep(1)
    except Exception as e:
        return get_error_trace(e)  # show done dialog with error
    return [dcc.Markdown("Loaded ingestion package."), html.A("Open in SPARQLGraph UI", href=sparqlgraph_url_str, target="_blank", style={"margin-top": "100px"})]


@callback(Output("status-div", "children"),
              Input("status-interval", "n_intervals"),  # triggered at regular interval
              Input("status-filepath", "data"),         # or triggered by resetting the file path (to clear out the status when selecting a new file)
              prevent_initial_call=True)
def update_status(n, status_filepath):
    """
    Update the displayed status
    """
    status = ""
    try:
        with open(status_filepath, "r") as file:
            status = file.read()
        return clean_for_display(status)
    except:
        return ""


####### simple callbacks to show/hide components #######

@callback(Output("load-div", "hidden"),
              Input("load-graph-radio", "options"),    # triggered by setting load graph radio options
              Input("load-button", "n_clicks"),
              Input("cancel-load-button", "n_clicks"),
              prevent_initial_call=True
              )
def manage_load_div(radio_options, load_clicks, cancel_clicks):
    """ Show or hide the load div """
    if (get_trigger() in ["load-button.n_clicks", "cancel-load-button.n_clicks"]):
        return True         # load or button pressed, hide div
    elif radio_options == []:
        return True         # no radio options provided, don't show div
    else:
        return False        # radio options provided, show div

@callback(Output("unzip-error-dialog", "is_open"),
              Input("unzip-error-dialog-body", "children"),
              Input("unzip-error-dialog-button", "n_clicks"),
              prevent_initial_call=True
              )
def manage_unzip_error_dialog(message, n_clicks):
    """ Show or hide the unzip error  dialog """
    if (get_trigger() == "unzip-error-dialog-button.n_clicks"):
        return False    # button pressed, hide the dialog
    elif message == None:
        return False    # no message, don't show the dialog
    else:
        return True     # child added, show the dialog

@callback(Output("done-dialog", "is_open"),
              Input("done-dialog-body", "children"),
              Input("done-dialog-button", "n_clicks"),
              prevent_initial_call=True
              )
def manage_done_dialog(children, n_clicks):
    """ Show or hide the done dialog """
    if (get_trigger() == "done-dialog-button.n_clicks"):
        return False    # button pressed, hide the dialog
    else:
        return True     # child added, show the dialog


####### convenience functions #######

def get_manifest(manifest_filepath) -> Manifest:
    """ Get manifest contents from file """
    with open(manifest_filepath, mode='r', encoding='utf-8-sig') as manifest_file:
        manifest = Manifest.fromYAML(manifest_file)
    return manifest