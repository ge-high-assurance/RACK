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
from rack import Graph, Manifest, sparql_connection
import semtk3

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
                html.Td(html.Img(src=app.get_asset_url('RACK_cartoon.jpg'), height="90px")),
                html.Td([dcc.Markdown("## RACK\n\nin-a-box\n\n_System manager_")])
            ])
        ]),
    ],
    style=SIDEBAR_STYLE,
)

# div showing load details/options and load/cancel buttons
load_div = html.Div(
    [
        dcc.Markdown("", id="load-div-message"),
        dcc.RadioItems([], value="manifest-graphs", id="load-graph-radio", labelStyle={'display': 'block'}, inputStyle={"margin-right": "10px"}),   # choose to load to manifest-specified or default graphs
        dbc.Button("Load", id="load-button", className="ms-auto", n_clicks=0, style=BUTTON_STYLE),                  # load button
        dbc.Button("Cancel", id="cancel-load-button", className="ms-auto", n_clicks=0, style=BUTTON_STYLE)          # cancel button
    ],
    id="load-div",
    hidden=True,
    style={"margin-top": "50px"},
)

# dialog indicating unzip error (e.g. no manifest)
unzip_error_dialog = dbc.Modal(
    [
        dbc.ModalBody("UNZIP ERROR PLACEHOLDER", id="unzip-error-dialog-body"),                                                     # message
        dbc.ModalFooter(dbc.Button("Close", id="unzip-error-dialog-button", className="ms-auto", n_clicks=0, style=BUTTON_STYLE)),  # close button
    ],
    id="unzip-error-dialog",
    is_open=False,
    backdrop=False,
)

# dialog confirming load done
done_dialog = dbc.Modal(
    [
        dbc.ModalBody("MESSAGE PLACEHOLDER", id="done-dialog-body"),                                        # message
        dbc.ModalFooter(dbc.Button("Close", id="done-dialog-button", className="ms-auto", n_clicks=0)),     # close button
    ],
    id="done-dialog",
    is_open=False,
    backdrop=False,
)

content = html.Div(
    [
        sidebar,
        dcc.Markdown("Welcome to RACK."),
        html.Div([dcc.Upload( html.Button(id="run-button", children="Select ingestion package", style=BUTTON_STYLE), id='run-button-upload', accept=".zip", multiple=False)]),  # upload button
        load_div,
        html.Div(id="status-div", style=SCROLLING_STATUS_STYLE),                                # displays ingestion status
        unzip_error_dialog,
        done_dialog,
    ],
    style=CONTENT_STYLE,
)

app.layout = html.Div([
        dcc.Location(id="url"),
        content,
        dcc.Store("status-filepath"),       # stores the filename of the temp file containing status
        dcc.Store("manifest-filepath"),         # stores the path to the manifest file
        dcc.Interval(id='status-interval', interval=0.5*1000, n_intervals=0, disabled=True), # triggers updating the status display    
    ])


@app.callback(Output("status-filepath", "data"),                    # store a status file path
              Input("run-button-upload", "contents"),               # triggered by user selecting an upload file
              prevent_initial_call=True
              )
def process_upload_selection(file_contents):
    """
    When an upload file is selected, generate a status file name (which will trigger run_zip)
    """
    return os.path.join(TEMP_DIR, "output_" + str(uuid.uuid4()))


@dash.callback(
    output=[
            Output("load-div-message", "children"),
            Output("load-graph-radio", "options"),
            Output("manifest-filepath", "data"), 
            Output("unzip-error-dialog-body", "children"),
            Output("run-button-upload", "contents")],               # set to None after extracting, else callback ignores re-uploaded file
    inputs=Input("status-filepath", "data"),                        # triggered by creating the status file path
    state=State('run-button-upload', 'contents'),
    background=True,                                                # background callback
    running=[
        (Output("run-button", "disabled"), True, False),            # disable the run button while running
    ],
    prevent_initial_call=True
)
def run_unzip(status_filepath, zip_file_contents):
    """
    Extract the selected zip file
    """
    try:
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

        manifest = get_manifest(manifest_path)
        manifest_graphs_option = "Load to " + str(manifest.getModelgraphsFootprint()) + " " + str(manifest.getDatagraphsFootprint())
        radio_choices = [{'label': manifest_graphs_option, 'value': 'manifest-graphs'}, {'label': 'Load to default graph (for optimized performance)', 'value': 'default-graph'}]

    except Exception as e:
        return "", [], None, get_error_trace(e), None
    return "You have selected package '" + manifest.getName() + "'", radio_choices, manifest_path, None, None


@dash.callback(
    output=Output("done-dialog-body", "children"),
    inputs=Input("load-button", "n_clicks"),                        # triggered by user clicking load button
    state=[
        State("load-graph-radio", "value"),                         # load to manifest or default graphs
        State("status-filepath", "data"),
        State("manifest-filepath", "data")],
    background=True,                                                # background callback
    running=[
        (Output("run-button", "disabled"), True, False),            # disable the run button while running
        (Output("status-interval", "disabled"), False, True)     # enable the interval component while running
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


@app.callback(Output("status-div", "children"),
              Input("status-interval", "n_intervals"),  # triggered at regular interval
              Input("status-filepath", "data"),         # or triggered by resetting the file path (to clear out the status when selecting a new file)
              prevent_initial_call=True)
def update_status(n, status_filepath):
    """
    Update the displayed status
    """
    print("update_status") # show it's running
    status = ""
    try:
        with open(status_filepath, "r") as file:
            status = file.read()
        ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')  # remove ANSI escape sequences (e.g. ESC[32m, ESC[0m) from command output
        return ansi_escape.sub('', status)
    except:
        return ""


####### simple callbacks to show/hide components #######

@app.callback(Output("load-div", "hidden"),
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

@app.callback(Output("unzip-error-dialog", "is_open"),
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

@app.callback(Output("done-dialog", "is_open"),
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

def get_trigger():
    """ Get the input that triggered a callback (for @app.callback only, not @dash.callback) """
    return dash.callback_context.triggered[0]['prop_id']

def get_error_trace(e) -> str:
    """ Get error trace string """
    trace = traceback.format_exception(None, e, e.__traceback__)
    return trace[-1]

def get_manifest(manifest_filepath) -> Manifest:
    """ Get manifest contents from file """
    with open(manifest_filepath, mode='r', encoding='utf-8-sig') as manifest_file:
        manifest = Manifest.fromYAML(manifest_file)
    return manifest

if __name__ == "__main__":
    app.run_server(host="0.0.0.0", debug=False)
