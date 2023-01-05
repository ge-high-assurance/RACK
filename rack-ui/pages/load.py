""" Content for the "load data" page """

import time
import io
import base64
import glob
from contextlib import redirect_stdout, redirect_stderr
from urllib.parse import urlparse
from pathlib import Path
from zipfile import ZipFile
from dash import html, dcc, callback, Input, Output, State
import dash_bootstrap_components as dbc
import rack
from rack import Manifest
import semtk3
from .helper import *

# name of default manifest file within ingestion package
MANIFEST_FILE_NAME = "manifest.yaml"

# div showing load details and buttons to load data or open SPARQLgraph
load_div = dbc.Spinner(html.Div(
    [
        html.Table(
            html.Tr([
                html.Td(dcc.Markdown("", id="load-div-message"), style={"padding-right": "20px"}),
                html.Td([
                    html.Button("Load data", id="load-button", n_clicks=0),      # load button
                    dbc.DropdownMenu([
                            dbc.DropdownMenuItem("Target graphs", href="", target="_blank", id="sparqlgraph-button"),
                            dbc.DropdownMenuItem("Optimized graph", href="", target="_blank", id="sparqlgraph-default-button")
                        ], label="View data", toggle_class_name="ddm")
                ])
            ])
        )
    ],
    id="load-div",
    hidden=True,
    style={"margin-top": "50px"},
))

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
        dbc.Row([
            dbc.Col(html.Button(id="turnstile-button", children="Select Turnstile data"), width="auto"),  # button to load turnstile
            dbc.Col(dcc.Upload(html.Button(id="select-button", children="Select ingestion package"), id='select-button-upload', accept=".zip", multiple=False), width="auto")  # button to show upload dialog to pick ingestion package
        ]),
        dbc.Tooltip("Select the Turnstile sample data provided with RACK", target="turnstile-button"),
        dbc.Tooltip("Select an ingestion package (in .zip format) from your local machine", target="select-button"),
        dbc.Tooltip("Load the data into RACK", target="load-button"),
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
            Output("load-div-message", "children"),                 # package information to display to the user before confirming load
            Output("sparqlgraph-button", "href"),                   # set the SG button link
            Output("sparqlgraph-default-button", "href"),           # set the SG button link (default graph)
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
        (Output("load-button", "disabled"), True, False),           # disable the button while running
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
            manifest_paths = glob.glob(f"{tmp_dir}/**/{MANIFEST_FILE_NAME}", recursive=True)
            if len(manifest_paths) == 0:
                raise Exception(f"Cannot load ingestion package: does not contain manifest file {MANIFEST_FILE_NAME}")
            if len(manifest_paths) > 1:
                raise Exception(f"Cannot load ingestion package: contains multiple default manifest files: {manifests}")
            manifest_path = manifest_paths[0]
        else:
            manifest_path = "../Turnstile-Example/Turnstile-IngestionPackage/manifest.yaml"
        manifest = get_manifest(manifest_path)

        # generate SPARQLgraph link
        sg_link = semtk3.get_sparqlgraph_url(SPARQLGRAPH_BASE_URL, conn_json_str=manifest.getConnection())
        sg_link_default = semtk3.get_sparqlgraph_url(SPARQLGRAPH_BASE_URL, conn_json_str=manifest.getDefaultGraphConnection())

        # gather displayable information about the package
        package_description = ""
        if manifest.getDescription() != None and manifest.getDescription().strip() != '':
            package_description = f"({manifest.getDescription()})"
        package_info = f"Data: `{manifest.getName()} {package_description}`  \n" + \
                       f"Target model graphs: `{', '.join(manifest.getModelgraphsFootprint())}`  \n" + \
                       f"Target data graphs: `{', '.join(manifest.getDatagraphsFootprint())}`  \n" + \
                       f"Copy to optimized graph? `{'Yes' if manifest.getCopyToDefaultGraph() else 'No'}`"

        # generate a file in which to capture the ingestion status
        status_filepath = get_temp_dir_unique("output")

    except Exception as e:
        return "", None, None, None, get_error_trace(e), None, None
    return package_info, sg_link, sg_link_default, manifest_path, None, status_filepath, None


@dash.callback(
    output=[Output("done-dialog-body", "children"),
            Output("last-loaded-graphs", "data")],                  # remember graphs loaded (used in the Verify tab)  NOTE this Store is from app.py layout - using it here disables prevent_initial_call=True
    inputs=Input("load-button", "n_clicks"),                        # triggered by user clicking load button
    state=[
        State("status-filepath", "data"),
        State("manifest-filepath", "data")],
    background=True,                                                # background callback
    running=[
        (Output("select-button", "disabled"), True, False),         # disable button while running
        (Output("turnstile-button", "disabled"), True, False),      # disable button while running
        (Output("load-button", "disabled"), True, False),           # disable button while running
        (Output("status-interval", "disabled"), False, True)        # enable the interval component while running
    ],
    prevent_initial_call=True                                       # NOTE won't work because last-loaded-graphs is in the layout before load-button (see https://dash.plotly.com/advanced-callbacks#prevent-callback-execution-upon-initial-component-render)
)
def run_ingest(load_button_clicks, status_filepath, manifest_filepath):
    """
    Ingest the selected zip file
    """
    # this callback gets triggered when the pages is loaded - if so don't proceed
    if load_button_clicks == 0:
        raise dash.exceptions.PreventUpdate

    try:
        # avoid a ConnectionError if SemTK services are not fully up yet
        if semtk3.check_services() == False:
            raise Exception("Cannot reach SemTK Services (wait for startup to complete, or check for failures)")

        f = open(status_filepath, "a")
        with redirect_stdout(f), redirect_stderr(f):    # send command output to temporary file
            rack.logger.setLevel("ERROR")
            rack.ingest_manifest_driver(Path(manifest_filepath), BASE_URL, TRIPLE_STORE, TRIPLE_STORE_TYPE, True, False)  # process the manifest

        # store list of loaded graphs
        manifest = get_manifest(manifest_filepath)
        last_loaded_graphs = manifest.getModelgraphsFootprint() + manifest.getDatagraphsFootprint()

        time.sleep(1)
    except Exception as e:
        return get_error_trace(e), []  # show done dialog with error
    return [dcc.Markdown("Data was loaded successfully.")], last_loaded_graphs


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
              Input("load-div-message", "children"),
              Input("load-button", "n_clicks"),
              prevent_initial_call=True
              )
def manage_load_div(load_message, load_clicks):
    """ Show or hide the load div """
    if len(load_message) > 0:
        return False    # show the div
    else:
        return True     # hide the div

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