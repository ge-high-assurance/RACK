""" Content for the "load data" page """

import yaml
import time
import io
import base64
import glob
from contextlib import redirect_stdout, redirect_stderr
from urllib.parse import urlparse
from pathlib import Path
from zipfile import *
from dash import html, dcc, callback, Input, Output, State
import dash_bootstrap_components as dbc
import rack
from rack import Manifest
import semtk3
from .helper import *

# name of default manifest file within ingestion package
MANIFEST_FILE_NAME = "manifest.yaml"

# get text for warning banner
def get_warning_banner_str() -> str:
    config_obj = yaml.safe_load(open("config/config.yml"))
    return config_obj.get('load-warning')

# display strings
CLEAR_BEFORE_LOADING_STR = "Clear before loading"

# div showing load details and buttons to load data or open SPARQLgraph
load_div = dbc.Spinner(html.Div(
    [
        # package metadata (from manifest)
        dcc.Markdown("", id="load-div-message"),

        # load options
        dbc.Accordion([
            dbc.AccordionItem(
                dcc.Checklist([CLEAR_BEFORE_LOADING_STR], [CLEAR_BEFORE_LOADING_STR], id="load-options-checklist"),
                title="Options")],
            start_collapsed=True, flush=True, style={"width": "250px"}),

        # load/view buttons
        dbc.Row([
            dbc.Col([html.Button("Load data", id="load-button", n_clicks=0)], width="auto"),      # load button
            dbc.Tooltip("Load the above data into RACK", target="load-button"),
            dbc.Col(dbc.DropdownMenu(children=[], id="view-dropdown", label="View data", toggle_class_name="ddm"), width="auto"),
            dbc.Tooltip("After loading, view data in SPARQLgraph", target="view-dropdown")
        ])

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
        html.Div(dcc.Markdown(get_warning_banner_str()), className="warningbanner"),
        dbc.Row([
            dbc.Col(dcc.Upload(html.Button(id="select-button", children="Select ingestion package"), id='select-button-upload', accept=".zip", multiple=False), width="auto")  # button to show upload dialog to pick ingestion package
        ]),
        dbc.Tooltip("Select an ingestion package (in .zip format) from your local machine", target="select-button"),
        load_div,
        html.Div(id="status-div", className="scrollarea"),      # displays ingestion status
        unzip_error_dialog,
        done_dialog,
        dcc.Store("status-filepath"),           # stores the filename of the temp file containing status
        dcc.Store("manifest-filepath"),         # stores the path to the manifest file
        dcc.Store("zip-filepath"),              # stores the path to the zip file
        dcc.Interval(id='status-interval', interval=0.5*1000, n_intervals=0, disabled=True), # triggers updating the status display
    ])

####### callbacks #######

@dash.callback(
    output=[
            Output("load-div-message", "children"),                 # package information to display to the user before confirming load
            Output("view-dropdown", "children"),                    # options to display for the view data dropdown
            Output("zip-filepath", "data"),                         # path to the zip file
            Output("unzip-error-dialog-body", "children"),
            Output("status-filepath", "data"),                      # store a status file path
            Output("select-button-upload", "contents")],            # set to None after extracting, else callback ignores re-uploaded file
    inputs=[
            Input("select-button-upload", "contents")],             # triggered by user selecting an upload file
    background=True,                                                # background callback
    running=[
        (Output("select-button", "disabled"), True, False),         # disable the button while running
        (Output("load-button", "disabled"), True, False),           # disable the button while running
    ],
    prevent_initial_call=True
)
def run_unzip(zip_file_contents):
    """
    Extract the selected zip file
    """
    try:
        # write zip file to disk
        tmp_dir = get_temp_dir_unique("ingest")
        os.mkdir(tmp_dir)
        zip_path = os.path.join(tmp_dir, "ingestion-package.zip")
        zip_str = io.BytesIO(base64.b64decode(zip_file_contents.split(',')[1]))
        with open(zip_path, 'wb') as f:
            f.write(zip_str.getbuffer())
        zip_str.close()

        # extract a manifest
        manifest = Manifest.getToplevelManifest(zip_path)

        # gather displayable information about the package
        package_description = ""
        if manifest.getDescription() is not None and manifest.getDescription().strip() != '':
            package_description = f"({manifest.getDescription()})"
        additional_actions = []
        if manifest.getCopyToGraph() is not None: additional_actions.append("copy to " + manifest.getCopyToGraph())
        if manifest.getPerformEntityResolution() is not None: additional_actions.append("resolve entities in " + manifest.getPerformEntityResolution())
        package_info = f"Data: `{manifest.getName()} {package_description}`  \n" + \
                       f"Target model graphs: `{', '.join(manifest.getModelgraphsFootprint())}`  \n" + \
                       f"Target data graphs: `{', '.join(manifest.getDatagraphsFootprint())}`  \n" + \
                       f"Additional actions: `{', '.join(additional_actions) if len(additional_actions) > 0 else 'None'}`"

        # generate a file in which to capture the ingestion status
        status_filepath = get_temp_dir_unique("output")

        # set options for the view data dropdown menu:  1) footprint graphs and sometimes 2) copy-to graph
        sg_link_footprint = semtk3.get_sparqlgraph_url(SPARQLGRAPH_BASE_URL, conn_json_str=manifest.getFootprintConnection())
        view_graph_children=[dbc.DropdownMenuItem("Target graphs", href=sg_link_footprint, target="_blank")]  # option to view footprint graphs
        if manifest.getCopyToGraph() is not None:
            conn_copyto = manifest.getConnection(model_graphs=[manifest.getCopyToGraph()], data_graph=manifest.getCopyToGraph(), extra_data_graphs=[])
            sg_link_copyto = semtk3.get_sparqlgraph_url(SPARQLGRAPH_BASE_URL, conn_json_str=conn_copyto)
            view_graph_children.append(dbc.DropdownMenuItem(manifest.getCopyToGraph(), href=sg_link_copyto, target="_blank"))  # option to view copy-to graph

    except Exception as e:
        return "", None, None, get_error_trace(e), None, None
    return package_info, view_graph_children, zip_path, None, status_filepath, None


@dash.callback(
    output=[Output("done-dialog-body", "children"),
            Output("last-loaded-graphs", "data")],                  # remember graphs loaded (used in the Verify tab)  NOTE this Store is from app.py layout - using it here disables prevent_initial_call=True
    inputs=Input("load-button", "n_clicks"),                        # triggered by user clicking load button
    state=[
        State("status-filepath", "data"),
        State("zip-filepath", "data"),
        State("load-options-checklist", "value")],                  # user-selected load options from the checklist
    background=True,                                                # background callback
    running=[
        (Output("select-button", "disabled"), True, False),         # disable button while running
        (Output("load-button", "disabled"), True, False),           # disable button while running
        (Output("status-interval", "disabled"), False, True)        # enable the interval component while running
    ],
    prevent_initial_call=True                                       # NOTE won't work because last-loaded-graphs is in the layout before load-button (see https://dash.plotly.com/advanced-callbacks#prevent-callback-execution-upon-initial-component-render)
)
def run_ingest(load_button_clicks, status_filepath, zip_filepath, load_options):
    """
    Ingest the selected zip file
    """
    # this callback gets triggered when the pages is loaded - if so don't proceed
    if load_button_clicks == 0:
        raise dash.exceptions.PreventUpdate

    clear = (CLEAR_BEFORE_LOADING_STR in load_options)  # clear the graph before loading (or not), depending on UI checkbox selection

    try:
        # avoid a ConnectionError if SemTK services are not fully up yet
        if semtk3.check_services() is False:
            raise Exception("Cannot reach SemTK Services (wait for startup to complete, or check for failures)")

        # load the ingestion package
        f = open(status_filepath, "a")
        with redirect_stdout(f), redirect_stderr(f):    # send command output to temporary file
            resp = semtk3.load_ingestion_package(rack.DEFAULT_TRIPLE_STORE, rack.DEFAULT_TRIPLE_STORE_TYPE, zip_filepath, clear, rack.MODEL_GRAPH, rack.DEFAULT_DATA_GRAPH)
            for line_bytes in resp.iter_lines():
                level, _, msg = line_bytes.decode().partition(': ')
                if level != "WARNING":
                    print(msg, flush=True)
                if level == "ERROR":
                    raise Exception(msg)

        # extract the manifest for actions below
        manifest = Manifest.getToplevelManifest(zip_filepath)

        # store list of loaded graphs
        last_loaded_graphs = manifest.getModelgraphsFootprint() + manifest.getDatagraphsFootprint()

        # optimize triple store
        if manifest.getNeedsOptimization():
            rack.invoke_optimization(None)

        time.sleep(3)
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
    elif message is None:
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

