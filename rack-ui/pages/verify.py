""" Content for the "verify data" page """

import time
import platform
from dash import html, dcc, callback, Input, Output, State
import dash_bootstrap_components as dbc
import semtk3
import json
from .helper import *

# name of default graph
DEFAULT_GRAPH_NAME = "uri://DefaultGraph"

# dialog confirming ASSIST verification done
verify_assist_done_dialog = dbc.Modal(
    [
        dbc.ModalBody("MESSAGE PLACEHOLDER", id="verify-assist-done-dialog-body"),      # message
        dbc.ModalFooter([
            html.Button("Download results", id="verify-assist-download-button"),        # download results button
            html.Button("Close", id="verify-assist-done-button", n_clicks=0)            # close button
        ]),
        dcc.Download(id="download"),
    ],
    id="verify-assist-done-dialog",
    is_open=False,
    backdrop=False,
)

# div showing graphs list
select_graphs_div = dbc.Spinner(html.Div(
    [
        dcc.Markdown("Select graphs to include:"),
        dcc.Checklist([], [], id="verify-graph-checklist", labelStyle={'display': 'block'}),   # choose which graphs to verify
        dbc.Row([
            dbc.Col(html.Button("Continue", id="select-graphs-continue-button", n_clicks=0), width="auto"),  # button to open SPARQLgraph
            dbc.Col(html.Button("Cancel", id="select-graphs-cancel-button", n_clicks=0), width="auto")       # button to cancel
        ])
    ],
    id="select-graphs-div",
    hidden=True,
    style={"margin-top": "50px"},
))

# dialog indicating an error generating the SPARQLgraph link (e.g. no graphs selected)
sg_link_error_dialog = dbc.Modal(
    [
        dbc.ModalBody("MESSAGE PLACEHOLDER", id="sg-link-error-dialog-body"),     # message
        dbc.ModalFooter([
            html.Button("Close", id="sg-link-error-button", n_clicks=0)           # close button
        ]),
    ],
    id="sg-link-error-dialog",
    is_open=False,
    backdrop=False,
)

# page elements
layout = html.Div([
    html.H2('Verify Data'),
    dcc.Markdown("_Run verification routines on the data loaded in RACK_"),
    dbc.Row([
        dbc.Col(html.Button("Verify using ASSIST", id="verify-assist-button", n_clicks=0), width="auto"),  # button to verify using ASSIST
        dbc.Col(html.Button("Verify using report", id="verify-report-button"), width="auto"),              # button to verify using SPARQLgraph report
        dbc.Col(html.Button("Check cardinality", id="cardinality-button"), width="auto")                   # button to check cardinality via SPARQLgraph
    ]),
    dbc.Tooltip("Run the ASSIST tool and download an error report", target="verify-assist-button"),
    dbc.Tooltip("Open SPARQLgraph and run data verification report on selected graphs", target="verify-report-button"),
    dbc.Tooltip("Open SPARQLgraph and check cardinality on selected graphs", target="cardinality-button"),
    select_graphs_div,
    html.Div(id="assist-status-div", className="scrollarea"),       # displays status
    verify_assist_done_dialog,
    sg_link_error_dialog,
    dcc.Store("assist-status-filepath"),        # stores the filename of the temp file containing status
    dcc.Store("report-vs-cardinality"),         # stores user choice of SPARQLgraph report or cardinality
    dcc.Store("sparqlgraph-url"),               # stores the SPARQLgraph URL 
    dcc.Store(id="clientside-dummy-store"),     # dummy store because callback needs an Output
    dcc.Interval(id='assist-status-interval', interval=0.5*1000, n_intervals=0, disabled=True), # triggers updating the status display
])

####### callbacks for ASSIST verification ######################################


@dash.callback(
    output=Output("assist-status-filepath", "data"),                # store a status file path
    inputs=Input("verify-assist-button", "n_clicks"),               # triggered by clicking ASSIST button
    background=True,                                                # background callback
    running=[
        (Output("verify-report-button", "disabled"), True, False),  # disable the button while running
        (Output("verify-assist-button", "disabled"), True, False),  # disable the button while running
        (Output("cardinality-button", "disabled"), True, False),    # disable the button while running
    ],
    prevent_initial_call=True
)
def create_assist_status_filepath(n_clicks):
    """
    Generate a file in which to capture the ASSIST status
    """
    status_filepath = get_temp_dir_unique("output")
    return status_filepath


@dash.callback(
    output=[Output("verify-assist-done-dialog-body", "children"),
            Output("verify-assist-download-button", "hidden")],
    inputs=Input("assist-status-filepath", "data"),                 # triggered by creating ASSIST status filepath
    background=True,                                                # background callback
    running=[
        (Output("verify-report-button", "disabled"), True, False),  # disable the button while running
        (Output("verify-assist-button", "disabled"), True, False),  # disable the button while running
        (Output("cardinality-button", "disabled"), True, False),    # disable the button while running
        (Output("assist-status-interval", "disabled"), False, True) # enable the interval component while running
    ],
    prevent_initial_call=True
)
def run_assist(status_filepath):
    """
    Run the ASSIST tool
    """
    try:
        if platform.system() == "Windows":
            raise Exception("Not yet supported on Windows.  (PROLOG checking is available through LINUX/Docker.)")
        else:
            if(rack.DEFAULT_TRIPLE_STORE_TYPE != "fuseki"):
                raise Exception("RACK UI currently only supports ASSIST tool using Fuseki triplestore")
            TRIPLE_STORE_BASE_URL = rack.DEFAULT_TRIPLE_STORE.rsplit("/", 1)[0] # e.g. get http://localhost:3030
            command = f"../assist/bin/check -v -m {TRIPLE_STORE_BASE_URL}/"     # ASSIST tool.  Runs on all graphs, minus exclusion list of internal SemTK graphs
            run_subprocess(command, status_filepath)                            # TODO returns error code 1 even though seems successful
        time.sleep(1)

        return [dcc.Markdown("Completed ASSIST verification.")], False
    except Exception as e:
        return get_error_trace(e), True  # show done dialog with error, hide the download button


@callback(Output("assist-status-div", "children"),
              Input("assist-status-interval", "n_intervals"),  # triggered at regular interval
              Input("assist-status-filepath", "data"),         # or triggered by resetting the file path (to clear out the status when selecting a new file)
              prevent_initial_call=True)
def update_assist_status(n, status_filepath):
    """
    Update the displayed status
    """
    status = ""
    try:
        with open(status_filepath, "r") as file:
            lines = file.readlines()
            status = "...\n\n" + "".join(lines[-1 * min(20, len(lines)):])   # get the last 20 lines (or fewer if there are not 20 in the list)
        return clean_for_display(status)
    except Exception as e:
        return ""


@callback(
    Output("download", "data"),
    Input("verify-assist-download-button", "n_clicks"),     # triggered by user clicking download button
    State("assist-status-filepath", "data"),                # the name of the file to download
    prevent_initial_call=True,
)
def download_assist_results(n_clicks, status_filepath):
    """
    Download file when user clicks button
    """
    # read contents of the result file
    with open(status_filepath, 'r') as file:
        file_content = file.read()
    return dict(content=file_content, filename="rack_verification_results.txt")


####### callbacks for SPARQLgraph report / cardinality ######################################


@dash.callback(
    output=[
        Output("report-vs-cardinality", "data"),                # remember whether user wanted SPARQLgraph report or cardinality
        Output("verify-graph-checklist", "options"),            # list of graphs populated in the triple store
        Output("verify-graph-checklist", "value")],             # list of graphs to pre-select (graphs recently loaded)
    inputs=[
        Input("verify-report-button", "n_clicks"),              # triggered by user clicking buttons
        Input("cardinality-button", "n_clicks")],
    state=State("last-loaded-graphs", "data"),                  # last loaded graphs
    background=True,                                            # background callback
    running=[
        (Output("verify-report-button", "disabled"), True, False),     # disable the button while running
        (Output("verify-assist-button", "disabled"), True, False),     # disable the button while running
        (Output("cardinality-button", "disabled"), True, False),       # disable the button while running
    ],
    prevent_initial_call=True
)
def show_graphs_checklist(report_button_clicks, cardinality_button_clicks, last_loaded_graphs):
    """
    Show list of graphs for generating SPARQLgraph link, with the last loaded graphs pre-selected
    """
    
    # remember which button triggered this: report or cardinality
    if (get_trigger() == "verify-report-button.n_clicks"):
        report_vs_cardinality = True    # user wants report
    else:
        report_vs_cardinality = False   # user wants cardinality
    
    # get list of graphs populated in the triple store
    graphs_list_values = get_graph_info().get_column(0)    # list of graphs, including uri://DefaultGraph
    graphs_list_labels = list(map(lambda x: x.replace(DEFAULT_GRAPH_NAME, 'Optimized graph'), graphs_list_values.copy()))  # display default graph as "Optimized graph"
    graphs_list = [{'label': label, 'value': val} for label, val in zip(graphs_list_labels, graphs_list_values)]

    # these are the graphs last loaded - check the checkboxes for these
    if last_loaded_graphs is None:
        last_loaded_graphs = []

    return report_vs_cardinality, graphs_list, last_loaded_graphs


@dash.callback(
    output=[Output("sparqlgraph-url", "data"),                      # output SPARQLgraph URL
            Output("sg-link-error-dialog-body", "children")],       # output error message
    inputs=Input("select-graphs-continue-button", "n_clicks"),      # triggered by clicking continue button
    state=[
        State("report-vs-cardinality", "data"),                     # true for report, false for cardinality
        State("verify-graph-checklist", "value")],                  # the currently selected graphs
    background=True,                                                # background callback
    running=[
        (Output("verify-report-button", "disabled"), True, False),  # disable the button while running
        (Output("verify-assist-button", "disabled"), True, False),  # disable the button while running
        (Output("cardinality-button", "disabled"), True, False),    # disable the button while running
    ],
    prevent_initial_call=True
)
def generate_sg_link(sg_button_clicks, report_vs_cardinality, graphs_selected):
    """
    Generate the SPARQLgraph link
    """
    # error if no graphs were selected
    if len(graphs_selected) == 0:
        return None, "Please select at least one graph"             # return error message and no URL

    # build a connection using selected graphs (no need to differentiate model vs data)
    graphs = []
    for graph in graphs_selected:
        graphs.append(graph)
    conn = semtk3.build_connection_str("Graphs To Verify", rack.DEFAULT_TRIPLE_STORE_TYPE, rack.DEFAULT_TRIPLE_STORE, graphs, graphs[0], graphs[1:])  # use all graphs for both model and data, to avoid either being empty

    # construct SG URL
    if(report_vs_cardinality):
        sparqlgraph_verify_url_str = semtk3.get_sparqlgraph_url(SPARQLGRAPH_BASE_URL, conn_json_str=str(conn), report_id="report data verification") # URL to generate report
    else:
        sparqlgraph_verify_url_str = semtk3.get_sparqlgraph_url(SPARQLGRAPH_BASE_URL, conn_json_str=str(conn), explore_restrictions=True)  # URL to explore cardinality

    # return SPARQLgraph URL
    return sparqlgraph_verify_url_str, None


# Open a browser tab with SPARQLgraph link  (this is a clientside callback written in JavaScript: https://dash.plotly.com/clientside-callbacks)
dash.clientside_callback(
    """
    function(url) {
        if(url != null){
            window.open(url);
        }
        return "dummy value"
    }
    """,
    Output("clientside-dummy-store", "data"),  # serves no purpose, but an output is required
    Input("sparqlgraph-url", "data"),
    prevent_initial_call=True
)


####### simple callbacks to show/hide components #######


@callback(Output("assist-status-div", "hidden"),
              Input("verify-assist-button", "n_clicks"),
              Input("verify-report-button", "n_clicks"),
              Input("cardinality-button", "n_clicks"),
              prevent_initial_call=True
              )
def manage_assist_status_div(assist_clicks, report_clicks):
    """ Show or hide the ASSIST status div """
    if (get_trigger() in ["verify-assist-button.n_clicks"]):
        return False        # user clicked ASSIST, show the div
    else:
        return True         # user clicked other button, hide the div


@callback(Output("select-graphs-div", "hidden"),
              Input("verify-graph-checklist", "options"),
              Input("verify-assist-button", "n_clicks"),
              Input("select-graphs-cancel-button", "n_clicks"),
              prevent_initial_call=True
              )
def manage_select_graphs_div(checklist_options, continue_clicks, cancel_clicks):
    """ Show or hide the graph checklist div """
    if (get_trigger() in ["verify-assist-button.n_clicks", "select-graphs-cancel-button.n_clicks"]):
        return True         # continue or cancel button pressed, hide div
    elif checklist_options == []:
        return True         # no checklist options provided, don't show div
    else:
        return False        # checklist options provided, show div


@callback(Output("verify-assist-done-dialog", "is_open"),
          Input("verify-assist-done-dialog-body", "children"),
          Input("verify-assist-done-button", "n_clicks"),
          prevent_initial_call=True
          )
def manage_verify_assist_done_dialog(children, n_clicks):
    """ Show or hide the done dialog after running ASSIST """
    if (get_trigger() == "verify-assist-done-button.n_clicks"):
        return False    # button pressed, hide the dialog
    else:
        return True     # child added, show the dialog


@callback(Output("sg-link-error-dialog", "is_open"),
          Input("sg-link-error-dialog-body", "children"),
          Input("sg-link-error-button", "n_clicks"),
          prevent_initial_call=True
          )
def manage_sg_link_error_dialog(children, n_clicks):
    """ Show or hide the SPARQLgraph link error dialog (e.g. if no graphs selected) """
    if (get_trigger() == "sg-link-error-button.n_clicks"):
        return False        # button pressed, hide the dialog
    else:
        if children is None:
            return False    # child added but it's None, hide the dialog
        else:
            return True     # child added, show it
