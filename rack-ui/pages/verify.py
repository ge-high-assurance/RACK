""" Content for the "verify data" page """

import time
import platform
import subprocess
import dash
from dash import html, dcc, callback, Input, Output, State
import dash_bootstrap_components as dbc
import semtk3
import rack
from .helper import *

# div showing verification details/options and continue/cancel buttons
verify_div = html.Div(
    [
        dcc.Markdown("Select graphs to verify:"),
        dcc.Checklist([], [], id="verify-graph-checklist", labelStyle={'display': 'block'}, inputStyle={"margin-right": "10px"}),   # choose which graphs to verify
        html.Button("Verify using ASSIST", id="verify-assist-button", n_clicks=0),                  # button to verify using ASSIST
        html.Button("Verify using report", id="verify-sparqlgraph-button", n_clicks=0),             # button to verify using SPARQLgraph report
        html.Button("Cancel", id="verify-cancel-button", n_clicks=0)                                # button to cancel
    ],
    id="verify-div",
    hidden=True,
    style={"margin-top": "50px"},
)

# dialog confirming verification done
verify_done_dialog = dbc.Modal(
    [
        dbc.ModalBody("MESSAGE PLACEHOLDER", id="verify-done-dialog-body"),     # message
        dbc.ModalFooter([
            html.Button("Download results", id="verify-assist-download-button"),                 # download results button
            html.Button("Close", id="verify-done-button", n_clicks=0)    # close button
        ]),
        dcc.Download(id="download"),
    ],
    id="verify-done-dialog",
    is_open=False,
    backdrop=False,
)

# page elements
layout = html.Div(children=[
    html.H2('Verify Data'),
    dcc.Markdown("_Run verification routines on the data loaded into in RACK_"),
    html.Button("Verify data", id="verify-button"),                 # run button
    verify_div,
    html.Div(id="assist-status-div", className="scrollarea"),       # displays status
    verify_done_dialog,
    dcc.Store("assist-status-filepath"),       # stores the filename of the temp file containing status
    dcc.Interval(id='assist-status-interval', interval=0.5*1000, n_intervals=0, disabled=True), # triggers updating the status display
])

####### callbacks #######

@dash.callback(
    output=[
        Output("verify-graph-checklist", "options"),            # list of graphs populated in the triple store
        Output("verify-graph-checklist", "value"),              # list of graphs to pre-select (graphs recently loaded)
        Output("assist-status-filepath", "data")],              # store a status file path
    inputs=Input("verify-button", "n_clicks"),                  # triggered by user clicking button
    state=State("last-loaded-graphs", "data"),                  # last loaded graphs
    background=True,                                            # background callback
    running=[
        (Output("verify-button", "disabled"), True, False),     # disable the run button while running
    ],
    prevent_initial_call=True
)
def show_verify_options(button_clicks, last_loaded_graphs):
    """
    Show verify options, prepare for verification
    """
    # get list of graphs populated in the triple store
    conn_str = rack.sparql_connection(BASE_URL, None, None, [], TRIPLE_STORE, TRIPLE_STORE_TYPE)  # TODO do we do this multiple places?  If so make a helper method
    graphs_list = semtk3.get_graph_names(conn_str)

    # these are the graphs for which to pre-select checkboxes
    if last_loaded_graphs == None:
        last_loaded_graphs = []

    # generate a file in which to capture the verification status (only used for ASSIST verification)
    status_filepath = get_temp_dir_unique("output")

    return graphs_list, last_loaded_graphs, status_filepath


@dash.callback(
    output=[Output("verify-done-dialog-body", "children"),
            Output("verify-assist-download-button", "hidden")],
    inputs=[Input("verify-assist-button", "n_clicks"),              # triggered by clicking ASSIST button or SG button
            Input("verify-sparqlgraph-button", "n_clicks")],
    state=[State("verify-graph-checklist", "value"),                # the currently selected graphs
            State("assist-status-filepath", "data")],
    background=True,                                                # background callback
    running=[
        (Output("verify-button", "disabled"), True, False),         # disable the button while running
        (Output("assist-status-interval", "disabled"), False, True) # enable the interval component while running
    ],
    prevent_initial_call=True
)
def run_assist_or_sg(assist_button_clicks, sg_button_clicks, graphs_selected, status_filepath):
    """
    Run the ASSIST tool or SPARQLgraph report
    """
    try:

        # if user selected the ASSIST tool
        if get_trigger() == "verify-assist-button.n_clicks":

            if False: # if platform.system() == "Windows": # TODO change back
                raise Exception("Not yet supported on Windows.  (PROLOG checking is available through LINUX/Docker.)")
            else:
                # TODO can we specify which graphs to use for the ASSIST call?
                subprocess.call("../assist/bin/check -v -m " + TRIPLE_STORE_BASE_URL + "/ > " + status_filepath + " 2>&1", shell=True)
            time.sleep(1)

            return [dcc.Markdown("Completed ASSIST verification.")], False

        # if user selected the SPARQLgraph report
        elif get_trigger() == "verify-sparqlgraph-button.n_clicks":

            # error if no graphs were selected
            if len(graphs_selected) == 0:
                raise Exception ("No graphs selected")

            # build a connection using selected graphs (no need to differentiate model vs data)
            graphs = []
            for graph in graphs_selected:
                graphs.append(graph)
            conn = semtk3.build_connection_str("Graphs To Verify", TRIPLE_STORE_TYPE, TRIPLE_STORE, graphs, graphs[0], graphs[1:])  # use all graphs for both model and data, to avoid either being empty

            # construct SG report URL
            sparqlgraph_verify_url_str = semtk3.get_sparqlgraph_url(SPARQLGRAPH_BASE_URL, conn_json_str=str(conn), report_id="report data verification")

            # provide link to SG
            #return [dcc.Markdown("Click this link to open the SG report:"), html.A("Report", href=sparqlgraph_verify_url_str, target="_blank", style={"margin-top": "100px"})], True
            return [html.A("Open the report in the SPARQLgraph UI", href=sparqlgraph_verify_url_str, target="_blank", style={"margin-top": "100px"})], True

    except Exception as e:
        return get_error_trace(e), True  # show done dialog with error, hide the download button
    return ["Unexpected condition"]


@callback(Output("assist-status-div", "children"),
              Input("assist-status-interval", "n_intervals"),  # triggered at regular interval
              Input("assist-status-filepath", "data"),         # or triggered by resetting the file path (to clear out the status when selecting a new file)
              prevent_initial_call=True)
def update_status(n, status_filepath):
    """
    Update the displayed status
    """
    status = ""
    try:
        with open(status_filepath, "r") as file:
            lines = file.readlines()
            status = "...\n\n" + "\n".join(lines[-1 * min(10, len(lines)):])   # get the last 10 lines (or fewer if there are not 10 in the list)
        return clean_for_display(status)
    except Exception as e:
        return ""


@callback(
    Output("download", "data"),
    Input("verify-assist-download-button", "n_clicks"),     # triggered by user clicking download button
    State("assist-status-filepath", "data"),                # the name of the file to download
    prevent_initial_call=True,
)
def download(n_clicks, status_filepath):
    """
    Download file when user clicks button
    """
    # read contents of the result file
    with open(status_filepath, 'r') as file:
        file_content = file.read()
    return dict(content=file_content, filename="rack_verification_results.txt")


####### simple callbacks to show/hide components #######


@callback(Output("verify-div", "hidden"),
              Input("verify-graph-checklist", "options"),
              Input("verify-assist-button", "n_clicks"),
              Input("verify-cancel-button", "n_clicks"),
              prevent_initial_call=True
              )
def manage_verify_div(checklist_options, continue_clicks, cancel_clicks):
    """ Show or hide the verify div """
    if (get_trigger() in ["verify-assist-button.n_clicks", "verify-cancel-button.n_clicks"]):
        return True         # continue or cancel button pressed, hide div
    elif checklist_options == []:
        return True         # no checklist options provided, don't show div
    else:
        return False        # checklist options provided, show div


@callback(Output("verify-done-dialog", "is_open"),
          Input("verify-done-dialog-body", "children"),
          Input("verify-done-button", "n_clicks"),
          prevent_initial_call=True
          )
def manage_verify_done_dialog(children, n_clicks):
    """ Show or hide the done dialog """
    if (get_trigger() == "verify-done-button.n_clicks"):
        return False    # button pressed, hide the dialog
    else:
        return True     # child added, show the dialog
