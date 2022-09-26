""" Content for the "verify data" page """

import time
import platform
import subprocess
import dash
from dash import html, dcc, callback, Input, Output, State
import dash_bootstrap_components as dbc
from .helper import *

# dialog confirming verification done
assist_done_dialog = dbc.Modal(
    [
        dbc.ModalBody("MESSAGE PLACEHOLDER", id="assist-done-dialog-body"),     # message
        dbc.ModalFooter([
            html.Button("Download results", id="btn-download"),                 # download results button
            html.Button("Close", id="assist-done-dialog-button", n_clicks=0)    # close button
        ]),
        dcc.Download(id="download"),
    ],
    id="assist-done-dialog",
    is_open=False,
    backdrop=False,
)

# page elements
layout = html.Div(children=[
    html.H2('Verify Data'),
    dcc.Markdown("_Run the ASSIST-DV tool to perform [verification checks](https://github.com/ge-high-assurance/RACK/wiki/Data-Verification#types-of-verification) on the data in RACK_"),
    html.Button("Verify data", id="assist-button"),                 # run button
    html.Div(id="assist-status-div", className="scrollarea"),       # displays status
    assist_done_dialog,
    dcc.Store("assist-status-filepath"),       # stores the filename of the temp file containing status
    dcc.Interval(id='assist-status-interval', interval=0.5*1000, n_intervals=0, disabled=True), # triggers updating the status display
])

####### callbacks #######

@dash.callback(
    output=Output("assist-status-filepath", "data"),                # store a status file path
    inputs=Input("assist-button", "n_clicks"),                      # triggered by user clicking button
    background=True,                                                # background callback
    running=[
        (Output("assist-button", "disabled"), True, False),         # disable the run button while running
    ],
    prevent_initial_call=True
)
def create_status_filepath(button_clicks):
    """
    Generate a file in which to capture the status
    """
    status_filepath = get_temp_dir_unique("output")
    return status_filepath


@dash.callback(
    output=[Output("assist-done-dialog-body", "children"), Output("btn-download", "disabled")],
    inputs=Input("assist-status-filepath", "data"),                 # triggered by creation of status file path
    background=True,                                                # background callback
    running=[
        (Output("assist-button", "disabled"), True, False),         # disable the button while running
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
            subprocess.call("../assist/bin/check -v -m http://localhost:3030/ > " + status_filepath + " 2>&1", shell=True)
        time.sleep(1)
    except Exception as e:
        return get_error_trace(e), True  # show done dialog with error, disable the download button
    return [dcc.Markdown("Verification complete.")], False


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
    Input("btn-download", "n_clicks"),          # triggered by user clicking download button
    State("assist-status-filepath", "data"),    # the name of the file to download
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

@callback(Output("assist-done-dialog", "is_open"),
          Input("assist-done-dialog-body", "children"),
          Input("assist-done-dialog-button", "n_clicks"),
          prevent_initial_call=True
          )
def manage_assist_done_dialog(children, n_clicks):
    """ Show or hide the done dialog """
    if (get_trigger() == "assist-done-dialog-button.n_clicks"):
        return False    # button pressed, hide the dialog
    else:
        return True     # child added, show the dialog
