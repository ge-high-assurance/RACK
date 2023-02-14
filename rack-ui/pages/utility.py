""" Content for the utility page """

import time
import platform
from dash import html, dcc, callback, Input, Output, State
import dash_bootstrap_components as dbc
from .helper import *


# dialog confirming triple store restart done
restart_done_dialog = dbc.Spinner(dbc.Modal(
    [
        dbc.ModalBody("MESSAGE PLACEHOLDER", id="restart-done-dialog-body"),      # message
        dbc.ModalFooter([
            html.Button("Close", id="restart-done-button", n_clicks=0)            # close button
        ]),
    ],
    id="restart-done-dialog",
    is_open=False,
    backdrop=False,
))

# page elements
layout = html.Div([
    html.H2('RACK Utilities'),
    dcc.Markdown("_Utility functions for RACK_"),
    html.Button("Restart triple store", id="restart-button", n_clicks=0),
    restart_done_dialog
])

####### callbacks ######################################

@dash.callback(
    output=Output("restart-done-dialog-body", "children"),
    inputs=Input("restart-button", "n_clicks"),                     # triggered by clicking restart button
    background=True,                                                # background callback
    running=[
        (Output("restart-button", "disabled"), True, False),        # disable the button while running
    ],
    prevent_initial_call=True
)
def run_restart(n_clicks):
    """
    Restart the triple store
    """
    try:
        # determine if we can restart fuseki
        if run_subprocess("systemctl is-enabled fuseki").returncode != 0:
            raise Exception("Triple store restart not supported in this deployment")

        # restart fuseki
        completed_process = run_subprocess("sudo systemctl restart fuseki", get_temp_dir_unique("restart-fuseki"))
        if completed_process.returncode == 0:
            return dcc.Markdown("Restarted the triple store.")
        else:
            raise Exception("Error restarting the triple store")
    except Exception as e:
        return get_error_trace(e)  # show done dialog with error

####### simple callbacks to show/hide components #######

@callback(Output("restart-done-dialog", "is_open"),
          Input("restart-done-dialog-body", "children"),
          Input("restart-done-button", "n_clicks"),
          prevent_initial_call=True
          )
def manage_restart_done_dialog(children, n_clicks):
    """ Show or hide the done dialog after restarting triple store """
    if (get_trigger() == "restart-done-button.n_clicks"):
        return False    # button pressed, hide the dialog
    else:
        return True     # child added, show the dialog
