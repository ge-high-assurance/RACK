import dash
import dash_bootstrap_components as dbc
import glob
import traceback
import io
import base64
from dash import Input, Output, State, dcc, html, dash_table
from zipfile import ZipFile
from datetime import datetime
from pathlib import Path
from contextlib import redirect_stdout

import rack
from rack import Graph

app = dash.Dash(external_stylesheets=[dbc.themes.BOOTSTRAP], suppress_callback_exceptions=True)
in_mem_file = io.StringIO()   # stores loading status

BASE_URL = "http://localhost"
TRIPLE_STORE = "http://localhost:3030/RACK"
TRIPLE_STORE_TYPE = "fuseki"

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

# style for the main content
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

# modal dialogs
modals = html.Div(
    [
        dbc.Modal(
            [
                dbc.ModalBody("MESSAGE", id="div-modal-upload"),
                dbc.ModalFooter(dbc.Button("Close", id="button-modal-upload-close", className="ms-auto", n_clicks=0)),
            ],
            id="modal-upload",
            is_open=False,
            backdrop=False,
        ),
    ]
)

content = html.Div(id="page-content", style=CONTENT_STYLE)

app.layout = html.Div([dcc.Location(id="url"), sidebar, content, modals])

@app.callback(Output("page-content", "children"), 
              Input("url", "pathname"))
def render_page_content(pathname: str) -> dbc.Container:
    """ Callback triggered when user selects a page from the sidebar menu """
    if pathname == "/":
        return page_main()
    return dbc.Container(
        [
            html.H1("404: Not found", className="text-danger"),
            html.Hr(),
            html.P(f"The pathname {pathname} was not recognized..."),
        ]
    )

@app.callback(Output("div-status", "children"),
              Input("interval-component", "n_intervals"))
def update_status(n):
    """ Callback triggered at regular interval to update loading status """
    return in_mem_file.getvalue()

@app.callback([Output('modal-upload', 'is_open'), Output('div-modal-upload', 'children')],
              Input('button-upload', 'contents'),
              Input('button-upload', 'filename'),
              Input('button-upload', 'last_modified'),
              Input('button-modal-upload-close', 'n_clicks'),
              State('modal-upload', 'is_open'),
              prevent_initial_call=True)
def upload_ingestion_package(list_of_contents, list_of_names, list_of_dates, n_clicks_close, modal_is_open):
    """ Callback triggered when user selects an ingestion package to load """
    if not modal_is_open and list_of_contents is not None:  # user clicked the load button
        try:
            with redirect_stdout(in_mem_file):  # capture the load status for display
                for content, name, date in zip(list_of_contents, list_of_names, list_of_dates):
                    tmp_dir = "/tmp/ingestion_package_uploaded_" + datetime.now().strftime("%Y%m%d-%H%M%S")  # temp directory to store the unzipped package
                    content_type, content_string = content.split(',')
                    zip_str = io.BytesIO(base64.b64decode(content_string))
                    zip_obj = ZipFile(zip_str, 'r')
                    zip_obj.extractall(path=tmp_dir)  # unzip the package
                    default_manifest_file_name = "manifest.yaml"  # look for this manifest file
                    manifests = glob.glob(tmp_dir + '/**/' + default_manifest_file_name, recursive=True)
                    if len(manifests) == 0:
                        raise Exception("Cannot load ingestion package: does not contain a default manifest file (" + default_manifest_file_name + ")")
                    if len(manifests) > 1:
                        raise Exception("Cannot load ingestion package: contains multiple default manifest files: " + str(manifests))
                    manifest = manifests[0]
                    rack.ingest_manifest_driver(Path(manifest), BASE_URL, TRIPLE_STORE, TRIPLE_STORE_TYPE, True)  # process the manifest
            return True, "Loaded ingestion package using manifest file " + manifest[(len(tmp_dir) + 1):]  # show modal dialog with success message
        except Exception as e:
            return True, get_error_trace(e)  # show modal dialog with error
    elif modal_is_open and n_clicks_close is not None and n_clicks_close > 0:  # user clicked close on modal dialog
        in_mem_file.truncate(0)  # clear status
        in_mem_file.seek(0)  # clear status
    return False, ""  # hide (or don't show) modal dialog

def page_main() -> html.Div:
    """ Components for main page """
    try:
        return html.Div([
            html.Table([
                html.Tr(dcc.Markdown("Welcome to RACK.")),
                html.Tr(dcc.Upload( html.Button('Load ingestion package', style=BUTTON_STYLE), id='button-upload', accept=".zip", multiple=True)),
            ]),
            html.Div(id="div-status", style={"margin-top": "100px", "white-space": "pre-wrap", "border-style": "none", "height": 500, "width": 1200, "overflow-y": "auto", "display": "flex", "flex-direction": "column-reverse"}),
            dcc.Interval(id='interval-component', interval=1*1000, n_intervals=0)
            ]
        )
    except Exception as e:
        return display_error(e)

def display_error(e) -> dcc.Markdown:
    """ Get a displayable error message """
    return dcc.Markdown("### RACK is not running properly.  Error: \n" + get_error_trace(e))

def get_error_trace(e) -> str:
    """ Get error trace string """
    trace = traceback.format_exception(None, e, e.__traceback__)
    return trace[-1]

if __name__ == '__main__':
    app.run_server(host="0.0.0.0", debug=True)
