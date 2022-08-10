import dash
import dash_bootstrap_components as dbc
import glob
import traceback
import io
import base64
import urllib
from dash import Input, Output, State, dcc, html, dash_table
from zipfile import ZipFile
from datetime import datetime
from pathlib import Path
from contextlib import redirect_stdout

import rack
from rack import Graph, Manifest

app = dash.Dash(external_stylesheets=[dbc.themes.BOOTSTRAP], suppress_callback_exceptions=True)
in_mem_file = io.StringIO()   # stores loading status

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
dialogs = html.Div(
    [
        # dialog confirming load done (or showing error)
        dbc.Modal(
            [
                dbc.ModalBody("MESSAGE", id="load-done-dialog-div"),
                dbc.ModalFooter(dbc.Button("Close", id="load-done-dialog-button", className="ms-auto", n_clicks=0)),
            ],
            id="load-done-dialog",
            is_open=False,
            backdrop=False,
        ),
    ]
)

content = html.Div(id="page-content", style=CONTENT_STYLE)

app.layout = html.Div([dcc.Location(id="url"), sidebar, content, dialogs])

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

@app.callback(Output('load-button', 'disabled'),
              Input('load-button', 'contents'),
              Input('load-done-dialog-button', 'n_clicks'),
              State('load-done-dialog', 'is_open'),
              prevent_initial_call=True)
def disable_upload_button(zip_file_contents, n_clicks_close, load_done_dialog_is_open):
    """ Callback to disable the load button while a load is in progress """
    if not load_done_dialog_is_open and zip_file_contents is not None:  # user clicked the load button
        return True  # disable the upload button
    return False  # enable the upload button

@app.callback(Output("div-status", "children"),
              Input("interval-component", "n_intervals"))
def update_status(n):
    """ Callback triggered at regular interval to update loading status """
    return in_mem_file.getvalue()

@app.callback(Output('load-done-dialog', 'is_open'),
              Output('load-done-dialog-div', 'children'),
              Input('load-button', 'contents'),
              Input('load-done-dialog-button', 'n_clicks'),
              State('load-done-dialog', 'is_open'),
              prevent_initial_call=True)
def upload_ingestion_package(zip_file_contents, n_clicks_close, load_done_dialog_is_open):
    """ Callback triggered when user selects an ingestion package to load """
    if not load_done_dialog_is_open and zip_file_contents is not None:  # user clicked the load button
        try:
            with redirect_stdout(in_mem_file):  # capture the load status for display
                tmp_dir = "/tmp/ingestion_package_uploaded_" + datetime.now().strftime("%Y%m%d-%H%M%S")  # temp directory to store the unzipped package
                zip_str = io.BytesIO(base64.b64decode(zip_file_contents.split(',')[1]))
                zip_obj = ZipFile(zip_str, 'r')
                zip_obj.extractall(path=tmp_dir)  # unzip the package
                manifest_paths = glob.glob(tmp_dir + '/**/' + MANIFEST_FILE_NAME, recursive=True)
                if len(manifest_paths) == 0:
                    raise Exception("Cannot load ingestion package: does not contain manifest file " + MANIFEST_FILE_NAME)
                if len(manifest_paths) > 1:
                    raise Exception("Cannot load ingestion package: contains multiple default manifest files: " + str(manifests))
                manifest_path = manifest_paths[0]

                # ingest the manifest
                rack.ingest_manifest_driver(Path(manifest_path), BASE_URL, TRIPLE_STORE, TRIPLE_STORE_TYPE, True)  # process the manifest

                # get connection from manifest, construct SPARQLGraph URL
                with open(manifest_path, mode='r', encoding='utf-8-sig') as manifest_file:
                    manifest = Manifest.fromYAML(manifest_file)
                conn = manifest.getConnection()
                sparqlgraph_url_str = "http://localhost:8080/sparqlGraph/main-oss/sparqlGraph.html?conn=" + urllib.parse.quote(conn, safe="")

            return True, html.Div([dcc.Markdown("Loaded ingestion package."), html.A("Open in SPARQLGraph UI", href=sparqlgraph_url_str, target="_blank", style={"margin-top": "100px"}) ]
        )
        except Exception as e:
            return True, get_error_trace(e)  # show load done dialog with error
    elif load_done_dialog_is_open and n_clicks_close is not None and n_clicks_close > 0:  # user clicked close on the load done dialog
        in_mem_file.truncate(0)  # clear status
        in_mem_file.seek(0)  # clear status
    return False, ""  # hide (or don't show) load done dialog

def page_main() -> html.Div:
    """ Components for main page """
    try:
        return html.Div([
            html.Table([
                html.Tr(dcc.Markdown("Welcome to RACK.")),
                html.Tr(dcc.Upload( html.Button('Load ingestion package', style=BUTTON_STYLE), id='load-button', accept=".zip", multiple=False)),
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
