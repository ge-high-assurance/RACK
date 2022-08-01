import dash
import dash_bootstrap_components as dbc
from dash import Input, Output, State, dcc, html, dash_table
import traceback
import rack
from rack import Graph
import io
import base64
from zipfile import ZipFile
from datetime import datetime
from pathlib import Path
import glob

app = dash.Dash(external_stylesheets=[dbc.themes.BOOTSTRAP], suppress_callback_exceptions=True)

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
    "font-size": "14px", 
    "width": "200px",
    "display": "inline-block",
    "margin-bottom": "10px",
    "margin-right": "5px",
    "height":"25px"}

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

        # dbc.Nav(
        #     [
        #         dbc.NavLink("Home", href="/", active="exact"),
        #         dbc.NavLink("More", href="/page2", active="exact"),
        #     ],
        #     vertical=True,
        #     pills=True,
        # ),
    ],
    style=SIDEBAR_STYLE,
)

modals = html.Div(
    [
        dbc.Modal(
            [
                dbc.ModalBody("MESSAGE", id="div-modal-arcos"),
                dbc.ModalFooter(dbc.Button("Close", id="button-modal-arcos-close", className="ms-auto", n_clicks=0)),
            ],
            id="modal-arcos",
            is_open=False,
            backdrop=False,
        ),
        dbc.Modal(
            [
                dbc.ModalBody("MESSAGE", id="div-modal-upload"),
                dbc.ModalFooter(dbc.Button("Close", id="button-modal-upload-close", className="ms-auto", n_clicks=0)),
            ],
            id="modal-upload",
            is_open=False,
            backdrop=False,
        ),
        dbc.Modal(
            [
                dbc.ModalBody("MESSAGE", id="div-modal-reset"),
                dbc.ModalFooter(dbc.Button("Close", id="button-modal-reset-close", className="ms-auto", n_clicks=0)),
            ],
            id="modal-reset",
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
    """ Callback triggered when user selects a page """
    if pathname == "/":
        return page_main()
    # elif pathname == "/page2":
    #     return page2()
    return dbc.Container(
        [
            html.H1("404: Not found", className="text-danger"),
            html.Hr(),
            html.P(f"The pathname {pathname} was not recognized..."),
        ]
    )

@app.callback([Output('modal-arcos', 'is_open'), Output('div-modal-arcos', 'children')],
              Input('button-load-arcos', 'n_clicks'),
              Input('button-modal-arcos-close', 'n_clicks'),
              State('modal-arcos', 'is_open'),
              prevent_initial_call=True)
def load_arcos(n_clicks_arcos, n_clicks_close, modal_is_open):
    """ Callback triggered when user selects Load ARCOS """
    if not modal_is_open and n_clicks_arcos is not None and n_clicks_arcos > 0:
        try:
            rack.ingest_manifest_driver(Path("../cli/manifest-arcos.yaml"), BASE_URL, TRIPLE_STORE, TRIPLE_STORE_TYPE)
            return True, "Loaded ARCOS"  # show modal dialog
        except Exception as e:
            return True, get_error_trace(e)  # show modal dialog with error
    return False, ""  # hide (or don't show) modal dialog

@app.callback([Output('modal-upload', 'is_open'), Output('div-modal-upload', 'children')],
              Input('button-upload', 'contents'),
              Input('button-upload', 'filename'),
              Input('button-upload', 'last_modified'),
              Input('button-modal-upload-close', 'n_clicks'),
              State('modal-upload', 'is_open'),
              prevent_initial_call=True)
def upload_ingestion_package(list_of_contents, list_of_names, list_of_dates, n_clicks_close, modal_is_open):
    """ Callback triggered when user selects an ingestion package to load """
    if not modal_is_open and list_of_contents is not None:
        try:
            for content, name, date in zip(list_of_contents, list_of_names, list_of_dates):
                tmp_dir = "/tmp/ingestion_package_uploaded_" + datetime.now().strftime("%Y%m%d-%H%M%S")
                content_type, content_string = content.split(',')
                zip_str = io.BytesIO(base64.b64decode(content_string))
                zip_obj = ZipFile(zip_str, 'r')
                zip_obj.extractall(path=tmp_dir)
                manifests = glob.glob(tmp_dir + '/**/*manifest*.yaml', recursive=True)
                if len(manifests) == 0:
                    raise Exception("Cannot load ingestion package: contains no manifest file")
                if len(manifests) > 1:
                    raise Exception("Cannot load ingestion package: contains multiple manifest files: " + str(manifests))
                manifest = manifests[0]
                rack.ingest_manifest_driver(Path(manifest), BASE_URL, TRIPLE_STORE, TRIPLE_STORE_TYPE)
                return True, "Loaded ingestion package using manifest file " + manifest[(len(tmp_dir) + 1):]
        except Exception as e:
            return True, get_error_trace(e)  # show modal dialog with error
    return False, ""  # hide (or don't show) modal dialog

@app.callback([Output('modal-reset', 'is_open'), Output('div-modal-reset', 'children')],
              Input('button-reset', 'n_clicks'),
              Input('button-modal-reset-close', 'n_clicks'),
              State('modal-reset', 'is_open'),
              prevent_initial_call=True)
def reset(n_clicks_reset, n_clicks_close, modal_is_open):
    """ Callback triggered when user selects reset """
    if not modal_is_open and n_clicks_reset is not None and n_clicks_reset > 0:
        try:

            # clear all model/data/nodegroups
            # TODO remove when manifest supports "clear-first"
            rack.clear_driver(BASE_URL, ["http://rack001/model"], TRIPLE_STORE, TRIPLE_STORE_TYPE, Graph.MODEL)
            rack.clear_driver(BASE_URL, ["http://rack001/data"], TRIPLE_STORE, TRIPLE_STORE_TYPE, Graph.DATA)
            rack.clear_driver(BASE_URL, ["http://rack001/arp-4754a"], TRIPLE_STORE, TRIPLE_STORE_TYPE, Graph.DATA)
            rack.clear_driver(BASE_URL, ["http://rack001/do-330"], TRIPLE_STORE, TRIPLE_STORE_TYPE, Graph.DATA)
            rack.clear_driver(BASE_URL, ["http://rack001/do-178c"], TRIPLE_STORE, TRIPLE_STORE_TYPE, Graph.DATA)
            rack.clear_driver(BASE_URL, ["http://rack001/mitre-cwe"], TRIPLE_STORE, TRIPLE_STORE_TYPE, Graph.DATA)
            rack.delete_all_nodegroups_driver(yes=True, base_url=BASE_URL)

            # load RACK
            rack.ingest_manifest_driver(Path("../cli/manifest-rack.yaml"), BASE_URL, TRIPLE_STORE, TRIPLE_STORE_TYPE)
            return True, "RACK has been reset"  # show modal dialog
        except Exception as e:
            return True, get_error_trace(e)  # show modal dialog with error
    return False, ""  # hide (or don't show) modal dialog


def page_main() -> html.Div:
    """ Components for main page """
    try:
        return html.Div(
            html.Table([
                html.Tr(dcc.Markdown("Welcome to RACK.")),
                html.Tr(html.Button('Load ARCOS', id='button-load-arcos', style=BUTTON_STYLE)),
                html.Tr(dcc.Upload( html.Button('Load ingestion package', style=BUTTON_STYLE), id='button-upload', accept=".zip", multiple=True)),
                html.Tr(html.Button('Reset', id='button-reset', style=BUTTON_STYLE)),
            ])
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
