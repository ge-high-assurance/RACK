import dash
import dash_bootstrap_components as dbc
from dash import Input, Output, State, dcc, html, dash_table
import semtk3
import pandas as pd
import sys
import traceback
import rack
import io
import base64
from zipfile import ZipFile
from datetime import datetime
from pathlib import Path

app = dash.Dash(external_stylesheets=[dbc.themes.BOOTSTRAP], suppress_callback_exceptions=True)

BASE_URL = "http://localhost"
TRIPLE_STORE = "http://localhost:3030/RACK"
TRIPLE_STORE_TYPE = "fuseki"
CONFIG_CONN = '{"name":"RACK","domain":"","enableOwlImports":false,"model":[{"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/model"}],"data":[{"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/data"}]}'

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

        dbc.Nav(
            [
                dbc.NavLink("Home", href="/", active="exact"),
                dbc.NavLink("Overlays", href="/overlay", active="exact"),
                dbc.NavLink("Load Data", href="/data", active="exact"),
            ],
            vertical=True,
            pills=True,
        ),
    ],
    style=SIDEBAR_STYLE,
)

content = html.Div(id="page-content", style=CONTENT_STYLE)

app.layout = html.Div([dcc.Location(id="url"), sidebar, content, html.Div(id='div-dummy')])

@app.callback(Output("page-content", "children"), 
              Input("url", "pathname"))
def render_page_content(pathname: str) -> dbc.Container:
    """ Callback triggered when user selects a page """
    if pathname == "/":
        return page_main()
    elif pathname == "/overlay":
        return page_overlay()
    elif pathname == "/data":
        return page_load_data()
    return dbc.Container(
        [
            html.H1("404: Not found", className="text-danger"),
            html.Hr(),
            html.P(f"The pathname {pathname} was not recognized..."),
        ]
    )

@app.callback(Output("div-dummy", "children"), 
              Input("button-load-overlay", "n_clicks"))
def load_overlay(n_clicks) -> dbc.Container:
    """ Callback triggered when user selects an overlay to load """
    if n_clicks is not None:
        if n_clicks > 0:
            rack.ingest_owl_driver(Path("../Boeing-Ontology/OwlModels/import.yaml"), BASE_URL, TRIPLE_STORE, TRIPLE_STORE_TYPE, False)
            rack.store_nodegroups_driver(Path("../nodegroups/ingestion/arcos.AH-64D"), BASE_URL)
    return dbc.Container()

@app.callback(Output('output-data-upload', 'children'),
              Input('upload-data', 'contents'),
              Input('upload-data', 'filename'),
              Input('upload-data', 'last_modified'),
              prevent_initial_call=True)
def upload_ingestion_package(list_of_contents, list_of_names, list_of_dates):
    """ Callback triggered when user selects an ingestion package to load """
    for content, name, date in zip(list_of_contents, list_of_names, list_of_dates):
        new_dir = "ingestion_package_uploaded_" + datetime.now().strftime("%Y%m%d-%H%M%S")
        content_type, content_string = content.split(',')
        content_decoded = base64.b64decode(content_string)
        zip_str = io.BytesIO(content_decoded)
        zip_obj = ZipFile(zip_str, 'r')
        zip_obj.extractall(path="/tmp/" + new_dir)
    return list_of_names

def page_main() -> html.Div:
    """ Components for main page """
    semtk3.set_connection_override(CONFIG_CONN)
    try:
        table = semtk3.get_oinfo_predicate_stats().get_class_count_table()

        # split the class into namespace and class
        col_names = table.get_column_names()
        col_types = table.get_column_types()
        rows = table.get_rows()
        col_names.insert(0, "namespace")
        col_types.insert(0, "string")
        new_rows = [r[0].split("#") + [r[1]] for r in rows]
        table3 = semtk3.semtktable.SemtkTable(semtk3.semtktable.SemtkTable.create_table_dict(col_names, col_types, new_rows))

        df = pd.DataFrame(table3.get_pandas_data())
        data_table = dash_table.DataTable(data=df.to_dict('records'), sort_action='native')

        return html.Div([
            dcc.Markdown("I called semtk_python3 and found\n\nthese are loaded in **http://rack001/data** "),
            data_table
            ])
    except Exception as e:
        return display_error(e)

def page_overlay() -> html.Div:
    """ Components for overlay page """
    try:
        return html.Div([
            dcc.Markdown("This page will show which overlays are loaded, and allow user to load/unload specific overlays"),
            html.Button('Load Boeing overlay', id='button-load-overlay'),
            ])
    except Exception as e:
        return display_error(e)

def page_load_data() -> html.Div:
    """ Components for data page """
    try:
        return html.Div([
            dcc.Markdown("Select an ingestion package to load:"),
            dcc.Upload(
                html.Button('Select Ingestion Package'),
                id='upload-data',
                accept=".zip",
                multiple=True
            ),
            html.Div(id='output-data-upload'),
            ])
    except Exception as e:
        return display_error(e)

def display_error(e) -> dcc.Markdown:
    """ Get an error traceback message """
    traceback = traceback.format_exception(None, e, e.__traceback__)
    return dcc.Markdown("### RACK is not running properly.  Error: \n" + traceback[-1])

if __name__ == '__main__':
    app.run_server(host="0.0.0.0", debug=True)
