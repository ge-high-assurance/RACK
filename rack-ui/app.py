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
from rack import Graph
import os

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

        dbc.Nav(
            [
                dbc.NavLink("Home", href="/", active="exact"),
                dbc.NavLink("More", href="/page2", active="exact"),
            ],
            vertical=True,
            pills=True,
        ),
    ],
    style=SIDEBAR_STYLE,
)

content = html.Div(id="page-content", style=CONTENT_STYLE)

app.layout = html.Div([dcc.Location(id="url"), sidebar, content])

@app.callback(Output("page-content", "children"), 
              Input("url", "pathname"))
def render_page_content(pathname: str) -> dbc.Container:
    """ Callback triggered when user selects a page """
    if pathname == "/":
        return page_main()
    elif pathname == "/page2":
        return page2()
    return dbc.Container(
        [
            html.H1("404: Not found", className="text-danger"),
            html.Hr(),
            html.P(f"The pathname {pathname} was not recognized..."),
        ]
    )

@app.callback(Output('div-load-arcos', 'children'),
              Input('button-load-arcos', 'n_clicks'),
              prevent_initial_call=True)
def load_arcos(n_clicks):
    """ Callback triggered when user selects Load ARCOS """
    if n_clicks is not None:
        if n_clicks > 0:
            rack.ingest_manifest_driver("/home/ubuntu/RACK/cli/manifest-arcos.yaml", BASE_URL, TRIPLE_STORE, TRIPLE_STORE_TYPE)
            return "Loaded ARCOS"
    return ""

@app.callback(Output('div-upload', 'children'),
              Input('button-upload', 'contents'),
              Input('button-upload', 'filename'),
              Input('button-upload', 'last_modified'),
              prevent_initial_call=True)
def upload_ingestion_package(list_of_contents, list_of_names, list_of_dates):
    """ Callback triggered when user selects an ingestion package to load """
    for content, name, date in zip(list_of_contents, list_of_names, list_of_dates):
        tmp_dir = "/tmp/ingestion_package_uploaded_" + datetime.now().strftime("%Y%m%d-%H%M%S")
        content_type, content_string = content.split(',')
        content_decoded = base64.b64decode(content_string)
        zip_str = io.BytesIO(content_decoded)
        zip_obj = ZipFile(zip_str, 'r')
        zip_obj.extractall(path=tmp_dir)
        manifest = tmp_dir + "/Apache-IngestionPackage-wSW-RACKv10.2-20220531/manifest.yaml"
        rack.ingest_manifest_driver(manifest, BASE_URL, TRIPLE_STORE, TRIPLE_STORE_TYPE)
    return "Tried to load " + manifest

@app.callback(Output('div-clear', 'children'),
              Input('button-clear', 'n_clicks'),
              prevent_initial_call=True)
def clear(n_clicks):
    """ Callback triggered when user selects clear """
    if n_clicks is not None:
        if n_clicks > 0:
            #rack.clear_driver(BASE_URL, ["http://rack001/nist-800-53"], TRIPLE_STORE, TRIPLE_STORE_TYPE, Graph.DATA)
            return "Reset not implemented yet"
    return ""


def page_main() -> html.Div:
    """ Components for main page """
    try:
        return html.Div([
            dcc.Markdown("Welcome to RACK."),
            html.Button('Load ARCOS', id='button-load-arcos', style=BUTTON_STYLE),
            dcc.Upload(
                html.Button('Load ingestion package', style=BUTTON_STYLE),
                id='button-upload',
                accept=".zip",
                multiple=True
            ),
            html.Button('Reset', id='button-clear', style=BUTTON_STYLE),
            html.Div(id='div-load-arcos'),
            html.Div(id='div-upload'),
            html.Div(id='div-clear'),
            ])
    except Exception as e:
        return display_error(e)

def page2() -> html.Div:
    """ Components for another page (empty for now) """
    try:
        return html.Div([
            ])
    except Exception as e:
        return display_error(e)

def display_error(e) -> dcc.Markdown:
    """ Get an error traceback message """
    traceback = traceback.format_exception(None, e, e.__traceback__)
    return dcc.Markdown("### RACK is not running properly.  Error: \n" + traceback[-1])

if __name__ == '__main__':
    app.run_server(host="0.0.0.0", debug=True)
