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

# setting suppress_callback_exceptions=True to avoid errors when defining callbacks on components not contained in initial layout
app = dash.Dash(external_stylesheets=[dbc.themes.BOOTSTRAP], suppress_callback_exceptions=True)

# the style arguments for the sidebar. We use position:fixed and a fixed width
SIDEBAR_STYLE = {
    "position": "fixed",
    "top": 0,
    "left": 0,
    "bottom": 0,
    "width": "16rem",
    "padding": "2rem 1rem",
    "background-color": "#f8f9fa",
}

# the styles for the main content position it to the right of the sidebar and
# add some padding.
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

@app.callback(Output("page-content", "children"), [Input("url", "pathname")])
def render_page_content(pathname: str) -> dbc.Container:
    if pathname == "/":
        return page_main()
    elif pathname == "/overlay":
        return page_overlay()
    elif pathname == "/data":
        return page_load_data()
    # If the user tries to reach a different page, return a 404 message
    return dbc.Container(
        [
            html.H1("404: Not found", className="text-danger"),
            html.Hr(),
            html.P(f"The pathname {pathname} was not recognized..."),
        ]
    )

@app.callback(Output("div-dummy", "children"), [Input("button-load-boeing-overlay", "n_clicks")])
def load_boeing_overlay(n_clicks) -> dbc.Container:
    if n_clicks is not None:
        if n_clicks > 0:
            rack.ingest_owl_driver(Path("/home/ubuntu/RACK/Boeing-Ontology/OwlModels/import.yaml"), "http://localhost", "http://localhost:3030/RACK", "fuseki", False)
            rack.store_nodegroups_driver(Path("/home/ubuntu/RACK/nodegroups/ingestion/arcos.AH-64D"),"http://localhost")
    return dbc.Container()

@app.callback(Output('output-data-upload', 'children'),
              Input('upload-data', 'contents'),
              State('upload-data', 'filename'),
              State('upload-data', 'last_modified'),
              prevent_initial_call=True)
def upload_ingestion_package(list_of_contents, list_of_names, list_of_dates):
    for content, name, date in zip(list_of_contents, list_of_names, list_of_dates):
        new_dir = "ingestion_package_uploaded_" + datetime.now().strftime("%Y%m%d-%H%M%S")
        content_type, content_string = content.split(',')
        content_decoded = base64.b64decode(content_string)
        zip_str = io.BytesIO(content_decoded)
        zip_obj = ZipFile(zip_str, 'r')
        zip_obj.extractall(path="/tmp/" + new_dir)
    return list_of_names

CONFIG_CONN = '{"name":"RACK","domain":"","enableOwlImports":false,"model":[{"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/model"}],"data":[{"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/data"}]}'

def page_main() -> html.Div:
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
        tb = traceback.format_exception(None, e, e.__traceback__)
        return dcc.Markdown("### RACK is not running properly.  Error: \n" + tb[-1])

def page_overlay() -> html.Div:
    try:
        return html.Div([
            dcc.Markdown("This page will show which overlays are loaded, and allow user to load/unload specific overlays"),
            html.Button('Load Boeing overlay', id='button-load-boeing-overlay'),
            ])

    except Exception as e:
        tb = traceback.format_exception(None, e, e.__traceback__)
        return dcc.Markdown("### RACK is not running properly.  Error: \n" + tb[-1])

def page_load_data() -> html.Div:
    try:
        return html.Div([
            dcc.Markdown("Select an ingestion package to load:"),
            dcc.Upload(
                id='upload-data',
                children=html.Div([
                    'Drag and Drop or ',
                    html.A('Select Files')
                ]),
                style={
                    'width': '100%',
                    'height': '60px',
                    'lineHeight': '60px',
                    'borderWidth': '1px',
                    'borderStyle': 'dashed',
                    'borderRadius': '5px',
                    'textAlign': 'center',
                    'margin': '10px'
                },
                accept=".zip",
                multiple=True
            ),
            html.Div(id='output-data-upload'),
            ])
    except Exception as e:
        tb = traceback.format_exception(None, e, e.__traceback__)
        return dcc.Markdown("### RACK is not running properly.  Error: \n" + tb[-1])

if __name__ == '__main__':
    app.run_server(host="0.0.0.0", debug=True)
