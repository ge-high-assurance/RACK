import dash
import dash_bootstrap_components as dbc
from dash import Input, Output, dcc, html, dash_table
import semtk3
import pandas as pd
import sys
import traceback
import rack
from pathlib import Path

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
        return dcc.Markdown("This page allow user to load and clear ingestion package data")
    # If the user tries to reach a different page, return a 404 message
    return dbc.Container(
        [
            html.H1("404: Not found", className="text-danger"),
            html.Hr(),
            html.P(f"The pathname {pathname} was not recognised..."),
        ]
    )

@app.callback(Output("div-dummy", "children"), [Input("button-load-boeing-overlay", "n_clicks")])
def load_boeing_overlay(n_clicks) -> dbc.Container:
    if n_clicks is not None:
        if n_clicks > 0:
            rack.ingest_owl_driver(Path("/home/ubuntu/RACK/Boeing-Ontology/OwlModels/import.yaml"), "http://localhost", "http://localhost:3030/RACK", "fuseki", False)
    return dbc.Container()

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

if __name__ == '__main__':
    app.run_server(host="0.0.0.0", debug=True)