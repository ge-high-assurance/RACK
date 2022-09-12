import tempfile
import diskcache

import dash
from dash import Dash, DiskcacheManager, html, dcc
import dash_bootstrap_components as dbc

TEMP_DIR = tempfile.gettempdir()

# diskcache for non-production apps when developing locally (fine for our Docker application).  Needed for @dash.callback with background=True
cache = diskcache.Cache(TEMP_DIR + "/cache")
background_callback_manager = DiskcacheManager(cache)

app = Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP], background_callback_manager=background_callback_manager, use_pages=True)
app.title = 'RACK UI'

links_div =     html.Div(
        [
            html.Div(dcc.Link(f"{dash.page_registry['pages.home']['name']}", href=dash.page_registry['pages.home']['relative_path'])),
            html.Div(dcc.Link(f"{dash.page_registry['pages.ingest']['name']}", href=dash.page_registry['pages.ingest']['relative_path'])),
            html.Div(dcc.Link(f"{dash.page_registry['pages.assist']['name']}", href=dash.page_registry['pages.assist']['relative_path'])),
        ]
    )

# menu
sidebar = html.Div(
    [
        html.Table([
            html.Tr([
                html.Td(html.Img(src=app.get_asset_url('RACK_cartoon.jpg'), height="90px")),
                html.Td([dcc.Markdown("## RACK\n_System manager_")]),
            ]),
            html.Tr([
                links_div
            ])
        ]),
    ],
    className="sidebar"
)

app.layout = html.Div([
    sidebar,
    dash.page_container
],
    style = { "margin-left": "18rem", "margin-right": "2rem", "padding": "2rem 1rem" }
)

if __name__ == '__main__':
    app.run_server(host="0.0.0.0", debug=False)