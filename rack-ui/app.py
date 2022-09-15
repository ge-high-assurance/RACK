""" Main application page """

import diskcache
import dash
from dash import Dash, DiskcacheManager, html, dcc
import dash_bootstrap_components as dbc
from pages.helper import *

# diskcache for non-production apps when developing locally (fine for our Docker application).  Needed for @dash.callback with background=True
cache = diskcache.Cache(get_temp_dir() + "/cache")
background_callback_manager = DiskcacheManager(cache)

app = Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP], background_callback_manager=background_callback_manager, use_pages=True)
app.title = 'RACK UI'

# menu
sidebar = html.Div(
    [
        html.Table([
            html.Tr([
                html.Td(html.Img(src=app.get_asset_url('RACK_cartoon.jpg'), height="90px")),
                html.Td([dcc.Markdown("## RACK\n_System manager_")]),
            ]),
            html.Tr(
                html.Td([
                    dbc.Nav([
                        # add a NavLink for each registered page
                        dbc.NavLink(f"{page['name']}", href=page['relative_path'], active="exact")
                        for page in dash.page_registry.values()
                    ],
                    vertical=True, pills=True,
                    )
                ], colSpan=2)
            )
        ]),
    ],
    className="sidebar"
)

app.layout = html.Div([
    sidebar,
    dash.page_container     # display page content
],
    style = { "margin-left": "18rem", "margin-right": "2rem", "padding": "2rem 1rem" }
)

if __name__ == '__main__':
    app.run_server(host="0.0.0.0", debug=False)