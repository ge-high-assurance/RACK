""" Main application page """

import diskcache
import dash
from dash import Dash, DiskcacheManager, html, dcc, callback, Input, Output
import dash_bootstrap_components as dbc
from pages import home, load, verify
from pages.helper import *
from flask import Flask
import json
import platform

# diskcache for non-production apps when developing locally (fine for our Docker application).  Needed for @dash.callback with background=True
cache = diskcache.Cache(get_temp_dir() + "/cache")
background_callback_manager = DiskcacheManager(cache)

server = Flask(__name__)
app = Dash(server=server, external_stylesheets=[dbc.themes.BOOTSTRAP], background_callback_manager=background_callback_manager)
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
                        dbc.NavLink("Home", href="/", active="exact"),
                        dbc.NavLink("Load", href="/load", active="exact"),
                        dbc.NavLink("Verify", href="/verify", active="exact"),
                    ],
                    vertical=True, pills=True,
                    )
                ], colSpan=2)
            )
        ]),
    ],
    className="sidebar"
)

# layout
app.layout = html.Div([
    dcc.Location(id='url', refresh=False),
    sidebar,
    html.Div(id='page-content'),        # display page content
    dcc.Store("last-loaded-graphs"),    # stores the last-loaded graphs (used by multiple pages)
],
    style = { "margin-left": "18rem", "margin-right": "2rem", "padding": "2rem 1rem" }
)

# validate using this layout (includes components from pages)
app.validation_layout = html.Div([app.layout, load.layout, verify.layout])


@callback(Output('page-content', 'children'),
            Input('url', 'pathname'))
def display_page(pathname):
    if pathname == '/':
        return home.layout()
    elif pathname == '/load':
        return load.layout
    elif pathname == '/verify':
        return verify.layout
    else:
        return '404'


# endpoint to run triplestore optimization script
# (runs as part of the Dash app, alongside but separate from the UI)
@server.route('/optimize')
def optimize():
    try:
        if platform.system() == "Windows":
            raise Exception("RACK UI triplestore optimization endpoint is not supported on Windows")
        command = "sudo ../cli/optimize.sh"
        completed_process = run_subprocess(command, get_temp_dir_unique("optimize"))
        if completed_process.returncode == 0:
            return json.dumps({'success': True})
        else:
            raise Exception(f"Optimize script returned exit code {completed_process.returncode}")
    except Exception as e:
        return json.dumps({'success': False, 'message': get_error_message(e)})


if __name__ == '__main__':
    app.run_server(host="0.0.0.0", debug=False)