""" Content for the home page """

from dash import html, dcc
import dash_bootstrap_components as dbc
from .helper import *


def layout():
    """ Provide the layout in a function, so that it is refreshed every time the page is displayed """

    # create table rows listing graph names
    table_rows = []
    for graph_name in get_graph_names():
        table_rows.append(html.Tr([html.Td(graph_name)]))
    table_body = [html.Tbody(table_rows)]

    layout = html.Div(children=[
        html.H2('Welcome to RACK.'),
        dcc.Markdown('The following graphs currently exist in RACK (may have 0 triples):', style={"margin-top": "50px"}),
        dbc.Table(table_body, color="primary", bordered=True, size="sm", style={"width": "auto"})
    ])

    return layout
