""" Content for the home page """

from dash import html, dcc
import dash_bootstrap_components as dbc
from .helper import *
import pandas as pd

def layout():
    """ Provide the layout in a function, so that it is refreshed every time the page is displayed """

    # get table with graph names and triple counts
    df = pd.DataFrame(get_graph_info().get_pandas_data())
    df.rename(columns={'graph': 'Graph', 'triples': '# Triples'}, inplace=True)   # rename columns for display

    layout = html.Div(children=[
        html.H2('Welcome to RACK.'),
        dcc.Markdown('Current graphs in RACK:', style={"margin-top": "50px"}),
        dbc.Table.from_dataframe(df, color="primary", bordered=True, size="sm", style={"width": "auto"}),
    ])

    return layout
