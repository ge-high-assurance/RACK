import dash
from dash import html, dcc

dash.register_page(__name__, path='/', title="RACK UI", order=1)

layout = html.Div(children=[
    html.H2('Welcome to RACK.'),
])