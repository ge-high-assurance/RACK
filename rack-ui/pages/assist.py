import dash
from dash import html, dcc

dash.register_page(__name__, name='Verify Data')

layout = html.Div(children=[
    html.H2('Verify Data'),
    dcc.Markdown('Run the ASSIST tool')
])