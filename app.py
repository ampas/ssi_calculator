import dash
from dash import dcc, html, Input, Output, State, callback_context
import dash_bootstrap_components as dbc
import plotly.graph_objs as go
import numpy as np
import pandas as pd
from dash import dash_table
from ssi_module import calculate_ssi
from planck_module import planck
from daylight_module import daylight
from cct_module import cct_mccamy
import io
import base64

# Constants
CORRECTION_FACTOR_DAYLIGHT = 14388 / 14380
CORRECTION_FACTOR_ILLUM_A = 14350 / 14388
MIN_WAVELENGTH = 300
MAX_WAVELENGTH = 830
DEFAULT_DAYLIGHT_CCT = 5000
DEFAULT_BLACKBODY_CCT = 3200
MIN_DAYLIGHT_CCT = 4000
MAX_DAYLIGHT_CCT = 25000
MIN_BLACKBODY_CCT = 1000
MAX_BLACKBODY_CCT = 10000
wavelengths = np.linspace(300, 830, 530)

CCT_MAPPING = {
    'D50': 5000 * CORRECTION_FACTOR_DAYLIGHT,
    'D55': 5500 * CORRECTION_FACTOR_DAYLIGHT,
    'D65': 6500 * CORRECTION_FACTOR_DAYLIGHT,
    'D75': 7500 * CORRECTION_FACTOR_DAYLIGHT,
    'Custom_Blackbody': 3200,
    'HMI': 5606,
    'A': 2855.542,
    'Xenon': 5159,
    'Warm LED': 3133,
    'Cool LED': 5300,
    'F1': 6425,
    'F2': 4224,
    'F3': 2447,
    'F4': 2939,
    'F5': 6342,
    'F6': 4148,
    'F7': 6489,
    'F8': 4995,
    'F9': 4147,
    'F10': 4988,
    'F11': 4001,
    'F12': 3002
}

# Function to interpolate and normalize spectra
def interpolate_and_normalize(spec):
    wavelengths = np.arange(MIN_WAVELENGTH, MAX_WAVELENGTH + 1)
    spec_resample = np.interp(wavelengths, spec['wavelength'], spec['intensity'])
    spec_resample /= spec_resample[np.where(wavelengths == 560)]
    return pd.DataFrame({'wavelength': wavelengths, 'intensity': spec_resample})

# Load test spectra from the provided text file
file_path_test = 'testSources_test.csv'
file_path_ref = 'daylighttestsources.csv'
test_spectra_df = pd.read_csv(file_path_test)
ref_spectra_df = pd.read_csv(file_path_ref)

def parse_contents(contents, filename):
    content_type, content_string = contents.split(',')
    decoded = base64.b64decode(content_string)
    if 'csv' in filename:
        return pd.read_csv(io.StringIO(decoded.decode('utf-8')))
    elif 'xls' in filename:
        return pd.read_excel(io.BytesIO(decoded))
    else:
        return pd.DataFrame()

# Interpolate and normalize each test spectrum
warm_led_spec = interpolate_and_normalize(test_spectra_df[['wavelength', 'Warm LED']].rename(columns={'Warm LED': 'intensity'}))
cool_led_spec = interpolate_and_normalize(test_spectra_df[['wavelength', 'Cool LED']].rename(columns={'Cool LED': 'intensity'}))
hmi_spec = interpolate_and_normalize(test_spectra_df[['wavelength', 'HMI']].rename(columns={'HMI': 'intensity'}))
xenon_spec = interpolate_and_normalize(test_spectra_df[['wavelength', 'Xenon']].rename(columns={'Xenon': 'intensity'}))
D50_spec = interpolate_and_normalize(ref_spectra_df[['wavelength', 'D50']].rename(columns={'D50': 'intensity'}))
D55_spec = interpolate_and_normalize(ref_spectra_df[['wavelength', 'D55']].rename(columns={'D55': 'intensity'}))
D65_spec = interpolate_and_normalize(ref_spectra_df[['wavelength', 'D65']].rename(columns={'D65': 'intensity'}))
D75_spec = interpolate_and_normalize(ref_spectra_df[['wavelength', 'D75']].rename(columns={'D75': 'intensity'}))
custom_spec_test = interpolate_and_normalize(test_spectra_df[['wavelength', 'Custom']].rename(columns={'Custom': 'intensity'}))
fluorescent_specs = {}
for i in range(1, 13):
    name = f'F{i}'
    fluorescent_specs[name] = interpolate_and_normalize(test_spectra_df[['wavelength', name]].rename(columns={name: 'intensity'}))

# Initialize Dash app with callback exceptions suppressed
app = dash.Dash(__name__, external_stylesheets=[dbc.themes.LUX], suppress_callback_exceptions=True)
server = app.server

intro_content = '''
Academy Spectral Similarity Index (SSI) Calculator has been written and is maintained by the Academy of Motion Picture Arts and Sciences. This tool implements the [Spectral Similarity Index (SSI)](http://www.oscars.org/ssi). The implementation is intended to be compliant with the [SSI specification](https://www.oscars.org/sites/oscars/files/ssi_overview_2018-12-04.pdf).

Source code can be found on [Github](https://www.github.com/ampas/ssi_calculator/)

For more information on the Spectral Similarity Index please visit: [http://www.oscars.org/ssi](http://www.oscars.org/ssi)
'''

software_content = '''
This calculator was built using [Python](https://www.python.org/) and [Dash](https://dash.plotly.com/)
'''

license = '''
The Academy Spectral Similarity Index (SSI) Calculator is provided by the
Academy under the following terms and conditions:

Copyright © 2019 Academy of Motion Picture Arts and Sciences ("A.M.P.A.S.").
Portions contributed by others as indicated. All rights reserved.

A worldwide, royalty-free, non-exclusive right to copy, modify, create
derivatives, and use, in source and binary forms, is hereby granted, subject to
acceptance of this license. Performance of any of the aforementioned acts
indicates acceptance to be bound by the following terms and conditions:

* Copies of source code, in whole or in part, must retain the above copyright
notice, this list of conditions and the Disclaimer of Warranty.

* Use in binary form must retain the above copyright notice, this list of
conditions and the Disclaimer of Warranty in the documentation and/or other
materials provided with the distribution.

* Nothing in this license shall be deemed to grant any rights to trademarks,
copyrights, patents, trade secrets or any other intellectual property of
A.M.P.A.S. or any contributors, except as expressly stated herein.

* Neither the name "A.M.P.A.S." nor the name of any other contributors to this
software may be used to endorse or promote products derivative of or based on
this software without express prior written permission of A.M.P.A.S. or the
contributors, as appropriate.

This license shall be construed pursuant to the laws of the State of California,
and any disputes related thereto shall be subject to the jurisdiction of the
courts therein.

Disclaimer of Warranty: THIS SOFTWARE IS PROVIDED BY A.M.P.A.S. AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, AND
NON-INFRINGEMENT ARE DISCLAIMED. IN NO EVENT SHALL A.M.P.A.S., OR ANY
CONTRIBUTORS OR DISTRIBUTORS, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, RESITUTIONARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

WITHOUT LIMITING THE GENERALITY OF THE FOREGOING, THE ACADEMY SPECIFICALLY
DISCLAIMS ANY REPRESENTATIONS OR WARRANTIES WHATSOEVER RELATED TO PATENT OR
OTHER INTELLECTUAL PROPERTY RIGHTS IN THE ACES CONTAINER REFERENCE
IMPLEMENTATION, OR APPLICATIONS THEREOF, HELD BY PARTIES OTHER THAN A.M.P.A.S.,
WHETHER DISCLOSED OR UNDISCLOSED.
'''
# Define layout
app.layout = dbc.Container(fluid=True, children=[
    dbc.NavbarSimple(
        brand="SSI Calculator",
        brand_href="#",
        color="dark",
        dark=True,
        fluid=True,
        style={'backgroundColor': '#000000'}
    ),
    dcc.Store(id='stored-cct-value'),
    dcc.Store(id='stored-custom-spec'),

    dbc.Tabs([
        dbc.Tab(label="Calculations", children=[
            dbc.Row([
                dbc.Col(width=4, children=[  # Half the screen width for settings, reference, and spectral data
                    dbc.Card([
                        dbc.CardHeader("Test Spectrum", style={'backgroundColor': '#000000', 'color': '#BA9E5E'}),
                        dbc.CardBody([
                            dcc.Dropdown(
                                id='testChoice',
                                options=[
                                    {'label': 'Warm LED', 'value': 'Warm LED'},
                                    {'label': 'Cool LED', 'value': 'Cool LED'},
                                    {'label': 'HMI', 'value': 'HMI'},
                                    {'label': 'Xenon', 'value': 'Xenon'}
                                ] +[{'label': f'F{i}', 'value': f'F{i}'} for i in range(1, 13)] + [{'label': 'Custom', 'value': 'Custom'}],
                                value='Warm LED'
                            ),
                            # html.Div(id='customTestSpecInputs')
                        ])
                    ],  style={'margin-top': '20px'}),
                    dbc.Card([
                        dbc.CardHeader("Upload CSV or Excel", style={'backgroundColor': '#000000', 'color': '#BA9E5E'}),
                        dbc.CardBody([
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
                                multiple=False
                            ),
                            html.Div(id='output-data-upload')
                        ])
                    ], id='upload-card', style={'margin-top': '20px', 'display': 'none'}),
                    dbc.Card([
                        dbc.CardHeader("Reference Spectrum", style={'backgroundColor': '#000000', 'color': '#BA9E5E'}),
                        dbc.CardBody([
                            dcc.Dropdown(
                                id='refChoice',
                                options=[
                                    {'label': 'Default', 'value': 'Default'},
                                    {'label': 'Blackbody: A', 'value': 'A'},
                                    {'label': 'Blackbody: Custom CCT', 'value': 'Custom_Blackbody'},
                                    {'label': 'Daylight: D50', 'value': 'D50'},
                                    {'label': 'Daylight: D55', 'value': 'D55'},
                                    {'label': 'Daylight: D65', 'value': 'D65'},
                                    {'label': 'Daylight: D75', 'value': 'D75'},
                                    {'label': 'Daylight: Custom CCT', 'value': 'Custom_Daylight'}
                                ],
                                value='Default'
                            ),
                            html.Div(id='refSpecInputs', style={'margin-top': '20px'}),
                            dbc.Row([
                                dbc.Label("CCT"),
                                dbc.Input(
                                    type="number",
                                    id="refCct",
                                    value=4000,
                                    min=MIN_BLACKBODY_CCT,
                                    max=MAX_BLACKBODY_CCT,
                                    step=1,
                                    debounce=True
                                ),
                            ], id='customCctInput', style={'margin-top': '20px', 'display': 'none'}),
                            dbc.Button("Submit CCT", id="submit-cct", color="primary", className="mr-2", style={'margin-top': '10px', 'display': 'none'}), 
                            html.Div(id='warning-message', style={'color': 'red', 'margin-top': '10px'})
                        ])
                    ], style={'margin-top': '20px'}),
                    
                    dbc.Card([
                        dbc.CardHeader("Spectral Data", style={'backgroundColor': '#000000', 'color': '#BA9E5E'}),
                        dbc.CardBody([
                            dcc.Tabs([
                                dcc.Tab(label='Test Spectrum', children=[
                                    html.Div(id='spectraTest', children=[dash_table.DataTable(
                                        id='spectra-test-table', 
                                        data=warm_led_spec.to_dict('records'),
                                        columns=[{"name": i, "id": i} for i in warm_led_spec.columns], 
                                        editable=True,
                                        page_size=1000,
                                        export_format="csv",
                                        export_headers="display",
                                    )])
                                ]),
                                dcc.Tab(label='Reference Spectrum', children=[
                                    html.Div(id='spectraRef')
                                ]),
                            ]),
                        ])
                    ], style={'margin-top': '20px'}),
                ]),
                dbc.Col(width=8, children=[  # Half the screen width for the graph
                    dbc.Card([
                        dbc.CardHeader("Graph", style={'backgroundColor': '#000000', 'color': '#BA9E5E'}),
                        dbc.CardBody([dcc.Graph(id='plotRef')]),
                    ], style={'margin-top': '20px'}),
                    dbc.Card([
                        dbc.CardHeader("Spectral Similarity Index (SSI)", style={'backgroundColor': '#000000', 'color': '#BA9E5E'}),
                        dbc.CardBody([html.H5(id='ssiText', className='card-text')]),
                    ], style={'margin-top': '20px'}),
                    dbc.Card([
                        dbc.CardHeader("Default Reference Spectrum Used", style={'backgroundColor': '#000000', 'color': '#BA9E5E'}),
                        dbc.CardBody([html.H5(id='defaultspecused', className='card-text')]),
                    ], id='default-spec-card', style={'margin-top': '20px'}),
                ]),
            ]),
        ]),
        dbc.Tab(label="About", children=[
            dbc.Container([
                html.H2('Introduction'),
                dcc.Markdown(intro_content),
                html.H2('Software'),
                dcc.Markdown(software_content),
                html.H2('License Terms'),
                dcc.Markdown(license),
            ], style={'margin-top': '20px'})
        ]),
    ]),
])


@app.callback(
    Output('upload-card', 'style'),
    Input('testChoice', 'value')
)
def update_upload_card_visibility(test_choice):
    if test_choice == 'Custom':
        return {'display': 'block'}
    return {'display': 'none'}

@app.callback(
    Output('default-spec-card', 'style'),
    Input('refChoice', 'value')
)
def update_defaultSpec_card_visibility(ref_choice):
    if  ref_choice == 'Default':
        return {'display': 'block'}
    return {'display': 'none'}


@app.callback(
    Output('spectra-test-table', 'data'),
    Output('spectra-test-table', 'columns'),
    Output('stored-custom-spec', 'data'),
    Input('testChoice', 'value'),
    Input('upload-data', 'contents'),
    State('upload-data', 'filename')
)
def update_spectra_test_table(test_choice, contents, filename):

    if test_choice == 'Warm LED':
        df = warm_led_spec
    elif test_choice == 'Cool LED':
        df = cool_led_spec
    elif test_choice == 'HMI':
        df = hmi_spec
    elif test_choice == 'Xenon':
        df = xenon_spec
    elif test_choice == 'Custom':
        if contents is not None:
            df = parse_contents(contents, filename)
            custom_spec = df
            df = interpolate_and_normalize(df)
            df = df.round(5) 
            return df.to_dict('records'), [{"name": i, "id": i} for i in df.columns], custom_spec.to_dict('records')
        else:
            df = warm_led_spec
    elif test_choice in fluorescent_specs:
        df = fluorescent_specs[test_choice]
    else:
        return [], [], {}
    
    df = df.round(5) 
    return df.to_dict('records'), [{"name": i, "id": i} for i in df.columns], {}

@app.callback(
    [Output('customCctInput', 'style'),
     Output('submit-cct', 'style'),
     Output('warning-message', 'children')],
    [Input('refChoice', 'value'),
     Input('refCct', 'value')]
)
def update_cct_input_visibility(ref_choice, ref_cct):
    if ref_choice == 'Custom_Blackbody' or ref_choice == 'Custom_Daylight':
        warning_message = ""
        if ref_choice == 'Custom_Daylight' and (ref_cct is not None and ref_cct < 4000):
            warning_message = "CCT must be >= 4000 for Daylight Spectra"
        return {'margin-top': '20px'}, {'margin-top': '10px', 'display': 'inline-block'}, warning_message
    return {'display': 'none'}, {'display': 'none'}, ""

@app.callback(
    [Output('plotRef', 'figure'),
     Output('spectraRef', 'children'),
     Output('ssiText', 'children'),
     Output('defaultspecused', 'children')],
    [Input('spectra-test-table', 'data'),
     Input('spectra-test-table', 'columns'),
     Input('testChoice', 'value'), 
     Input('refChoice', 'value'), 
     Input('stored-cct-value', 'data'),
     Input('submit-cct', 'n_clicks'), 
     Input('stored-custom-spec', 'data')],
    [State('refCct', 'value')]
)
def update_all_outputs(rows, columns, test_choice, ref_choice, stored_cct_value, n_clicks, custom_spec_data, ref_cct_value):
    fig = go.Figure()
    
    # Check for valid data in the DataTable
    if not rows or not isinstance(rows, list) or not all(isinstance(row, dict) for row in rows):
        return fig, html.Div("Invalid data format."), "Spectral Similarity Index: N/A"
    
    test_df = pd.DataFrame(rows, columns=[c['name'] for c in columns])  # Create DataFrame from DataTable rows
    test_df = test_df.round(5)

    testcct = cct_mccamy(test_df).round(2)

    # Plot test spectrum
    if test_choice == 'Custom' and custom_spec_data:
        fig.add_trace(go.Scatter(x=test_df['wavelength'], y=test_df['intensity'], mode='lines', name=f'Test Spectrum [CCT: {testcct}]'))
    else:
        fig.add_trace(go.Scatter(x=test_df['wavelength'], y=test_df['intensity'], mode='lines', name=f'Test Spectrum [CCT: {testcct}]'))

    # Update reference spectrum based on the CCT value
    if ref_choice == 'D50':
        df = D50_spec
    elif ref_choice == 'D55':
        df = D55_spec
    elif ref_choice == 'D65':
        df = D65_spec
    elif ref_choice == 'D75':
        df = D75_spec
    elif ref_choice == 'Custom_Blackbody':
        if n_clicks is not None:
            custom_spec_bb = planck(ref_cct_value, wavelengths)
            df = interpolate_and_normalize(custom_spec_bb)
        else:
            return fig, html.Div("Please submit a CCT value."), "Spectral Similarity Index: N/A"
    elif ref_choice == 'A':
        custom_spec_bb = planck(2855.542, wavelengths)
        df = interpolate_and_normalize(custom_spec_bb)
    elif ref_choice == 'Custom_Daylight':
        if n_clicks is not None:
            custom_spec_daylight = daylight(ref_cct_value, wavelengths)
            df = interpolate_and_normalize(custom_spec_daylight)
        else:
            return fig, html.Div("Please submit a CCT value."), "Spectral Similarity Index: N/A"
    elif ref_choice == 'Default':
        if cct_mccamy(test_df) < 4000:
            custom_spec_bb = planck(cct_mccamy(test_df), wavelengths)
            df = interpolate_and_normalize(custom_spec_bb)
        else: 
            custom_spec_daylight = daylight(cct_mccamy(test_df), wavelengths)
            df = interpolate_and_normalize(custom_spec_daylight)


    df = df.round(5)

    refcct = cct_mccamy(df).round(2) 

    fig.add_trace(go.Scatter(x=df['wavelength'], y=df['intensity'], mode='lines', name=f'Reference Spectrum [CCT: {refcct}]'))

    table = dash_table.DataTable(
            data=df.to_dict('records'),
            columns=[{"name": i, "id": i} for i in df.columns],
            editable=False,  # Allows users to edit the data in the table
            page_size=1000,  # Number of rows per page
            export_format="csv",
            export_headers="display",
            merge_duplicate_headers=True
        )

    # SSI Calculation
    ref_data = df
    test_data = test_df  # Use the updated test_df from the DataTable

    if test_data is None or ref_data is None:
        ssi_value_text = "Spectral Similarity Index: N/A"
    else:
        test_intensity = test_data['intensity']
        ref_intensity = ref_data['intensity']
        test_wavelengths = test_data['wavelength']
        ref_wavelengths = ref_data['wavelength']

        ssi_value = calculate_ssi(test_wavelengths, test_intensity, ref_wavelengths, ref_intensity)
        ssi_value_text = f"Spectral Similarity Index: {int(ssi_value)}"
    # testcct = cct_mccamy(test_data).round(2)
    # refcct = cct_mccamy(ref_data).round(2)           
    if testcct < 4000:
        default_spec_text = f"Blackbody with CCT = {refcct}"
    else:
        default_spec_text = f"CIE Daylight with CCT = {refcct}"

    return fig, table, ssi_value_text, default_spec_text

# Run the app
if __name__ == '__main__':
    app.run_server(debug=True)


