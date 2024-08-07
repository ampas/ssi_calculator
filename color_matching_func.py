import numpy as np
import pandas as pd

MIN_WAVELENGTH = 360
MAX_WAVELENGTH = 830

def get_color_matching_functions():
    # CIE 1931 color matching functions for wavelengths from 380 nm to 780 nm

    # Read the text file and extract the values
    file_path = 'ciexyz31.txt'

    wavelengths = []
    x_bar = []
    y_bar = []
    z_bar = []

    with open(file_path, 'r') as file:
        for line in file:
            parts = line.split()
            if len(parts) == 4:
                wavelength, x, y, z = parts
                wavelengths.append(float(wavelength))
                x_bar.append(float(x))
                y_bar.append(float(y))
                z_bar.append(float(z))

    # print("Wavelengths:", wavelengths)
    # print("x_bar:", x_bar)
    # print("y_bar:", y_bar)
    # print("z_bar:", z_bar)


    cie_data = {
        'wavelength': wavelengths,
        'x_bar': x_bar,
        'y_bar': y_bar,
        'z_bar': z_bar
    }

    return pd.DataFrame(cie_data)

get_color_matching_functions()
