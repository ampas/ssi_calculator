import numpy as np
import pandas as pd
from color_matching_func import get_color_matching_functions


def cct_mccamy(spec):
    if spec is None or spec.empty:
        raise ValueError('missing required input argument spec')

    
    cmf = get_color_matching_functions()
    merged = pd.merge(spec, cmf, on='wavelength')
    
    X = np.sum(merged['intensity'] * merged['x_bar'])
    Y = np.sum(merged['intensity'] * merged['y_bar'])
    Z = np.sum(merged['intensity'] * merged['z_bar'])
    
    x = X / (X + Y + Z)
    y = Y / (X + Y + Z)
    
    xe = 0.3320
    ye = 0.1858
    n = (x - xe) / (y - ye)
    cct = -449 * (n ** 3) + 3525 * (n ** 2) - 6823.3 * n + 5520.33
    
    return cct