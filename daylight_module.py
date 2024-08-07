import numpy as np
import scipy.interpolate as interp

def daylight(cct, wl=None):
    # Validate input
    if cct is None:
        raise ValueError('missing required input argument cct')

    if wl is None:
        wl = np.arange(300, 835, 5)

    if not isinstance(cct, (float, int)):
        raise ValueError('cct must be a float or int')

    if not isinstance(wl, (list, np.ndarray)):
        raise ValueError('wl must be a list or numpy array')

    if cct < 4000:
        raise ValueError('D-Illuminants are not defined for CCTs <4000K')

    if cct > 25000:
        raise ValueError('D-Illuminants are not defined for CCTs >25000K')

    wl = np.array(wl).reshape(-1)

    # Define constants
    s_series = np.array([
        [300, 0.04, 0.02, 0],
        [310, 6, 4.5, 2],
        [320, 29.6, 22.4, 4],
        [330, 55.3, 42, 8.5],
        [340, 57.3, 40.6, 7.8],
        [350, 61.8, 41.6, 6.7],
        [360, 61.5, 38, 5.3],
        [370, 68.8, 42.4, 6.1],
        [380, 63.4, 38.5, 3.0],
        [390, 65.8, 35, 1.2],
        [400, 94.8, 43.4, -1.1],
        [410, 104.8, 46.3, -0.5],
        [420, 105.9, 43.9, -0.7],
        [430, 96.8, 37.1, -1.2],
        [440, 113.9, 36.7, -2.6],
        [450, 125.6, 35.9, -2.9],
        [460, 125.5, 32.6, -2.8],
        [470, 121.3, 27.9, -2.6],
        [480, 121.3, 24.3, -2.6],
        [490, 113.5, 20.1, -1.8],
        [500, 113.1, 16.2, -1.5],
        [510, 110.8, 13.2, -1.3],
        [520, 106.5, 8.6, -1.2],
        [530, 108.8, 6.1, -1],
        [540, 105.3, 4.2, -0.5],
        [550, 104.4, 1.9, -0.3],
        [560, 100, 0, 0],
        [570, 96, -1.6, 0.2],
        [580, 95.1, -3.5, 0.5],
        [590, 89.1, -3.5, 2.1],
        [600, 90.5, -5.8, 3.2],
        [610, 90.3, -7.2, 4.1],
        [620, 88.4, -8.6, 4.7],
        [630, 84, -9.5, 5.1],
        [640, 85.1, -10.9, 6.7],
        [650, 81.9, -10.7, 7.3],
        [660, 82.6, -12, 8.6],
        [670, 84.9, -14, 9.8],
        [680, 81.3, -13.6, 10.2],
        [690, 71.9, -12, 8.3],
        [700, 74.3, -13.3, 9.6],
        [710, 76.4, -12.9, 8.5],
        [720, 63.3, -10.6, 7],
        [730, 71.7, -11.6, 7.6],
        [740, 77, -12.2, 8],
        [750, 65.2, -10.2, 6.7],
        [760, 47.7, -7.8, 5.2],
        [770, 68.6, -11.2, 7.4],
        [780, 65, -10.4, 6.8],
        [790, 66, -10.6, 7],
        [800, 61, -9.7, 6.4],
        [810, 53.3, -8.3, 5.5],
        [820, 58.9, -9.3, 6.1],
        [830, 61.9, -9.8, 6.5]
    ])

    # Interpolate S0, S1, S2
    s_interp = interp.interp1d(s_series[:, 0], s_series[:, 1:], axis=0, kind='linear', fill_value="extrapolate")
    s_series_interp = s_interp(wl)

    s0 = s_series_interp[:, 0]
    s1 = s_series_interp[:, 1]
    s2 = s_series_interp[:, 2]

    # Calculate xy from CCT
    xy = cct_2_xy(cct)
    x = xy[0]
    y = xy[1]

    # Calculate m, m1, m2
    m = 0.0241 + 0.2562 * x - 0.7341 * y
    m1 = -1.3515 - 1.7703 * x + 5.9114 * y
    m2 = 0.03000 - 31.4424 * x + 30.0717 * y

    m1 = np.round(m1 / m, 3)
    m2 = np.round(m2 / m, 3)

    spectral_radiance = s0 + m1 * s1 + m2 * s2

    return {'wavelength': wl, 'intensity': spectral_radiance}


class Illum:
    def __init__(self, spectra, wl, description):
        self.spectra = spectra
        self.wl = wl
        self.description = description



def cct_2_xy(cct):
    if cct < 4000 or cct > 25000:
        raise ValueError('CCT must be between 4000K and 25000K')

    if cct <= 7000:
        x = -4.6070 * (10**9) / (cct**3) + 2.9678 * (10**6) / (cct**2) + 0.09911 * (10**3) / cct + 0.244063
    else:
        x = -2.0064 * (10**9) / (cct**3) + 1.9018 * (10**6) / (cct**2) + 0.24748 * (10**3) / cct + 0.237040
    
    y = -3.000 * (x**2) + 2.870 * x - 0.275

    return np.array([x, y])



# # Example usage:
# cct = 6500
# wl = np.arange(300, 835, 5)
# illum = daylight(cct, wl)
# print(illum)

