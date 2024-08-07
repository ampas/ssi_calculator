import numpy as np
import matplotlib.pyplot as plt

def planck(temp, wl):
    """
    Calculate the spectral radiance of a blackbody at given temperatures and wavelengths using Planck's Law.
    
    Parameters:
    temp (float or np.array): Temperature(s) in Kelvin
    wl (np.array): Wavelengths in nanometers
    
    Returns:
    np.array: Spectral radiance values
    """
    h = 6.626e-34  # Planck's constant (J·s)
    c = 3e8        # Speed of light (m/s)
    k = 1.381e-23  # Boltzmann's constant (J/K)
    
    # Convert wavelength from nanometers to meters
    wl_m = wl * 1e-9
    
    # Planck's law
    spectral_radiance = (2 * h * c**2) / (wl_m**5) / (np.exp((h * c) / (wl_m * k * temp)) - 1)
    
    return {'wavelength': wl, 'intensity': spectral_radiance}