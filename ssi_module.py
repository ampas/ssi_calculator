import numpy as np
from scipy.interpolate import interp1d
from scipy.signal import convolve

def calculate_ssi(test_wavelengths, test_intensities, ref_wavelengths, ref_intensities):
    lambda_range = np.arange(375, 676, 1)
    interpol_test = interp1d(test_wavelengths, test_intensities, kind='linear', bounds_error=False, fill_value=0)
    interpol_ref = interp1d(ref_wavelengths, ref_intensities, kind='linear', bounds_error=False, fill_value=0)
    
    TI = interpol_test(lambda_range)
    RI = interpol_ref(lambda_range)
    
    # Resampling at 10 nm intervals, sum weighted values within ±5 nm
    TR = np.array([0.5 * TI[i - 5] + sum(TI[i - 4:i + 5]) + 0.5 * TI[i + 5] for i in range(5, len(TI) - 5, 10)])
    RR = np.array([0.5 * RI[i - 5] + sum(RI[i - 4:i + 5]) + 0.5 * RI[i + 5] for i in range(5, len(RI) - 5, 10)])
    
    TN = TR / np.sum(TR)
    RN = RR / np.sum(RR)
    
    D = (TN - RN) / (RN + 1/30)
    weights = np.array([4/15, 22/45, 32/45, 8/9, 44/45] + [1]*23 + [11/15, 3/15])
    W = D * weights
    
    # Extend with zeros
    Z = np.concatenate(([0], W, [0]))
    
    # Smooth
    F = convolve(Z, [0.22, 0.56, 0.22], mode='valid')
    
    e = np.sqrt(np.sum(F**2))
    ssi = round(100 - 32 * e)
    return ssi

# Example usage with dummy data
test_wavelengths = np.linspace(380, 780, 401)
test_intensities = np.random.rand(401)
ref_wavelengths = np.linspace(380, 780, 401)
ref_intensities = np.random.rand(401)
ssi_value = calculate_ssi(test_wavelengths, test_intensities, ref_wavelengths, ref_intensities)
print(f"SSI Value: {ssi_value}")
