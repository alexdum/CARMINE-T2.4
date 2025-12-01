import xarray as xr
import numpy as np
from glob import glob
import os
import xclim
from xclim.indices.stats import frequency_analysis
from xclim.core.calendar import percentile_doy
from xclim import units
from xclim.indices.stats import frequency_analysis

def extreme_wind_speed(sfcWind, baseline = (1981, 2010), percentile_threshold=90, freq='YS'):
    """
    Compute Extreme Wind Speed (EWS) as percentage of days exceeding 90th percentile.
    """
    
    sfcWind.attrs['units'] = 'm/s'
    
    # Calculate the 90th percentile from baseline period
    dspercentile = percentile_doy(sfcWind.sel(time=slice(str(baseline[0]), str(baseline[1]-1))), 
                                   window=5, per=percentile_threshold).sel(percentiles=percentile_threshold)
    
    # Create binary indicator for extreme wind days
    ews_indicator = xr.where(sfcWind > dspercentile, 1, 0)
    
    # Calculate percentage of extreme days per year
    ews_percentage = ews_indicator.resample(time=freq).sum(dim='time') / sfcWind.resample(time=freq).count(dim='time') * 100
    ews_percentage.attrs['units'] = '%'
    
    return ews_percentage
