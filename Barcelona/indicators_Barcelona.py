import xarray as xr
import numpy as np
from glob import glob
import os
import xclim
from xclim.indices.stats import frequency_analysis
from xclim.core.calendar import percentile_doy
from xclim import units
from xclim.indices.stats import frequency_analysis

def cdd(pr, thresh='1 mm/day', freq='YS'):
   
    pr = xr.where(pr < 1, 0, pr)

    # combined_ds['TOT_PREC'] = combined_ds['TOT_PREC'].assign_attrs(units='mm/day')
    pr.attrs['units'] = 'mm/day'
 
    indicator = xclim.indices.maximum_consecutive_dry_days(pr, thresh=thresh, freq=freq,
                                                                resample_before_rl=True)
    return indicator
    
def drought_spi(tp_daily, baseline = (1981, 2011), scale=3):
        from climate_indices import indices as cindx
        """ Using Standard Precipitation index """

        ## total monthly precipitation
        #tp_monthly = tp_daily.resample(time='MS').sum(dim='time')
        tp_monthly = tp_daily.resample(time='MS').sum(dim = 'time')  # Riga corretta
        
        ## Wrap daily precipitation function
        DISTRIBUTION = cindx.Distribution['gamma']
        DATA_START_YEAR = int(tp_daily.isel(time=0)['time.year'])
        CALIBRATION_YEAR_INITIAL = baseline[0]
        CALIBRATION_YEAR_FINAL = baseline[1] - 1
        PERIODICITY = cindx.compute.Periodicity['monthly']
        compute_spi =   lambda x : cindx.spi(x,
                                            scale,
                                            DISTRIBUTION,
                                            DATA_START_YEAR,
                                            CALIBRATION_YEAR_INITIAL,
                                            CALIBRATION_YEAR_FINAL,
                                            PERIODICITY)

        ## compute SPI/gamma at 3-month scale
        tp_monthly = tp_monthly.chunk({'time' : -1}) # re-chunk along time axis
        time_dim = tp_monthly.sizes['time']

        stand_prec_index = xr.apply_ufunc(compute_spi,
                                tp_monthly,
                                input_core_dims=[['time']],
                                exclude_dims=set(('time',)),
                                output_core_dims=[['time']],
                                output_sizes={'time': time_dim},
                                dask = 'parallelized',
                                output_dtypes  = [float],
                                vectorize = True
                                )

        ## setting up the original time axis
        old_time_ax = tp_monthly['time'].values
        #stand_prec_index = stand_prec_index.assign_coords(time=old_time_ax)
        #print(list(stand_prec_index.variables.keys())[0] )
        #prec_label = list(stand_prec_index.variables.keys())[0]
        #print(stand_prec_index)
        #stand_prec_index = stand_prec_index.rename({ prec_label : 'drought'})
        #stand_prec_index['drought'].attrs['long_name'] = 'drought index '
        #stand_prec_index['time'].attrs['long_name'] = 'time'
        stand_prec_index = stand_prec_index.assign_coords(time=old_time_ax)
        stand_prec_index.name = 'drought'
        stand_prec_index.attrs['long_name'] = 'drought index'



        return  stand_prec_index
    
def spi3(pr, scale = 3):
    baseline = (1981, 2011)
    spi = drought_spi(pr, scale=scale, baseline = baseline)
    return spi
    
def spi12(pr, scale = 12):
    baseline = (1981, 2011)
    spi = drought_spi(pr, scale=scale, baseline = baseline)
    return spi


def drought_spei(daily_water_budget, baseline = (1981, 2011), scale=3):
        from climate_indices import indices as cindx
        """ Using Standard Precipitation index """

        ## Wrap daily precipitation function
        DISTRIBUTION = cindx.Distribution['pearson']
        DATA_START_YEAR = int(daily_water_budget.isel(time=0)['time.year'])
        CALIBRATION_YEAR_INITIAL = baseline[0]
        CALIBRATION_YEAR_FINAL = baseline[1] - 1
        PERIODICITY = cindx.compute.Periodicity['monthly']
        compute_spei =   lambda x : cindx.spi(x,
                                            scale,
                                            DISTRIBUTION,
                                            DATA_START_YEAR,
                                            CALIBRATION_YEAR_INITIAL,
                                            CALIBRATION_YEAR_FINAL,
                                            PERIODICITY)
        print(compute_spei)

        ## compute SPEI/pearson at scale-month scale
        tp_monthly = daily_water_budget.chunk({'time' : -1}) # re-chunk along time axis
        time_dim = tp_monthly.sizes['time']

        stand_prec_index = xr.apply_ufunc(compute_spei,
                                tp_monthly,
                                input_core_dims=[['time']],
                                exclude_dims=set(('time',)),
                                output_core_dims=[['time']],
                                output_sizes={'time': time_dim},
                                dask = 'parallelized',
                                output_dtypes  = [float],
                                vectorize = True
                                )

        ## setting up the original time axis
        old_time_ax = tp_monthly['time'].values
        stand_prec_index = stand_prec_index.assign_coords(time=old_time_ax)
        #print(list(stand_prec_index.variables.keys())[0] )
        prec_label = list(stand_prec_index.variables.keys())[0]
        #print(stand_prec_index)
        stand_prec_index = stand_prec_index.rename({ prec_label : 'drought'})
        stand_prec_index['drought'].attrs['long_name'] = 'drought index '
        stand_prec_index['time'].attrs['long_name'] = 'time'

        return  stand_prec_index
def spei3(pr, tas, scale = 3):
    tas.attrs['units'] = 'degK'
    pet = xclim.indices.potential_evapotranspiration(method='TW48', tas = tas)
    pr_mon = pr.resample(time='MS').sum(dim = 'time') 
    dwb = pr - pet
    dwb = dwb.to_dataset(name = 'DWsB')
    baseline = (1981, 2011)
    
    spei = drought_spei(dwb, scale=scale, baseline = baseline)
    return spei

def txx(tasmax, freq = 'seas', season = ''):
    tasmax.attrs['units'] = 'degC'
    if freq == 'seas':
        # txx = xclim.indices.tx_max(tasmax, freq='QS-DEC').groupby('time.season').sel(season = season)
        # Get seasonal maxima
        txx = xclim.indices.tx_max(tasmax, freq='QS-DEC')        
        # Then group by season and reduce, e.g. take the max across each season
        #txx = txx.groupby('time.season').max()
        txx = txx.sel(time=txx['time.season'] == season)


        # Now you can select one season
        #txx = txx.sel(season=season)
    else:
        txx = xclim.indices.tx_max(tasmax, freq='YE')
    txx.attrs['units'] = 'degC'

    return txx


def hot_days_30(tasmax, freq = 'seas', season = ''):
    tasmax.attrs['units'] = 'degC'
    
    if freq == 'seas':
        hd30 = xclim.indices.tx_days_above(tasmax, thresh='30 degC', freq='QS-DEC')
        if season:
            hd30 = hd30.sel(time=hd30['time.season'] == season)
    else:  # annual
        hd30 = xclim.indices.tx_days_above(tasmax, thresh='30 degC', freq='YS')
    
    hd30.attrs['units'] = 'days'
    return hd30
    

def hot_nights_20(tasmin, freq = 'seas', season = ''):
    tasmin.attrs['units'] = 'degC'
    
    if freq == 'seas':
        hn20 = xclim.indices.tn_days_above(tasmin, thresh='20 degC', freq='QS-DEC')
        if season:
            hn20 = hn20.sel(time=hn20['time.season'] == season)
    else:  # annual
        hn20 = xclim.indices.tn_days_above(tasmin, thresh='20 degC', freq='YS')
    
    hn20.attrs['units'] = 'days'
    return hn20
    


def hot_nights_25(tasmin, freq = 'seas', season = ''):
    tasmin.attrs['units'] = 'degC'
    
    if freq == 'seas':
        hn25 = xclim.indices.tn_days_above(tasmin, thresh='25 degC', freq='QS-DEC')
        if season:
            hn25 = hn25.sel(time=hn25['time.season'] == season)
    else:  # annual
        hn25 = xclim.indices.tn_days_above(tasmin, thresh='25 degC', freq='YS')
    
    hn25.attrs['units'] = 'days'
    return hn25
    


def hot_nights_30(tasmin, freq = 'seas', season = ''):
    tasmin.attrs['units'] = 'degC'
    
    if freq == 'seas':
        hn30 = xclim.indices.tn_days_above(tasmin, thresh='30 degC', freq='QS-DEC')
        if season:
            hn30 = hn30.sel(time=hn30['time.season'] == season)
    else:  # annual
        hn30 = xclim.indices.tn_days_above(tasmin, thresh='30 degC', freq='YS')
    
    hn30.attrs['units'] = 'days'
    return hn30
    


def hot_days_35(tasmax, freq = 'seas', season = ''):
    tasmax.attrs['units'] = 'degC'
    
    if freq == 'seas':
        hd35 = xclim.indices.tx_days_above(tasmax, thresh='35 degC', freq='QS-DEC')
        if season:
            hd35 = hd35.sel(time=hd30['time.season'] == season)
    else:  # annual
        hd35 = xclim.indices.tx_days_above(tasmax, thresh='35 degC', freq='YS')
    
    hd35.attrs['units'] = 'days'
    return hd35
    
# xclim.indicators.atmos.hot_days(tasmax='tasmax', *, thresh='25 degC', freq='YS', ds=None, **indexer) 

def heatwave_length(tx, tn, freq='YS'):
    tx.attrs['units'] = 'degC'
    tn.attrs['units'] = 'degC'

    indicator = xclim.indices.heat_wave_max_length(tn, tx, freq=freq)
    indicator.attrs['units'] = 'days'
    return indicator

def rx5day(pr, thresh = 5, freq = 'YS'):
    pr = xr.where(pr < 1, 0, pr)
    pr.attrs['units'] = 'mm/day'

    indicatore_tot = xclim.indices.max_n_day_precipitation_amount(pr, window=thresh, freq=freq)
    return indicatore_tot

