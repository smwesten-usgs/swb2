import netCDF4
import os
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap

mydir = '/Users/smwesten/SMWData/Active_Projects/Pacific_Islands_SWB/Maui_test_case_swb/'

os.chdir(mydir)

f = netCDF4.Dataset('fog_2000_2001__490_by_745.nc')

lons = f.variables['lon'][:][:]
lats = f.variables['lat'][:][:]
fog  = f.variables['fog'][0][:][:]
time = f.variables['time'][:]
fog_units = f.variables['fog'].units
time_units = f.variables['time'].units

# Get some parameters for the Stereographic Projection
lon_0 = lons.mean()
lat_0 = lats.mean()


