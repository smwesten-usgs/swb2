#!/Users/smwesten/anaconda/bin/python
import xarray as xr
import pandas as pd
import numpy as np
import os
import sys

# 'ncdump -h pr_2000.nc' reveals the following about the dataset:
#<xarray.Dataset>
#Dimensions:               (crs: 1, day: 366, lat: 585, lon: 1386)
#Coordinates:
#  * lon                   (lon) float64 -124.8 -124.7 -124.7 ... -67.1 -67.06
#  * lat                   (lat) float64 49.4 49.36 49.32 ... 25.15 25.11 25.07
#  * day                   (day) datetime64[ns] 2000-01-01 ... 2000-12-31
#  * crs                   (crs) uint16 3

# define the approximate bounds of the area of interest
#
# note that 'min' and 'max' identify the x and y associated with the python
# min and max index values, and are not necessarily the same as the
# numerical min and max
#
ymax = 45.5
ymin = 41.5
xmin = -121.5
xmax = -117.0

ul = str(xmin) + ' ' + str(ymax)
ll = str(xmin) + ' ' + str(ymin)
lr = str(xmax) + ' ' + str(ymin)
ur = str(xmax) + ' ' + str(ymax)

bb = 'POLYGON((' + ul + ', ' + ll + ', ' + lr + ', ' + ur + ', ' + ul + '))'

output_path = '.'

gridmet_files = ['pr_2000.nc', 'tmmn_2000.nc', 'tmmx_2000.nc']

for f in gridmet_files:
        ds = xr.open_dataset(f)

        y_bnds, x_bnds = [ymax, ymin], [xmin, xmax]

        subset = ds.sel(lat=slice(*y_bnds), lon=slice(*x_bnds))

        # Gridmet has a 'CRS' dimension that is never used and really should be
        # an attribute
        subset = subset.drop_dims('crs')

        # Overwrite the attribute defining the spatial extents to reflect the
        # subsetted area
        subset.attrs['geospatial_bounds']=bb
        subset.attrs['geospatial_lat_min']=str(ymin)
        subset.attrs['geospatial_lat_max']=str(ymax)
        subset.attrs['geospatial_lon_min']=str(xmin)
        subset.attrs['geospatial_lon_max']=str(xmax)

        basename = f.split(".")[0]
        outname = basename + '__subset.nc'
        outfile = os.path.join( output_path, outname )

        print('  creating output file: ' + outfile )
        subset.to_netcdf(outfile, mode='w', format='NETCDF4')
