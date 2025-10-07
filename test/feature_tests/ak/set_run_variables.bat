:: Hail Mary attempt to keep netCDF from erroring out based on this SO entry:
::https://stackoverflow.com/questions/49317927/errno-101-netcdf-hdf-error-when-opening-netcdf-file
::
set HDF5_USE_FILE_LOCKING=FALSE

set RES=45_72m
set DATA_DIR=../../test_data/ak/
set LOOKUP_DIR=../../test_data/tables/
set WEATHER_DIR=../../test_data/tables/
set SWB_CONTROL_FILE=beaver_ak_swb_run.ctl
set OUTPUT_FILE_PREFIX=beaver_crk_%RES%_

set SWB2=swb2.exe
set SWBSTATS2=swbstats2.exe
