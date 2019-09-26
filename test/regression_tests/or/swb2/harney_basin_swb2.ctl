# This covers a grid centered on the Harney Basin
# in SE Oregon. Atmospheric data for this
# area is derived from GRIDMET netcdf4 files
#
#--------------------------------------------------------------------------
# MODEL DOMAIN DEFINITION
#
# Definition of the model domain.  Units of meters are assumed.
# All subsequent input grids must match the specified model domain exactly.
#
#              Lower LH Corner     Upper Grid
#            |_________________________| Cell
#     NX  NY  X0            Y0           Size
GRID 251 299  -1971403.1441   2336322.5259   1000.0
BASE_PROJECTION_DEFINITION +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs

%% Define methods
-----------------

INTERCEPTION_METHOD              BUCKET
EVAPOTRANSPIRATION_METHOD        HARGREAVES
RUNOFF_METHOD                    CURVE_NUMBER
SOIL_MOISTURE_METHOD             THORNTHWAITE
PRECIPITATION_METHOD             GRIDDED
GROWING_DEGREE_DAY_METHOD        BASKERVILLE_EMIN
FOG_METHOD                       NONE
FLOW_ROUTING_METHOD              NONE
IRRIGATION_METHOD                NONE
ROOTING_DEPTH_METHOD             STATIC
CROP_COEFFICIENT_METHOD          NONE
DIRECT_RECHARGE_METHOD           NONE
SOIL_STORAGE_MAX_METHOD          CALCULATED
AVAILABLE_WATER_CONTENT_METHOD   GRIDDED

%% define location, projection, and conversions for GRIDMET weather data
------------------------------------------------------------------------

PRECIPITATION NETCDF ../../../test_data/or/pr_%y__subset.nc
PRECIPITATION_GRID_PROJECTION_DEFINITION +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs
PRECIPITATION_NETCDF_Z_VAR          precipitation_amount
PRECIPITATION_NETCDF_X_VAR                           lon
PRECIPITATION_NETCDF_Y_VAR                           lat
PRECIPITATION_NETCDF_TIME_VAR                        day
PRECIPITATION_SCALE_FACTOR                    0.03937008
PRECIPITATION_MISSING_VALUES_CODE                 1000.0
PRECIPITATION_MISSING_VALUES_OPERATOR                 >=
PRECIPITATION_MISSING_VALUES_ACTION                 zero

TMAX NETCDF ../../../test_data/or/tmmx_%y__subset.nc
TMAX_GRID_PROJECTION_DEFINITION +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs
TMAX_NETCDF_Z_VAR               air_temperature
TMAX_NETCDF_X_VAR                           lon
TMAX_NETCDF_Y_VAR                           lat
TMAX_NETCDF_TIME_VAR                        day
TMAX_SCALE_FACTOR                 1.8
TMAX_ADD_OFFSET               -459.67
TMAX_MISSING_VALUES_CODE      -9999.0
TMAX_MISSING_VALUES_OPERATOR      <=
TMAX_MISSING_VALUES_ACTION       mean

TMIN NETCDF ../../../test_data/or/tmmn_%y__subset.nc
TMIN_GRID_PROJECTION_DEFINITION +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs
TMIN_NETCDF_Z_VAR               air_temperature
TMIN_NETCDF_X_VAR                           lon
TMIN_NETCDF_Y_VAR                           lat
TMIN_NETCDF_TIME_VAR                        day
TMIN_SCALE_FACTOR                 1.8
TMIN_ADD_OFFSET               -459.67
TMIN_MISSING_VALUES_CODE      -9999.0
TMIN_MISSING_VALUES_OPERATOR      <=
TMIN_MISSING_VALUES_ACTION       mean

INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX CONSTANT 100.0
UPPER_LIMIT_CFGI 83.
LOWER_LIMIT_CFGI 55.

%% specify location and projection for input GIS grids
------------------------------------------------------

HYDROLOGIC_SOILS_GROUP ARC_GRID ../../../test_data/or/hydrologic_soils_group.asc
HYDROLOGIC_SOILS_GROUP_PROJECTION_DEFINITION +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs

LAND_USE ARC_GRID ../../../test_data/or/landuse.asc
LANDUSE_PROJECTION_DEFINITION +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs

AVAILABLE_WATER_CONTENT ARC_GRID ../../../test_data/or/available_water_capacity.asc
AVAILABLE_WATER_CONTENT_PROJECTION_DEFINITION +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs

%% specify location and names for all lookup tables
---------------------------------------------------

LAND_USE_LOOKUP_TABLE ../../../test_data/tables/Landuse_lookup__Oregon_veg__bucket_interception.txt

%% initial conditions for soil moisture and snow storage amounts
%% may be specified as grids, but using a constant amount and
%% allowing the model to "spin up" for a year is also acceptable.

INITIAL_PERCENT_SOIL_MOISTURE CONSTANT 100.0
INITIAL_SNOW_COVER_STORAGE CONSTANT 2.0

# POTATOES
#DUMP_VARIABLES COORDINATES 561167. 445224.

%% start and end date may be any valid dates in SWB version 2.0
%% remember to allow for adequate model spin up; running the
%% model for just a month or two will give questionable results

START_DATE 01/01/2000
END_DATE 12/31/2000
