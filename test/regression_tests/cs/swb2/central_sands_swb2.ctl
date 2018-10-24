%% Central Sands, Wisconsin
%% Example of net infiltration calculation with crop water demand
%% and irrigation included in water budget

! Grid definition: projected coordinates are Wisconsin Transverse Mercator (83/91), meters
!      nx    ny     xll          yll     resolution
GRID  400   346  545300       432200       90.0
BASE_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

%% Define methods
-----------------

INTERCEPTION_METHOD              BUCKET
EVAPOTRANSPIRATION_METHOD        HARGREAVES
RUNOFF_METHOD                    CURVE_NUMBER
SOIL_MOISTURE_METHOD             FAO-56_TWO_STAGE
PRECIPITATION_METHOD             GRIDDED
GROWING_DEGREE_DAY_METHOD        BASKERVILLE_EMIN
FOG_METHOD                       NONE
FLOW_ROUTING_METHOD              NONE
IRRIGATION_METHOD                FAO-56
ROOTING_DEPTH_METHOD             DYNAMIC
CROP_COEFFICIENT_METHOD          FAO-56
DIRECT_RECHARGE_METHOD           NONE
SOIL_STORAGE_MAX_METHOD          CALCULATED
AVAILABLE_WATER_CONTENT_METHOD   GRIDDED

GROWING_SEASON 133 268 TRUE


%% define location, projection, and conversions for weather data
----------------------------------------------------------------

PRECIPITATION NETCDF ../../../test_data/cs/prcp_Daymet_v3_%y.nc
PRECIPITATION_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
PRECIPITATION_NETCDF_Z_VAR                prcp
PRECIPITATION_SCALE_FACTOR          0.03937008
PRECIPITATION_MISSING_VALUES_CODE      -9999.0
PRECIPITATION_MISSING_VALUES_OPERATOR      <=
PRECIPITATION_MISSING_VALUES_ACTION       zero

TMAX NETCDF ../../../test_data/cs/tmax_Daymet_v3_%y.nc
TMAX_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
TMAX_SCALE_FACTOR                 1.8
TMAX_ADD_OFFSET                  32.0
TMAX_MISSING_VALUES_CODE      -9999.0
TMAX_MISSING_VALUES_OPERATOR      <=
TMAX_MISSING_VALUES_ACTION       mean

TMIN NETCDF ../../../test_data/cs/tmin_Daymet_v3_%y.nc
TMIN_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
TMIN_SCALE_FACTOR                 1.8
TMIN_ADD_OFFSET                  32.0
TMIN_MISSING_VALUES_CODE      -9999.0
TMIN_MISSING_VALUES_OPERATOR      <=
TMIN_MISSING_VALUES_ACTION       mean

INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX CONSTANT 100.0
UPPER_LIMIT_CFGI 83.
LOWER_LIMIT_CFGI 55.

%% specify location and projection for input GIS grids
------------------------------------------------------

FLOW_DIRECTION ARC_GRID ../../../test_data/cs/d8_flow_direction.asc
FLOW_DIRECTION_PROJECTION_DEFINITION  +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

HYDROLOGIC_SOILS_GROUP ARC_GRID ../../../test_data/cs/hydrologic_soils_group.asc
HYDROLOGIC_SOILS_GROUP_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

LAND_USE ARC_GRID ../../../test_data/cs/landuse.asc
LANDUSE_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

AVAILABLE_WATER_CONTENT ARC_GRID ../../../test_data/cs/available_water_capacity.asc
AVAILABLE_WATER_CONTENT_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

IRRIGATION_MASK ARC_GRID ../../../test_data/cs/irrigation_mask_from_cdl.asc
IRRIGATION_MASK_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m


%% specify location and names for all lookup tables
---------------------------------------------------

LAND_USE_LOOKUP_TABLE std_input/Landuse_lookup_CDL.txt
IRRIGATION_LOOKUP_TABLE std_input/Irrigation_lookup_CDL.txt

%% initial conditions for soil moisture and snow storage amounts
%% may be specified as grids, but using a constant amount and
%% allowing the model to "spin up" for a year is also acceptable.

INITIAL_PERCENT_SOIL_MOISTURE CONSTANT 100.0
INITIAL_SNOW_COVER_STORAGE CONSTANT 2.0

# POTATOES
DUMP_VARIABLES COORDINATES 561167. 445224.

# RYE
DUMP_VARIABLES COORDINATES 546094., 438492.

# EVERGREEN FOREST
DUMP_VARIABLES COORDINATES 556791. 457569.

# WINTER WHEAT
DUMP_VARIABLES COORDINATES 568129. 458340.

# DEC FOREST
DUMP_VARIABLES COORDINATES 553927. 459454.

# SWEET CORN
DUMP_VARIABLES COORDINATES 555602. 434644.

# SOYBEANS
DUMP_VARIABLES COORDINATES 558663. 432949.

# SUGAR BEETS
DUMP_VARIABLES COORDINATES 570741. 445112.


OUTPUT ENABLE bare_soil_evaporation crop_et snowmelt soil_storage delta_soil_storage
OUTPUT ENABLE growing_season growing_degree_day

%% start and end date may be any valid dates in SWB version 2.0
%% remember to allow for adequate model spin up; running the
%% model for just a month or two will give questionable results

START_DATE 01/01/2012
END_DATE 12/31/2013
