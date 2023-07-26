%% Copied from central sands version & Badger.

! Grid definition: projected coordinates are EPSG 3338, meters
!      nx    ny     xll          yll     resolution
# GRID  1350   1200    139685       1164943       45.72
# GRID  135   120    139685       1164943       450.72
GRID   246   219    139685       1164943        250
BASE_PROJECTION_DEFINITION +proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs

%% Define methods
-----------------

INTERCEPTION_METHOD              BUCKET
EVAPOTRANSPIRATION_METHOD        HARGREAVES
RUNOFF_METHOD                    CURVE_NUMBER
SOIL_MOISTURE_METHOD             FAO-56_TWO_STAGE
PRECIPITATION_METHOD             TABULAR
GROWING_DEGREE_DAY_METHOD        BASKERVILLE_EMIN
FOG_METHOD                       NONE
FLOW_ROUTING_METHOD              NONE
IRRIGATION_METHOD                NONE  #NONE turns off all irrigation  #FAO-56 uses that method
ROOTING_DEPTH_METHOD             DYNAMIC
CROP_COEFFICIENT_METHOD          FAO-56  #Used to calculate actual ET with soil moisture
DIRECT_RECHARGE_METHOD           NONE
SOIL_STORAGE_MAX_METHOD          CALCULATED
AVAILABLE_WATER_CONTENT_METHOD   GRIDDED

GROWING_SEASON 133 268 TRUE


%% define location, projection, and conversions for weather data
----------------------------------------------------------------


INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX CONSTANT 100.0
UPPER_LIMIT_CFGI 83.
LOWER_LIMIT_CFGI 55.

%% specify location and projection for input GIS grids
------------------------------------------------------

HYDROLOGIC_SOILS_GROUP ARC_GRID hsg_resample_clip.asc
HYDROLOGIC_SOILS_GROUP_PROJECTION_DEFINITION +proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs


LAND_USE ARC_GRID nlcd_resample_clip.asc
LANDUSE_PROJECTION_DEFINITION +proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs

AVAILABLE_WATER_CONTENT ARC_GRID awc_resample_clip.asc
AVAILABLE_WATER_CONTENT_PROJECTION_DEFINITION +proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs


%%IRRIGATION_MASK ARC_GRID input/irrigation_mask_from_cdl.asc
%%IRRIGATION_MASK_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

%% specify location and names for all lookup tables
---------------------------------------------------

LAND_USE_LOOKUP_TABLE LU_Lookup_AK.txt
IRRIGATION_LOOKUP_TABLE IRR_Lookup_AK.txt
WEATHER_DATA_LOOKUP_TABLE 1944-2022_3037835_kenai_airport.txt

%% initial conditions for soil moisture and snow storage amounts
%% may be specified as grids, but using a constant amount and
%% allowing the model to "spin up" for a year is also acceptable.

INITIAL_PERCENT_SOIL_MOISTURE CONSTANT 100.0
INITIAL_SNOW_COVER_STORAGE CONSTANT 2.0


%% OUTPUT CONTROL SECTION:
OUTPUT DISABLE snow_storage
OUTPUT ENABLE tmin tmax
OUTPUT DISABLE crop_et soil_storage delta_soil_storage reference_ET0 
OUTPUT DISABLE runon interception soil_storage delta_soil_storage

OUTPUT ENABLE gross_precipitation 
OUTPUT ENABLE runoff_outside rejected_net_infiltration 
OUTPUT ENABLE runoff actual_et rainfall snowmelt 

DUMP_VARIABLES COORDINATES 150393 1179331 

%% start and end date may be any valid dates in SWB version 2.0
%% remember to allow for adequate model spin up; running the
%% model for just a month or two will give questionable results

START_DATE 01/01/1967
END_DATE 07/22/2022
