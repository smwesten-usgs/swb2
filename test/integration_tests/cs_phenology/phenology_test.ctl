%% Phenology Integration Test — Central Sands, Wisconsin
%% Purpose: Verify that the new phenology module (DOY_BASED and GDD_THRESHOLD)
%%          correctly transitions between growing and dormant seasons.
%%
%% This test uses Thornthwaite-Mather soil moisture (no FAO-56) to exercise
%% the phenology module in isolation from the crop coefficient machinery.
%%
%% Three DUMP_VARIABLES points target:
%%   1. Deciduous Forest (LU 141) — DOY_BASED: start=04/15, end=10/15
%%   2. Corn (LU 1)               — GDD_THRESHOLD: GDD=200, frost=28°F
%%   3. Winter Wheat (LU 24)      — DOY_BASED winter crop: start=10/01, end=05/30

GRID  400   346  545300       432200       90.0
BASE_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

%% Methods — simple T-M configuration, no FAO-56 involvement
INTERCEPTION_METHOD              BUCKET
EVAPOTRANSPIRATION_METHOD        HARGREAVES
RUNOFF_METHOD                    CURVE_NUMBER
SOIL_MOISTURE_METHOD             THORNTHWAITE-MATHER
PRECIPITATION_METHOD             GRIDDED
GROWING_DEGREE_DAY_METHOD        BASKERVILLE_EMIN
FOG_METHOD                       NONE
FLOW_ROUTING_METHOD              NONE
IRRIGATION_METHOD                NONE
CROP_COEFFICIENT_METHOD          NONE
DIRECT_RECHARGE_METHOD           NONE
SOIL_STORAGE_MAX_METHOD          CALCULATED
AVAILABLE_WATER_CONTENT_METHOD   GRIDDED
ROOTING_DEPTH_METHOD             STATIC

%% Climate data — use existing Central Sands Daymet NetCDFs (2012 only)
PRECIPITATION NETCDF ../../test_data/cs/prcp_Daymet_v3_2012.nc
PRECIPITATION_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
PRECIPITATION_NETCDF_Z_VAR                prcp
PRECIPITATION_SCALE_FACTOR          0.03937008
PRECIPITATION_MISSING_VALUES_CODE      -9999.0
PRECIPITATION_MISSING_VALUES_OPERATOR      <=
PRECIPITATION_MISSING_VALUES_ACTION       zero

TMAX NETCDF ../../test_data/cs/tmax_Daymet_v3_2012.nc
TMAX_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
TMAX_SCALE_FACTOR                 1.8
TMAX_ADD_OFFSET                  32.0
TMAX_MISSING_VALUES_CODE      -9999.0
TMAX_MISSING_VALUES_OPERATOR      <=
TMAX_MISSING_VALUES_ACTION       mean

TMIN NETCDF ../../test_data/cs/tmin_Daymet_v3_2012.nc
TMIN_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
TMIN_SCALE_FACTOR                 1.8
TMIN_ADD_OFFSET                  32.0
TMIN_MISSING_VALUES_CODE      -9999.0
TMIN_MISSING_VALUES_OPERATOR      <=
TMIN_MISSING_VALUES_ACTION       mean

%% Static grids — use existing Central Sands data
FLOW_DIRECTION ARC_GRID ../../test_data/cs/d8_flow_direction.asc
FLOW_DIRECTION_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

HYDROLOGIC_SOILS_GROUP ARC_GRID ../../test_data/cs/hydrologic_soils_group.asc
HYDROLOGIC_SOILS_GROUP_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

LAND_USE ARC_GRID ../../test_data/cs/landuse.asc
LANDUSE_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

AVAILABLE_WATER_CONTENT ARC_GRID ../../test_data/cs/available_water_capacity.asc
AVAILABLE_WATER_CONTENT_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

%% Frozen ground
INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX CONSTANT 100.0
CFGI_UPPER_LIMIT CONSTANT 83.
CFGI_LOWER_LIMIT CONSTANT 55.

%% Initial conditions
INITIAL_PERCENT_SOIL_MOISTURE CONSTANT 100.0
INITIAL_SNOW_COVER_STORAGE CONSTANT 2.0

%% Lookup tables — CDL table for CN/interception, phenology table for growing season
LAND_USE_LOOKUP_TABLE ../../test_data/tables/Landuse_lookup_CDL.txt
PHENOLOGY_LOOKUP_TABLE phenology_lookup.tsv

%% DUMP_VARIABLES points — three land uses exercising different phenology methods
%%
%% Point 1: DECIDUOUS FOREST (LU 141) — DOY_BASED, start=04/15 (DOY 105), end=10/15 (DOY 288)
DUMP_VARIABLES COORDINATES 553927. 459454.

%% Point 2: CORN (LU 1) — GDD_THRESHOLD, GDD_start=200.0, killing_frost=28.0°F
DUMP_VARIABLES COORDINATES 557946. 434408.

%% Point 3: WINTER WHEAT (LU 24) — DOY_BASED winter crop, start=10/01 (DOY 274), end=05/30 (DOY 150)
DUMP_VARIABLES COORDINATES 568129. 458340.

%% Output — enable growing season and GDD for verification
OUTPUT ENABLE growing_season growing_degree_day

%% Run for one year (2012)
START_DATE 01/01/2012
END_DATE 12/30/2012
