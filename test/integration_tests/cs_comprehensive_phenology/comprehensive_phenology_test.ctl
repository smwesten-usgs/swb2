%% Comprehensive Phenology Integration Test — Central Sands, Wisconsin
%% Purpose: Exercise ALL 5 phenology methods in a single run:
%%   1. PHENOLOGY_NONE      — LU 131 (Barren): always dormant, no vegetation
%%   2. DOY_BASED           — LU 141 (Deciduous Forest): start=04/15, end=10/15
%%   3. GDD_THRESHOLD       — LU   1 (Corn): GDD>=200, killing frost<=28F
%%   4. FAO56_DATES         — LU   5 (Soybeans): start=05/20, L=20/30/60/25 days
%%   5. FAO56_GDD           — LU  12 (Sweet Corn): start_GDD=100, GDD=350/1000/1600/2000, frost<=28F
%%
%% CROP_COEFFICIENT_METHOD is NONE — phenology produces growth_stage and growth_fraction
%% independently of crop coefficients. Interception values are non-trivial for all land
%% uses so storage_max transitions can be verified (binary for DOY/GDD, ramped for FAO56).
%% A separate test with FAO56 crop coefficients can verify Kcb curves for FAO56 methods.

GRID  400   346  545300       432200       90.0
BASE_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

%% Methods
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

%% Climate data — Central Sands Daymet NetCDFs (2012 only)
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

%% Static grids — Central Sands data
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

%% Lookup table — single unified table, no multi-table loading
LAND_USE_LOOKUP_TABLE Landuse_lookup_comprehensive.tsv

%% DUMP_VARIABLES — one point per phenology method
%%
%% Point 1: BARREN (LU 131) — PHENOLOGY_NONE: always dormant
DUMP_VARIABLES COORDINATES 567261. 447588.
%%
%% Point 2: DECIDUOUS FOREST (LU 141) — DOY_BASED: growing DOY 105–288
DUMP_VARIABLES COORDINATES 553927. 459454.
%%
%% Point 3: CORN (LU 1) — GDD_THRESHOLD: GDD>=200, frost<=28F
DUMP_VARIABLES COORDINATES 557946. 434408.
%%
%% Point 4: SOYBEANS (LU 5) — FAO56_DATES: start=05/20, L=20/30/60/25
DUMP_VARIABLES COORDINATES 560932. 443024.
%%
%% Point 5: SWEET CORN (LU 12) — FAO56_GDD: start_GDD=100, GDD=350/1000/1600/2000
DUMP_VARIABLES COORDINATES 561232. 442007.

%% Output — enable growing season and GDD for verification
OUTPUT ENABLE growing_season growing_degree_day

%% Run for one year (2012)
START_DATE 01/01/2012
END_DATE 12/30/2012
