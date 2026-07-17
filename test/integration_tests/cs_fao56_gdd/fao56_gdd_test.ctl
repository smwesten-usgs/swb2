%% FAO56 GDD-based Crop Coefficient Integration Test — Central Sands, Wisconsin
%% Purpose: Exercise CROP_COEFFICIENT_METHOD FAO-56 with FAO56_GDD phenology
%%          for ALL land uses. Daily weather drives phenology via GDD accumulation,
%%          which in turn drives the Kcb curve (ini→dev→mid→late→dormant).
%%
%% Key verification points:
%%   1. Corn      (LU  1):  Warm-season annual, Tbase=50°F, late start, high Kcb_mid=1.15
%%   2. Potatoes  (LU 43):  Moderate Tbase=35.6°F, earlier start, Kcb_mid=1.05
%%   3. Spring Wheat (LU 23): Cool-season, Tbase=32°F, early start, short season
%%   4. Deciduous Forest (LU 141): Tbase=32°F, leaf-on from ~100 GDD
%%   5. Grass/Pasture (LU 176): Tbase=32°F, early green-up, moderate Kcb_mid=0.85
%%   6. Barren (LU 131): Never enters growing season (Start_GDD=9999), Kcb=0.05

GRID  400   346  545300       432200       90.0
BASE_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

%% Methods — FAO-56 crop coefficients with GDD-based phenology
INTERCEPTION_METHOD              BUCKET
EVAPOTRANSPIRATION_METHOD        HARGREAVES
RUNOFF_METHOD                    CURVE_NUMBER
SOIL_MOISTURE_METHOD             THORNTHWAITE-MATHER
PRECIPITATION_METHOD             GRIDDED
GROWING_DEGREE_DAY_METHOD        SIMPLE
FOG_METHOD                       NONE
FLOW_ROUTING_METHOD              NONE
IRRIGATION_METHOD                NONE
CROP_COEFFICIENT_METHOD          FAO-56
DIRECT_RECHARGE_METHOD           NONE
SOIL_STORAGE_MAX_METHOD          CALCULATED
AVAILABLE_WATER_CONTENT_METHOD   GRIDDED
ROOTING_DEPTH_METHOD             STATIC

%% Weather data — Central Sands Daymet NetCDFs
PRECIPITATION NETCDF daymet_v4_daily_prcp_%Y_cs.nc
PRECIPITATION_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
PRECIPITATION_NETCDF_Z_VAR                prcp
PRECIPITATION_SCALE_FACTOR          0.03937008
PRECIPITATION_MISSING_VALUES_CODE      -9999.0
PRECIPITATION_MISSING_VALUES_OPERATOR      <=
PRECIPITATION_MISSING_VALUES_ACTION       zero

TMAX NETCDF daymet_v4_daily_tmax_%Y_cs.nc
TMAX_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
TMAX_SCALE_FACTOR                 1.8
TMAX_ADD_OFFSET                  32.0
TMAX_MISSING_VALUES_CODE      -9999.0
TMAX_MISSING_VALUES_OPERATOR      <=
TMAX_MISSING_VALUES_ACTION       mean

TMIN NETCDF daymet_v4_daily_tmin_%Y_cs.nc
TMIN_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
TMIN_SCALE_FACTOR                 1.8
TMIN_ADD_OFFSET                  32.0
TMIN_MISSING_VALUES_CODE      -9999.0
TMIN_MISSING_VALUES_OPERATOR      <=
TMIN_MISSING_VALUES_ACTION       mean

%% Static grids — Central Sands data
FLOW_DIRECTION ARC_GRID d8_flow_direction.asc
FLOW_DIRECTION_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

HYDROLOGIC_SOILS_GROUP ARC_GRID hydrologic_soils_group.asc
HYDROLOGIC_SOILS_GROUP_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

LAND_USE ARC_GRID landuse.asc
LANDUSE_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

AVAILABLE_WATER_CONTENT ARC_GRID available_water_capacity.asc
AVAILABLE_WATER_CONTENT_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m

%% Frozen ground
INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX CONSTANT 100.0
CFGI_UPPER_LIMIT CONSTANT 83.
CFGI_LOWER_LIMIT CONSTANT 55.

%% Initial conditions
INITIAL_PERCENT_SOIL_MOISTURE CONSTANT 100.0
INITIAL_SNOW_COVER_STORAGE CONSTANT 2.0

%% Lookup table — single unified table with GDD-based Kcb for all land uses
LAND_USE_LOOKUP_TABLE Landuse_lookup_FAO56_GDD.tsv

%% DUMP_VARIABLES — diagnostic output for key land uses
%%
%% Point 1: CORN (LU 1) — warm-season annual, Tbase=50°F
DUMP_VARIABLES COORDINATES 557946. 434408.
%%
%% Point 2: POTATOES (LU 43) — moderate Tbase=35.6°F
DUMP_VARIABLES COORDINATES 561167. 445224.
%%
%% Point 3: OATS (LU 28) — cool-season cereal, Tbase=32°F
DUMP_VARIABLES COORDINATES 569015. 439355.
%%
%% Point 4: DECIDUOUS FOREST (LU 141) — Tbase=32°F, leaf phenology
DUMP_VARIABLES COORDINATES 553927. 459454.
%%
%% Point 5: GRASS/PASTURE (LU 176) — Tbase=32°F, early green-up
DUMP_VARIABLES COORDINATES 562985. 448265.
%%
%% Point 6: SWEET CORN (LU 12) — for comparison with comprehensive test
DUMP_VARIABLES COORDINATES 561232. 442007.

%% Output — enable key diagnostics for Kcb verification
OUTPUT ENABLE growing_season growing_degree_day crop_et

%% Specify the run period dates
START_DATE 01/01/2012
END_DATE 12/30/2018
