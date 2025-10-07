GRID	233 257	1543928	4356904	50.0
#GRID	1170 1291	1543926.18	4356903.28	10.0
BASE_PROJECTION_DEFINITION +proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs

INTERCEPTION_METHOD              BUCKET
EVAPOTRANSPIRATION_METHOD        HARGREAVES
RUNOFF_METHOD                    CURVE_NUMBER
SOIL_MOISTURE_METHOD             THORNTHWAITE
PRECIPITATION_METHOD             GRIDDED
GROWING_DEGREE_DAY_METHOD        BASKERVILLE_EMIN
FOG_METHOD                       NONE
FLOW_ROUTING_METHOD              D8
IRRIGATION_METHOD                NONE
ROOTING_DEPTH_METHOD             STATIC
CROP_COEFFICIENT_METHOD          NONE
DIRECT_RECHARGE_METHOD           NONE
SOIL_STORAGE_MAX_METHOD          CALCULATED
AVAILABLE_WATER_CONTENT_METHOD   GRIDDED

PRECIPITATION TABLE ../../../test_data/sar/daily_weather_data__2006_2018.txt
TMIN TABLE ../../../test_data/sar/daily_weather_data__2006_2018.txt
TMAX TABLE ../../../test_data/sar/daily_weather_data__2006_2018.txt

FLOW_DIRECTION ARC_GRID ../../../test_data/sar/flowdir.asc
HYDROLOGIC_SOILS_GROUP ARC_GRID ../../../test_data/sar/hydrosoilgroup.asc
LAND_USE ARC_GRID ../../../test_data/sar/land_cover.asc
AVAILABLE_WATER_CONTENT ARC_GRID ../../../test_data/sar/awc.asc

# initial conditions
#-------------------
INITIAL_PERCENT_SOIL_MOISTURE           CONSTANT 25
INITIAL_SNOW_COVER_STORAGE              CONSTANT 0
INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX  CONSTANT 0

LAND_USE_LOOKUP_TABLE ../../../test_data/tables/lu_lookup__Muravera_SWB2.tsv
#
# Model time period for simulation
#---------------------------------
START_DATE 01/01/2006
END_DATE 12/31/2018