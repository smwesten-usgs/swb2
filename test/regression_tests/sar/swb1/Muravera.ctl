GRID	233 257	1543928	4356904	50.0
#GRID	1170 1291	1543926.18	4356903.28	10.0
BASE_PROJECTION_DEFINITION +proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs

GRID_LENGTH_UNITS METERS
GROWING_SEASON 1 364 TRUE
#
PRECIPITATION SINGLE_STATION
TEMPERATURE SINGLE_STATION
#
FLOW_DIRECTION ARC_GRID ../../../test_data/sar/flowdir.asc
FLOW_DIRECTION_PROJECTION_DEFINITION +proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs

SOIL_GROUP ARC_GRID ../../../test_data/sar/hydrosoilgroup.asc
SOIL_GROUP_PROJECTION_DEFINITION +proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs

LANDUSE ARC_GRID ../../../test_data/sar/land_cover.asc
LANDUSE_PROJECTION_DEFINITION +proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs

LAND_USE_LOOKUP_TABLE ../../../test_data/tables/lu_lookup__Muravera_SWB1.tsv
WATER_CAPACITY ARC_GRID ../../../test_data/sar/awc.asc
WATER_CAPACITY_PROJECTION_DEFINITION +proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs
#
#
# 'EQUATIONS' causes SWB to use a set of equations to calculate soil moisture rather
# than reading the values from a table. Using the 'EQUATIONS' method should reduce mass balance
# errors slightly
SM T-M EQUATIONS
INITIAL_SOIL_MOISTURE CONSTANT 25
INITIAL_SNOW_COVER CONSTANT 0
RUNOFF C-N DOWNHILL
#
#
# the two numbers must be in decimal degrees, and define the southern-most and northern-most latitudes of the model grid
ET HARGREAVES 39.3833 39.45

# need to use 'OUTPUT_OPTIONS' to have SWB create output for water balance terms
# options are:
#   NONE: no output
#   GRID: create Arc ASCII output grid
#   PLOT: create a 'png' image file
#   BOTH: create both a PLOT and a GRID as output#
#
# the position of the option determines what type of output (daily, monthly, or annual) will be created
# most of the output options below are set to 'NONE NONE BOTH', which will create
# no daily or monthly outputs, but will create both an image file and a grid file for annual output
#
OUTPUT_FORMAT ARC_GRID
OUTPUT_OPTIONS RECHARGE NONE NONE BOTH
OUTPUT_OPTIONS GROSS_PRECIP NONE NONE BOTH
OUTPUT_OPTIONS SM_APWL NONE NONE NONE
OUTPUT_OPTIONS SOIL_MOISTURE NONE NONE NONE PLOT
#OUTPUT_OPTIONS SNOWCOVER NONE NONE NONE
OUTPUT_OPTIONS INTERCEPTION NONE NONE BOTH
OUTPUT_OPTIONS RUNOFF_OUTSIDE NONE NONE BOTH
OUTPUT_OPTIONS ACT_ET NONE NONE BOTH
OUTPUT_OPTIONS REFERENCE_ET NONE NONE BOTH
OUTPUT_OPTIONS REJECTED_RECHARGE NONE NONE BOTH

SOLVE ../../../test_data/sar/PREC_2006.txt
SOLVE ../../../test_data/sar/PREC_2007.txt
SOLVE ../../../test_data/sar/PREC_2008.txt
SOLVE ../../../test_data/sar/PREC_2009.txt
SOLVE ../../../test_data/sar/PREC_2010.txt
SOLVE ../../../test_data/sar/PREC_2011.txt
SOLVE ../../../test_data/sar/PREC_2012.txt
SOLVE ../../../test_data/sar/PREC_2013.txt
SOLVE ../../../test_data/sar/PREC_2014.txt
SOLVE ../../../test_data/sar/PREC_2015.txt
SOLVE ../../../test_data/sar/PREC_2016.txt
SOLVE ../../../test_data/sar/PREC_2017.txt
SOLVE ../../../test_data/sar/PREC_2018.txt
EOJ
