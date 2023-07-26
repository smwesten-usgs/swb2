# SWB Model annotated control file
#
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
BASE_PROJECTION_DEFINITION +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +no_defs
GRID_LENGTH_UNITS METERS

#########################################################
# SCREEN OUTPUT CONTROL
#########################################################
SUPPRESS_SCREEN_OUTPUT
#SUPPRESS_INTEGRATED_OUTPUT
#SUPPRESS_DAILY_FILES
SUPPRESS_DISLIN_MESSAGES

RLE_MULTIPLIER 10000


###################################################
# PLANT GROWTH SETTINGS
###################################################
GROWING_SEASON 133 268 TRUE


###################################################
# FROZEN GROUND SETTINGS
###################################################
INITIAL_FROZEN_GROUND_INDEX CONSTANT 100.0
UPPER_LIMIT_CFGI 83.
LOWER_LIMIT_CFGI 55.

#########################################################
# CLIMATE DATA
#########################################################

PRECIPITATION NETCDF ..\..\..\test_data\or\pr_%y.nc
PRECIPITATION_GRID_PROJECTION_DEFINITION +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs
NETCDF_PRECIP_Z_VAR          precipitation_amount
NETCDF_PRECIP_X_VAR                           lon
NETCDF_PRECIP_Y_VAR                           lat
NETCDF_PRECIP_TIME_VAR                        day
PRECIPITATION_SCALE_FACTOR                    0.03937008
PRECIPITATION_MISSING_VALUES_CODE                 1000.0
PRECIPITATION_MISSING_VALUES_OPERATOR                 >=
PRECIPITATION_MISSING_VALUES_ACTION                 ZERO

TMAX NETCDF ..\..\..\test_data\or\tmmx_%y__subset.nc
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

TMIN NETCDF ..\..\..\test_data\or\tmmn_%y__subset.nc
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

###################################################
# WATER CONTENT, HYDROLOGIC SOIL GROUP, and LANDUSE GRIDS
###################################################
HYDROLOGIC_SOILS_GROUP ARC_GRID ..\..\..\test_data\or\hydrologic_soils_group.asc

LAND_USE ARC_GRID ..\..\..\test_data\or\landuse.asc

AVAILABLE_WATER_CONTENT ARC_GRID ..\..\..\test_data\or\available_water_capacity.asc

###################################################
# LANDUSE SETTINGS
###################################################
LAND_USE_LOOKUP_TABLE ..\..\..\test_data\tables\Landuse_lookup__Oregon_veg__bucket_interception__SWB1.txt

OPEN_WATER_LAND_USE 111

###################################################
# SOIL MOISTURE SETTINGS
###################################################
SM T-M EQUATIONS
INITIAL_SOIL_MOISTURE CONSTANT 100
INITIAL_SNOW_COVER CONSTANT 0


###################################################
# RUNOFF SETTINGS
###################################################
RUNOFF C-N NO_ROUTING
FLOW_DIRECTION CONSTANT 1
INITIAL_ABSTRACTION_METHOD HAWKINS

###################################################
# EVAPOTRANSPIRATION SETTINGS
###################################################
ET HARGREAVES 42.1 44.5

SNOWMELT_CONSTANT 0.4

###################################################
# PLOTTING CUSTOMIZATION
###################################################
DISLIN_PARAMETERS RECHARGE
SET_Z_AXIS_RANGE DAILY 0 1.5 0.1
SET_Z_AXIS_RANGE MONTHLY 0 7 1.0
SET_Z_AXIS_RANGE ANNUAL 0 20 2.
Z_AXIS_TITLE POTENTIAL RECHARGE, IN INCHES
#
DISLIN_PARAMETERS GROSS_PRECIP
SET_Z_AXIS_RANGE DAILY 0 1.5 0.2
SET_Z_AXIS_RANGE MONTHLY 0 7 1.0
SET_Z_AXIS_RANGE ANNUAL 0 60 10.
Z_AXIS_TITLE GROSS_PRECIP, IN INCHES
#
DISLIN_PARAMETERS ACT_ET
SET_Z_AXIS_RANGE DAILY 0 0.8 0.05
SET_Z_AXIS_RANGE MONTHLY 0 10. 0.5
SET_Z_AXIS_RANGE ANNUAL 0 40. 5.0
#SET_DEVICE PDF
#SET_FONT Helvetica-Bold
Z_AXIS_TITLE ACTUAL ET, IN INCHES
#
DISLIN_PARAMETERS REFERENCE_ET
SET_Z_AXIS_RANGE DAILY 0 1.0 0.05
SET_Z_AXIS_RANGE MONTHLY 0 10. 0.5
SET_Z_AXIS_RANGE ANNUAL 0 50. 10.0
#SET_DEVICE PDF
#SET_FONT Helvetica-Bold
Z_AXIS_TITLE REFERENCE ET, IN INCHES
#
DISLIN_PARAMETERS SNOWCOVER
SET_Z_AXIS_RANGE DAILY 0 60. 10.
SET_Z_AXIS_RANGE MONTHLY 0 200. 20.
SET_Z_AXIS_RANGE ANNUAL 0 1000. 100.0
#SET_DEVICE PDF
#SET_FONT Helvetica-Bold
Z_AXIS_TITLE SNOWCOVER, IN INCHES
#
DISLIN_PARAMETERS MAX_TEMP
SET_Z_AXIS_RANGE DAILY -20 110. 20.
SET_Z_AXIS_RANGE MONTHLY 0 80. 20.
SET_Z_AXIS_RANGE ANNUAL 0 80. 100.0
#SET_DEVICE PDF
#SET_FONT Helvetica-Bold
Z_AXIS_TITLE TMAX, IN INCHES
#
DISLIN_PARAMETERS MIN_TEMP
SET_Z_AXIS_RANGE DAILY -20 70. 20.
SET_Z_AXIS_RANGE MONTHLY 0 80. 20
SET_Z_AXIS_RANGE ANNUAL 0 80. 10.
#SET_DEVICE PDF
#SET_FONT Helvetica-Bold
Z_AXIS_TITLE TMIN, IN INCHES
#
#DISLIN_PARAMETERS RUNOFF_OUTSIDE
#SET_Z_AXIS_RANGE DAILY 0 5. 0.5
#SET_Z_AXIS_RANGE MONTHLY 0 12. 0.5
#SET_Z_AXIS_RANGE ANNUAL 0 25. 5.
#Z_AXIS_TITLE RUNOFF OUT OF GRID, IN INCHES
#
#DISLIN_PARAMETERS SNOWCOVER
SET_Z_AXIS_RANGE DAILY 0 12. 0.5
Z_AXIS_TITLE SNOW COVER, IN INCHES (WATER EQUIVALENT)
#
DISLIN_PARAMETERS SM_APWL
SET_Z_AXIS_RANGE DAILY -20. 0. 2.0
Z_AXIS_TITLE ACCUMULATED POTENTIAL WATER LOSS, IN INCHES
SET_COLOR_TABLE RRAIN


###################################################
# OUTPUT CONTROL
###################################################
OUTPUT_FORMAT ARC_GRID

#                                  <daily> <monthly> <annual>
OUTPUT_OPTIONS RECHARGE             NONE     GRID      GRID
OUTPUT_OPTIONS GROSS_PRECIP         NONE     GRID      GRID
OUTPUT_OPTIONS AVG_TEMP             NONE     NONE      NONE
OUTPUT_OPTIONS MAX_TEMP             NONE     NONE      NONE
OUTPUT_OPTIONS MIN_TEMP             NONE     NONE      NONE
OUTPUT_OPTIONS SM_APWL              NONE     NONE      NONE
OUTPUT_OPTIONS SNOWCOVER            NONE     GRID      GRID
OUTPUT_OPTIONS INTERCEPTION         NONE     GRID      GRID
OUTPUT_OPTIONS RUNOFF_OUTSIDE       NONE     GRID      GRID
OUTPUT_OPTIONS ACT_ET               NONE     GRID      GRID
OUTPUT_OPTIONS REFERENCE_ET         NONE     GRID      GRID
OUTPUT_OPTIONS REFERENCE_ET_ADJ     NONE     NONE      NONE
OUTPUT_OPTIONS GROWING_SEASON       NONE     NONE      NONE
OUTPUT_OPTIONS NET_INFLOW      			NONE     NONE      NONE
OUTPUT_OPTIONS NET_INFIL     			  NONE     NONE      NONE
OUTPUT_OPTIONS ROOTING_DEPTH	      NONE     NONE      NONE
OUTPUT_OPTIONS GDD						      NONE     NONE      NONE
OUTPUT_OPTIONS BARE_SOIL_EVAP		    NONE     NONE      NONE


DUMP_VARIABLES COORDINATES -1814392 2425869

OUTPUT_GRID_SUFFIX asc

###################################################
# TIME SETTINGS
###################################################
SOLVE_NO_TS_FILE 2000 2000
EOJ
