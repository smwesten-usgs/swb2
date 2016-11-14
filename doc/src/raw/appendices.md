SWB Technical Reference {#appendices}
=======================

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo
ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis
dis parturient montes, nascetur ridiculus mus. Donec quam felis,
ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa
quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget,
arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo.
Nullam dictum felis eu pede mollis pretium.

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo
ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis
dis parturient montes, nascetur ridiculus mus. Donec quam felis,
ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa
quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget,
arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo.
Nullam dictum felis eu pede mollis pretium.

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo
ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis
dis parturient montes, nascetur ridiculus mus. Donec quam felis,
ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa
quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget,
arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo.
Nullam dictum felis eu pede mollis pretium.

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo
ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis
dis parturient montes, nascetur ridiculus mus. Donec quam felis,
ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa
quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget,
arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo.
Nullam dictum felis eu pede mollis pretium.

Appendix 1. Data, Parameter, and Control File Requirements by Process Option {\#appendix\_1\_data\_and\_param\_requirements}
============================================================================================================================

SWB 2.0 was designed so that parameters may be supplied to the model on
an as-needed basis. The original SWB 1.0 input requirements grew ever
larger as more possible calculations methods were added. For this
reason, we created a flexible table-based format that allows for
parameters to be supplied in any order convenient to the user.

For table-based parameter entry, the crucial detail is to enter the
proper parameter name in the header of the file. Case does not matter
for these heading entries: "DEPLETION\_FRACTION" will work as well as
"depletion\_fraction" or "Depletion\_Fraction". For some modules,
multiple heading values are recognized as equivalent to one another. For
example, to identify a particular table column as holding landuse / land
cover codes, SWB 2.0 recognizes any of the following: "LU\_Code",
"Landuse\_Code", or "Landuse Lookup Code". Note also that SWB will fill
any blank spaces in the header with underscores before evaluating the
values therein. Thus, "Landuse Lookup Code" will be treated as
"Landuse\_Lookup\_Code" by SWB. The idea is that whatever identification
makes sense to the modeler should be recognized by SWB and acted upon.

This section describes in detail the data, parameter, and control file
requirements for each module currently implemented in the SWB 2.0 code.

Process: Actual Evapotranspiration
----------------------------------

Three actual ET modules are available. The actual ET modules are
responsible for determining how much soil moisture can be extracted
given the climate and soil moisture conditions being simulated. The
Thornthwaite-Mather actual ET modules should provide equivalent results
and should provide identical performance.

### FAO-56

*Control File Entry*

    SOIL_MOISTURE_METHOD  FAO-56

      Parameter Description         Allowable Lookup Table headers
  ----------------------------- --------------------------------------
       Depletion fraction                Depletion\_Fraction

### Thornthwaite-Mather

*Control File Entry*

    SOIL_MOISTURE_METHOD  THORNTHWAITE-MATHER

      Parameter Description         Allowable Lookup Table headers
  ----------------------------- --------------------------------------
               \*                                 \*

### Thornthwaite-Mather Equations

*Control File Entry*

    SOIL_MOISTURE_METHOD  THORNTHWAITE-MATHER_EQUATIONS
                         -or-
    SOIL_MOISTURE_METHOD THORNTHWAITE_MATHER_EQUATIONS

      Parameter Description         Allowable Lookup Table headers
  ----------------------------- --------------------------------------
               \*                                 \*

Process: Available Water Capacity / Available Water Content
-----------------------------------------------------------

### Gridded Values

*Control File Entry*

    AVAILABLE_WATER_CONTENT GRIDDED
               -or-
    AVAILABLE_WATER_CAPACITY GRIDDED

      Parameter Description         Allowable Lookup Table headers
  ----------------------------- --------------------------------------
               \*                                 \*

### Table Values

*Control File Entry*

    AVAILABLE_WATER_CONTENT TABLE
                -or-
    AVAILABLE_WATER_CAPACITY TABLE

  --------------------------------------------------------------------
      Parameter Description         Allowable Lookup Table headers
  ----------------------------- --------------------------------------
  Available water capacity, in                   AWC
         inches per foot        
  --------------------------------------------------------------------

### Table Values, Depth-Integrated

*Control File Entry*

    AVAILABLE_WATER_CONTENT DEPTH_INTEGRATED
                      -or-
    AVAILABLE_WATER_CAPACITY DEPTH_INTEGRATED

  -------------------------------------------------------------------------
  Parameter Description    Allowable Lookup Table             Note
                                   headers            
  --------------------- ----------------------------- ---------------------
      Landuse code         LU\_Code Landuse\_Code     
                            Landuse\_Lookup\_Code     

       Soils code                Soils\_Code          
                             Soils\_Lookup\_Code      
                                 Soil\_Code           
                             Soils\_Lookup\_Code      

      Soils horizon            Soils\_Horizon         
                           Soils\_Horizon\_Number     
                                Soil\_Horizon         
                            Soil\_Horizon\_Number     

     Soils top depth          Soils\_Top\_Depth       
                              Soil\_Top\_Depth        
                                Soils\_Z\_Top         
                           Soils\_Top\_of\_Horizon    

   Soils bottom depth       Soils\_Bottom\_Depth      
                             Soil\_Bottom\_Depth      
                              Soils\_Z\_Bottom        
                         Soils\_Bottom\_of\_Horizon   

     Soils component          Soils\_Component        
                          Soils\_Component\_Number    
                               Soil\_Component        
                           Soil\_Component\_Number    

     Soils component     Soils\_Component\_Fraction   
        fraction          Soil\_Component\_Fraction   

     Available water    Soils\_Available\_Water\_Cont 
         content                     ent              
                                 Soils\_AWC           
                        Soil\_Available\_Water\_Conte 
                                     nt               
                                  Soil\_AWC           
                        Available\_Water\_Content AWC 
  -------------------------------------------------------------------------

Process: Soil Storage Maximum / Plant Available Water
-----------------------------------------------------

The original way to parameterize the total volume of soil moisture
storage (or plant available water) was to specify an available water
capacity grid, plus a set of effective plant rooting depths in the
lookup table. SWB would multiply these two values to come up with the
size of the soil storage reservoir.

In some cases it may be useful to calculate the size of the soil
moisture reservoir outside of the SWB framework. This may be
accomplished by specifying that the soil storage maximum will be read
into SWB from an external grid file. *Specifying the soil storage
maximum this way will cause the rooting depths and available water
capacity values to be ignored.*

*Control File Entry*

    SOIL_STORAGE_MAXIMUM GRIDDED
                -or-
    PLANT_AVAILABLE_WATER GRIDDED
                ...
    SOIL_STORAGE_MAX ARC_GRID Common_Data/input/soil_moisture_storage__10m.asc
    SOIL_STORAGE_MAX_PROJECTION_DEFINITION +proj=utm +zone=4 +ellps=WGS84 +datum=WGS84 +units=m +no_defs

Process: Runoff
---------------

### Soil Conservation Service Curve Number

*Control File Entry*

    RUNOFF_METHOD CURVE_NUMBER
              -or-
    RUNOFF_METHOD C-N          

### Monthly Runoff Ratio

*Control File Entry*

    RUNOFF_METHOD RUNOFF_RATIO
              -or-
    RUNOFF_METHOD MONTHLY_GRID

Process: Runoff from Impervious surfaces
----------------------------------------

Runoff from impervious surfaces may be simulated in a more detailed
manner by including a gridded dataset defining the proportion of each
gridcell that is comprised of impervious materials. Data may be supplied
as either a fraction (0.0-1.0) or percentage (0-100%) of either pervious
or *im*pervious surface area.

Process: Crop Coefficients (FAO-56)
-----------------------------------

This module handles all processes associated with simulating the growth
and senescence of plants and their effect on soil moisture. There are
three ways in which crop coefficient curves may be specified; each
landuse code may use one of the three methods. The three methods that
may be used to specify the crop coefficient curves are:

1.  Time-based: specified in terms of the number of days that have
    elapsed since planting;
2.  Growing degree-day based: specified in terms of the number of
    growing degree-days that have passed since planting;
3.  Monthly: specific crop coefficients may be supplied with a single
    value per month.

*Control File Entry*

    CROP_COEFFICIENT_METHOD  FAO-56

  -------------------------------------------------------------------------
  Parameter Description    Allowable Lookup Table             Note
                                   headers            
  --------------------- ----------------------------- ---------------------
      Landuse code         LU\_Code Landuse\_Code     
                            Landuse\_Lookup\_Code     

                               Planting\_date         

  Inflection points on  L\_ini L\_dev L\_mid L\_late    Day values may be
     the Kcb curve,               L\_fallow             specified as the
   defined in terms of                                  integer number of
   time (days) elapsed                                 days elapsed since
   since the start of                                 planting, *or* may be
      plant growth.                                    specified as a date
                                                        in mm/dd format.

  Inflection points on  GDD\_plant GDD\_ini GDD\_dev  
     the Kcb curve,          GDD\_mid GDD\_late       
   defined in terms of                                
  growing degree-days.                                

    These values are     Kcb\_ini Kcb\_mid Kcb\_end   
  typically used along            Kcb\_min            
   with the GDD or day                                
    length values to                                  
     define a simple                                  
     K\_{cb} curve.                                   

  Mean plant height is       Mean\_plant\_height      
  used to determine how                               
  much bare soil might                                
      be exposed to                                   
     evaporation at                                   
     various growth                                   
         stages.                                      

    Monthly values to    Kcb\_Jan Kcb\_Feb Kcb\_Mar   
  define the Kcb curve   Kcb\_Apr Kcb\_May Kcb\_Jun   
  more completely. This  Kcb\_Jul Kcb\_Aug Kcb\_Sep   
   may be useful for a   Kcb\_Oct Kcb\_Nov Kcb\_Dec   
      crop that has                                   
   multiple plantings                                 
   and harvests in the                                
    course of a year.                                 
  -------------------------------------------------------------------------

Process: Direct Additions
-------------------------

External estimates for important components of the water budget may be
supplied as supplemental grids or as table values. These additional
water sources may be applied to the soil storage reservoir or added
directly as potential recharge (deep percolation).

For both direct addition types, either gridded or table data may be
supplied. SWB will always look for gridded data first. If no gridded
data are found, SWB will look for table values. Table values are
expected to correspond to the landuse codes contained in the main
landuse grid.

### Direct Additions to Potential Recharge

The grid or table values supplied as direct recharge may represent any
source of water that is not simulated as part of SWB's normal water
budget accounting. For convenience, a number of data types are defined:

-   cesspool leakage
-   disposal well injection
-   storm drain leakage
-   water body or reservoir leakage
-   water main leakage
-   other direct recharge

-   Control File Entry \*

<!-- -->

    CESSPOOL_LEAKAGE ARC_GRID Common_Data/input/cesspool_effluent_inches_day.asc
    CESSPOOL_LEAKAGE_PROJECTION_DEFINITION +proj=utm +zone=4 +ellps=WGS84 +datum=WGS84 +units=m +no_defs

  -------------------------------------------------------------------------
  Parameter Description    Allowable Lookup Table             Note
                                   headers            
  --------------------- ----------------------------- ---------------------
      Landuse code         LU\_Code Landuse\_Code     
                            Landuse\_Lookup\_Code     

     Generic direct     Annual\_direct\_recharge\_rat    direct recharge
        recharge                      e                  expressed as an
                           Annual\_recharge\_rate         *ANNUAL SUM*
                          Annual\_direct\_recharge    

    Cesspool leakage     Cesspool\_direct\_recharge      direct recharge
                             Cesspool\_recharge       expressed as a *DAILY
                             Cesspool\_discharge              SUM*
                              Cesspool\_leakage       

   Storm drain leakage     Storm\_drain\_discharge       direct recharge
                           Storm\_drain\_recharge     expressed as a *DAILY
                            Storm\_drain\_leakage             SUM*

      Water body /          Water\_body\_recharge        direct recharge
    reservoir leakage      Water\_body\_discharge     expressed as a *DAILY
                            Water\_body\_leakage              SUM*

   Water main leakage       Water\_main\_recharge        direct recharge
                           Water\_main\_discharge     expressed as a *DAILY
                            Water\_main\_leakage              SUM*

      Disposal well       Disposal\_well\_recharge       direct recharge
                          Disposal\_well\_discharge   expressed as a *DAILY
                                                              SUM*
  -------------------------------------------------------------------------

### Direct Additions to Soil Moisture

Additional sources of water may also be supplied directly to the soil
moisture reservoir. Currently the named data types include daily and
annual septic system discharge.

Process: Potential evapotranspiration
-------------------------------------

### Gridded

*Control File Entry*

    POTENTIAL_EVAPOTRANSPIRATION_METHOD GRIDDED
                         -or-
    REFERENCE_EVAPOTRANSPIRATION_METHOD GRIDDED

                         ...

    POTENTIAL_ET ARC_GRID              Common_Data/input/gr0_in_month_ascii/gr0_in_%b.asc
    POTENTIAL_ET_PROJECTION_DEFINITION +proj=lonlat +datum=WGS84 +no_defs                     

### Jensen-Haise

*Control File Entry*

    POTENTIAL_EVAPOTRANSPIRATION_METHOD JENSEN-HAISE
                         -or-
    REFERENCE_EVAPOTRANSPIRATION_METHOD JENSEN-HAISE
                         -or-
    POTENTIAL_EVAPOTRANSPIRATION_METHOD JENSEN_HAISE
                         -or-
    REFERENCE_EVAPOTRANSPIRATION_METHOD JENSEN_HAISE

### Hargreaves-Samani

SWB 1.0 required that the northern and southern latitudes of the project
area be supplied by the user. Since SWB 2.0 requires that a project grid
is established along with a PROJ.4 string, the northern and southern
latitudes can be calculated by SWB 2.0; the user need not enter these
values in the control file.

*Control File Entry*

    POTENTIAL_EVAPOTRANSPIRATION_METHOD HARGREAVES_SAMANI
                         -or-
    REFERENCE_EVAPOTRANSPIRATION_METHOD HARGREAVES_SAMANI
                         -or-
    POTENTIAL_EVAPOTRANSPIRATION_METHOD HARGREAVES-SAMANI
                         -or-
    REFERENCE_EVAPOTRANSPIRATION_METHOD HARGREAVES-SAMANI                     

Process: Fog Interception
-------------------------

*Control File Entry*

    FOG_METHOD MONTHLY_GRID
             -or-
    FOG_METHOD GRIDDED
             ...
    FOG_RATIO ARC_GRID ../../Maui_Common_Data/input/fog_fraction_grids/maui_fog_ratio_monthly_%0m.asc
    FOG_RATIO_PROJECTION_DEFINITION +proj=utm +zone=4 +ellps=WGS84 +datum=WGS84 +units=m +no_defs

Process Support: Growing Degree-Day
-----------------------------------

*Control File Entry*

    *

Process Support: Growing Season
-------------------------------

*Control File Entry*

    *

Process: Interception
---------------------

### Bucket

*Control File Entry*

    INTERCEPTION_METHOD BUCKET

### Horton

*Control File Entry*

    INTERCEPTION_METHOD HORTON

### Gash

*Control File Entry*

    INTERCEPTION_METHOD GASH
             ...
    FRACTION_CANOPY_COVER ARC_GRID Common_Data/input/CANOPY_COVER_FRACTION.asc
    FRACTION_CANOPY_COVER_PROJECTION_DEFINITION +proj=utm +zone=4 +ellps=WGS84 +datum=WGS84 +units=m +no_defs

    EVAPORATION_TO_RAINFALL_RATIO ARC_GRID Common_Data/input/EVAPORATION_TO_RAINFALL_RATIO.asc
    EVAPORATION_TO_RAINFALL_RATIO_PROJECTION_DEFINITION +proj=utm +zone=4 +ellps=WGS84 +datum=WGS84 +units=m +no_defs

Process: Irrigation
-------------------

*Control File Entry*

    IRRIGATION_METHOD FAO-56

Process Support: Precipitation
------------------------------

### Table Values

*Control File Entry*

    PRECIPITATION_METHOD TABLE

### Gridded Values

*Control File Entry*

    PRECIPITATION_METHOD NORMAL
             -or-
    PRECIPITATION_METHOD GRIDDED

### Method of Fragments

    PRECIPITATION_METHOD METHOD_OF_FRAGMENTS
                      ...
    RAINFALL_ZONE ARC_GRID Common_Data/input/maui_RAIN_ZONE__10m.asc
    RAINFALL_ZONE_PROJECTION_DEFINITION +proj=utm +zone=4 +ellps=WGS84 +datum=WGS84 +units=m +no_defs

    RAINFALL_ADJUST_FACTOR ARC_GRID Common_Data/input/Maui_RF_adj_factors/maui_RF_adj_%b.asc
    RAINFALL_ADJUST_FACTOR_PROJECTION_DEFINITION  +proj=utm +zone=4 +ellps=WGS84 +datum=WGS84 +units=m +no_defs

Process: Storm Drain Capture
----------------------------

  ---------------------------------
   Parameter Allowable Lookup Note
      Description Table headers
  ---------------------------------

Landuse code LU\_Code Landuse\_Code Landuse\_Lookup\_Code

                                Storm_drain_capture
                                Storm_drain_capture_fraction

Appendix 2. Control File Directives {#appendix_2_control_file_directives}
===================================

This section provides a complete list of the control file statements
understood by SWB.

Control File Directives {#appendix_2_control_file_directives_original_swb}
-----------------------

This section provides a complete list of the control file statements
understood by SWB, version 1.x.

### Required control file entries

#### GRID

*Specify the horizontal extent and gridcell resolution for a SWB run.*

The `GRID` directive should be in the first line of the control file.
Units of meters are assumed; an optional control-file statement
(`GRID_LENGTH_UNITS`) may be used to specify that units of feet are used
instead. If any subsequent input grid fails to match the specified model
domain exactly, the program will end.

*Syntax:*

    GRID nx ny x0 y0 x1 y1 cellsize

-or-

    GRID nx ny X0 Y0 cellsize

where

    nx

is the number of columns in the grid domain,

    ny

is the number of rows in the grid domain, `x0, y0` are the coordinates
for the lower, left-hand corner of the model domain `x1, y1` are the
coordinates for the upper, right-hand corner of the model domain, and
`cellsize` is the grid cell size in meters or feet.

*Example:*

<div custom-style="Source_Code">

GRID 344 320 528093. 274821. 100.0

</div>

In the example shown above, nx (344) is the number of grid cells in the
x direction, ny (320) is the number of grid cells in the y direction, X0
and Y0 (528093, 274821) are the coordinates for the lower left-hand
corner of the grid.

#### GROWING\_SEASON

*Specify the start and end of the growing season; influences
interception and antecedent runoff conditions.*

This flag controls when the growing season is considered to begin and
end (expressed as the day of year) and whether or not the problem is in
the Northern hemisphere (possible values: `TRUE` or `FALSE`).

*Example*

`GROWING_SEASON 133 268 TRUE`

Alternatively, the growing season statement may be specified as
`GROWING_SEASON GDD`, which will cause SWB to track the growing degree
days for each gridcell. When using this option, the growing season is
considered active when the number of growing degree-days for the cell
exceed 90. The growing season is considered to end when the minimum
daily air temperature falls below 28.5 degree Fahrenheit. One drawback
to this approach is that the growing season status may toggle several
times between growing and nongrowing season in early spring if a string
of warm days is followed by a single cold night, for example. A benefit
to this approach is that each cell is allowed to toggle between
nongrowing and growing season as the local air temperatures dictate.

This option affects which interception value (growing season or dormant
season) and which antecedent runoff condition thresholds apply to a grid
cell.

#### PRECIPITATION

*Specify one or more files that contain precipitation data.*

This control statement specifies a file type and a file prefix that the
SWB code will associate with daily *gridded* precipitation values.

*Syntax:*

`PRECIPITATION file_format location`

where

`file_format` may be `ARC_GRID`, `SURFER`, or `NETCDF`, and `location`
is the relative or absolute path and name of the relevant file or file
template.

*Example--Arc ASCII daily grid:*

`PRECIPITATION ARC_GRID climate\precip\PRCP_%m_%d_%y.asc`

*Example -- Surfer daily grid:*

`PRECIPITATION SURFER climate\precip\PRCP_%m_%d_%y.asc`

#### TEMPERATURE

*Specify one or more files containing minimum and maximum daily air
temperature.*

This control statement either indicates that temperature at a single
station is to be used or, alternatively, specifies a file type and file
prefixes associated with daily gridded maximum and minimum temperature
values. The required syntax for gridded temperature data is:

`TEMPERATURE` *FILE\_FORMAT TMAX template TMIN template*

*Example -- single station:*

`TEMPERATURE SINGLE_STATION`

--or--

*Example -- Arc ASCII daily grid:*

`TEMPERATURE ARC_GRID climate\precip\TMAX_%m_%d_%y.asc climate\precip\TMIN_%m_%d_%y.asc`

--or--

*Example - Surfer daily grid:*

`TEMPERATURE SURFER climate\precip\TMAX_%m_%d_%y.asc climate\precip\TMIN_%m_%d_%y.asc`

--or--

*Example - NetCDF daily grids encapsulated in an annual file:*

`TEMPERATURE NETCDF climate\precip\TMAX_%y.asc climate\precip\TMIN_y.asc`

Note that the file template may specify one or more relative
subdirectory names separated by backslashes. Temperature grids are
expected to be of data type real, and in units of degrees Fahrenheit.

#### FLOW\_DIRECTION

*Specify a D8 flow-direction grid.*

Arc ASCII or Surfer grid of D8 flow directions. The flow-direction grid
must be of data type integer.

*Example:*

`FLOW_DIRECTION ARC_GRID input\new_fl_dir.asc`

#### SOIL\_GROUP

*Specify a soil-group grid.*

Arc ASCII or Surfer grid of hydrologic soil groups; values must
correspond to the hydrologic soil groups contained in the land-use
lookup table. The soil group must be of data type integer. \*Example:
SOIL\_GROUP ARC\_GRID input\soil\_hyd\_grp.asc

#### LAND\_USE

*Specify a landuse grid.*

Arc ASCII or Surfer grid of land-use\land-cover classification values;
the values must correspond to the land-use\land-cover values contained
in the land-use lookup table. The land-use grid must be of data type
integer.

*Example:*

`LAND_USE ARC_GRID input\land_use.asc`

#### LAND\_USE\_LOOKUP\_TABLE

Specify the name and location of the landuse lookup table.

The name of the land-use lookup table must be specified within the
control file.

*Example:*

`LAND_USE_LOOKUP_TABLE std_input\LU_lookup.txt` \`\`\`\`\`\` \#\#\#\#
WATER\_CAPACITY \#\#\#\#

*Specify a soil-water capacity grid.*

The model uses soil information, together with land-cover information,
to calculate surface runoff and assign a maximum soil-moisture holding
capacity to each grid cell ($\theta_{fc}$). The soil-group grid may be
used along with the values included in table 7 to produce this grid.
Available water capacity is expected to be of data type real and is
expressed in units of inches of water per foot of soil.

*Example: *

`WATER_CAPACITY ARC_GRID input\avail_water_cap.asc`

#### SM (Soil Moisture accounting method)

*Specify a soil-moisture accounting calculation option.*

The model currently only contains one soil-moisture-accounting
calculation option: Thornthwaite-Mather (1948, 1957). The
Thornthwaite-Mather soil-moisture-retention tables are included in the
standard table soil-moisture-retention-extended.grd.

*Example:*

`SM T-M std_input\soil-moisture-retention-extended.grd`

#### INITIAL\_SOIL\_MOISTURE

*Set initial soil moisture.*

Initial soil moisture can be specified either as a single constant value
or as a grid of values. Initial soil moisture is expressed as a
percentage saturation of the available water capacity (0--100 percent).
If supplied as gridded data, the initial soil-moisture grid is expected
to be of data type real.

*Example--uniform constant value:*

`INITIAL_SOIL_MOISTURE CONSTANT 100`

--or--

*Example--Arc ASCII grid:*

`INITIAL_SOIL_MOISTURE ARC_GRID input\initial_pct_sm_dec_31_1999.asc`

#### INITIAL\_SNOW\_COVER

*Supply snow cover initial conditions.*

Initial snow cover can be specified either as a single constant value or
as a grid of values. Initial snow cover is expressed as a
water-equivalent value (inches of water). If supplied as gridded data,
the initial-snow-cover grid is expected to be of data type real.

*Example--uniform constant value:*

`INITIAL_SNOW_COVER CONSTANT 0.3`

--or--

*Example--Arc ASCII grid:*

`INITIAL_SNOW_COVER ARC_GRID input\initial_snow_cover_dec_31_1999.asc`

#### RUNOFF

*Specify the method to use when calculating runoff.*

Currently, only one runoff calculation method is available: the NRCS
curve number method [@cronshey_urban_1986]. This calculation method must
be specified by including the letters `C-N` after the keyword `RUNOFF`.

Three methods are available for the routing of surface water through the
model domain. The `ITERATIVE` method is based on the original VBA code
solution method, in which water is iteratively moved across the entire
grid until all water has either infiltrated or left the grid via surface
flow. The `DOWNHILL` method involves a pre-simulation step in which the
model grid cells are sorted upslope to downslope. Runoff is calculated
in a single iteration for each time step over the entire model domain,
proceeding from the upstream cells to the downstream cells. There should
be no difference between the two routing methods except that the
`DOWNHILL` method executes much more quickly. The `ITERATIVE` solution
method will be removed from future versions of the code.

Specifying the method as `NONE` disables routing altogether. Any cell
outflow is assumed to find its way to a surface-water feature and exit
the model domain. Outflow under this option is tracked as
`RUNOFF_OUTSIDE`.

The SWB code writes a binary file to disk after the first pass through
the `DOWNHILL` iteration method. This binary file is read in for
subsequent SWB model runs, eliminating the potentially time-consuming
task of grid-cell sorting. Any changes to the grid extent will trigger
an error message reminding the user to delete the file `swb_routing.bin`
before rerunning the SWB code.

*Example--iterative routing; runoff calculated by means of SCS curve
number:*

`RUNOFF C-N ITERATIVE`

--or--

*Example--downhill routing; runoff calculated by means of SCS curve
number:*

`RUNOFF C-N DOWNHILL`

--or--

*Example--routing disabled; runoff calculated by means of SCS curve
number:*

`RUNOFF C-N NONE`

#### ET (evapotranspiration method)

The model implements several methods for estimating potential
evapotranspiration, specifically, the Thornthwaite-Mather, Jensen-Haise,
Blaney-Criddle, Turc, and Hargreaves-Samani methods. Note that, given
the same root-zone depths, the Thornthwaite-Mather method will produce
lower estimates of potential evapotranspiration (and thus, higher
estimates of recharge) than the other methods
[@vorosmarty_potential_1998]. The Hargreaves-Samani method is the only
one suitable for use with gridded precipitation and air-temperature
data.

Examples showing the use of all possible program options for specifying
an ET calculation method are given below.

*Example: Thornthwaite-Mather*

`ET T-M` *latitude*

--or--

*Example: Jensen-Haise*

`ET J-H` *latitude albedo* $a_s$ $b_s$

--or--

*Example: Blaney-Criddle *

`ET B-C` *latitude*

--or--

*Example: Turc*

`ET TURC` *latitude albedo* $a_s$ $b_s$

--or--

*Example: Hargreaves-Samani*

`ET HARGREAVES` *southerly\_latitude northerly\_latitude*

Values must be entered for all specified options. In the absence of more
specific information, a reasonable value of the albedo for the
Jensen-Haise and Turc methods is 0.23; similarly, $a_s$ may be set to
0.25 and $b_s$ set to 0.5. The coefficients $a_s$ and $b_s$ are used in
the Angstrom formula for estimation of daily solar radiation
[@allen_crop_1998]. The term $a_s$ expresses the fraction of
extraterrestrial radiation that reaches earth on overcast days; $b_s$
expresses the additional fraction of extraterrestrial radiation that
reaches earth on clear days.

#### SOLVE / SOLVE\_NO\_TS\_FILE

*Begins the actual recharge calculation.*

When using a single climate data station (in other words, no
precipitation or temperature), the `SOLVE` statement is used. The name
of the single-station climate time-series data file is required.

\*Example: SOLVE bec1999v2.txt

Note that a simulation for more than 1 year is made possible by simply
including additional SOLVE statements.

*Example: multiple years, single station*

`SOLVE MSN_1999.txt` `SOLVE MSN_2000.txt` `SOLVE MSN_2001.txt` `--etc--`

If gridded temperature and precipitation data are supplied to the SWB
model and the Hargreaves-Samani (1985) ET calculation method is selected
for use, the single-station climate time-series file may be eliminated
altogether. In this case, the starting and ending year of the simulation
must be supplied.

*Example: multiple years, gridded climate data:*

`SOLVE_NO_TS_FILE 1983 1992`

#### EOJ

*End of job.*

The `EOJ` option stands for End-Of-Job; it triggers actions to write
grids to disk, calculate statistics, and de-allocate memory.

*Example:*

`EOJ`

### Optional Control-File Entries

The control-file entries discussed above are the only ones required to
run the SWB model. However, there are additional control-file entries
available that change the method of calculation or enable additional
output options. This section describes optional control-file entries
that may be used.

#### ANSI\_COLORS

*Request colored screen output.*

If you have access to a terminal program such as rxvt, the SWB model can
generate screen output with color coding for positive and negative
values (possible values: TRUE\slash FALSE). The rxvt package can be
installed as an option along with the Cygwin Unix emulation package
(www.cygwin.com).

*Example:*

`ANSI_COLORS TRUE`

#### SUPPRESS\_SCREEN\_OUTPUT\SUPPRESS\_DAILY\_FILES\SUPPRESS\_DISLIN\_MESSAGES \#\#\#\#

In order to speed model runtimes, certain types of text messages
normally printed to the screen and\slash or disk may be suppressed.

`SUPPRESS_SCREEN_OUTPUT` will turn off the detailed mass-balance
information that is normally printed to the screen for each daily
timestep.

`SUPPRESS_DAILY_FILES` will prevent detailed mass balance from being
written to disk as recharge\_daily\_statistics.csv,
recharge\_annual\_report.csv, and recharge\_daily\_report.csv.

`SUPPRESS_DISLIN_MESSAGES` will prevent the progress messages normally
generated by the DISLIN graphics library from being written to the
screen.

*Example:*

`SUPPRESS_SCREEN_OUTPUT`

#### ADJUSTED\_WATER\_CAPACITY

*Override maximum water water capacity calculations with externally
calculated values.*

The model will normally calculate the total available water capacity
from the base soil-water-capacity grid and the land-use grid, using the
rooting-depth values as specified in the land-use lookup table. When
this directive is used, the adjusted water capacity may be calculated
independently of the model and read in as a real-valued ASCII grid. If
this is done, internal calculation of the rooting depth and resulting
adjusted water capacity is disabled in the model.

*Example:*

`ADJUSTED_WATER_CAPACITY ARC_GRID input\MAX_SM_STORAGE.asc`

UPPER\_LIMIT\_CFGI\LOWER\_LIMIT\_CFGI The upper and lower
continuous-frozen-ground indices may be set with the UPPER\_LIMIT\_CFGI
and LOWER\_LIMIT\_CFGI statements. As discussed elsewhere, these values
define the boundaries between completely frozen soil (the upper limit)
and completely unfrozen soil (the lower limit). The CFGI threshold
values are expressed in degree-Celcius-days. \*Example:
UPPER\_LIMIT\_CFGI 83\
LOWER\_LIMIT\_CFGI 56

Values for \\texttt{UPPER\_LIMIT\_CFGI and \\texttt{LOWER\_LIMIT\_CFGI
were reported as 83 and 56, respectively, by Molnau and Bissell (1983);
their work was conducted in the Pacific Northwest of the United States.

INITIAL\_FROZEN\_GROUND\_INDEX This statement sets the initial (year 1)
continuous-frozen-ground index. This may be supplied as a constant
(\\texttt{CONSTANT) or as a gridded data set (\\texttt{ARC\_GRID or
\\texttt{SURFER). *Example: INITIAL\_FROZEN\_GROUND\_INDEX CONSTANT
100.0 --or-- *Example: INITIAL\_FROZEN\_GROUND\_INDEX ARC\_GRID
input\INIT\_CFGI.asc

INITIAL\_ABSTRACTION\_METHOD The method for calculating the initial
abstraction within the NRCS curve number method may be specified in one
of two ways: 1, TR--55: Ia is assumed equal to 0.2 S, 2, Woodward and
others (2003): Ia is assumed equal to 0.05 S.

If the Hawkins method is used, curve numbers are adjusted as given in
equation 9 of \\citet{woodward\_runoff\_2003. The overall effect should
be to increase runoff for smaller precipitation events. This method has
been suggested to be more appropriate to long-term simulation model
applications.

*Example -- original TR-55 method: INITIAL\_ABSTRACTION\_METHOD TR55
--or-- *Example -- Hawkins modified method: INITIAL\_ABSTRACTION\_METHOD
HAWKINS

OUTPUT\_FORMAT (file format for gridded data output) This option allows
the user to choose the format of grid output. Currently two formats are
supported: ESRI ASCII Grid, and Golden Software Surfer. The default is
\\texttt{ARC\_GRID. *Example--Arc Grid output: OUTPUT\_FORMAT ARC\_GRID
--or-- *Example--Surfer format: OUTPUT\_FORMAT SURFER

OUTPUT\_GRID\_PREFIX This option sets the output grid filename prefix.
If no value is supplied, output file names will begin with an underscore
(\_).

\*Example: OUTPUT\_GRID\_PREFIX BlkErth

OUTPUT\_GRID\_SUFFIX This option sets the output grid filename suffix.
The default value is asc. \*Example: OUTPUT\_GRID\_SUFFIX txt

OUTPUT\_OPTIONS The code will write out gridded data files or image
files (portable network graphics, Adobe Portable Document Format, or
bitmap) for any of 24 internal and state variables simulated in the
model (see table 11). The required syntax of this option is

-   OUTPUT\_OPTIONS variable\_name daily\_output monthly\_output
    annual\_output

The list of valid output types is \\texttt{NONE, \\texttt{GRAPH,
\\texttt{GRID, or \\texttt{BOTH.

For example, if one wished to produce output for actual
evapotranspiration, the following would yield no daily output, graphical
monthly output, and both gridded and graphical annual output. \*Example:
OUTPUT\_OPTIONS ACT\_ET NONE GRAPH BOTH

Note that the monthly and annual output types represent the summation of
the simulated daily values and may not have any physical meaning. For
example, although annual gridded \\texttt{SNOWFALL output will represent
the annual snowfall amount as water equivalent precipitation, the annual
gridded \\texttt{SNOWCOVER output represents the summation of the daily
amount of snow storage. It is unclear what utility this summation would
have. Also note that the graphical output is not publication quality but
is rather included as a quick way to visualize changes in key variables
over space and through time.

DISLIN\_PARAMETERS (graphical output options) Plots of any of the
variables listed in the ?Output Variables? section above are created by
use of the DISLIN plotting library (Michels, 2007). The plots created by
the SWB code are quite basic. The plotting functionality in the SWB code
is included primarily as a quick diagnostic tool for users.

SWB enables use of some of the more important DISLIN parameters. These
parameters control how the DISLIN plotting library formats each plot.
The syntax is DISLIN\_PARAMETERS SWB Output Variable Name, followed on
the next lines by one or more of the following statements:
SET\_Z\_AXIS\_RANGE, SET\_DEVICE, SET\_FONT, or Z\_AXIS\_TITLE. The
SET\_Z\_AXIS\_RANGE statement allows the user to specify the range of
values that will be plotted. Three sets of ranges may be specified for
plots of daily, monthly, and annual values. The minimum, maximum, and
increment size must be specified each time the SET\_Z\_AXIS\_RANGE
statement is used. \*Example: DISLIN\_PARAMETERS RECHARGE
\SET\_Z\_AXIS\_RANGE DAILY 0 1.5 0.1 \SET\_Z\_AXIS\_RANGE MONTHLY 0 7
1.0 \SET\_Z\_AXIS\_RANGE ANNUAL 0 20 2. \SET\_DEVICE PDF \SET\_FONT
Times-Bold \Z\_AXIS\_TITLE RECHARGE, IN IN.

In the second line of the example above, the plotting range for the
\\texttt{RECHARGE output variable on a daily time scale is specified
with a minimum of 0 in., a maximum of 1.5 in., and a plotting increment
of 0.1 in.

The \\texttt{SET\_DEVICE statement can be used to change the output file
format. The default value is \\texttt{PNG, for Portable Network
Graphics. This format is compact and is supported by most modern
applications. Other possible output types include Windows Metafile
(WMF), Adobe Postscript (PS), Adobe Encapsulated Postscript (EPS), Adobe
Portable Document Format (PDF), Scalable Vector Graphics (SVG), and
Windows Bitmap (BMP).

The \\texttt{SET\_FONT statement can be used to alter the font used to
annotate the plots. For plot device type \\texttt{PS, \\texttt{EPS,
\\texttt{PDF, and \\texttt{SVG, the following subset of fonts should
work: \\texttt{ \Times-Roman Courier Helvetica AvantGarde-Book
Helvetica-Narrow Bookman-Light NewCenturySchlbk-Roman Palatino-Roman
NewCenturySchlbk-Italic Palatino-Italic

For more details about supported fonts, see the DISLIN documentation
(http:/\slash www.dislin.de\slash ). The statement
\\texttt{Z\_AXIS\_TITLE sets the text that is used for the plot legend.
Text does not need to be enclosed by quotes and may include punctuation.

ITERATIVE METHOD TOLERANCE \[OPTIONAL\] The iterative method sometimes
fails to converge for small solution tolerances (that is, less than
1.0E--6 change in calculated runoff in a cell from one iteration to the
next). Increasing this value will improve convergence, but at the
potential cost of also increasing mass-balance errors in the overall
water balance. The default value is 1E--6. Example:
ITERATIVE\_METHOD\_TOLERANCE 1.0E--4 UNITS OF LENGTH \[OPTIONAL\] The
SWB code is written with default units of meters to define the gridded
model domain. If units of feet are desired instead, the
GRID\_LENGTH\_UNITS statement may be used. The only place in the code
where this is important is with regard to the mass balance as expressed
in units of acre-feet. If the grid length units are feet while the code
treats them as meters, there will be a corresponding error in the values
of the mass-balance terms. Example: GRID\_LENGTH\_UNITS FEET

### Gridded Datasets

For each of the three major climate datasets (precipitation, minimum and
maximum air temperature), a standard set of suffixes may be added to the
dataset name to control how SWB treats the dataset. The list of suffixes
understood by SWB is long:

  --------------------------------------------------------------------------
  Suffix                    Argument      Description                   Defa
                                                                        ult
  ------------------------- ------------- ----------------------------- ----
  \_SCALE\_FACTOR           *real value*  amount to multiply raw grid   1.0
                                          value by prior to use         

  \_ADD\_OFFSET             *real value*  amount to add to the raw grid 0.0
                                          value following application   
                                          of the scale factor, if any   

  \_NETCDF\_X\_VAR          *string*      name of the variable to be    x
                                          used as the "x" axis          

  \_NETCDF\_Y\_VAR          *string*      name of the variable to be    y
                                          used as the "y" axis          

  \_NETCDF\_Z\_VAR          *string*      name of the variable to be    prcp
                                          used as the "z" (value) axis  

  \_NETCDF\_TIME\_VAR       *string*      name of the variable to be    time
                                          used as the "time" axis       

  \_NETCDF\_VARIABLE\_ORDER "xyt or txy"  description of the order in   tyx
                                          which the gridded data were   
                                          written                       

  \_NETCDF\_FLIP\_VERTICAL  **none**      if present, all gridded data  NA
                                          will be "flipped" around the  
                                          vertical axis.                

  \_NETCDF\_FLIP\_HORIZONTA **none**      if present, all gridded data  
  L                                       will be "flipped" around the  
                                          horizontal axis               

  \_NETCDF\_MAKE\_LOCAL\_AR                                             
  CHIVE                                                                 

  \_PROJECTION\_DEFINITION                PROJ.4 string describing the  
                                          geographic projection of the  
                                          dataset                       

  \_MINIMUM\_ALLOWED\_VALUE *real value*  ceiling to be applied to the  
                                          data; data above this value   
                                          will be reset to this amount  

  \_MAXIMUM\_ALLOWED\_VALUE *real value*  floor to be applied to the
                                          data; data beneath this value
                                          will be reset to this amount

  \_MISSING\_VALUES\_CODE   *real or      value                         
                            integer                                     
                            value*                                      

  \_MISSING\_VALUES\_OPERAT "&lt;",       trigger missing values action 
  OR                        "&lt;=",      if the data value meets this  
                            "&gt;",       condition                     
                            "&gt;="                                     

  \_MISSING\_VALUES\_ACTION "mean" or     "mean" will substitute the
                            "zero"        mean value calculated over
                                          the remaining valid cells;
                                          "zero" will substitute a
                                          value of 0.0 in place of
                                          missing values
  --------------------------------------------------------------------------

Control File Directives {#appendix_2_control_file_directives_version_2}
-----------------------

This section provides a complete list of the control file statements
understood by SWB, version 2.0.

Control File Directives: Gridded Datasets
-----------------------------------------

SWB has a set of common control file directives that may be used with
any input gridded dataset. The types of data recognized by SWB (as of
July 2015) includes:

  Gridded Dataset Name
  ----------------------------------
  PRECIPITATION
  TMIN
  TMAX
  AVAILABLE\_WATER\_CONTENT
  POTENTIAL\_ET
  SOLAR\_RADIATION
  WIND\_SPEED
  RAINFALL\_ZONE
  FLOW\_DIRECTION
  FOG\_RATIO
  LAND\_USE
  SOILS\_GROUP
  INITIAL\_PERCENT\_SOIL\_MOISTURE
  INITIAL\_SNOW\_COVER\_STORAGE
  CANOPY\_COVER\_FRACTION
  PERVIOUS\_SURFACE\_FRACTION
  IMPERVIOUS\_SURFACE\_FRACTION
  STEMFLOW\_FRACTION
  EVAPORATION\_TO\_RAINFALL\_RATIO
  RAINFALL\_ADJUST\_FACTOR
  CESSPOOL\_LEAKAGE
  STORM\_DRAIN\_LEAKAGE
  WATER\_BODY\_LEAKAGE
  WATER\_MAIN\_LEAKAGE
  DISPOSAL\_WELL\_DISCHARGE
  ANNUAL\_DIRECT\_RECHARGE\_RATE
  RUNOFF\_ZONE
  IRRIGATION\_MASK
  RELATIVE\_HUMIDITY

For each of the gridded datasets listed above, a standard set of
suffixes may be added to the dataset name to control how SWB treats the
dataset. The list of suffixes understood by SWB is long:

  ------------------------------------------------------------------------
  Suffix                      Argument      Description
  --------------------------- ------------- ------------------------------
  \_SCALE\_FACTOR             *real value*  amount to multiply raw grid
                                            value by prior to use

  \_ADD\_OFFSET               *real value*  amount to add to the raw grid
                                            value following application of
                                            the scale factor, if any

  \_NETCDF\_X\_VAR            *string*      name of the variable to be
                                            used as the "x" axis

  \_NETCDF\_Y\_VAR            *string*      name of the variable to be
                                            used as the "y" axis

  \_NETCDF\_Z\_VAR            *string*      name of the variable to be
                                            used as the "z" (value) axis

  \_NETCDF\_TIME\_VAR         *string*      name of the variable to be
                                            used as the "time" axis

  \_NETCDF\_VARIABLE\_ORDER   "xyt or txy"  description of the order in
                                            which the gridded data were
                                            written

  \_NETCDF\_FLIP\_VERTICAL    **none**      if present, all gridded data
                                            will be "flipped" around the
                                            vertical axis.

  \_NETCDF\_FLIP\_HORIZONTAL  **none**      if present, all gridded data
                                            will be "flipped" around the
                                            horizontal axis

  \_NETCDF\_MAKE\_LOCAL\_ARCH               
  IVE                                       

  \_PROJECTION\_DEFINITION                  PROJ.4 string describing the
                                            geographic projection of the
                                            dataset

  \_MINIMUM\_ALLOWED\_VALUE   *real value*  ceiling to be applied to the
                                            data; data above this value
                                            will be reset to this amount

  \_MAXIMUM\_ALLOWED\_VALUE   *real value*  floor to be applied to the
                                            data; data beneath this value
                                            will be reset to this amount

  \_MISSING\_VALUES\_CODE     *real or      value
                              integer       
                              value*        

  \_MISSING\_VALUES\_OPERATOR "&lt;",       
                              "&lt;=",      
                              "&gt;",       
                              "&gt;="       

  \_MISSING\_VALUES\_ACTION   "mean" or     "mean" will substitute the
                              "zero"        mean value calculated over the
                                            remaining valid cells; "zero"
                                            will substitute a value of 0.0
                                            in place of missing values
  ------------------------------------------------------------------------

Appendix 3. Parameter Definitions {#appendix_3_definition_of_SWB2_parameters}
=================================

This section provides a complete list of all required and optional
parameters used by SWB.

Depletion Fraction
------------------

The soil water depletion fraction is defined in FAO-56
[@allen_crop_1998] as the average fraction of total available water
(TAW) that can be removed from the root zone without causing plant
stress. Soil water content ($\theta$) is assumed to be bounded by the
field capacity ($\theta_{s}$) at a maximum, and by the permanent wilting
point ($\theta_{wp}$) at a minimum. Plant evapotranspiration will
proceed at maximum values until soil water depletion exceeds the readily
availible water (RAW) content for the crop under consideration. As
depletion of soil water continues beyond the readily available water
content, plant evapotranspiration is reduced until it reaches zero as
the soil water content reaches field capacity.

![Figure placeholder:
Parameters\_\_Depletion\_Fraction.png](../images/Parameters__Depletion_Fraction.png)

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo
ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis
dis parturient montes, nascetur ridiculus mus. Donec quam felis,
ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa
quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget,
arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo.
Nullam dictum felis eu pede mollis pretium.

Appendix 4. Example of control file setting selection for a new climate data source. {#appendix_4_example_new_climate_data_source}
====================================================================================

New sources of gridded climate data are continuing to come online.
Inevitably, these new climate data sources will require that the SWB
control file directives be modified in order to work properly. This
example shows the process used to configure SWB control file parameters
for a set of downscaled climate model results produced by a consortium
of agencies including USGS, BLM, NCAR, U.S. Army Corps. of Engineers,
and others (Brekke and others, 2013), available here:

http://gdo-dcp.ucllnl.org/downscaled\_cmip\_projections/dcpInterface.html.

For this example, we downloaded model results for a single climate
scenario, for a subset of the national grid. As will be shown later,
sometimes the tools employed in creating the data subset end up changing
the output NetCDF file in a way that prevents it from being used with
SWB. Once the necessary NetCDF files have been downloaded to a local
hard drive, the metadata they contain should be examined. A simple
command-line tool that can accomplish this is called “ncdump”,
distributed by Unidata. As of February, 2016, it is available as part of
the NetCDF C library for Windows, which may be obtained here:
http://www.unidata.ucar.edu/software/netcdf/docs/winbin.html. Once the
library is installed, ncdump may be run at the command line to extract
the metadata.

    >C:\”Program Files\netCDF 4.4.0”\bin\ncdump –h Extraction_pr.nc

The output from running this command is shown below.

    netcdf Extraction_pr {
    dimensions:
            longitude = 129 ;
            latitude = 64 ;
            time = 2557 ;
            projection = UNLIMITED ; // (1 currently)
    variables:
            float longitude(longitude) ;
                    longitude:standard_name = "longitude" ;
                    longitude:long_name = "Longitude" ;
                    longitude:units = "degrees_east" ;
                    longitude:axis = "X" ;
            float latitude(latitude) ;
                    latitude:standard_name = "latitude" ;
                    latitude:long_name = "Latitude" ;
                    latitude:units = "degrees_north" ;
                    latitude:axis = "Y" ;
            double time(time) ;
                    time:standard_name = "time" ;
                    time:long_name = "time" ;
                    time:units = "days since 1950-01-01 00:00:00" ;
                    time:calendar = "standard" ;
            float pr(projection, time, latitude, longitude) ;
                    pr:standard_name = "precipitation_flux" ;
                    pr:long_name = "Precipitation" ;
                    pr:units = "mm/d" ;
                    pr:_FillValue = 1.e+020f ;
                    pr:missing_value = 1.e+020f ;
                    pr:typeConversion_op_ncl = "double converted to float" ;
                    pr:cell_methods = "time: mean" ;
                    pr:interp_method = "conserve_order1" ;
                    pr:original_units = "kg/m2/s" ;
                    pr:original_name = "precip" ;
                    pr:associated_files = "baseURL: http://cmip-pcmdi.llnl.gov/CMIP5/dataLocation areacella: areacella_fx_GFDL-CM3_rcp26_r0i0p0.nc" ;
                    pr:time = 38716.5 ;

    // global attributes:
                    :CDI = "Climate Data Interface version 1.6.2 (http://code.zmaw.de/projects/cdi)" ;
                    :Conventions = "CF-1.4" ;
                    :history = "12/2014 corrected the historical bias in the mean" ;
                    :institution = "NOAA GFDL(201 Forrestal Rd, Princeton, NJ, 08540)" ;
                    :institute_id = "NOAA GFDL" ;
                    :model_id = "GFDL-CM3" ;
                    :frequency = "day" ;
                    :experiment = "RCP2.6" ;
                    :experiment_id = "rcp26" ;
                    :parent_experiment_id = "historical" ;
                    :parent_experiment_rip = "r1i1p1" ;
                    :creation_date = "Mon Sep 10 22:41:18 PDT 2012" ;
                    :references = "Daily BC method: modified version of Maurer EP, Hidalgo HG, Das T, Dettinger MD, Cayan DR, 2010, Hydrol Earth Syst Sci 14:1125-1138\n",
                            "CA method: Hidalgo HG, Dettinger MD, Cayan DR, 2008, California Energy Commission technical report CEC-500-2007-123\n",
                            "Reference period obs: updated version of Maurer EP, Wood AW, Adam JC, Lettenmaier DP, Nijssen B, 2002, J Climate 15(22):3237ΓÇô3251, \n",
                            "provided via http://www.engr.scu.edu/~emaurer/gridded_obs/index_gridded_obs.html" ;
                    :contacts = "Bridget Thrasher: bridget@climateanalyticsgroup.org or Ed Maurer: emaurer@scu.edu" ;
                    :documentation = "http://gdo-dcp.ucllnl.org" ;
                    :NCO = "4.0.8" ;
                    :CDO = "Climate Data Operators version 1.6.2 (http://code.zmaw.de/projects/cdo)" ;
                    :Projections = "gfdl-cm3.1.rcp26, " ;
    }

There is a lot of useful information in this particular set of metadata.
NetCDF files of this sort typically have a number of dimensions and
variables defined in the first part of the file description. In this
example, four dimensions are defined: longitude, latitude, time, and
projection. In addition, the file contains four variables: longitude,
latitude, time, and pr (precipitation). Three of the variable names are
also names of dimensions. The dimension “longitude” in this case refers
to a set of index values ranging from 0 to 128. The variable “longitude”
contains the actual longitudinal value associated with each of the
indices contained in the longitude dimension.

    C:\"Program Files\netCDF 4.4.0"\bin\ncdump -v longitude Extraction_pr.nc

Running ncdump with the “-v” option and a variable name returns a list
of all variable values:

    longitude = 251.9375, 252.0625, 252.1875, 252.3125, 252.4375, 252.5625,
       252.6875, 252.8125, 252.9375, 253.0625, 253.1875, 253.3125, 253.4375,
       253.5625, 253.6875, 253.8125, 253.9375, 254.0625, 254.1875, 254.3125,
       254.4375, 254.5625, 254.6875, 254.8125, 254.9375, 255.0625, 255.1875,
       255.3125, 255.4375, 255.5625, 255.6875, 255.8125, 255.9375, 256.0625,
       256.1875, 256.3125, 256.4375, 256.5625, 256.6875, 256.8125, 256.9375,
       257.0625, 257.1875, 257.3125, 257.4375, 257.5625, 257.6875, 257.8125,
       257.9375, 258.0625, 258.1875, 258.3125, 258.4375, 258.5625, 258.6875,
       258.8125, 258.9375, 259.0625, 259.1875, 259.3125, 259.4375, 259.5625,
       259.6875, 259.8125, 259.9375, 260.0625, 260.1875, 260.3125, 260.4375,
       260.5625, 260.6875, 260.8125, 260.9375, 261.0625, 261.1875, 261.3125,
       261.4375, 261.5625, 261.6875, 261.8125, 261.9375, 262.0625, 262.1875,
       262.3125, 262.4375, 262.5625, 262.6875, 262.8125, 262.9375, 263.0625,
       263.1875, 263.3125, 263.4375, 263.5625, 263.6875, 263.8125, 263.9375,
       264.0625, 264.1875, 264.3125, 264.4375, 264.5625, 264.6875, 264.8125,
       264.9375, 265.0625, 265.1875, 265.3125, 265.4375, 265.5625, 265.6875,
       265.8125, 265.9375, 266.0625, 266.1875, 266.3125, 266.4375, 266.5625,
       266.6875, 266.8125, 266.9375, 267.0625, 267.1875, 267.3125, 267.4375,
       267.5625, 267.6875, 267.8125, 267.9375 ;

An interesting this to note about the values of longitude is that they
seem unusual relative to the longitudes we are used to working with in
North America. Indeed, this example dataset is centered on the state of
Nebraska, USA; we commonly would see the longitude values range from
about 108° to 93° West longitude, perhaps expressed as -108° to -93°.
Many of the downscaled climate model datasets refer to longitude as
ranging from 0° to 360°, with the longitude of 0°/360° centered on the
parallel running through Greenwich, England. If we subtract 360° from
the longitude values above, the range looks more familiar:
251°-360°=-108°; 267°-360°=-93°. Presumably the reason for defining
longitudes this way is because it is easier to have model grid for which
all longitude values are greater than zero!

One item we need to look at first is the organization of the data of
interest on the disk file. SWB expects climate data files to be arranged
in such a way that the data may be accessed by referencing a specific
datetime, y-coordinate, and x-coordinate value. The precipitation
variable we are interested in is dimensioned as follows:

    float pr(projection, time, latitude, longitude) ;

SWB is written under the assumption that the variable of interest will
be referenced by just three dimensions: time, x, and y. The fourth
dimension listed above, projection, was added apparently to allow
results for more than one climate emissions scenario to be stored in a
single NetCDF file. In order to use this file with SWB we must get rid
of this fourth dimension. To remove the fourth dimension, we can use a
third-party tool called NCO, NetCDF Climate Operators, to calculate an
“average” over the fourth dimension. Because there is only a single
projection contained in the file, the resulting file will be the same as
the input file *without* the projection dimension. NCO as available at:
http://nco.sourceforge.net/. As of February, 2016, a Windows executable
for NCO may be found here:
http://nco.sourceforge.net/src/nco-4.5.4.windows.mvs.exe.

The NCO package is not overly user friendly. Luckily, it has a helpful
discussion page, which suggests that to eliminate a “degenerate”
dimension (a dimension with only a single value), the “averaging” tool
may be used:

    ncwa -a projection Extraction_pr.nc Extraction_pr_3d.nc

ncwa stands for “NetCDF Weighted Averager”. The “-a” flag allows one to
specify a dimension over which to average (“projection”). The last two
entries are the input and output filenames, respectively. Once this
command-line utility is run, the NetCDF files are rendered usable by
SWB. With all of the metadata available we can finally generate the SWB
control file statements to make this file work with SWB:

    # 001: specify the filename containing precipitation data
    PRECIPITATION NETCDF Extraction_pr_3D.nc

    # 002: define PRECIPITATION projection and NetCDF variable names
    PRECIPITATION_GRID_PROJECTION_DEFINITION +proj=lonlat +ellps=GRS80 +datum=NAD83 +lon_wrap=180 +no_defs
    NETCDF_PRECIP_X_VAR longitude
    NETCDF_PRECIP_Y_VAR latitude
    NETCDF_PRECIP_Z_VAR pr

    # 003: define PRECIPITATION missing values action
    PRECIPITATION_MISSING_VALUES_CODE 1.0E+20
    PRECIPITATION_MISSING_VALUES_OPERATOR >=
    PRECIPITATION_MISSING_VALUES_ACTION ZERO

    # 004: PRECIPITATION is given in mm/day; need to convert to inches/day
    PRECIPITATION_SCALE_FACTOR 0.03937008

The first line of the control file snippet above specifies the name of
the NetCDF file that contains the precipitation data we wish to use.

The commands under group 002 define the geographic projection and
specify the variable names. In this case, the exact projection of the
data is unknown; in any event climate models rarely seem to reflect
anything other than a global (unprojected) coordinate system. It is not
clear from the metadata what datum was used in defining the latitude and
longitude. We’ll guess NAD83 with an ellipsoid specified by GRS80.
Downscaled climate model cells are generally far larger than the error
we would incur by selecting the wrong datum and ellipsoid. Note that
this dataset requires the “+lon\_wrap=180” addition to the projection
definition. This is required in order to convert the longitude values to
+/- 180°. The variable names are found in the metadata and must be
supplied in the control file in order for SWB to find them: x=&gt;
“longitude”, y=&gt;”latitude”, z=”pr”.

The commands under group 003 specify what actions SWB should take in the
event it encounters missing values within the file. The first line
defines the numeric value associated with a missing value. The second
line defines the operator, which can be one of “&lt;”, “&lt;=”, “&gt;”,
“&gt;=”. The third line specifies what should be done with the missing
value. In this case, we’ve specified that any values &gt;= 1.0E+20 will
be treated as 0.0.

The command under group 004 informs SWB how the values in the file are
to be converted to inches per day. The PRECIPITATION\_SCALE\_FACTOR of
0.03937 = 1.0 / 25.4, or one over the number of millimeters per inch.

Appendix 5. Earlier implementations of soil moisture retention relations. {#appendix_5_earlier_SM_relations}
=========================================================================

SWB has evolved over time, incorporating additional and improved process
formulations whenever possible. This is how SWB and SWB 2.0 ended up
with no less than three ways by which to update soil moisture values by
means of the Thornthwaite-Mather approach
[@thornthwaite_approach_1948; @thornthwaite_water_1955; @thornthwaite_instructions_1957].
The direct solution method is now the method of choice; the other two
methods are included here to document the functioning of earlier
versions of the code.

##### Table Values

The original version of the SWB model reads in digitized versions of the
Thornthwaite-Mather soil-moisture retention tables and follows the
original instructions for calculation faithfully. In order to track
changes in soil moisture, several intermediary values are calculated,
including precipitation minus potential evapotranspiration ($P-PE$),
accumulated potential water loss ($APWL$), actual evapotranspiration,
soil-moisture surplus, and soil-moisture deficit. These terms are
described below.

*P minus PE* $(P-PE)$. The first step in calculating a new soil moisture
value for any given grid cell is to subtract potential
evapotranspiration from the daily precipitation ($P-PE$). Negative
values of $P-PE$ represent a potential deficiency of water, whereas
positive $P-PE$ values represent a potential surplus of water.

*Soil moisture*, $\Delta\,soil\,moisture$. The soil-moisture term
represents the amount of water held in soil storage for a given grid
cell. Soil moisture has an upper bound that corresponds to the soil's
maximum water-holding capacity (roughly equivalent to the field
capacity); soil moisture has a lower bound that corresponds to the
soil's permanent wilting point.

When $P-PE$ is positive, the new soil-moisture value is found by adding
this $P-PE$ term directly to the preceding soil-moisture value. If the
new soil-moisture value is still below the maximum water-holding
capacity, the Thornthwaite-Mather soil-moisture tables are consulted to
back-calculate a new, reduced accumulated potential water-loss value. If
the new soil-moisture value exceeds the maximum water-holding capacity,
the soil-moisture value is capped at the value of the maximum
water-holding capacity, the excess moisture is converted to recharge,
and the accumulated potential water-loss term is reset to zero.

When $P-PE$ is negative, the new soil-moisture term is found by looking
up the soil-moisture value associated with the current accumulated
potential water-loss value in the Thornthwaite-Mather tables.

*Actual ET*. When $P-PE$ is positive, the actual evapotranspiration
equals the potential evapotranspiration. When $P-PE$ is negative, the
actual evapotranspiration is equal only to the amount of water that can
be extracted from the soil ($\Delta$ soil moisture).

During the course of a model run, the soil layer is considered to be in
one of three states:

  --------------------------------------------------------------------------
  $P - PE                Soil Status    $SM_{t}$ $APWL_{ $AET_{       Excess
        $                                            t}$    t}$   (potential
                                                                   recharge)
  ------- -------------------------- ----------- ------- ------ ------------
   &lt; 0                     drying SM from T-M $APWL_{ $SM_{t          0.0
                                          tables t-1} +  -1} -  
                                                 (P-PE)$ SM_{t} 
                                                              $ 

   &gt; 0              wetting *and* $SM_{t-1} +  $APWL$   $PE$          0.0
          \n $SM_{t-1} + (P - PE) <      (P-PE)$    from        
                           SM_{max}$              tables        

   &gt; 0              wetting *and*  $SM_{max}$     0.0   $PE$ $SM_{t-1} + 
          \n $SM_{t-1} + (P - PE) >                             (P - PE) - S
                           SM_{max}$                                M_{max}$
  --------------------------------------------------------------------------

  : Soil moisture states. {\#tbl:sm\_states}

##### Fitted Equations

The amount of computing involved in negotiating the lookup tables and
interpolating a result was significant enough to warrant generalization;
in addition, small roundoff errors were accumulated in the course of
repeated conversions between accumulated potential water loss and the
corresponding soil moisture values. In order to avoid the use of lookup
tables altogether a generalized equation was developed, using the
Thornthwaite and Mather [-@thornthwaite_instructions_1957] table values
as the basis for the equations.

Two equations were fitted and implemented in the SWB 1.0 code. The first
equation relates the current soil moisture to an equivalent accumulated
potential water loss +@eq:SM_fitted:

$$soil\,moisture = {10^{({{\log }_{10}}\theta }}^{ - APWL\, \cdot \,0.4788{\kern 1pt} {\theta ^{ - 1.037}})}$$
{\#eq:SM\_fitted}

The second equation is used to back-calculate the equivalent accumulated
potential water loss value for a given soil moisture amount:

$$APWL = {\log_{10}}({\theta_{fc}}) - {\log_{10}}\left( {\frac{\theta }{{\left( {0.4788\,{\theta_{fc}}^{ - 1.037}} \right)}}} \right)$$
{\#eq:APWL\_fitted}
