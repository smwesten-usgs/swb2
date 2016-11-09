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

### Project Setup and Grid Specification

`GRID *NX* *NY* *X0* *Y0* *Cell_size*`

-or-

`GRID *NX* *NY* *X0* *Y0* *X1* *Y1* *Cell_size*`

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
