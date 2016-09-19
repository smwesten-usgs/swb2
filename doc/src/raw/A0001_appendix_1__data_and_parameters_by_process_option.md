# Appendix 1. Data, Parameter, and Control File Requirements by Process Option {#appendix\_1\_data\_and\_param\_requirements}

[TOC]

SWB 2.0 was designed so that parameters may be supplied to the model on an as-needed basis. The original SWB 1.0 input requirements grew ever larger as more possible calculations methods were added. For this reason, we created a flexible table-based format that allows for parameters to be supplied in any order convenient to the user.

For table-based parameter entry, the crucial detail is to enter the proper parameter name in the header of the file. Case does not matter for these heading entries: "DEPLETION_FRACTION" will work as well as "depletion_fraction" or "Depletion_Fraction". For some modules, multiple heading values are recognized as equivalent to one another. For example, to identify a particular table column as holding landuse / land cover codes, SWB 2.0 recognizes any of the following: "LU_Code", "Landuse_Code", or "Landuse Lookup Code". Note also that SWB will fill any blank spaces in the header with underscores before evaluating the values therein. Thus, "Landuse Lookup Code" will be treated as "Landuse_Lookup_Code" by SWB. The idea is that whatever identification makes sense to the modeler should be recognized by SWB and acted upon.

This section describes in detail the data, parameter, and control file requirements for each module currently implemented in the SWB 2.0 code.

## Process: Actual Evapotranspiration

Three actual ET modules are available. The actual ET modules are responsible for determining how much soil moisture can be extracted given the climate and soil moisture conditions being simulated. The Thornthwaite-Mather actual ET modules should provide equivalent results and should provide identical performance.

### FAO-56


*Control File Entry*
```
SOIL_MOISTURE_METHOD  FAO-56
```

-------------------------------------------------------------------
 Parameter                    Allowable Lookup
 Description                   Table headers
---------------------------- --------------------------------------
 Depletion fraction           Depletion_Fraction
-------------------------------------------------------------------

### Thornthwaite-Mather

*Control File Entry*
```
SOIL_MOISTURE_METHOD  THORNTHWAITE-MATHER
```

-------------------------------------------------------------------
  Parameter                    Allowable Lookup
 Description                   Table headers
---------------------------- --------------------------------------
   *                            *
-------------------------------------------------------------------   

### Thornthwaite-Mather Equations

*Control File Entry*
```
SOIL_MOISTURE_METHOD  THORNTHWAITE-MATHER_EQUATIONS
                     -or-
SOIL_MOISTURE_METHOD THORNTHWAITE_MATHER_EQUATIONS
```

-------------------------------------------------------------------
     Parameter                    Allowable Lookup
    Description                   Table headers
---------------------------- --------------------------------------
   *                            *
-------------------------------------------------------------------

## Process: Available Water Capacity / Available Water Content



### Gridded Values

*Control File Entry*
```
AVAILABLE_WATER_CONTENT GRIDDED
           -or-
AVAILABLE_WATER_CAPACITY GRIDDED
```


-------------------------------------------------------------------
      Parameter                    Allowable Lookup
     Description                    Table headers
---------------------------- --------------------------------------
   *                            *
-------------------------------------------------------------------

### Table Values

*Control File Entry*
```
AVAILABLE_WATER_CONTENT TABLE
            -or-
AVAILABLE_WATER_CAPACITY TABLE
```

-------------------------------------------------------------------
      Parameter                    Allowable Lookup
     Description                    Table headers
---------------------------- --------------------------------------
   Available water capacity,    AWC
   in inches per foot
-------------------------------------------------------------------


### Table Values, Depth-Integrated

*Control File Entry*
```
AVAILABLE_WATER_CONTENT DEPTH_INTEGRATED
                  -or-
AVAILABLE_WATER_CAPACITY DEPTH_INTEGRATED
```

------------------------------------------------------------------------------------------------
     Parameter                    Allowable Lookup                               Note
    Description                   Table headers
---------------------------- -------------------------------------- ----------------------------
    Landuse code                LU_Code
                                Landuse_Code
                                Landuse_Lookup_Code

    Soils code                  Soils_Code
                                Soils_Lookup_Code
                                Soil_Code
                                Soils_Lookup_Code

    Soils horizon               Soils_Horizon
                                Soils_Horizon_Number
                                Soil_Horizon
                                Soil_Horizon_Number

    Soils top depth             Soils_Top_Depth
                                Soil_Top_Depth
                                Soils_Z_Top
                                Soils_Top_of_Horizon

    Soils bottom depth          Soils_Bottom_Depth
                                Soil_Bottom_Depth
                                Soils_Z_Bottom
                                Soils_Bottom_of_Horizon

    Soils component             Soils_Component
                                Soils_Component_Number
                                Soil_Component
                                Soil_Component_Number

    Soils component fraction    Soils_Component_Fraction
                                Soil_Component_Fraction

    Available water content     Soils_Available_Water_Content
                                Soils_AWC
                                Soil_Available_Water_Content
                                Soil_AWC
                                Available_Water_Content
                                AWC
------------------------------------------------------------------------------------------------

## Process: Runoff

### Soil Conservation Service Curve Number

*Control File Entry*
```
RUNOFF_METHOD CURVE_NUMBER
          -or-
RUNOFF_METHOD C-N          
```


### Monthly Runoff Ratio

*Control File Entry*
```
RUNOFF_METHOD RUNOFF_RATIO
          -or-
RUNOFF_METHOD MONTHLY_GRID
```


## Process: Crop Coefficients (FAO-56)

This module handles all processes associated with simulating the growth and senescence of plants and their effect on soil moisture. There are three ways in which crop coefficient curves may be specified; each landuse code may use one of the three methods. The three methods that may be used to specify the crop coefficient curves are:

1. Time-based: specified in terms of the number of days that have elapsed since planting;
2. Growing degree-day based: specified in terms of the number of growing degree-days that have passed since planting;
3. Monthly: specific crop coefficients may be supplied with a single value per month.



*Control File Entry*
```
CROP_COEFFICIENT_METHOD  FAO-56
```

------------------------------------------------------------------------------------------------
     Parameter                    Allowable Lookup                               Note
    Description                   Table headers
---------------------------- -------------------------------------- ----------------------------
  Landuse code                  LU_Code
                                Landuse_Code
                                Landuse_Lookup_Code

                                Planting_date

  Inflection points on the     L_ini                                  Day values may be specified
  Kcb curve, defined in terms  L_dev                                  as the integer number of
  of time (days) elapsed        L_mid                                  days elapsed since planting,
  since the start of plant     L_late                                 *or* may be specified as a
  growth.                      L_fallow                               date in mm/dd format.

  Inflection points on the      GDD_plant                                
  Kcb curve, defined in terms   GDD_ini                                
  of growing degree-days.       GDD_dev                                
                                GDD_mid                                
                                GDD_late

  These values are typically    Kcb_ini                                
  used along with the GDD       Kcb_mid                                
  or day length values to       Kcb_end                                
  define a simple K_{cb}        Kcb_min
  curve.

  Mean plant height is used      Mean_plant_height
  to determine how much bare
  soil might be exposed to
  evaporation at various
  growth stages.

  Monthly values to define      Kcb_Jan
  the Kcb curve more            Kcb_Feb
  completely. This may be       Kcb_Mar
  useful for a crop that        Kcb_Apr
  has multiple plantings        Kcb_May
  and harvests in the           Kcb_Jun
  course of a year.             Kcb_Jul
                                Kcb_Aug
                                Kcb_Sep
                                Kcb_Oct
                                Kcb_Nov
                                Kcb_Dec
------------------------------------------------------------------------------------------------            

## Process: Direct Additions

### Direct Additions to Recharge

### Direct Additions to Soil Moisture

## Process: Potential evapotranspiration

### Gridded

*Control File Entry*
```
POTENTIAL_EVAPOTRANSPIRATION_METHOD GRIDDED
                     -or-
REFERENCE_EVAPOTRANSPIRATION_METHOD GRIDDED

                     ...

POTENTIAL_ET ARC_GRID              Common_Data/input/gr0_in_month_ascii/gr0_in_%b.asc
POTENTIAL_ET_PROJECTION_DEFINITION +proj=lonlat +datum=WGS84 +no_defs                     
```

### Jensen-Haise

*Control File Entry*
```
POTENTIAL_EVAPOTRANSPIRATION_METHOD JENSEN-HAISE
                     -or-
REFERENCE_EVAPOTRANSPIRATION_METHOD JENSEN-HAISE
                     -or-
POTENTIAL_EVAPOTRANSPIRATION_METHOD JENSEN_HAISE
                     -or-
REFERENCE_EVAPOTRANSPIRATION_METHOD JENSEN_HAISE
```

### Hargreaves-Samani

SWB 1.0 required that the northern and southern latitudes of the project area be supplied by the user. Since SWB 2.0 requires that a project grid is established along with a PROJ.4 string, the northern and southern latitudes can be calculated by SWB 2.0; the user need not enter these values in the control file.

*Control File Entry*
```
POTENTIAL_EVAPOTRANSPIRATION_METHOD HARGREAVES_SAMANI
                     -or-
REFERENCE_EVAPOTRANSPIRATION_METHOD HARGREAVES_SAMANI
                     -or-
POTENTIAL_EVAPOTRANSPIRATION_METHOD HARGREAVES-SAMANI
                     -or-
REFERENCE_EVAPOTRANSPIRATION_METHOD HARGREAVES-SAMANI                     
```


## Process: Fog Interception

## Process Support: Growing Degree-Day

*Control File Entry*
```
*
```


## Process Support: Growing Season

*Control File Entry*
```
*
```

## Process: Interception

### Bucket

*Control File Entry*
```
INTERCEPTION_METHOD BUCKET
```

### Horton

*Control File Entry*
```
INTERCEPTION_METHOD HORTON
```

### Gash
*Control File Entry*
```
INTERCEPTION_METHOD GASH
```

## Process: Irrigation

*Control File Entry*
```
IRRIGATION_METHOD FAO-56
```

## Process Support: Precipitation

### Table Values

*Control File Entry*
```
PRECIPITATION_METHOD TABLE
```


### Gridded Values

*Control File Entry*
```
PRECIPITATION_METHOD NORMAL
         -or-
PRECIPITATION_METHOD GRIDDED
```

### Method of Fragments

```
PRECIPITATION_METHOD METHOD_OF_FRAGMENTS
```


## Process: Storm Drain Capture

------------------------------------------------------------------------------------------------
     Parameter                    Allowable Lookup                               Note
    Description                   Table headers
---------------------------- -------------------------------------- ----------------------------
  Landuse code                  LU_Code
                                Landuse_Code
                                Landuse_Lookup_Code

                                Storm_drain_capture
                                Storm_drain_capture_fraction
