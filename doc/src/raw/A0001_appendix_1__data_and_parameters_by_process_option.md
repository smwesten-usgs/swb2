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

                                L_shift
                                L_ini
                                L_dev
                                L_mid
                                L_late
                                L_fallow

                                GDD_plant
                                GDD_ini
                                GDD_dev
                                GDD_mid
                                GDD_late

                                Kcb_ini
                                Kcb_mid
                                Kcb_end
                                Kcb_min

                                Mean_plant_height

                                Kcb_Jan                   
                                Kcb_Feb
                                Kcb_Mar
                                Kcb_Apr
                                Kcb_May
                                Kcb_Jun
                                Kcb_Jul
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

### Jensen-Haise

### Hargreaves-Samani

## Process: Fog Interception

## Process Support: Growing Degree-Day

## Process Support: Growing Season

## Process: Interception

### Bucket

### Horton

### Gash

## Process: Irrigation

## Process Support: Precipitation

### Table Values

### Gridded Values

### Method of Fragments

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
