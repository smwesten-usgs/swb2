## Control File Directives {#appendix_2_control_file_directives_original_swb}

[TOC]

This section provides a complete list of the control file statements understood by SWB, version 1.x.

### Required control file entries ###

#### GRID ####

*Specify the horizontal extent and gridcell resolution for a SWB run.*

The ```GRID``` directive should be in the first line of the control file. Units of meters are assumed; an optional control-file statement (```GRID_LENGTH_UNITS```) may be used to specify that units of feet are used instead. If any subsequent input grid fails to match the specified model domain exactly, the program will end.

*Syntax:*

```
GRID nx ny x0 y0 x1 y1 cellsize
```

-or-  

```
GRID nx ny X0 Y0 cellsize
```

where

```
nx
```
 is the number of columns in the grid domain,
```
ny
```
 is the number of rows in the grid domain,
```x0, y0``` are the coordinates for the lower, left-hand corner of the model domain
```x1, y1``` are the coordinates for the upper, right-hand corner of the model domain, and
```cellsize``` is the grid cell size in meters or feet.

*Example:*

<div custom-style="Source_Code">GRID 344 320 528093. 274821.  100.0</div>

In the example shown above, nx (344) is the number of grid cells in the x direction, ny (320) is the number of grid cells in the y direction, X0 and Y0 (528093, 274821) are the coordinates for the lower left-hand corner of the grid.

#### GROWING_SEASON ####

*Specify the start and end of the growing season; influences interception and antecedent runoff conditions.*

This flag controls when the growing season is considered to begin and end (expressed as the day of year) and whether or not the problem is in the Northern hemisphere (possible values: ```TRUE``` or ```FALSE```).

*Example*

```GROWING_SEASON 133 268 TRUE```

Alternatively, the growing season statement may be specified as ```GROWING_SEASON GDD```, which will cause SWB to track the growing degree days for each gridcell. When using this option, the growing season is considered active when the number of growing degree-days for the cell exceed 90. The growing season is considered to end when the minimum daily air temperature falls below 28.5 degree Fahrenheit. One drawback to this approach is that the growing season status may toggle several times between growing and nongrowing season in early spring if a string of warm days is followed by a single cold night, for example. A benefit to this approach is that each cell is allowed to toggle between nongrowing and growing season as the local air temperatures dictate.

This option affects which interception value (growing season or dormant season) and which antecedent runoff condition thresholds apply to a grid cell.

#### PRECIPITATION ####

*Specify one or more files that contain precipitation data.*

This control statement specifies a file type and a file prefix that the SWB code will associate with daily _gridded_ precipitation values.

*Syntax:*

```PRECIPITATION file_format location```

where

```file_format``` may be ```ARC_GRID```, ```SURFER```, or ```NETCDF```, and
```location``` is the relative or absolute path and name of the relevant file or file template.

*Example--Arc ASCII daily grid:*

```PRECIPITATION ARC_GRID climate\precip\PRCP_%m_%d_%y.asc```


*Example -- Surfer daily grid:*

```PRECIPITATION SURFER climate\precip\PRCP_%m_%d_%y.asc```

#### TEMPERATURE ####

*Specify one or more files containing minimum and maximum daily air temperature.*

This control statement either indicates that temperature at a single station is to be used or, alternatively, specifies a file type and file prefixes associated with daily gridded maximum and minimum temperature values. The required syntax for gridded temperature data is:

```TEMPERATURE``` *FILE_FORMAT TMAX template TMIN template*

*Example -- single station:*

```TEMPERATURE SINGLE_STATION```

--or--

*Example -- Arc ASCII daily grid:*

```TEMPERATURE ARC_GRID climate\precip\TMAX_%m_%d_%y.asc climate\precip\TMIN_%m_%d_%y.asc```

--or--

*Example - Surfer daily grid:*

```TEMPERATURE SURFER climate\precip\TMAX_%m_%d_%y.asc climate\precip\TMIN_%m_%d_%y.asc```

--or--

*Example - NetCDF daily grids encapsulated in an annual file:*

```TEMPERATURE NETCDF climate\precip\TMAX_%y.asc climate\precip\TMIN_y.asc```

Note that the file template may specify one or more relative subdirectory names separated by backslashes. Temperature grids are expected to be of data type real, and in units of degrees Fahrenheit.

#### FLOW_DIRECTION ####

*Specify a D8 flow-direction grid.*

Arc ASCII or Surfer grid of D8 flow directions. The flow-direction grid must be of data type integer.

*Example:*

```FLOW_DIRECTION ARC_GRID input\new_fl_dir.asc```

#### SOIL_GROUP ####

*Specify a soil-group grid.*

Arc ASCII or Surfer grid of hydrologic soil groups; values must correspond to the hydrologic soil groups contained in the land-use lookup table. The soil group must be of data type integer.
*Example: SOIL_GROUP ARC_GRID input\soil_hyd_grp.asc

#### LAND_USE ####

*Specify a landuse grid.*

Arc ASCII or Surfer grid of land-use\land-cover classification values; the values must correspond to the land-use\land-cover values contained in the land-use lookup table. The land-use grid must be of data type integer.

*Example:*

```LAND_USE ARC_GRID input\land_use.asc```

#### LAND_USE_LOOKUP_TABLE ####

Specify the name and location of the landuse lookup table.

The name of the land-use lookup table must be specified within the control file.

*Example:*

```LAND_USE_LOOKUP_TABLE std_input\LU_lookup.txt```
``````
#### WATER_CAPACITY ####

*Specify a soil-water capacity grid.*

 The model uses soil information, together with land-cover information, to calculate surface runoff and assign a maximum soil-moisture holding capacity to each grid cell ($\theta_{fc}$). The soil-group grid may be used along with the values included in table 7 to produce this grid. Available water capacity is expected to be of data type real and is expressed in units of inches of water per foot of soil.

*Example: *

```WATER_CAPACITY ARC_GRID input\avail_water_cap.asc```


#### SM (Soil Moisture accounting method) ####

*Specify a soil-moisture accounting calculation option.*

 The model currently only contains one soil-moisture-accounting calculation option: Thornthwaite-Mather (1948, 1957). The Thornthwaite-Mather soil-moisture-retention tables are included in the standard table soil-moisture-retention-extended.grd.

*Example:*

```SM T-M std_input\soil-moisture-retention-extended.grd```


#### INITIAL_SOIL_MOISTURE ####

*Set initial soil moisture.*

 Initial soil moisture can be specified either as a single constant value or as a grid of values. Initial soil moisture is expressed as a percentage saturation of the available water capacity (0--100 percent). If supplied as gridded data, the initial soil-moisture grid is expected to be of data type real.

*Example--uniform constant value:*

```INITIAL_SOIL_MOISTURE CONSTANT 100```

--or--

*Example--Arc ASCII grid:*

```INITIAL_SOIL_MOISTURE ARC_GRID input\initial_pct_sm_dec_31_1999.asc```

#### INITIAL_SNOW_COVER ####

*Supply snow cover initial conditions.*

Initial snow cover can be specified either as a single constant value or as a grid of values. Initial snow cover is expressed as a water-equivalent value (inches of water). If supplied as gridded data, the initial-snow-cover grid is expected to be of data type real.

*Example--uniform constant value:*

```INITIAL_SNOW_COVER CONSTANT 0.3```

 --or--

 *Example--Arc ASCII grid:*

```INITIAL_SNOW_COVER ARC_GRID input\initial_snow_cover_dec_31_1999.asc```


#### RUNOFF ####

*Specify the method to use when calculating runoff.*

Currently, only one runoff calculation method is available: the NRCS curve number method [@cronshey_urban_1986]. This calculation method must be specified by including the letters ```C-N``` after the keyword ```RUNOFF```.

Three methods are available for the routing of surface water through the model domain. The ```ITERATIVE``` method is based on the original VBA code solution method, in which water is iteratively moved across the entire grid until all water has either infiltrated or left the grid via surface flow. The ```DOWNHILL``` method involves a pre-simulation step in which the model grid cells are sorted upslope to downslope. Runoff is calculated in a single iteration for each time step over the entire model domain, proceeding from the upstream cells to the downstream cells. There should be no difference between the two routing methods except that the ```DOWNHILL``` method executes much more quickly. The ```ITERATIVE``` solution method will be removed from future versions of the code.

Specifying the method as ```NONE``` disables routing altogether. Any cell outflow is assumed to find its way to a surface-water feature and exit the model domain. Outflow under this option is tracked as ```RUNOFF_OUTSIDE```.

The SWB code writes a binary file to disk after the first pass through the ```DOWNHILL``` iteration method. This binary file is read in for subsequent SWB model runs, eliminating the potentially time-consuming task of grid-cell sorting. Any changes to the grid extent will trigger an error message reminding the user to delete the file ```swb_routing.bin``` before rerunning the SWB code.

*Example--iterative routing; runoff calculated by means of SCS curve number:*

```RUNOFF C-N ITERATIVE```

--or--

*Example--downhill routing; runoff calculated by means of SCS curve number:*

```RUNOFF C-N DOWNHILL```

--or--

*Example--routing disabled; runoff calculated by means of SCS curve number:*

```RUNOFF C-N NONE```

#### ET (evapotranspiration method) ####

The model implements several methods for estimating potential evapotranspiration, specifically, the Thornthwaite-Mather, Jensen-Haise, Blaney-Criddle, Turc, and Hargreaves-Samani methods. Note that, given the same root-zone depths, the Thornthwaite-Mather method will produce lower estimates of potential evapotranspiration (and thus, higher estimates of recharge) than the other methods [@vorosmarty_potential_1998]. The Hargreaves-Samani method is the only one suitable for use with gridded precipitation and air-temperature data.

Examples showing the use of all possible program options for specifying an ET calculation method are given below.

*Example: Thornthwaite-Mather*

```ET T-M``` *latitude*

--or--

*Example: Jensen-Haise*

```ET J-H``` *latitude albedo* $a_s$ $b_s$

--or--

*Example: Blaney-Criddle *

```ET B-C``` *latitude*

--or--

*Example: Turc*

```ET TURC``` *latitude albedo* $a_s$ $b_s$

--or--

*Example: Hargreaves-Samani*

```ET HARGREAVES``` *southerly_latitude northerly_latitude*

Values must be entered for all specified options. In the absence of more specific information, a reasonable value of the albedo for the Jensen-Haise and Turc methods is 0.23; similarly, $a_s$ may be set to 0.25 and $b_s$ set to 0.5. The coefficients $a_s$ and $b_s$ are used in the Angstrom formula for estimation of daily solar radiation [@allen_crop_1998]. The term $a_s$ expresses the fraction of extraterrestrial radiation that reaches earth on overcast days; $b_s$ expresses the additional fraction of extraterrestrial radiation that reaches earth on clear days.

#### SOLVE / SOLVE_NO_TS_FILE ####

*Begins the actual recharge calculation.*

When using a single climate data station (in other words, no precipitation or temperature), the ```SOLVE``` statement is used. The name of the single-station climate time-series data file is required.

*Example: SOLVE bec1999v2.txt

Note that a simulation for more than 1 year is made possible by simply including additional SOLVE statements.

*Example: multiple years, single station*

```SOLVE MSN_1999.txt```
```SOLVE MSN_2000.txt```
```SOLVE MSN_2001.txt```
```--etc--```

If gridded temperature and precipitation data are supplied to the SWB model and the Hargreaves-Samani (1985) ET calculation method is selected for use, the single-station climate time-series file may be eliminated altogether. In this case, the starting and ending year of the simulation must be supplied.

*Example: multiple years, gridded climate data:*

```SOLVE_NO_TS_FILE 1983 1992```


#### EOJ ####

*End of job.*

The ```EOJ``` option stands for End-Of-Job; it triggers actions to write grids to disk, calculate statistics, and de-allocate memory.

*Example:*

```EOJ```

### Optional Control-File Entries ###

The control-file entries discussed above are the only ones required to run the SWB model. However, there are additional control-file entries available that change the method of calculation or enable additional output options. This section describes optional control-file entries that may be used.

#### ANSI_COLORS ####

*Request colored screen output.*

 If you have access to a terminal program such as rxvt, the SWB model can generate screen output with color coding for positive and negative values (possible values: TRUE\slash FALSE). The rxvt package can be installed as an option along with the Cygwin Unix emulation package (www.cygwin.com).

*Example:*

```ANSI_COLORS TRUE```

#### SUPPRESS_SCREEN_OUTPUT\SUPPRESS_DAILY_FILES\SUPPRESS_DISLIN_MESSAGES ####

In order to speed model runtimes, certain types of text messages normally printed to the screen and\slash or disk may be suppressed.

```SUPPRESS_SCREEN_OUTPUT``` will turn off the detailed mass-balance information that is normally printed to the screen for each daily timestep.

```SUPPRESS_DAILY_FILES``` will prevent detailed mass balance from being written to disk as recharge_daily_statistics.csv, recharge_annual_report.csv, and recharge_daily_report.csv.

```SUPPRESS_DISLIN_MESSAGES``` will prevent the progress messages normally generated by the DISLIN graphics library from being written to the screen.

*Example:*

```SUPPRESS_SCREEN_OUTPUT```


#### ADJUSTED_WATER_CAPACITY ####

*Override maximum water water capacity calculations with externally calculated values.*

The model will normally calculate the total available water capacity from the base soil-water-capacity grid and the land-use grid, using the rooting-depth values as specified in the land-use lookup table. When this directive is used, the adjusted water capacity may be calculated independently of the model and read in as a real-valued ASCII grid. If this is done, internal calculation of the rooting depth and resulting adjusted water capacity is disabled in the model.

*Example:*

```ADJUSTED_WATER_CAPACITY ARC_GRID input\MAX_SM_STORAGE.asc```


UPPER_LIMIT_CFGI\LOWER_LIMIT_CFGI
The upper and lower continuous-frozen-ground indices may be set with the UPPER_LIMIT_CFGI and LOWER_LIMIT_CFGI statements. As discussed elsewhere, these values define the boundaries between completely frozen soil (the upper limit) and completely unfrozen soil (the lower limit). The CFGI threshold values are expressed in degree-Celcius-days.
*Example: UPPER_LIMIT_CFGI 83 \
LOWER_LIMIT_CFGI 56

Values for \texttt{UPPER_LIMIT_CFGI and \texttt{LOWER_LIMIT_CFGI were reported as 83 and 56, respectively, by Molnau and Bissell (1983); their work was conducted in the Pacific Northwest of the United States.


INITIAL_FROZEN_GROUND_INDEX This statement sets the initial (year 1) continuous-frozen-ground index. This may be supplied as a constant (\texttt{CONSTANT) or as a gridded data set (\texttt{ARC_GRID or \texttt{SURFER).
*Example: INITIAL_FROZEN_GROUND_INDEX CONSTANT 100.0
--or--
*Example: INITIAL_FROZEN_GROUND_INDEX ARC_GRID input\INIT_CFGI.asc


INITIAL_ABSTRACTION_METHOD
The method for calculating the initial abstraction within the NRCS curve number method may be specified in one of two ways:
1, TR--55: Ia is assumed equal to 0.2 S,
2, Woodward and others (2003): Ia is assumed equal to 0.05 S.

If the Hawkins method is used, curve numbers are adjusted as given in equation 9 of \citet{woodward_runoff_2003. The overall effect should be to increase runoff for smaller precipitation events. This method has been suggested to be more appropriate to long-term simulation model applications.

*Example -- original TR-55 method: INITIAL_ABSTRACTION_METHOD TR55
--or--
*Example -- Hawkins modified method: INITIAL_ABSTRACTION_METHOD HAWKINS


OUTPUT_FORMAT (file format for gridded data output)
This option allows the user to choose the format of grid output. Currently two formats are supported: ESRI ASCII Grid, and Golden Software Surfer. The default is \texttt{ARC_GRID.
*Example--Arc Grid output: OUTPUT_FORMAT ARC_GRID
--or--
*Example--Surfer format: OUTPUT_FORMAT SURFER


OUTPUT_GRID_PREFIX
This option sets the output grid filename prefix. If no value is supplied, output file names will begin with an underscore (_).

*Example: OUTPUT_GRID_PREFIX BlkErth


OUTPUT_GRID_SUFFIX
This option sets the output grid filename suffix. The default value is asc.
*Example: OUTPUT_GRID_SUFFIX txt


OUTPUT_OPTIONS
The code will write out gridded data files or image files (portable network graphics, Adobe Portable Document Format, or bitmap) for any of 24 internal and state variables simulated in the model (see table 11). The required syntax of this option is

* OUTPUT_OPTIONS variable_name daily_output monthly_output annual_output

The list of valid output types is \texttt{NONE, \texttt{GRAPH, \texttt{GRID, or \texttt{BOTH.

For example, if one wished to produce output for actual evapotranspiration, the following would yield no daily output, graphical monthly output, and both gridded and graphical annual output.
*Example: OUTPUT_OPTIONS ACT_ET NONE GRAPH BOTH

Note that the monthly and annual output types represent the summation of the simulated daily values and may not have any physical meaning. For example, although annual gridded \texttt{SNOWFALL output will represent the annual snowfall amount as water equivalent precipitation, the annual gridded \texttt{SNOWCOVER output represents the summation of the daily amount of snow storage. It is unclear what utility this summation would have.
Also note that the graphical output is not publication quality but is rather included as a quick way to visualize changes in key variables over space and through time.


DISLIN_PARAMETERS (graphical output options)
Plots of any of the variables listed in the ?Output Variables? section above are created by use of the DISLIN plotting library (Michels, 2007). The plots created by the SWB code are quite basic. The plotting functionality in the SWB code is included primarily as a quick diagnostic tool for users.

SWB enables use of some of the more important DISLIN parameters. These parameters control how the DISLIN plotting library formats each plot. The syntax is DISLIN_PARAMETERS SWB Output Variable Name, followed on the next lines by one or more of the following statements: SET_Z_AXIS_RANGE, SET_DEVICE, SET_FONT, or Z_AXIS_TITLE.
The SET_Z_AXIS_RANGE statement allows the user to specify the range of values that will be plotted. Three sets of ranges may be specified for plots of daily, monthly, and annual values. The minimum, maximum, and increment size must be specified each time the SET_Z_AXIS_RANGE statement is used.
*Example: DISLIN_PARAMETERS RECHARGE
\SET_Z_AXIS_RANGE DAILY 0 1.5 0.1
\SET_Z_AXIS_RANGE MONTHLY 0 7 1.0
\SET_Z_AXIS_RANGE ANNUAL 0 20 2.
\SET_DEVICE PDF
\SET_FONT Times-Bold
\Z_AXIS_TITLE RECHARGE, IN IN.

In the second line of the example above, the plotting range for the \texttt{RECHARGE output variable on a daily time scale is specified with a minimum of 0 in., a maximum of 1.5 in., and a plotting increment of 0.1 in.

The \texttt{SET_DEVICE statement can be used to change the output file format. The default value is \texttt{PNG, for Portable Network Graphics. This format is compact and is supported by most modern applications. Other possible output types include Windows Metafile (WMF), Adobe Postscript (PS), Adobe Encapsulated Postscript (EPS), Adobe Portable Document Format (PDF), Scalable Vector Graphics (SVG), and Windows Bitmap (BMP).

The \texttt{SET_FONT statement can be used to alter the font used to annotate the plots. For plot device type \texttt{PS, \texttt{EPS, \texttt{PDF, and \texttt{SVG, the following subset of fonts should work:
\texttt{ \Times-Roman
Courier
Helvetica
AvantGarde-Book
Helvetica-Narrow Bookman-Light
NewCenturySchlbk-Roman
Palatino-Roman
NewCenturySchlbk-Italic
Palatino-Italic

For more details about supported fonts, see the DISLIN documentation (http:/\slash www.dislin.de\slash ).
The statement \texttt{Z_AXIS_TITLE sets the text that is used for the plot legend. Text does not need to be enclosed by quotes and may include punctuation.


ITERATIVE METHOD TOLERANCE [OPTIONAL]
 The iterative method sometimes fails to converge for small solution tolerances (that is, less than 1.0E--6 change in calculated runoff in a cell from one iteration to the next). Increasing this value will improve convergence, but at the potential cost of also increasing mass-balance errors in the overall water balance. The default value is 1E--6.
Example:
ITERATIVE_METHOD_TOLERANCE 1.0E--4
UNITS OF LENGTH [OPTIONAL]
 The SWB code is written with default units of meters to define the gridded model domain. If units of feet are desired instead, the GRID_LENGTH_UNITS statement may be used. The only place in the code where this is important is with regard to the mass balance as expressed in units of acre-feet. If the grid length units are feet while the code treats them as meters, there will be a corresponding error in the values of the mass-balance terms.
Example:
GRID_LENGTH_UNITS FEET



### Gridded Datasets

For each of the three major climate datasets (precipitation, minimum and maximum air temperature), a standard set of suffixes may be added to the dataset name to control how SWB treats the dataset. The list of suffixes understood by SWB is long:

| Suffix                             | Argument         | Description                             | Default |
|------------------------------------|------------------|-----------------------------------------|-----|
| _SCALE_FACTOR                      |  *real value*    | amount to multiply raw grid value by prior to use | 1.0 |
| _ADD_OFFSET                        |  *real value*    | amount to add to the raw grid value following application of the scale factor, if any | 0.0 |
| _NETCDF_X_VAR                      |  *string*        | name of the variable to be used as the "x" axis | x |
| _NETCDF_Y_VAR                      |  *string*        | name of the variable to be used as the "y" axis | y |
| _NETCDF_Z_VAR                      |  *string*        | name of the variable to be used as the "z" (value) axis | prcp |
| _NETCDF_TIME_VAR                   |  *string*        | name of the variable to be used as the "time" axis | time |
| _NETCDF_VARIABLE_ORDER             |  "xyt or txy"    | description of the order in which the gridded data were written | tyx |
| _NETCDF_FLIP_VERTICAL               |  **none**        | if present, all gridded data will be "flipped" around the vertical axis. | NA |
| _NETCDF_FLIP_HORIZONTAL            |  **none**        | if present, all gridded data will be "flipped" around the horizontal axis  |  |
| _NETCDF_MAKE_LOCAL_ARCHIVE         |  |  |
| _PROJECTION_DEFINITION             |  | PROJ.4 string describing the geographic projection of the dataset |  |
| _MINIMUM_ALLOWED_VALUE             | *real value* | ceiling to be applied to the data; data above this value will be reset to this amount | |
| _MAXIMUM_ALLOWED_VALUE             | *real value* | floor to be applied to the data; data beneath this value will be reset to this amount   
| _MISSING_VALUES_CODE               | *real or integer value* | value |
| _MISSING_VALUES_OPERATOR           | "<", "<=", ">", ">=" | trigger missing values action if the data value meets this condition |
| _MISSING_VALUES_ACTION             | "mean" or "zero" | "mean" will substitute the mean value calculated over the remaining valid cells; "zero" will substitute a value of 0.0 in place of missing values
