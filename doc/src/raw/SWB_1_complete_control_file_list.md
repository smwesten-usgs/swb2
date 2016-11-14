### Grid definition: `GRID` ###

The `GRID` directive should be in the first line of the control file. Units of meters are assumed; an optional control-file statement (`GRID_LENGTH_UNITS`) may be used to specify that units of feet are used instead. If any subsequent input grid fails to match the specified model domain exactly, the program will end.

*Syntax:*

`GRID nx ny x0 y0 x1 y1 cellsize`

-or-  

`GRID nx ny X0 Y0 cellsize`

where

`nx` is the number of columns in the grid domain,
`ny` is the number of rows in the grid domain,
`x0, y0` are the coordinates for the lower, left-hand corner of the model domain
`x1, y1` are the coordinates for the upper, right-hand corner of the model domain, and
`cellsize` is the grid cell size in meters or feet.

*Example:*

`GRID 344 320 528093. 274821.  100.0`

In the example shown above, nx (344) is the number of grid cells in the x direction, ny (320) is the number of grid cells in the y direction, X0 and Y0 (528093, 274821) are the coordinates for the lower left-hand corner of the grid.

### Growing season specification: `GROWING_SEASON` ###

This flag controls when the growing season is considered to begin and end (expressed as the day of year) and whether or not the problem is in the Northern hemisphere (possible values: `TRUE` or `FALSE`).

*Example*

`GROWING_SEASON 133 268 TRUE`

Alternatively, the growing season statement may be specified as `GROWING_SEASON GDD`, which will cause SWB to track the growing degree days for each gridcell. When using this option, the growing season is considered active when the number of growing degree-days for the cell exceed 90. The growing season is considered to end when the minimum daily air temperature falls below 28.5 degree Fahrenheit. One drawback to this approach is that the growing season status may toggle several times between growing and nongrowing season in early spring if a string of warm days is followed by a single cold night, for example. A benefit to this approach is that each cell is allowed to toggle between nongrowing and growing season as the local air temperatures dictate.

This option affects which interception value (growing season or dormant season) and which antecedent runoff condition thresholds apply to a grid cell.

### Precipitation source: `PRECIPITATION`

This control statement specifies a file type and a file prefix that the SWB code will associate with daily _gridded_ precipitation values.

*Syntax:*

`PRECIPITATION file_format location`

where

`file_format` may be `ARC_GRID`, `SURFER`, or `NETCDF`, and
`location` is the relative or absolute path and name of the relevant file or file template.

*Example--Arc ASCII daily grid:*

`PRECIPITATION ARC_GRID climate\precip\PRCP_%m_%d_%y.asc`


*Example -- Surfer daily grid:*
`PRECIPITATION SURFER climate\precip\PRCP_%m_%d_%y.asc`


### Temperature data source

This control statement either indicates that temperature at a single station is to be used or, alternatively, specifies a file type and file prefixes associated with daily gridded maximum and minimum temperature values. The required syntax for gridded temperature data is:

`TEMPERATURE FILE_FORMAT TMAXprefix TMINprefix`

*Example -- single station:*

`TEMPERATURE SINGLE_STATION`

--or--

*Example -- Arc ASCII daily grid:*

`TEMPERATURE ARC_GRID climate\precip\TMAX_%m_%d_%y.asc climate\precip\TMIN_%m_%d_%y.asc`

--or--

*Example - Surfer daily grid:*

`TEMPERATURE SURFER climate\precip\TMAX_%m_%d_%y.asc climate\precip\TMIN_%m_%d_%y.asc`

 Note that the file prefix may specify one or more relative subdirectory names separated by backslashes. Temperature grids are expected to be of data type real, and in units of degrees Fahrenheit.
}

### Flow direction data source

cmd{FLOW\_DIRECTION}{required (if flow routing enabled)}{black!70!white}{
 Arc ASCII or Surfer grid of D8 flow directions. The flow-direction grid must be of data type integer.
*Example:}{FLOW\_DIRECTION ARC\_GRID input\textbackslash{}new\_fl\_dir.asc}
}
\requiredcmd{SOIL\_GROUP}{
Arc ASCII or Surfer grid of hydrologic soil groups; values must correspond to the hydrologic soil groups contained in the land-use lookup table. The soil group must be of data type integer.
*Example:}{SOIL\_GROUP ARC\_GRID input\textbackslash{}soil\_hyd\_grp.asc}
}

\requiredcmd{LAND\_USE}{
Arc ASCII or Surfer grid of land-use\slash land-cover classification values; the values must correspond to the land-use\slash land-cover values contained in the land-use lookup table. The land-use grid must be of data type integer.
*Example:}{LAND\_USE ARC\_GRID input\textbackslash{}land\_use.asc}
}

\requiredcmd{LAND\_USE\_LOOKUP\_TABLE}{
The name of the land-use lookup table must be specified within the control file.
*Example:}{LAND\_USE\_LOOKUP\_TABLE std\_input\textbackslash{}LU\_lookup.txt}
}

\requiredcmd{WATER\_CAPACITY}{
 The model uses soil information, together with land-cover information, to calculate surface runoff and assign a maximum soil-moisture holding capacity to each grid cell. The soil-group grid may be used along with the values included in table 7 to produce this grid. Available water capacity is expected to be of data type real and is expressed in units of inches of water per foot of soil.
*Example:}{
WATER\_CAPACITY ARC\_GRID input\textbackslash{}avail\_water\_cap.asc}
}

\requiredcmd{SM (Soil Moisture accounting method)}{
 The model currently only contains one soil-moisture-accounting calculation option: Thornthwaite-Mather (1948, 1957). The Thornthwaite-Mather soil-moisture-retention tables are included in the standard table soil-moisture-retention-extended.grd.
*Example:}{SM T-M std\_input\textbackslash{}soil-moisture-retention-extended.grd}
}

\requiredcmd{INITIAL\_SOIL\_MOISTURE}{
 Initial soil moisture can be specified either as a single constant value or as a grid of values. Initial soil moisture is expressed as a percentage saturation of the available water capacity (0--100 percent). If supplied as gridded data, the initial soil-moisture grid is expected to be of data type real.

*Example--uniform constant value:}{INITIAL\_SOIL\_MOISTURE CONSTANT 100}
--or--
*Example--Arc ASCII grid:}{INITIAL\_SOIL\_MOISTURE ARC\_GRID input\textbackslash{}initial\_pct\_sm\_dec\_31\_1999.asc}
}

\requiredcmd{INITIAL\_SNOW\_COVER}{
Initial snow cover can be specified either as a single constant value or as a grid of values. Initial snow cover is expressed as a water-equivalent value (inches of water). If supplied as gridded data, the initial-snow-cover grid is expected to be of data type real.

*Example--uniform constant value:}{INITIAL\_SNOW\_COVER CONSTANT 0}
 --or--
 *Example--Arc ASCII grid:}{INITIAL\_SNOW\_COVER ARC\_GRID input\textbackslash{}initial\_snow\_cover\_dec\_31\_1999.asc}
}

\requiredcmd{RUNOFF}{
Currently, only one runoff calculation method is available: the NRCS curve number method (Cronshey and others, 1986). This calculation method must be specified by including the letters \texttt{C-N} after the keyword \texttt{RUNOFF}.

 Three methods are available for the routing of surface water through the model domain. The \texttt{ITERATIVE} method is based on the original VBA code solution method, in which water is iteratively moved across the entire grid until all water has either infiltrated or left the grid via surface flow. The \texttt{DOWNHILL} method involves a pre-simulation step in which the model grid cells are sorted upslope to downslope. Runoff is calculated in a single iteration for each time step over the entire model domain, proceeding from the upstream cells to the downstream cells. There should be no difference between the two routing methods except that the \texttt{DOWNHILL} method executes much more quickly.

Specifying the method as \texttt{NONE} disables routing altogether. Any cell outflow is assumed to find its way to a surface-water feature and exit the model domain. Outflow under this option is tracked as RUNOFF\_OUTSIDE.
The SWB code writes a binary file to disk after the first pass through the \texttt{DOWNHILL} iteration method. This binary file is read in for subsequent SWB model runs, eliminating the potentially time-consuming task of grid-cell sorting. Any changes to the grid extent will trigger an error message reminding the user to delete the file swb\_routing.bin before rerunning the SWB code.

*Example--iterative routing; runoff calculated by means of SCS curve number:}{RUNOFF C-N ITERATIVE}
--or--
*Example--downhill routing; runoff calculated by means of SCS curve number:}{RUNOFF C-N DOWNHILL}
--or--
*Example--routing disabled; runoff calculated by means of SCS curve number:}{RUNOFF C-N NONE}
}

\requiredcmd{ET (evapotranspiration method)}{
The model implements several methods for estimating potential evapotranspiration, specifically, the Thornthwaite-Mather, Jensen-Haise, Blaney-Criddle, Turc, and Hargreaves-Samani methods. Note that, given the same root-zone depths, the Thornthwaite-Mather method will produce lower estimates of potential evapotranspiration (and thus, higher estimates of recharge) than the other methods (Vrsmarty and others, 1998). The Hargreaves-Samani method is the only one suitable for use with gridded precipitation and air-temperature data.
The complete list of possible program options for specifying an ET calculation method is given below.

*Example: Thornthwaite-Mather}{
ET T-M \emph{latitude}}
--or--
*Example: Jensen-Haise}{
ET J-H \emph{latitude albedo a\txtdn{s} b\txtdn{s}}}
--or--
*Example: Blaney-Criddle}{
ET B-C \emph{latitude}}
--or--
*Example: Turc}{ET TURC \emph{latitude albedo a\txtdn{s} b\txtdn{s}}}
--or--
*Example: Hargreaves-Samani}{ET HARGREAVES \emph{southerly\_latitude northerly\_latitude}}

Values must be entered for all specified options. In the absence of more specific information, a reasonable value of the albedo for the Jensen-Haise and Turc methods is 0.23; similarly, as may be set to 0.25 and $b_s$ set to 0.5. The coefficients $a_s$ and $b_s$ are used in the Angstrom formula for estimation of daily solar radiation \citep{allen_crop_1998}. The term $a_s$ expresses the fraction of extraterrestrial radiation that reaches earth on overcast days; $b_s$ expresses the additional fraction of extraterrestrial radiation that reaches earth on clear days.
}

\requiredcmd{SOLVE}{
This option begins the actual recharge calculation. The name of the single-station climate time-series data file is required.

*Example:}{SOLVE bec1999v2.txt}

Note that a simulation for more than 1 year is made possible by simply including additional SOLVE statements.

*Example:}{SOLVE MSN\_1999.txt\\SOLVE MSN\_2000.txt \\SOLVE MSN\_2001.txt\\\textit{--etc--}}

If gridded temperature and precipitation data are supplied to the SWB model and the Hargreaves-Samani (1985) ET calculation method is selected for use, the single-station climate time-series file may be eliminated altogether. In this case, the starting and ending year of the simulation must be supplied.

*Example:}{SOLVE\_NO\_TS\_FILE 1983 1992}
}

\requiredcmd{EOJ}{
The \texttt{EOJ} option stands for End-Of-Job; it triggers actions to write grids to disk, calculate statistics, and de-allocate memory.
*Example:}{EOJ}
}

\subsection*{Optional Control-File Entries}

The control-file entries discussed above are the only ones required to run the SWB model. However, there are additional control-file entries available that change the method of calculation or enable additional output options. This section describes optional control-file entries that may be used.

\optionalcmd{ANSI\_COLORS}{
 If you have access to a terminal program such as rxvt, the SWB model can generate screen output with color coding for positive and negative values (possible values: TRUE\slash FALSE). The rxvt package can be installed as an option along with the Cygwin Unix emulation package (www.cygwin.com).

*Example:}{ANSI\_COLORS TRUE}
}

\optionalcmd{SUPPRESS\_SCREEN\_OUTPUT\\SUPPRESS\_DAILY\_FILES\\SUPPRESS\_DISLIN\_MESSAGES}{

In order to speed model runtimes, certain types of text messages normally printed to the screen and\slash or disk may be suppressed. \texttt{SUPPRESS\_SCREEN\_OUTPUT} will turn off the detailed mass-balance information that is normally printed to the screen for each daily timestep. \texttt{SUPPRESS\_DAILY\_FILES} will prevent detailed mass balance from being written to disk as recharge\_daily\_statistics.csv, recharge\_annual\_report.csv, and recharge\_daily\_report.csv. \texttt{SUPPRESS\_DISLIN\_MESSAGES} will prevent the progress messages normally generated by the DISLIN graphics library from being written to the screen.

*Example:}{SUPPRESS\_SCREEN\_OUTPUT }

}

\optionalcmd{ADJUSTED\_WATER\_CAPACITY}{
 The model will calculate the total available water capacity from the base soil-water-capacity grid and the land-use grid, using the rooting-depth values as specified in the land-use lookup table. Alternatively, the adjusted water capacity may be calculated independently of the model and read in as a real-number ASCII grid. If this is done, internal calculation of the rooting depth and resulting adjusted water capacity is disabled in the model.

*Example:}{ADJUSTED\_WATER\_CAPACITY ARC\_GRID input\textbackslash{}MAX\_SM\_STORAGE.asc}
}

\optionalcmd{UPPER\_LIMIT\_CFGI\\LOWER\_LIMIT\_CFGI}{
The upper and lower continuous-frozen-ground indices may be set with the UPPER\_LIMIT\_CFGI and LOWER\_LIMIT\_CFGI statements. As discussed elsewhere, these values define the boundaries between completely frozen soil (the upper limit) and completely unfrozen soil (the lower limit). The CFGI threshold values are expressed in degree-Celcius-days.
*Example:}{UPPER\_LIMIT\_CFGI 83 \\
LOWER\_LIMIT\_CFGI 56}

Values for \texttt{UPPER\_LIMIT\_CFGI} and \texttt{LOWER\_LIMIT\_CFGI} were reported as 83 and 56, respectively, by Molnau and Bissell (1983); their work was conducted in the Pacific Northwest of the United States.
}

\optionalcmd{INITIAL\_FROZEN\_GROUND\_INDEX}{This statement sets the initial (year 1) continuous-frozen-ground index. This may be supplied as a constant (\texttt{CONSTANT}) or as a gridded data set (\texttt{ARC\_GRID} or \texttt{SURFER}).
*Example:}{INITIAL\_FROZEN\_GROUND\_INDEX CONSTANT 100.0}
--or--
*Example:}{INITIAL\_FROZEN\_GROUND\_INDEX ARC\_GRID input\textbackslash{}INIT\_CFGI.asc}
}

\optionalcmd{INITIAL\_ABSTRACTION\_METHOD}{
The method for calculating the initial abstraction within the NRCS curve number method may be specified in one of two ways:
1, TR--55: Ia is assumed equal to 0.2 S,
2, Woodward and others (2003): Ia is assumed equal to 0.05 S.

If the Hawkins method is used, curve numbers are adjusted as given in equation 9 of \citet{woodward_runoff_2003}. The overall effect should be to increase runoff for smaller precipitation events. This method has been suggested to be more appropriate to long-term simulation model applications.

*Example -- original TR-55 method:}{INITIAL\_ABSTRACTION\_METHOD TR55}
--or--
*Example -- Hawkins modified method:}{INITIAL\_ABSTRACTION\_METHOD HAWKINS}
}

\optionalcmd{OUTPUT\_FORMAT (file format for gridded data output)}{
This option allows the user to choose the format of grid output. Currently two formats are supported: ESRI ASCII Grid, and Golden Software Surfer. The default is \texttt{ARC\_GRID}.
*Example--Arc Grid output:}{OUTPUT\_FORMAT ARC\_GRID}
--or--
*Example--Surfer format:}{OUTPUT\_FORMAT SURFER}
}

\optionalcmd{OUTPUT\_GRID\_PREFIX}{
This option sets the output grid filename prefix. If no value is supplied, output file names will begin with an underscore (\_).

*Example:}{OUTPUT\_GRID\_PREFIX BlkErth}
}

\optionalcmd{OUTPUT\_GRID\_SUFFIX}{
This option sets the output grid filename suffix. The default value is asc.
*Example:}{OUTPUT\_GRID\_SUFFIX txt}
}

\optionalcmd{OUTPUT\_OPTIONS}{
The code will write out gridded data files or image files (portable network graphics, Adobe Portable Document Format, or bitmap) for any of 24 internal and state variables simulated in the model (see table 11). The required syntax of this option is

*}{OUTPUT\_OPTIONS \emph{variable\_name daily\_output monthly\_output annual\_output}}

The list of valid output types is \texttt{NONE}, \texttt{GRAPH}, \texttt{GRID}, or \texttt{BOTH}.

For example, if one wished to produce output for actual evapotranspiration, the following would yield no daily output, graphical monthly output, and both gridded and graphical annual output.
*Example:}{OUTPUT\_OPTIONS ACT\_ET NONE GRAPH BOTH}

Note that the monthly and annual output types represent the summation of the simulated daily values and may not have any physical meaning. For example, although annual gridded \texttt{SNOWFALL} output will represent the annual snowfall amount as water equivalent precipitation, the annual gridded \texttt{SNOWCOVER} output represents the summation of the daily amount of snow storage. It is unclear what utility this summation would have.
Also note that the graphical output is not publication quality but is rather included as a quick way to visualize changes in key variables over space and through time.
}

\optionalcmd{DISLIN\_PARAMETERS (graphical output options)}{
Plots of any of the variables listed in the ?Output Variables? section above are created by use of the DISLIN plotting library (Michels, 2007). The plots created by the SWB code are quite basic. The plotting functionality in the SWB code is included primarily as a quick diagnostic tool for users.

SWB enables use of some of the more important DISLIN parameters. These parameters control how the DISLIN plotting library formats each plot. The syntax is DISLIN\_PARAMETERS SWB Output Variable Name, followed on the next lines by one or more of the following statements: SET\_Z\_AXIS\_RANGE, SET\_DEVICE, SET\_FONT, or Z\_AXIS\_TITLE.
The SET\_Z\_AXIS\_RANGE statement allows the user to specify the range of values that will be plotted. Three sets of ranges may be specified for plots of daily, monthly, and annual values. The minimum, maximum, and increment size must be specified each time the SET\_Z\_AXIS\_RANGE statement is used.
*Example:}{DISLIN\_PARAMETERS RECHARGE
\\SET\_Z\_AXIS\_RANGE DAILY 0 1.5 0.1
\\SET\_Z\_AXIS\_RANGE MONTHLY 0 7 1.0
\\SET\_Z\_AXIS\_RANGE ANNUAL 0 20 2.
\\SET\_DEVICE PDF
\\SET\_FONT Times-Bold
\\Z\_AXIS\_TITLE RECHARGE, IN IN.}

In the second line of the example above, the plotting range for the \texttt{RECHARGE} output variable on a daily time scale is specified with a minimum of 0 in., a maximum of 1.5 in., and a plotting increment of 0.1 in.

The \texttt{SET\_DEVICE} statement can be used to change the output file format. The default value is \texttt{PNG}, for Portable Network Graphics. This format is compact and is supported by most modern applications. Other possible output types include Windows Metafile (WMF), Adobe Postscript (PS), Adobe Encapsulated Postscript (EPS), Adobe Portable Document Format (PDF), Scalable Vector Graphics (SVG), and Windows Bitmap (BMP).

The \texttt{SET\_FONT} statement can be used to alter the font used to annotate the plots. For plot device type \texttt{PS}, \texttt{EPS}, \texttt{PDF}, and \texttt{SVG}, the following subset of fonts should work:
\texttt{ \\Times-Roman
Courier
Helvetica
AvantGarde-Book
Helvetica-Narrow Bookman-Light
NewCenturySchlbk-Roman
Palatino-Roman
NewCenturySchlbk-Italic
Palatino-Italic}

For more details about supported fonts, see the DISLIN documentation (http:/\slash www.dislin.de\slash ).
The statement \texttt{Z\_AXIS\_TITLE} sets the text that is used for the plot legend. Text does not need to be enclosed by quotes and may include punctuation.
}

ITERATIVE METHOD TOLERANCE [OPTIONAL]
 The iterative method sometimes fails to converge for small solution tolerances (that is, less than 1.0E--6 change in calculated runoff in a cell from one iteration to the next). Increasing this value will improve convergence, but at the potential cost of also increasing mass-balance errors in the overall water balance. The default value is 1E--6.
Example:
ITERATIVE\_METHOD\_TOLERANCE 1.0E--4
UNITS OF LENGTH [OPTIONAL]
 The SWB code is written with default units of meters to define the gridded model domain. If units of feet are desired instead, the GRID\_LENGTH\_UNITS statement may be used. The only place in the code where this is important is with regard to the mass balance as expressed in units of acre-feet. If the grid length units are feet while the code treats them as meters, there will be a corresponding error in the values of the mass-balance terms.
Example:
GRID\_LENGTH\_UNITS FEET
