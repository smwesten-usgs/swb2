# Appendix 4. Example Applications

This appendix includes two example Soil-Water-Balance (SWB)
applications. The first example application is the base test case for
the Hawaii Water Budget code, which simulates net infiltration for the
Hawaiian Island of Maui (not shown). The original Hawaii Water Budget
code contains numerous features and capabilities that are adapted to
calculation of net infiltration in a tropical island environment (Engott
and others, 2015; Izuka and others, 2010; Oki, 2002). The Maui example
demonstrates the use of several new methods, including the simulation of
fog, direct additions to net infiltration, rainfall syntheses by the
method of fragments, and runoff and evapotranspiration by means of
gridded monthly inputs. The second example, the Central Sands example
application, demonstrates the application of SWB’s crop-water
demand/irrigation estimation capabilities to an irrigated region of
Wisconsin.

All model daily weather grids and tables, control files, lookup tables,
and data files are available for download at
<https://github.com/smwesten-usgs/swb2_examples>.

## Maui, Hawaii

SWB was used to estimate net infiltration for the Island of Maui,
Hawaii. These estimates were compared to the net-infiltration estimates
generated for a previous U.S. Geological Survey (USGS) study (Johnson
and others, 2014) that made use of the Hawaii Water Budget code.

### Study Area

The Island of Maui has an area of 728 square miles and is the second
largest island in the Hawaiian archipelago. Maui is composed of two
shield volcanoes. The older volcano, the West Maui Mountain, is at an
altitude of 5,788 feet at Pu‘u Kukui, and the younger volcano, the East
Maui Volcano (commonly referred to as Haleakalā), is at an altitude of
10,023 feet at Pu‘u ‘Ula‘ula (Red Hill). The two volcanoes are connected
by an isthmus that is covered with terrestrial and marine sedimentary
deposits that are more than 5 miles wide (Stearns and Macdonald, 1942).
Erosion of the West Maui Mountain has carved deep valleys and sharp
crested ridges that radiate from near the summit. On Haleakalā, the
rainy eastern slope has valleys that are separated by broad areas and
ridges. The drier western slope of Haleakalā is less incised and retains
the broad, smooth topography of the shield volcano (fig. 4–1).

1.  Mean annual rainfall during 1978–2007 for the islands of Maui and
    Kaho’olawe, Hawaii.

Steep gradients in mean annual rainfall patterns on Maui reflect the
influence of persistent trade winds and orographic rainfall (Giambelluca
and others, 2013). On an island-wide basis, mean rainfall on Maui is
about 81 inches per year (in/yr). Mean rainfall is more than 360 in/yr
at Pu‘u Kukui. About 5 miles southwest of Pu‘u Kukui, mean rainfall is
less than 15 in/yr. Mean rainfall exceeds 100 in/yr for much of the
interior uplands of the West Maui Mountain. On Haleakalā, mean rainfall
exceeds 200 in/yr on midaltitude windward slopes. At a rain gage (not
shown) about an altitude of 5,400 feet on windward Haleakalā, mean
rainfall is about 404 in/yr, which is among the highest rainfall values
in the Hawaiian Islands and the world during 1978–2007 (Giambelluca and
others, 2013). Leeward slopes in the rain shadow of Haleakalā are much
drier. Mean rainfall is less than 25 in/yr for most leeward areas along
the coastline and the isthmus. The summit area of Haleakalā is also
relatively dry, with mean rainfall between about 35 and 50 in/yr.

### Input Grids and Tables

This section discusses the important features of each of the gridded and
tabular datasets needed to run a simplified version of the Maui example.
An SWB version 1.0 application would typically have four gridded
datasets that provide data regarding the D8 flow direction, available
water capacity, hydrologic soil group, and land use. SWB version 2.0
incorporates many of the features of the Hawaii Water Budget code
(Engott and others, 2015; Izuka and others, 2010) because many of the
SWB version 1.0 methods (curve-number method, bucket interception) have
been determined to be poorly suited for application to islands in the
Pacific Ocean. A comparison between process methods used in a more
typical or traditional SWB application (in other words, a humid
environment on the conterminous United States) to methods used in the
Maui example is listed in table 4–1.

1.  Comparison of differences between typical Soil-Water-Balance (SWB)
    process methods and those used in the Maui example application.

\[SWB, soil water balance; --, no
data\]

| Process                                                                                                                 | Typical SWB application                                    | Maui SWB application                                                                          |
| ----------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------- | --------------------------------------------------------------------------------------------- |
| Runoff generation                                                                                                       | Curve-number hydrology                                     | Monthly runoff coefficients.                                                                  |
| Flow routing                                                                                                            | D8                                                         | None.                                                                                         |
| Precipitation                                                                                                           | Gridded daily values                                       | Method of fragments.                                                                          |
| Interception by vegetation                                                                                              | Bucket                                                     | Gash.                                                                                         |
| Fog interception by vegetation                                                                                          | \--                                                        | Gridded                                                                                       |
| Potential or reference evapotranspiration                                                                               | Hargreaves-Samani                                          | Gridded Priestly-Taylor, externally calculated.                                               |
| Irrigation demand                                                                                                       | Replenish to field capacity                                | Scheduled application, amount determined by monthly rainfall, evapotranspiration, and runoff. |
| Direct contributions to net infiltration (leakage from reservoirs, taro ponds, storm drains, and other diffuse sources) | \--                                                        | Gridded or tabular.                                                                           |
| Soil-moisture reservoir capacity                                                                                        | Calculated from available water capacity and rooting depth | Gridded.                                                                                      |

The use of some different methods in the Maui example requires a
different set of inputs relative to a more typical SWB application. A
data requirement that does not change, however, is the requirement to
provide a gridded land-use code as a means to structure the relevant
model parameters. The land-use grid supplied with the SWB Maui example
is shown in figure 4–2. Irrigated land uses include the major crop types
(pineapple, coffee, diversified agriculture, macadamia nuts, sugarcane),
golf courses, and tree plantations. An interesting aspect of the Maui
example is that unlike applications in more temperate climates, crops
such as sugarcane have growing seasons that persist for multiple years;
SWB accommodates multiple-year crop-coefficient curves for this reason.

2.  Soil-Water-Balance (SWB) land-use grid for the Maui, Hawaii, study
    area.

In a more typical SWB application, a gridded, hydrologic soil-group
dataset would be provided, and rooting depths corresponding to each
soils group would be included in the lookup table. The maximum size of
the soil-moisture reservoir would then be calculated during model
initialization by multiplying the available water capacity by the
rooting depth. For the Maui example, the size of the soil-moisture
reservoir for each cell was provided by means of an externally
calculated grid (fig. 4–3).

3.  Soil-Water-Balance (SWB) soil-moisture storage grid for the Maui,
    Hawaii, study area.

Many Pacific Islands, if not most, do not have a convenient source of
gridded daily air temperature and precipitation data. For Maui, the
method of fragments (discussed in appendix 1) was applied; the data
requirements for this method include a table of rainfall fragments
calculated from observed daily rainfall records at discrete locations,
as well as a grid of rainfall zones corresponding to the observation
locations associated with the fragment sets. These rainfall zones are
derived by drawing Thiessen polygons around the set of daily rain gages
from which the fragments are generated.

The rainfall zones shown in figure 4–4 allow for the rainfall fragment
sets to be linked to the appropriate grid cells; daily rainfall for each
cell is produced by multiplying the monthly rainfall sum by the rainfall
fragment corresponding to the rainfall-zone number and day of month. A
small piece of the Maui rainfall fragment file is listed in table 4–2;
the reduced-case table included with this example contains only a single
fragment set for each month of the year for each of the 56 rainfall
zones, for a total of 672 lines of fragment data. A single fragment set
corresponds to a calendar year of rainfall observations. Because each
rainfall gage might have 20 or 30 years of observations, a file used in
a real application might be tens of thousands of lines long.

4.  Soil-Water-Balance (SWB) rainfall zone number grid for the Maui,
    Hawaii, study area.

<!-- end list -->

2.  Subset of a rainfall fragment file for use with the method of
    fragments.

\[\* \* \*, one or more data entries
omitted\]

|       |                      |              |        |        |        |        |          |        |
| ----- | -------------------- | ------------ | ------ | ------ | ------ | ------ | -------- | ------ |
| Month | Rainfall zone number | Fragment set | Day 1  | Day 2  | Day 3  | Day 4  | \* \* \* | Day 31 |
| 1     | 1                    | 1            | 0.1895 | 0.0000 | 0.0000 | 0.0000 | \* \* \* | 0.0000 |
| 1     | 2                    | 1            | 0.0463 | 0.0000 | 0.0366 | 0.0171 | \* \* \* | 0.0341 |
| 1     | 3                    | 1            | 0.0000 | 0.0000 | 0.0000 | 0.0000 | \* \* \* | 0.0000 |
| 1     | 4                    | 1            | 0.1571 | 0.0000 | 0.0000 | 0.0000 | \* \* \* | 0.0616 |

<table>
<tbody>
<tr class="odd">
<td><p>The method of fragments as implemented in SWB allows for a rainfall correction grid to be applied as a way to alter the spatial distribution of the rainfall as calculated by the method of fragments. This method was done in the Maui example because the grid files used as the source of the month-year precipitation grids (Frazier and others, 2016), when summed and averaged, result in a slightly different spatial rainfall distribution than the Hawaii Rainfall Atlas (Giambelluca and others, 2013). A rainfall correction grid was developed and supplied to SWB to ensure similar rainfall distributions between this application and earlier projects (Johnson and others, 2014).</p>
<p>Fog interception on the windward slopes of Maui can alter the water budget (Juvik and others, 2011; Juvik and Ekern, 1978). Simulating the mechanics of fog formation and interception is beyond the capabilities of SWB; however, externally calculated gridded datasets quantifying fog interception as a function of the total rainfall received during a month is possible. A discussion regarding the development of the relations between fog interception and rainfall is documented in Johnson and others (2014). An example of the grid defining fog as a fraction of rainfall for March is shown in figure 4–5. Fog interception, thus, is calculated for each day by multiplying the disaggregated monthly rainfall amounts by the monthly fog-fraction grid.</p></td>
</tr>
</tbody>
</table>

5.  Soil-Water-Balance (SWB) fog-fraction grid for the Maui, Hawaii,
    study area for March.

A feature of the Hawaii Water Budget model involves subgrid simulation
of impervious-surface runoff (Engott and others, 2015; Johnson and
others, 2014). SWB simulates the subgrid impervious-surface runoff
processes for any cell for which impervious-surface percentages are
specified greater than zero. Subgrid impervious-surface simulation,
thus, was activated for the cells in figure 4–6, which contain nonzero
percentages of impervious-surface cover; these cells are represented in
figure 4–6 by the red-shaded cells.

6.  Soil-Water-Balance (SWB) percent pervious cover grid for the Maui,
    Hawaii, study area.

The curve-number approach has not been determined to be successful as
applied to the steep mountainous slopes of Maui. For this reason, SWB
applies a set of monthly runoff ratios (as a set of tabular data) to
calculate runoff as a function of monthly rainfall. The runoff ratio
table values are calculated from streamflow and rainfall records and are
applied to individual runoff zones. The runoff zones were developed
through spatial analysis of digital-elevation models and land-use and
soil-type data; runoff ratios were developed through an analysis of
streamflow records relative to daily rainfall amounts. The runoff zones
are shown in figure 4–7.

7.  Soil-Water-Balance (SWB) runoff zone number grid for the Maui,
    Hawaii, study area.

A table of monthly runoff ratios must be supplied along with the runoff
zone number grid so that runoff may be calculated for each grid cell. A
small subsection of a runoff ratio file is listed in table 4–3. A column
is required for each runoff zone in the model; file may contain as many
dates as needed to cover the time period of interest.

3.  Subsection of a runoff ratio file.

\[\* \* \*, date or data value
omitted\]

| Date       | 1        | 2        | 3        | 4        | 5        | 6        | \* \* \* | 765      |
| ---------- | -------- | -------- | -------- | -------- | -------- | -------- | -------- | -------- |
| 01/01/2000 | 0.2705   | 0.2182   | 0.3372   | 0.0626   | 0.0963   | 0.3850   | \* \* \* | 0.3905   |
| 02/01/2000 | 0.2705   | 0.2182   | 0.3372   | 0.0626   | 0.0963   | 0.3850   | \* \* \* | 0.3905   |
| 03/01/2000 | 0.2705   | 0.2182   | 0.3372   | 0.0626   | 0.0963   | 0.3850   | \* \* \* | 0.3905   |
| 04/01/2000 | 0.2705   | 0.2182   | 0.3372   | 0.0626   | 0.0963   | 0.3850   | \* \* \* | 0.3905   |
| 05/01/2000 | 0.2167   | 0.1398   | 0.2788   | 0.0645   | 0.0956   | 0.3000   | \* \* \* | 0.3229   |
| \* \* \*   | \* \* \* | \* \* \* | \* \* \* | \* \* \* | \* \* \* | \* \* \* | \* \* \* | \* \* \* |
| 12/31/2008 | 0.1722   | 0.1811   | 0.2214   | 0.0523   | 0.0804   | 0.2901   | \* \* \* | 0.3065   |

The runoff ratio file contains a single date column, which indicates the
first day of each month that the ratios pertain to, and as many
additional columns as there are runoff zones. The Maui example has 765
runoff zones; therefore, table 4–3 contains 765 columns of runoff ratio
values for each month of the file. Derivation of these runoff ratios is
documented in Johnson and others (2014).

Interception of rainfall by vegetation is simulated for Maui with the
Gash method (Gash, 1979; Gash and others, 1995). The Gash method
requires several additional parameter values to be supplied to SWB. An
example of an additional parameter is the canopy cover fraction, which
is used to scale the amount of interception by the estimated amount of
canopy cover present in each grid cell (fig. 4–8).

8.  Soil-Water-Balance (SWB) canopy cover fraction grid for the Maui,
    Hawaii, study area.

Another parameter necessary for application of the Gash canopy
interception method is the evaporation to rainfall ratio. This parameter
is shown in figure 4–9; the derivation of this grid is described in
Johnson and others (2014).

9.  Soil-Water-Balance (SWB) evaporation to rainfall ratio grid for the
    Maui, Hawaii, study area.

### Control File

This section presents an SWB control file that may be used to run a
simplified version of the Maui Hawaii model. The control file has been
separated into three figures (figs. 4–10, 4–11, and 4–12) for ease of
viewing and explanation.

\# Input file for swb2, Maui low-res Test Case

\# Base projection: Hawaii Albers Equal Area Conic

\# (comment characters: \!\#$%\*()-\[\] )

\-------------------------------------------------

(0) PROJECT GRID DEFINITION

\---------------------------

\! Lower LH Corner Grid

\! |\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_| Cell

\! NX NY X0 Y0 Size

\! "hi-res" version

\#GRID 1582 1054 739800. 2276900. 50.

\! "lo-res" version

GRID 316 210 739800. 2276900. 250.

BASE\_PROJECTION\_DEFINITION +proj=utm +zone=4 +ellps=WGS84 +datum=WGS84
+units=m +no\_defs

(1) MODULE SPECIFICATION

\-------------------------

INTERCEPTION\_METHOD GASH

EVAPOTRANSPIRATION\_METHOD MONTHLY\_GRID

RUNOFF\_METHOD RUNOFF\_RATIO

SOIL\_MOISTURE\_METHOD FAO-56

PRECIPITATION\_METHOD METHOD\_OF\_FRAGMENTS

FOG\_METHOD MONTHLY\_GRID

FLOW\_ROUTING\_METHOD NONE

IRRIGATION\_METHOD FAO-56

CROP\_COEFFICIENT\_METHOD FAO-56

DIRECT\_RECHARGE\_METHOD GRIDDED

SOIL\_STORAGE\_MAX\_METHOD GRIDDED

10. Part 1 of an example Soil-Water-Balance (SWB) control file showing
    specification of standard SWB gridded inputs for the Maui, Hawaii,
    study area.

(2) Initial conditions for soil moisture, snow

\-----------------------------------------------

INITIAL\_PERCENT\_SOIL\_MOISTURE CONSTANT 50.0

INITIAL\_SNOW\_COVER\_STORAGE CONSTANT 0.0

(3) Daily rainfall-related grids and data

\------------------------------------------

PRECIPITATION ARC\_GRID
input/month\_year\_rainfall/maui\_prcp\_%0m\_%Y.asc

PRECIPITATION\_GRID\_PROJECTION\_DEFINITION +proj=lonlat +datum=WGS84
+no\_defs

FRAGMENTS\_DAILY\_FILE input/rain\_fragments\_maui\_reduced\_case.prn

FRAGMENTS\_SEQUENCE\_FILE input/frag\_sequence\_2yrs\_5sims.out

FRAGMENTS\_SEQUENCE\_SIMULATION\_NUMBER 1

RAINFALL\_ZONE ARC\_GRID input/maui\_RAIN\_ZONE\_\_50m.asc

RAINFALL\_ZONE\_PROJECTION\_DEFINITION +proj=utm +zone=4 +ellps=WGS84
+datum=WGS84 +units=m +no\_defs

RAINFALL\_ADJUST\_FACTOR ARC\_GRID
input/Maui\_RF\_adj\_factors/maui\_RF\_adj\_%b\_\_50m.asc

RAINFALL\_ADJUST\_FACTOR\_PROJECTION\_DEFINITION +proj=utm +zone=4
+ellps=WGS84 +datum=WGS84 +units=m +no\_defs

RAINFALL\_ADJUST\_FACTOR\_MONTHNAMES\_LOWERCASE

(4) Monthly air temperature grids

\----------------------------------

TMAX ARC\_GRID input/Air\_Temperature\_Monthly/Tmax%b\_250m\_maui.asc

TMAX\_GRID\_PROJECTION\_DEFINITION +proj=lonlat +datum=WGS84 +no\_defs

TMAX\_SCALE\_FACTOR 1.8

TMAX\_ADD\_OFFSET 32.0

TMAX\_MISSING\_VALUES\_CODE -9999.0

TMAX\_MISSING\_VALUES\_OPERATOR \<=

TMAX\_MISSING\_VALUES\_ACTION mean

TMIN ARC\_GRID input/Air\_Temperature\_Monthly/Tmin%b\_250m\_maui.asc

TMIN\_GRID\_PROJECTION\_DEFINITION +proj=lonlat +datum=WGS84 +no\_defs

TMIN\_SCALE\_FACTOR 1.8

TMIN\_ADD\_OFFSET 32.0

TMIN\_MISSING\_VALUES\_CODE -9999.0

TMIN\_MISSING\_VALUES\_OPERATOR \<=

TMIN\_MISSING\_VALUES\_ACTION mean

11. Part 2 of an example Soil-Water-Balance (SWB) control file showing
    specification of standard SWB gridded inputs for the Maui, Hawaii,
    study area.

(5) "standard" GIS input grids: hydrologic soils group, available water
capacity, soils, and flow
direction

\-----------------------------------------------------------------------------------------------------------

\# HYDROLOGIC\_SOILS\_GROUP ARC\_GRID
input/maui\_HYDROLOGIC\_SOILS\_GROUP\_\_50m.asc

\# HYDROLOGIC\_SOILS\_GROUP\_PROJECTION\_DEFINITION +proj=utm +zone=4
+ellps=WGS84 +datum=WGS84 +units=m +no\_defs

LAND\_USE ARC\_GRID
input/LU2010\_w\_2\_season\_sugarcane\_\_simulation\_1\_\_50m.asc

LAND\_USE\_PROJECTION\_DEFINITION +proj=utm +zone=4 +ellps=WGS84
+datum=WGS84 +units=m +no\_defs

%% in this case, the maximum soil storage is read in directly, so there
is no need

%% for an available water capacity grid (soil\_storage\_max = awc \*
rooting\_depth).

SOIL\_STORAGE\_MAX ARC\_GRID
input/maui\_SOIL\_MOISTURE\_STORAGE\_\_50m.asc

SOIL\_STORAGE\_MAX\_PROJECTION\_DEFINITION +proj=utm +zone=4
+ellps=WGS84 +datum=WGS84 +units=m +no\_defs

SOIL\_STORAGE\_MAX\_MISSING\_VALUES\_CODE 0.0

SOIL\_STORAGE\_MAX\_MISSING\_VALUES\_OPERATOR \<

SOIL\_STORAGE\_MAX\_MISSING\_VALUES\_ACTION mean

12. Part 3 of an example Soil-Water-Balance (SWB) control file showing
    specification of standard SWB gridded inputs for the Maui, Hawaii,
    study area.

The first part of the SWB control file specifies the spatial resolution
and base projection for the study area. A variety of characters
(including the characters \!\#$%\*()-\[\]) may be used to indicate
comment lines (fig. 4–10), which allows, for more flexibility in writing
internal documentation into the control file. Section (0) of the control
file (a comment line) specifies the project-grid definition; section (1)
of the control file contains the module specifications.

Section (2) of figure 4–11 defines initial conditions for the snow and
soil-moisture reservoirs. The INITIAL\_SNOW\_COVER\_STORAGE directive is
not necessary because SWB will set the snow storage to zero if this
directive is omitted. However, best practice is to include such
directives if only to better document the initial conditions.

Section (3) of figure 4–11 specifies the name, location, and projection
of several datasets required for use with the method of fragments—the
month-year, rain zone, and rainfall adjustment factor grids.

Section (4) of figure 4–11 specifies the template names for the minimum
and maximum air temperatures. These data are not necessary for the Maui
application. Air temperature data, however, are included in section (4)
to allow for more accurate partitioning of precipitation into rain and
snow, despite the fact that precipitation only falls in the form of snow
on rare occasions at the points of highest elevation on Maui. Air
temperature data are typically needed to drive the calculation of
growing degree day and reference ET<sub>0</sub>; however, in the Maui
example, ET<sub>0</sub> is input directly as a series of grids, and
growing degree day is not used.

Section (5) of figure 4–12 defines only one of the standard grids—the
land-use grid. An available water-capacity grid is not needed because
the capacity of the soil-storage reservoir is read in directly, a D8
flow-direction grid is not needed because flow routing is disabled, and
a hydrologic-soil grid is not needed because runoff is not calculated
with the curve-number methodology.

Section (6) of figure 4–13 specifies several grids not normally used but
required when using the gridded ET, fog, and direct net- infiltration
methods.

(6) Other gridded datasets required for the Maui example

\--------------------------------------------------------

REFERENCE\_ET0 ARC\_GRID
input/gr0\_in\_month\_ascii/gr0\_in\_%b\_\_maui.asc

REFERENCE\_ET0\_PROJECTION\_DEFINITION +proj=lonlat +datum=WGS84
+no\_defs

FOG\_RATIO ARC\_GRID
input/fog\_fraction\_grids/maui\_fog\_ratio\_monthly\_%0m\_\_50m.asc

FOG\_RATIO\_PROJECTION\_DEFINITION +proj=utm +zone=4 +ellps=WGS84
+datum=WGS84 +units=m +no\_defs

FOG\_RATIO\_MISSING\_VALUES\_CODE 0.0

FOG\_RATIO\_MISSING\_VALUES\_OPERATOR \<

FOG\_RATIO\_MISSING\_VALUES\_ACTION zero

CESSPOOL\_LEAKAGE ARC\_GRID
input/maui\_CESSPOOL\_EFFLUENT\_INCHES\_DAY\_\_50m.asc

CESSPOOL\_LEAKAGE\_PROJECTION\_DEFINITION +proj=utm +zone=4 +ellps=WGS84
+datum=WGS84 +units=m +no\_defs

(7) Grids required for Gash Interception

\-----------------------------------------

FRACTION\_CANOPY\_COVER ARC\_GRID
input/maui\_CANOPY\_COVER\_FRACTION\_\_50m.asc

FRACTION\_CANOPY\_COVER\_PROJECTION\_DEFINITION +proj=utm +zone=4
+ellps=WGS84 +datum=WGS84 +units=m +no\_defs

EVAPORATION\_TO\_RAINFALL\_RATIO ARC\_GRID
input/maui\_EVAPORATION\_TO\_RAINFALL\_RATIO\_\_50m.asc

EVAPORATION\_TO\_RAINFALL\_RATIO\_PROJECTION\_DEFINITION +proj=utm
+zone=4 +ellps=WGS84 +datum=WGS84 +units=m +no\_defs

(8) Runoff-related data and grid

\--------------------------------

RUNOFF\_ZONE ARC\_GRID input/maui\_RUNOFF\_ZONE\_\_50m.asc

RUNOFF\_ZONE\_PROJECTION\_DEFINITION +proj=utm +zone=4 +ellps=WGS84
+datum=WGS84 +units=m +no\_defs

RUNOFF\_RATIO\_MONTHLY\_FILE
input/monthly\_runoff\_ratios\_maui\_2000\_2010\_TRANSPOSED.txt

PERCENT\_PERVIOUS\_COVER ARC\_GRID
input/maui\_\_PERCENT\_PERVIOUS\_COVER\_\_50m.asc

PERCENT\_PERVIOUS\_COVER\_PROJECTION\_DEFINITION +proj=utm +zone=4
+ellps=WGS84 +datum=WGS84 +units=m +no\_defs

(9) Lookup table(s)

\--------------------

LAND\_USE\_LOOKUP\_TABLE std\_input/Landuse\_lookup\_maui.txt

(10) Start and end date for simulation

\--------------------------------------

START\_DATE 01/01/2001

END\_DATE 12/31/2002

13. Part 4 of a SWB control file showing definition of additional
    required grids and model start and end dates for the Maui, Hawaii
    study area.

Section (7) of figure 4–13 specifies two grids needed for use with the
Gash canopy interception method—an evaporation to ratio grid and a
fraction of canopy cover grid.

Section (8) of figure 4–13 specifies two grids and a text file for
handling surface runoff—a runoff zone grid file associates runoff zones
with other information contained in the runoff ratio monthly file. A
percent pervious, cover-grid file triggers subgrid-scale,
impervious- surface runoff calculations.

Section (9) of figure 4–13 specifies the location and name of the
land-use lookup table.

Section (10) of figure 4–13 gives the start and end date for the
simulation.

### Model Application

For the Maui example application, SWB model input was generated by
resampling the polygon-based model input originally used for the Hawaii
Water Budget code in Johnson and others (2014) onto a 75-meter grid. For
both models, the monthly rainfall time series used is 1978–2007 and the
land cover is representative of 2010. The 75-meter grid size was
determined after running SWB using multiple other resolutions, both
finer and coarser than 75 meters and evaluating both (1) the differences
in output between Johnson and others (2014) and SWB and (2) the
computational effort. As expected, finer SWB grid sizes produced smaller
differences in output between the two models, but at the cost of longer
model-execution times and larger input/output file sizes. Using a
75-meter grid, SWB produced a net-infiltration estimate of 1,301 million
gallons per day for the Island of Maui, which is 2.8 percent less than
the estimate published in Johnson and others (2014); this result was
achieved with a reasonable model-run time (several hours). A comparison
of the output from SWB and Hawaii Water Budget is shown in figure 4–14.

14. Comparison of the Soil-Water-Balance (SWB) and Hawaii Water Budget
    model outputs.

The use of the method of fragments involves the random selection and use
of one fragment from the many assembled for a given rain zone. Because
this results in run-to-run variations owing to different sequences of
storm events being applied to the soil, the Hawaii Water Budget code was
set up to run 15 or 20 simulations at a time, averaging all results
together so as to average out the influence of storm sequencing. SWB is
not set up with this facility in mind. SWB can replicate the Hawaii
Water Budget results by running SWB repeatedly for a number of randomly
selected fragment sets, then calculating a mean of all output SWB grids.

The Maui example application demonstrates how SWB can be effectively
used with alternative process methods to simulate net infiltration in
climates different from the humid, temperate climate SWB was originally
designed for (Wisconsin). In addition, SWB 2.0 is designed so that
alternate process methods may be easily coded up and incorporated should
the methods be needed for a particular study area.

## Central Sands, Wisconsin

SWB version 1.0 was applied to the Little Plover River in support of a
groundwater study led jointly by the USGS and by the Wisconsin State
Geological and Natural History Survey (Bradbury and others, 2017). SWB
was used to estimate irrigation demand based on the crops present, as
well as to calculate net infiltration for use in an underlying MODFLOW
model. This example application uses the files from the Little Lover
River project to demonstrate the use of the SWB (version 2.0) irrigation
module.

### Study Area

The Little Plover River runs for 21 miles and is in the Central Sands
region of Wisconsin (Henrich and Daniel, 1983). The river is listed as a
Class 1 trout stream for much of its length. Sandy soils in the region
provide good conditions for crop growth, but require irrigation to
sustain economically feasible crop yields (Weeks and others, 1965).
Increases in irrigation pumping during the past 30 years to irrigate
vegetable crops in the region have led to reduced summertime low flows
in the Little Plover River; in some years, the discharge in July or
August has decreased to zero.

### Input Grids and Tables

Unlike the Maui example application, the Central Sands application uses
a set of more typical input grids and tables. The curve-number approach
is used to estimate runoff, which requires a hydrologic soil-group grid.
The maximum soil-moisture storage is calculated from the rooting depths
contained in a lookup table and an available water-capacity grid. Flow
routing is enabled, which requires a D8 flow-direction grid. Land use is
a required grid as well.

The land-use grid supplied to SWB for the Central Sands example is shown
in figure 4–15. Because crops and irrigation play a significant role in
this study area, the Cropland Data Layer (CDL) (Boryan and others, 2011)
was used as the source of land-use data rather than the more typical
National Land Cover Database (Homer and others, 2015). The CDL is a
remote-sensing product; a certain fraction of the crops depicted in a
given CDL image are incorrectly identified. To ensure that the most
accurate land-use grid was being used, growers in the area of interest
were invited to verify and correct the CDL crops indicated on their
lands.

15. Soil-Water-Balance (SWB) land-use grid for the Central Sands study
    area.

Hydrologic soil group and available water-capacity data were extracted
from Soil Survey Geographic Database (SSURGO) soils datasets (U.S.
Department of Agriculture, 2009). The hydrologic soils group grid as
supplied to SWB is shown in figure 4–16. The available water-capacity
grid used in the example application is shown in figure 4–17.

16. Soil-Water-Balance (SWB) hydrologic soil group grid for the Central
    Sands study area.

17. Soil-Water-Balance (SWB) available water-capacity grid for the
    Central Sands study area.

A D8 flow-direction grid was prepared from 30-meter USGS digital
elevation model data (Gesch and others, 2002). Sinks in the digital
elevation dataset were filled prior to assigning D8 flow directions. The
D8 flow-direction grid supplied to SWB is shown in figure 4–18.

18. Soil-Water-Balance (SWB) D8 flow-direction grid for the Central
    Sands study area.

Because of the nature of the project, finer control regarding which
cells received simulated irrigation water was needed than could be
accomplished by establishing irrigated and nonirrigated land-use codes.
An irrigation mask (fig. 4–19) was developed from the crop-data layer,
which was modified after discussions with growers and after examination
of well pumping records. Fields associated with wells not pumped during
the period of interest were assumed to have nonirrigated fields.

19. Soil-Water-Balance (SWB) irrigation mask grid for the Central Sands
    study area.

The grid depicted in figure 4–19 is a simple Arc ASCII integer grid with
values of 0 and 1; nonirrigated land is indicated by 0, and irrigated
land is indicated by 1.

### Control File

This section presents an SWB control file that may be used to run a
simplified version of the Central Sands model.

Section (0) of figure 4–20 shows the control file syntax that defines
the grid and base projection for the Central Sands example application;
section (1) shows the module specification.

%% Central Sands, Wisconsin

%% Example of net infiltration calculation with crop water demand

%% and irrigation included in water budget

%% (comment characters: \!\#$%\*()-\[\] )

\-------------------------------------------------

(0) PROJECT GRID DEFINITION

\---------------------------

\! Grid definition: projected coordinates are Wisconsin Transverse
Mercator (83/91), meters

\! nx ny xll yll resolution

GRID 300 150 545300 432200 45.0

BASE\_PROJECTION\_DEFINITION +proj=tmerc +lat\_0=0.0 +lon\_0=-90.0
+k=0.9996 +x\_0=520000 +y\_0=-4480000 +datum=NAD83 +units=m

(1) MODULE SPECIFICATION

\------------------------

INTERCEPTION\_METHOD BUCKET

EVAPOTRANSPIRATION\_METHOD HARGREAVES

RUNOFF\_METHOD CURVE\_NUMBER

SOIL\_MOISTURE\_METHOD FAO-56\_TWO\_STAGE

PRECIPITATION\_METHOD GRIDDED

FOG\_METHOD NONE

FLOW\_ROUTING\_METHOD D8

IRRIGATION\_METHOD FAO-56

ROOTING\_DEPTH\_METHOD DYNAMIC

CROP\_COEFFICIENT\_METHOD FAO-56

DIRECT\_RECHARGE\_METHOD NONE

SOIL\_STORAGE\_MAX\_METHOD CALCULATED

AVAILABLE\_WATER\_CONTENT\_METHOD GRIDDED

20. Part 1 of an example Soil-Water-Balance (SWB) control file showing
    grid definition and method specification for the Central Sands,
    Wisconsin, study area.

Section (2) of figure 4–21 shows the syntax specifying gridded datasets
for precipitation and air temperature as well as the syntax for missing
data handling and specification of the initial continuous frozen ground
index. The weather grids specified are tiled versions of the Daymet
gridded daily weather data (Thornton and others, 2016).

(2) Define location, projection, and conversions for daily weather
data

\------------------------------------------------------------------------

PRECIPITATION NETCDF ../COMMON/prcp\_Daymet\_v3\_%y.nc

PRECIPITATION\_GRID\_PROJECTION\_DEFINITION +proj=lcc +lat\_1=25.0
+lat\_2=60.0 +lat\_0=42.5 +lon\_0=-100.0 +x\_0=0.0 +y\_0=0.0
+ellps=GRS80 +datum=NAD83 +units=m +no\_defs

PRECIPITATION\_NETCDF\_Z\_VAR prcp

PRECIPITATION\_SCALE\_FACTOR 0.03937008

PRECIPITATION\_MISSING\_VALUES\_CODE -9999.0

PRECIPITATION\_MISSING\_VALUES\_OPERATOR \<=

PRECIPITATION\_MISSING\_VALUES\_ACTION zero

TMAX NETCDF ../COMMON/tmax\_Daymet\_v3\_%y.nc

TMAX\_GRID\_PROJECTION\_DEFINITION +proj=lcc +lat\_1=25.0 +lat\_2=60.0
+lat\_0=42.5 +lon\_0=-100.0 +x\_0=0.0 +y\_0=0.0 +ellps=GRS80
+datum=NAD83 +units=m +no\_defs

TMAX\_SCALE\_FACTOR 1.8

TMAX\_ADD\_OFFSET 32.0

TMAX\_MISSING\_VALUES\_CODE -9999.0

TMAX\_MISSING\_VALUES\_OPERATOR \<=

TMAX\_MISSING\_VALUES\_ACTION mean

TMIN NETCDF ../COMMON/tmin\_Daymet\_v3\_%y.nc

TMIN\_GRID\_PROJECTION\_DEFINITION +proj=lcc +lat\_1=25.0 +lat\_2=60.0
+lat\_0=42.5 +lon\_0=-100.0 +x\_0=0.0 +y\_0=0.0 +ellps=GRS80
+datum=NAD83 +units=m +no\_defs

TMIN\_SCALE\_FACTOR 1.8

TMIN\_ADD\_OFFSET 32.0

TMIN\_MISSING\_VALUES\_CODE -9999.0

TMIN\_MISSING\_VALUES\_OPERATOR \<=

TMIN\_MISSING\_VALUES\_ACTION mean

INITIAL\_CONTINUOUS\_FROZEN\_GROUND\_INDEX CONSTANT 100.0

UPPER\_LIMIT\_CFGI 83.

LOWER\_LIMIT\_CFGI 55.

21. Part 2 of an example Soil-Water-Balance (SWB) control file showing
    precipitation and air temperature specification for the Central
    Sands, Wisconsin, study area.

Section (3) of figure 4–22 shows the specification of the land use,
hydrologic soil group, available water capacity, and flow-direction
grids. Note that section (3) also includes the specification of an
irrigation mask to limit simulated irrigation applications to areas
known to actually make use of center-pivot irrigation systems. Sections
(4) and (5) specify the names of the lookup tables and provide initial
condition values. Section (6) demonstrates syntax that may be used to
extract all pertinent variable values, including some temporary variable
values; values may be extracted for a cell identified with either
project coordinates or with a cell/row pair. Section (7) specifies the
start and end date for the simulation.

(3) specify location and projection for “standard” input GIS grids

\-----------------------------------------------------------------

FLOW\_DIRECTION ARC\_GRID input/d8\_flow\_direction.asc

FLOW\_DIRECTION\_PROJECTION\_DEFINITION +proj=tmerc +lat\_0=0.0
+lon\_0=-90.0 +k=0.9996 +x\_0=520000 +y\_0=-4480000 +datum=NAD83
+units=m

HYDROLOGIC\_SOILS\_GROUP ARC\_GRID input/hydrologic\_soils\_group.asc

HYDROLOGIC\_SOILS\_GROUP\_PROJECTION\_DEFINITION +proj=tmerc +lat\_0=0.0
+lon\_0=-90.0 +k=0.9996 +x\_0=520000 +y\_0=-4480000 +datum=NAD83
+units=m

LAND\_USE ARC\_GRID input/landuse.asc

LANDUSE\_PROJECTION\_DEFINITION +proj=tmerc +lat\_0=0.0 +lon\_0=-90.0
+k=0.9996 +x\_0=520000 +y\_0=-4480000 +datum=NAD83 +units=m

AVAILABLE\_WATER\_CONTENT ARC\_GRID input/available\_water\_capacity.asc

AVAILABLE\_WATER\_CONTENT\_PROJECTION\_DEFINITION +proj=tmerc
+lat\_0=0.0 +lon\_0=-90.0 +k=0.9996 +x\_0=520000 +y\_0=-4480000
+datum=NAD83 +units=m

IRRIGATION\_MASK ARC\_GRID input/irrigation\_mask\_from\_cdl.asc

IRRIGATION\_MASK\_PROJECTION\_DEFINITION +proj=tmerc +lat\_0=0.0
+lon\_0=-90.0 +k=0.9996 +x\_0=520000 +y\_0=-4480000 +datum=NAD83
+units=m

(4) specify location and names for all lookup tables

\---------------------------------------------------

LAND\_USE\_LOOKUP\_TABLE std\_input/Landuse\_lookup\_CDL.txt

IRRIGATION\_LOOKUP\_TABLE std\_input/Irrigation\_lookup\_CDL.txt

%% initial conditions for soil moisture and snow storage amounts

%% may be specified as grids, but using a constant amount and

%% allowing the model to "spin up" for a year is also acceptable.

(5) Specify initial conditions

\------------------------------

INITIAL\_PERCENT\_SOIL\_MOISTURE CONSTANT 100.0

INITIAL\_SNOW\_COVER\_STORAGE CONSTANT 2.0

(6) Specify locations or grid cell column and row for which detailed
variable dump is
desired

\---------------------------------------------------------------------------------------------

DUMP\_VARIABLES COORDINATES 558059. 432426.

DUMP\_VARIABLES 286 56

DUMP\_VARIABLES 31 138

DUMP\_VARIABLES 74 106

%% start and end date may be any valid dates in SWB version 2.0

%% remember to allow for adequate model spin up; running the

%% model for just a month or two will give questionable results

(7) Specify start and end dates for model run

\---------------------------------------------

START\_DATE 01/01/2012

END\_DATE 12/31/2013

22. Part 3 of an example Soil-Water-Balance (SWB) control file showing
    standard data grid specification, lookup-table names, and start and
    end dates for the Central Sands, Wisconsin, study area.

### Model Application

The SWB model was applied to the Central Sands region for 2012, 2013,
and 2014 to estimate irrigation amounts and their influence on
net-infiltration amounts. The published SWB output (Bradbury and others,
2017) was generated with SWB version 1.0 (Westenbroek and others, 2010)
and made use of precipitation data from a single station. The SWB 2.0
application was created by making some small changes to the SWB 1.0
control file so that it functions correctly with SWB version 2.0; the
example files also make use of Daymet version 3 gridded daily weather
data (Thornton and others, 2016). Results for the model made with the
SWB version 2.0 code are nearly identical to results for the model made
with the SWB version 1.0 code.

Estimated irrigation amounts for 2013 are shown in figure 4–23. The
underlying soils in the extreme southwest of the grid (irrigation
amounts that range from about 2 to 5 inches) correspond to the area of
higher available water capacity (from 1.3 to 2.2 inches per foot) shown
in figure 4–17. Crops slightly to the north indicate estimated
irrigation amounts that range from about 8 to 11 inches and correspond
to soils of lower available water capacity (from 0.5 to 1.0 inch per
foot) shown in figure 4–17.

23. Soil-Water-Balance (SWB) estimated crop-irrigation water demand for
    2013.

The SWB model produced estimates of irrigation water requirements
(table 4–4) that were at most within about 10 percent difference than
the reported irrigation application amounts (Bradbury and others, 2017).
These results indicate that SWB may be useful in estimating irrigation
amounts in study areas for which no pumping records exist.

4.  Comparison of irrigation amounts for specific crops estimated from
    pumping records and from Soil-Water-Balance (SWB) estimates.

\[Irrigation amounts are in
inches\]

| Irrigated crop type | Estimates based on pumping records | SWB estimates |
| ------------------- | ---------------------------------- | ------------- |
| Potatoes            | 9.0                                | 10.3          |
| Corn                | 8.6                                | 7.5           |
| Sweet Corn          | 7.6                                | 7.2           |
| Snap Beans          | 7.4                                | 8.5           |

Monthly SWB version 1.0 results for net infiltration (fig. 4–24) were
supplied to an underlying transient MODFLOW model (Harbaugh, 2005;
McDonald and Harbaugh, 1988) to assist in completing a water balance of
the area. This process indicated that a monthly time step may be too
coarse in terms of SWB output utility in a transient model setting. The
precipitation, snowmelt, runoff, and net-infiltration outputs from SWB
were highly variable. The highly variable outputs resulted in an
unrealistically smooth net-infiltration time series driving MODFLOW,
with much of the temporal variability averaged away and the underlying
MODFLOW model receiving a highly smeared net-infiltration pulse. In many
settings, monthly aggregation of SWB results may be too coarse for
realistic simulation of net infiltration.

24. Soil-Water-Balance (SWB) estimated net infiltration for 2013.

Thoughtful application of the SWB model in areas where irrigation is
active can yield reasonable estimates of irrigation application and
net-infiltration amounts. Comparison of the SWB outputs to
baseflow-estimated recharge amounts and to recorded pumping records is
recommended if possible.

# References Cited

Boryan, Claire, Yang, Zhengwei, Mueller, Rick, and Craig, Mike, 2011,
Monitoring US agriculture—The US Department of Agriculture, National
Agricultural Statistics Service, Cropland Data Layer Program: Geocarto
International, v. 26, no. 5, p. 341–358.

Bradbury, Ken, Fienen, M.N., Kniffin, Maribeth, Krause, Jacob,
Westenbroek, S.M., Leaf, A.T., and Barlow, P.M., 2017, Groundwater flow
model for the Little Plover River basin in Wisconsin’s Central Sands:
Wisconsin Geological and Natural History Survey Bulletin 111, accessed
June 9, 2017, at <https://pubs.er.usgs.gov/publication/70186797>.

Engott, J.A., Johnson, A.G., Bassiouni, Maoya, and Izuka, S.K., 2015,
Spatially distributed groundwater recharge for 2010 land cover estimated
using a water-budget model for the island of O’ahu, Hawaii: U.S.
Geological Survey Scientific Investigations Report 2015–5010, accessed
July 22, 2015, at <https://pubs.er.usgs.gov/publication/sir20155010>.

Frazier, A.G., Giambelluca, T.W., Diaz, H.F., and Needham, H.L., 2016,
Comparison of geostatistical approaches to spatially interpolate
month-year rainfall for the Hawaiian Islands: International Journal of
Climatology, v. 36, no. 3, p. 1459–1470.

Gash, J.H.C., 1979, An analytical model of rainfall interception by
forests: Quarterly Journal of the Royal Meteorological Society, v. 105,
no. 443, p. 43–55.

Gash, J.H.C., Lloyd, C.R., and Lachaud, G., 1995, Estimating sparse
forest rainfall interception with an analytical model: Journal of
Hydrology, v. 170, no. 1, p. 79–86.

Gesch, D., Oimoen, M., Greenlee, S., Nelson, C., Steuck, M., and Tyler,
D., 2002, The national elevation dataset: Photogrammetric Engineering
and Remote Sensing, v. 68, p. 5–12.

Giambelluca, Thomas., Chen, Qi, Frazier, Abby, Price, Jonathan, Chen,
Yi-Leng, Chu, Pao-Shin, Eischeid, Jon, and Delparte, Donna, 2013, Online
rainfall atlas of Hawaii: Bulletin of the American Meteorological
Society, v. 94, no. 3, p. 313–316.

Harbaugh, A.W., 2005, MODFLOW-2005, The U.S. Geological Survey modular
ground-water model—The ground-water flow process: U.S. Geological Survey
Techniques and Methods book 6, Chap. A16, \[variously paged\].

Henrich, E.W., and Daniel, D.N., 1983, Drainage-area data for Wisconsin
streams: U.S. Geological Survey Open-File Report 83–933, 326 p.,
accessed January 14, 2016, at
<https://pubs.er.usgs.gov/publication/ofr83933>.

Homer, C.G., Dewitz, J.A., Yang, L., Jin, S., Danielson, P., Xian, G.,
Coulston, J., Herold, N.D., Wickham, J.D., and Megown, K., 2015,
Completion of the 2011 National Land Cover Database for the conterminous
United States—Representing a decade of land cover change information:
Photogrammetry Engineering and Remote Sensing, v. 81, no. 5, p. 345–354.

Izuka, S.K., Oki, D.S., and Engott, J.A., 2010, Simple method for
estimating groundwater recharge on tropical islands: Journal of
Hydrology, v. 387, no. 1–2, p. 81–89.

Johnson, A.G., Engott, J.A., and Bassiouni, Maoya, 2014, Spatially
distributed groundwater recharge estimated using a water-budget model
for the Island of Maui, Hawaii, 1978–2007: U.S. Geological Survey
Scientific Investigations Report 2014–5168, 53 p., accessed July 22,
2015, at <https://pubs.er.usgs.gov/publication/sir20145168>.

Juvik, J.O., DeLay, J.K., Kinney, K.M., and Hansen, E.W., 2011, A 50th
anniversary reassessment of the seminal ‘L ana’i fog drip study’in
Hawaii: Hydrological Processes, v. 25, no. 3, p. 402–410.

Juvik, J.O., and Ekern, P.C., 1978, WRRCTR no. 118—A climatology of
mountain fog on Mauna Loa, Hawaii Island: Honolulu, Hawaii, Water
Resources Research Center, University of Hawaii at Manoa, WRRC Technical
Report 118, 63 p.

McDonald, M.G., and Harbaugh, A.W., 1988, A modular three-dimensional
finite difference ground-water flow model: U.S. Geological Survey
Techniques of Water-Resource Investigations, book 6, chap. A1, 586 p.

Oki, D.S., 2002, Reassessment of ground-water recharge and simulated
ground-water availability for the Hawi area of north Kohala, Hawaii:
U.S. Geological Survey Water-Resources Investigations Report 02–4006, 62
p.

Stearns, H.T., and Macdonald, G.A., 1942, Geology and ground-water
resources of the Island of Maui, Hawaii: Territory of Hawaii, Division
of Hydrography, bulletin 7, 344 p., accessed September 1, 2017, at
<https://pubs.usgs.gov/misc/stearns/Maui.pdf>.

Thornton, P.E., Thornton, M.M., Mayer, B.W., Wei, Y., Devarakonda, R.,
Vose, R.S., and Cook, R.B., 2016, Daymet—Daily surface weather data on a
1-km grid for North America, version 3: accessed August 16, 2016, at
[http://dx.doi.org/10.3334/ORNLDAAC/1328](%20http:/dx.doi.org/10.3334/ORNLDAAC/1328).

U.S. Department of Agriculture, 2009, Soil survey geographic (SSURGO)
database for Wisconsin: accessed September 29, 2017, at
<https://nrcs.app.box.com/v/soils>.

Weeks, E.P., Ericson, D.W., Holt, C.L.R., and others, 1965, Hydrology of
the Little Plover River basin, Portage County, Wisconsin and the effects
of water resource development: Washington, D.C., United States
Government Printing Office, U.S. Geological Survey Water-Supply Paper
1811, 78 p.

Westenbroek, S.M., Kelson, V.A., Dripps, W.R., Hunt, R.J., and Bradbury,
K.R., 2010, SWB—A modified Thornthwaite-Mather Soil-Water-Balance code
for estimating groundwater recharge: U.S. Geological Survey Techniques
and Methods, book 6, chap. A31, 60 p.
