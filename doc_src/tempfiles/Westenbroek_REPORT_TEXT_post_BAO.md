![Description: USGS](./media/image1.png)

Water Availability and Use Science Program

National Water Quality Program

SWB Version 2.0—A Soil-Water-Balance Code for Estimating Net
Infiltration and Other Water-Budget Components

By Stephen M. Westenbroek, John A. Engott, Vic A. Kelson, and Randall J.
Hunt

Techniques and Methods
Book 6

Section A
Chapter 59

Version X.X, Month 201X \[if needed\]

U.S. Department of the Interior

RYAN K. ZINKE, Secretary

U.S. Geological Survey

William H. Werkheiser, Deputy Director exercising the authority of the
Director

U.S. Geological Survey, Reston, Virginia: 2017

For more information on the USGS—the Federal source for science about
the Earth,
its natural and living resources, natural hazards, and the
environment—visit
<https://www.usgs.gov>/ or call 1–888–ASK–USGS (1–888–275–8747).

For an overview of USGS information products, including maps, imagery,
and publications,
visit https://store.usgs.gov/.

Any use of trade, firm, or product names is for descriptive purposes
only and does not imply
endorsement by the U.S. Government.

Although this information product, for the most part, is in the public
domain, it also may
contain copyrighted materials as noted in the text. Permission to
reproduce copyrighted items
must be secured from the copyright owner.

Suggested citation:
Westenbroek, S.M., Engott, J.A., Kelson, V.A., and Hunt, R.J., 2017, SWB
Version 2.0—A Soil-Water-Balance code for estimating groundwater
recharge and other water budget components: U.S. Geological Survey
Techniques and Methods, book ??, chap. ??, ?? p.,
https://dx.doi.org/10.3133/XXXXX..

ISSN XXXX-XXXX (online)

# Preface

The Soil-Water-Balance (SWB) version 2.0 code can be downloaded from the
U.S. Geological Survey for free. The performance of SWB version 2.0 has
been tested in a variety of applications. Future applications, however,
might reveal errors that were not detected in the test simulations.
Users are requested to send notification of any errors found in this
model documentation report or in the model program to the contact listed
on the web page (specify DOI of release point for the software:
https://doi.org/10.5066/...).

# Contents

Preface iii

Abstract 1

Introduction 2

Background and Terminology 3

Scope and Purpose 5

Changes from Previous Versions 6

Overview of Data and Input Requirements 8

Units of Measurement 9

Model Description 9

Processes and Methods 12

Precipitation and Air Temperature 13

Interception 14

Snowfall 14

Snowmelt 14

Fog Interception 15

Runoff 15

Impervious Surface Runoff 16

Runoff Routing 16

Potential/Reference Evapotranspiration 17

Soil-Moisture Retention/Actual Evapotranspiration 18

Growing Degree Day 19

Crop Coefficients 19

Rooting Depth 20

Irrigation Demand and Application 20

Rejected Net Infiltration 21

Summary 22

Acknowledgments 23

References Cited 23

Appendixes 31

# Figures

**Figure 1.** Conceptual diagram of Soil-Water-Balance storage
reservoirs and processes. 10

# Tables

1\. Summary of new process methods available in SWB version 2.0. 8

2\. List of minimum required data and input files for a typical SWB run.
9

# Conversion Factors

U.S. customary units to International System of
Units

|                              |        |                                   |
| ---------------------------- | ------ | --------------------------------- |
| Multiply                     | By     | To obtain                         |
| Length                       |
| inch (in.)                   | 2.54   | centimeter (cm)                   |
| inch (in.)                   | 25.4   | millimeter (mm)                   |
| foot (ft)                    | 0.3048 | meter (m)                         |
| mile (mi)                    | 1.609  | kilometer (km)                    |
| Area                         |
| square mile (mi<sup>2</sup>) | 2.590  | square kilometer (km<sup>2</sup>) |

International System of Units to U.S. customary
units

|                                   |         |                                             |
| --------------------------------- | ------- | ------------------------------------------- |
| Multiply                          | By      | To obtain                                   |
| Length                            |
| centimeter (cm.)                  | 0.3937  | inch (in.)                                  |
| millimeter (mm)                   | 0.03937 | inch (in.)                                  |
| meter (m)                         | 3.2808  | foot (ft)                                   |
| kilometer (km)                    | 0.6215  | mile (mi)                                   |
| Area                              |
| square kilometer (km<sup>2</sup>) | 0.3862  | square mile (mi<sup>2</sup>)                |
| Density of Heat                   |
| langley (ly)                      | 0.673   | equivalent water evaporated in inches (in.) |

International System of Units to International System of
Units

|                 |        |                                                 |
| --------------- | ------ | ----------------------------------------------- |
| Multiply        | By     | To obtain                                       |
| Density of Heat |
| langley (ly)    | 0.0171 | equivalent water evaporated in millimeters (mm) |

Temperature in degrees Celsius (°C) may be converted to degrees
Fahrenheit (°F) as follows:

°F = (1.8 × °C) + 32.

Temperature in degrees Fahrenheit (°F) may be converted to degrees
Celsius (°C) as follows:

°C = (°F – 32) / 1.8.

Temperature in Kelvin (K) may be converted to degrees Celsius (°C) as
follows:

°C = K - 273.15.

Temperature in Kelvin (K) may be converted to degrees Fahrenheit (°F) as
follows:

°F = (K - 273.15) × 1.8 + 32.

Temperature in Celsius (°C) may be converted to Kelvin (K) as follows:

K = °C + 273.15.

Temperature in Fahrenheit (°F) may be converted to Kelvin (K) as
follows:

K = (°F - 32) / 1.8 + 273.15.

SWB Version 2.0—A Soil-Water-Balance Code for Estimating Net
Infiltration and Other Water-Budget Components

By Stephen M. Westenbroek, John A. Engott, Vic A. Kelson\[1\], and
Randall J. Hunt

# Abstract

The U.S. Geological Survey’s Soil-Water-Balance (SWB) code was developed
as a tool to estimate distribution and timing of net infiltration out of
the root zone by means of an approach that uses readily available data
and minimizes user effort required to begin a SWB application. SWB
calculates other components of the water balance, including soil
moisture, reference and actual evapotranspiration, snowfall, snowmelt,
canopy interception, and crop-water demand. SWB is based on a modified
Thornthwaite-Mather soil-water-balance approach, with components of the
soil-water balance calculated at a daily time step. Net-infiltration
calculations are computed by means of a rectangular grid of
computational elements, which allows the calculated infiltration rates
to be imported into grid-based regional groundwater-flow models. SWB
makes use of gridded datasets, including datasets describing hydrologic
soil groups, moisture-retaining capacity, flow direction, and land use.
Climate data may be supplied in gridded or tabular form. The SWB 2.0
code described in this report extends capabilities of the original SWB
version 1.0 model by adding new options for representing physical
processes and additional data input and output capabilities. New methods
included in SWB 2.0 allow for direct gridded input of externally
calculated water-budget components (fog, septic, and storm-sewer
leakage), simulation of canopy interception by several alternative
processes, and a crop-water demand method for estimating irrigation
amounts. New input and output capabilities allow for grids with
differing spatial extents and projections to be combined without
requiring the user to resample and resize the grids before use.

# Introduction

Accurate estimates of the spatial and temporal distribution of
groundwater recharge are important for many types of hydrologic
assessments, including assessments that concern water availability,
water-quality protection, streamflow and riparian-ecosystem management,
aquifer replenishment, groundwater-flow modeling, and contaminant
transport; these recharge estimates often are key to understanding
effects of land-use change in urban, industrial, and agricultural
regions. With increasing demand for science-supported hydrologic
management comes an increased need for robust and practical methods to
quantify groundwater recharge rates (Scanlon and others, 2002).

To fill this need, Dripps and Bradbury (2007) created a spreadsheet code
that calculates components of the soil-zone water balance at a daily
time step by means of a modified version of the Thornthwaite-Mather
soil-moisture-balance approach (Thornthwaite, 1948; Thornthwaite and
Mather, 1957). The primary output of the water-balance code is net
infiltration out of the root zone. In areas where groundwater is close
to the surface (less than 10 meters), net infiltration may be assumed to
become recharge. In areas with deeper groundwater tables, or for dynamic
models that require simulation of recharge timing, MODFLOW’s Unsaturated
Zone Flow Package (Niswonger and others, 2006) may be used to simulate
the transport of net infiltration to the groundwater table.

Data requirements for the original water-balance code included the
following several commonly available tabular and gridded data types: (1)
precipitation and temperature, (2) land-use classification, (3)
hydrologic soil group, (4) flow direction, and (5) soil-water capacity.
The data and required formats were designed to take advantage of widely
available geographic information systems (GIS) datasets and file
structures. To increase ease of use, reduce reliance on proprietary
software, and increase the size of model domain that could be simulated,
the U.S. Geological Survey (USGS) translated the original
soil-water-balance code from the spreadsheet Visual Basic to modern
Fortran 2008; the Fortran version was called the Soil-Water-Balance
(SWB) code version 1.0 (Westenbroek and others, 2010). SWB 1.0 was used
to estimate net infiltration out of the root zone in a wide variety of
environmental settings; for examples, see Feinstein and others (2010) or
Hunt and others (2016). The SWB code has evolved since the original
release, with the addition of crop-water demand calculations and some of
the functionality of the Hawaii Water Budget code (Izuka and others,
2010). This report documents version 2.0 of the SWB software, hereafter
referred to as SWB.

## Background and Terminology

Standard terminology is not available for use in discussions of the
water-budget components that result in groundwater recharge. The
nomenclature and following definitions from Healy (2010) are used in
this report.

*Potential recharge*—Water that has infiltrated into the root zone.
Potential recharge may leave the bottom of the root zone, eventually
becoming recharge. Alternatively, potential recharge may be removed from
the soil column by means of evaporation and transpiration.

*Net infiltration*—Water that has escaped the evapotranspiration sinks
of the root zone, some portion of which will eventually find its way to
the groundwater table.

*Groundwater recharge*—Water that actually crosses the water table.

This report uses slightly different terminology than Westenbroek and
others (2010). In both reports, however, the terms used underscore the
fact that SWB does not simulate unsaturated-zone processes beneath the
root zone. When the unsaturated zone is sufficiently thick, it can
impart appreciable lags between the time when water leaves the bottom of
the root zone and the time when water crosses the water table and enters
the groundwater system (Healy, 2010; Hunt and others, 2008; Nimmo and
others, 2005); under some conditions, separate net-infiltration events
coalesce in the unsaturated zone and enter the water table as a single
recharge event. Therefore, the output of the SWB code is here referred
to as “net infiltration” rather than “groundwater recharge”, in keeping
with Healy's (2010) usage. The distinction between infiltration and
recharge has been explored in detail by others; see for example Anderson
and others (2015, p. 232–234).

Net infiltration and related groundwater recharge can vary with time and
space. Site-specific measurements of net infiltration and recharge, if
available, are difficult to upscale for application in regional-scale
problems. Yet, in groundwater-modeling problems, application of
physically based, spatially variable recharge values to the water table
have been determined to improve model performance (Jyrkama and Sykes,
2007; Hunt and others 2008).

A soil-water-balance modeling approach is currently the preferred method
for distributing net infiltration in space and time for use in applied
groundwater modeling (Anderson and others, 2015, p. 232). The temporal
discretization used in the soil-water-balance model should not be overly
coarse. For example, Rushton and Ward (1979) determined that running a
soil-water-balance calculation with monthly time steps gave
net-infiltration values 25 percent less than soil-water-balance
calculations using daily values.

Many soil-water-balance models are described in the literature, most
developed for specific applications. Soil-water-balance models have been
developed to evaluate crop irrigation requirements and impacts
(Boisvert, 1990; Braud and others, 2013; Jensen, 1969; Kendy and others,
2003), crop yield prediction (Akinremi and others, 1996), and landfill
cover design (Schroeder and others, 1994), and to estimate net
infiltration (Batelaan and De Smedt, 2001; Eilers and others, 2007;
Finch, 2001; Fitzsimons and Misstear, 2006; Jyrkama and others, 2002;
Lee and others, 2006; Manghi and others, 2009).

Within the USGS, many different water-balance models have been used as a
means to estimate net infiltration. The Yucca Mountain Project of the
1980s and 1990s produced the INFIL 3.0 model (U.S. Geological Survey,
2008). The Basin Characterization Model has been applied to significant
tracts of the western United States (Flint and others, 2014; Flint and
Flint, 2007). A similar model was developed and applied in Montana,
Idaho, and Washington State (not shown) (Bauer and Vaccaro, 1987; Bauer
and Vaccaro, 1990). A custom water balance model has been applied to the
Hawaiian Islands (not shown) for decades (Izuka and others, 2010).
Another custom water balance model was applied to the central Midwest
regional aquifer system, with special emphasis on estimating consumptive
use of water and the resulting impact on recharge (Dugan and
Peckenpaugh, 1985). These models have generally been developed with
specific environmental settings in mind (Yucca Mountain, Hawaiian
Islands) but include processes and algorithms that may be suited for
future versions of the SWB code documented in this report.

## Scope and Purpose

The purpose of this report is to document version 2.0 of the SWB code.
Version 2.0 is designed to estimate components of the water budget,
particularly net infiltration, for a model domain represented by a grid
of uniformly sized square cells on a daily timescale. Version 1.0 of the
code is documented in Westenbroek and others (2010). This report focuses
on features and implementations that are part of the version 2.0 code.

An overview of the conceptual basis, data requirements for use, and
limitations and assumptions relating to SWB are presented in this
report. Additional details are provided in four appendixes in this
report; one of the appendixes provides two test cases featuring the SWB
code. The first test case (Maui, Hawaii), allows comparison of the
performance of the SWB code relative to the Hawaii Water Budget code.
The second test case (Central Sands, Wisconsin), demonstrates the
application of the SWB code to a model domain that includes many
irrigated land-use types.

## Changes from Previous Versions

The design goals and operation of SWB are similar to the original
release documented by Westenbroek and others (2010). The code still
performs a modified Thornthwaite-Mather soil-water balance at each grid
point within the model domain. However, the scope of recent additions to
the process methods and modifications of the input and output file
structures are significant enough to warrant a new major SWB release
along with new documentation and input instructions.

Many of the SWB code changes will be apparent to users familiar with the
original SWB code. These changes pertain to model input and output and
include the following list of SWB code changes:

  - elimination of **swbstats**, (a program to handle post-processing of
    SWB output);

  - elimination of internally generated graphics;

  - elimination of the custom **swb** binary output files;

  - addition of cartographic reprojection and resampling by means of the
    PROJ4 library that allows SWB to read grids with differing
    geographic projections;

  - upgrading Network Common Data Form (NetCDF) input and output to
    NetCDF version 4;

  - modification of internal structure to make adding new methods
    easier;

  - addition of code to allow for more flexible tabular data and
    parameter input; and

  - rearrangement of internal data structures to more efficiently
    accommodate blocks of inactive cells within model domains.

Many of the SWB code changes were made in response to user frustrations
related to the difficulty of aligning and resampling input grids; SWB
1.0 required that every grid supplied to the code be in exactly the same
geographic projection, cover the same extents as the SWB 1.0 project
grid, and be discretized at the same grid-cell resolution. This
requirement resulted in excessive data management and consumed project
time that would have been better spent on other tasks. In addition, SWB
1.0 stored results in a custom-programmed, binary-file format. Following
a SWB 1.0 model run, a program called swbstats could be used to extract
daily, monthly, annual, or period grids as well as to generate plots and
calculate basic statistics.

With the opportunity to modify and enhance SWB 1.0, the authors decided
to standardize model output using an existing file format. SWB now
stores all gridded output in the common and widely used format NetCDF
(Unidata, 2014). The NetCDF file format is commonly used among climate
scientists and meteorologists and is slowly being adopted in other
scientific fields. A benefit of switching to a well-known binary-file
format is that rather than relying on a single program, swbstats, to
handle post-processing, dozens of actively maintained open-source tools
are designed to make post-processing of NetCDF files easier (for example
<http://www.unidata.ucar.edu/software/netcdf/utilities.html>). Other
changes made since the initial SWB 1.0 release add or modify the actual
hydrologic processes simulated by SWB. These changes include the
addition of the methods listed in table 1.

1.  Summary of new process methods available in Soil-Water-Balance (SWB)
    code version
2.0.

| Process                 | Method              | Description                                                                                                                                                                                                                                                                                                                                                          |
| ----------------------- | ------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Irrigation water demand | FAO–56              | Simulates addition of water to the soil root zone in an amount necessary to sustain crop growth. FAO is the Food and Agriculture Organization, a branch of the United Nations. FAO–56 is the publication that describes methodology for calculation of crop-water demand and irrigation scheduling (Allen and others, 1998).                                         |
| Soil-moisture retention | FAO–56              | The soil-moisture retention relation in FAO–56 allows plant evapotranspiration to proceed at the rate of potential evapotranspiration until soil moisture drops to some user-defined threshold soil-moisture value; when soil-moisture values drop beneath this threshold evapotranspiration is assumed to proceed at some fraction of potential evapotranspiration. |
| Crop-coefficient curves | FAO–56              | Evapotranspiration by crops and other plants is assumed to be represented as some fraction of potential evapotranspiration; the crop-coefficient curve defines this fraction. FAO–56 represents the crop-coefficient curve as a piecewise linear relation indexed to stages of plant growth.                                                                         |
| Interception            | Gash, Horton        | The Horton (1919) method allows for interception amounts to grow relative to total storm rainfall; the modified Gash (Gash, 1979; Gash and others, 1995) method accounts for partitioning of intercepted water between canopy and stemflow and accounts for canopy density.                                                                                          |
| Fog interception        | Gridded             | Fog interception may be specified as some fraction of rainfall to account for capture of fog moisture by vegetation.                                                                                                                                                                                                                                                 |
| Rainfall                | Method of fragments | Disaggregates monthly gridded precipitation data using a set of fragments generated from daily observations (Srikanthan and McMahon, 1982).                                                                                                                                                                                                                          |
| Runoff                  | Ratio               | Runoff may be simulated as a fraction of rainfall by means of an externally calculated set of runoff ratios.                                                                                                                                                                                                                                                         |
| Direct net infiltration | Gridded or tabular  | Direct additions to net infiltration from septic systems, leaky water mains, storage reservoirs, and other diffuse sources.                                                                                                                                                                                                                                          |

## Overview of Data and Input Requirements

Input data requirements for SWB become more demanding as more modules
are activated. However, a typical SWB application may be made with a
handful of gridded datasets, a daily weather data source, a control file
specifying SWB program options, and a lookup table specifying parameter
values as a function of land use and soil type. The minimum required
data and input files for a typical SWB run are listed in table 2.

2.  List of minimum required data and input files for a typical
    Soil-Water-Balance (SWB) run.

\[--, unitless; SSURGO, Soil Survey Geographic database; gSSURGO,
Gridded Soil Survey Geographic
database\]

| Data or input type                                                       | Units                    | Format                    | Example source/description                                                                                                                                                |
| ------------------------------------------------------------------------ | ------------------------ | ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Land use                                                                 | \--                      | Grid (integer)            | National Land Cover Database (Homer and others, 2015).                                                                                                                    |
| D8 flow direction                                                        | \--                      | Grid (integer)            | National Elevation Dataset (Gesch and others, 2002); D8 flow direction must be generated from elevation data using geographic information systems or other such software. |
| Available water capacity                                                 | Inch per foot            | Grid (float or real)      | SSURGO or gSSURGO (Soil Survey Staff, 2015); typically averaged over the top 0 to 100 centimeters of the soil profile.                                                    |
| Soil hydrologic group                                                    | \--                      | Grid (integer)            | SSURGO or gSSURGO (Soil Survey Staff, 2015).                                                                                                                              |
| Weather data—daily precipitation and minimum and maximum air temperature | Inch; degrees Fahrenheit | Table or grid (float)     | Daymet 1 kilometer gridded data (Thornton and others, 2017); many other gridded data sources may be used instead.                                                         |
| SWB control file                                                         | \--                      | Text file                 | SWB control file specifies the location of the data elements listed above, as well as which modules are active during the run.                                            |
| SWB lookup table                                                         | \--                      | Text file (tab delimited) | SWB lookup table contains parameter values for each land use; some parameters are given as a function of the land use and the hydrologic soil group.                      |

## Units of Measurement

This report contains units given in a mixture of U.S. customary and
International System of Units (SI) units, sometimes in the same equation
or paragraph. This dual usage of units of measurement is because many of
the early hydrologists and soil scientists worked for the U.S. Federal
Government and published using U.S. customary units, whereas most
scientific works use SI units. Literature related to irrigation is often
in U.S. customary units. The authors have attempted to use SI units
where convenient, but have retained the units used originally by the
cited authors. Although dual usage of units of measurement may be
confusing at times, the dual usage allows SWB users to access some of
the original data tables without having to convert units.

# Model Description

The SWB code uses a modified Thornthwaite-Mather soil-moisture
accounting method (Thornthwaite and Mather, 1955; Thornthwaite and
Mather, 1957) to calculate net infiltration; net infiltration is
calculated separately for each grid cell in the model domain. Sources
and sinks of water within each grid cell are determined on the basis of
input climate data and landscape characteristics (fig. 1). Soil moisture
is updated on a daily basis as the difference among these sources and
sinks as in equation 1. The terms in equation 1 are expressed in units
of length; SWB uses units of inches.

$${\theta _t} = {\theta _{t - 1}} + rainfall + runon + snowmelt + fog\;interception + irrigation - interception - runoff - ET$$

where
	${\theta _t}$	is the soil moisture for the current simulation day,
	${\theta _{t - 1}}$	is the soil moisture on the previous simulation day, and
	$ET$	is the actual evapotranspiration.

1.  Conceptual diagram of Soil-Water-Balance storage reservoirs and
    processes.

How the terms from equation 1 relate is shown in figure 1. In addition
to the soil-moisture reservoir described by equation 1, two additional
storage reservoirs are tracked by SWB—interception and snow. The daily
calculation for the interception amounts to new intercepted rainfall and
snowfall minus any evaporated interception water. The daily calculation
for the snow reservoir is simply the running sum of snowfall minus
snowmelt.

The range of possible soil-moisture values described by equation 1 is
assumed to be bounded by two values—the field capacity and the permanent
wilting point. The field capacity of a soil is defined as the amount of
moisture remaining in a soil after it has been saturated and allowed to
drain freely. The permanent wilting point of a soil is defined as the
moisture content at which plants will wilt and fail to recover even when
later supplied with sufficient moisture (Barker and others, 2005). The
available water capacity—one of the gridded datasets required by SWB—is
defined as the difference between a soil’s field capacity and its
permanent wilting point. The total available water for the soil in a
given grid cell is calculated as shown in equation 2.

where

is total available water, in inches;

is field capacity, in inches per foot;

is permanent wilting point, in inches per foot; and

is the effective rooting depth of vegetation, in feet.

Net infiltration is assumed to take place any time the soil-moisture
value (eq. 1) exceeds the total available water (eq. 2) for the cell.

The following is a list of steps to calculate net infiltration.

1.  Precipitation is partitioned into gross rainfall or gross snowfall,
    or both.

2.  Intercepted rain or snow is added to the interception storage
    reservoir.

3.  Net snowfall is added to the snow storage reservoir.

4.  Snowmelt (if any) is calculated.

5.  Potential evapotranspiration (PET) is calculated.

6.  Interim soil moisture is calculated as .

7.  Direct additions to soil moisture, if any, are added to .

8.  Interim soil-moisture fraction is calculated as .

9.  Actual evapotranspiration (AET) from the soil storage reservoir is
    calculated as some function of and .

10. Updated soil moisture is calculated as .

11. If the updated soil moisture () exceeds the field capacity of the
    soil, the updated soil moisture is set to , making the change in
    soil moisture .

12. If the updated soil moisture is less than the field capacity, net
    infiltration is considered to be zero.

13. Otherwise, net infiltration is calculated as .

14. Direct net-infiltration amounts, if any, are added to the amount
    calculated in step 13.

Surface runoff from a cell may be routed to the next downslope cell or
may be considered to have reached an unmodeled surface-water feature
(stream, lake, ditch) and removed from the model domain. In urban areas
or in areas with significant impervious surfaces, results of simulating
runoff and net infiltration in a more detailed manner might be
desirable. This option is triggered in SWB 2.0 when a percent or
fraction impervious area grid is supplied to the code. When this option
is active, an additional storage reservoir is created—impervious surface
storage. In addition, storm drains possibly can be taking into account
by supplying the fraction of impervious surface storage that is
intercepted by storm drains. The processes referenced in the calculation
steps are discussed briefly in the next section and are discussed more
fully in appendix 1.

# Processes and Methods

The previous section describes the general outline of daily water budget
calculations. At each step in the calculation of the water budget,
different methods for estimating hydrologic processes may be specified
allowing SWB to be adapted to conditions specific to a particular
project area. These methods are described in the following section. SWB
control file syntax is indicated in the following section by highlighted
capital letters. For example, the precipitation method might be
specified in the SWB control file as PRECIPITATION\_METHOD GRIDDED. The
data and parameter requirements, SWB control file syntax, and a
description of underlying physical processes and equations for each
method are given in the appendixes

### Precipitation and Air Temperature

The following are three methods to specify daily precipitation data for
an SWB simulation: TABULAR, GRIDDED, and METHOD\_OF\_FRAGMENTS. Air
temperature data are supplied in TABULAR or GRIDDED form.

The TABULAR method allows a set of tabular daily precipitation and air
temperature data to be supplied to all grid cells within the model
domain. This method makes the use of tabular data and is suitable for
application only to small project areas with dimensions of perhaps
100 square kilometers or less. Of course, the suitability of using a
single precipitation and air temperature station in a SWB simulation
must be tempered by knowledge of the spatial variability in rainfall, as
well as by the project goals. If only annual water budget components are
of interest, a single precipitation gage may be adequate. If, however,
SWB output is to be used at a monthly or daily time step, some type of
gridded data probably are best if available.

The GRIDDED method instructs SWB to expect further PRECIPITATION or
TMIN/TMAX grids to be specified elsewhere in the control file.

The METHOD\_OF\_FRAGMENTS method creates synthetic sequences of daily
rainfall from monthly rainfall by imposing the rainfall pattern from
selected rain gages with daily data (Srikanthan and others, 2005;
Srikanthan and McMahon, 1999). The synthesized daily rainfall data
approximates the long-term (annual) average character of daily rainfall,
such as frequency, duration, and intensity, but may not necessarily
reproduce the actual historical daily rainfall record.

### Interception

The interception of precipitation by crops and other vegetation is
sometimes overlooked in hydrological models, but can amount to a
significant part of the water budget (Gerrits, 2010; Savenije, 2004).
The SWB 1.0 code used a bucket method to estimate the amount of
interception; in the bucket interception method, a constant amount of
interception is assumed regardless of the total daily precipitation. In
an attempt to model this part of the water budget more accurately, two
additional interception process formulations have been added. The three
methods implemented in SWB are the BUCKET, GASH, and HORTON methods. The
Gash method (Gash, 1979; Gash and others, 1995) models interception by
vegetation by simulating canopy storage and flow and evaporation from
stems or trunk. The Horton method (Horton, 1919) is an extension of the
bucket model that allows for interception values to increase in
proportion to the total daily precipitation value.

### Snowfall

SWB includes a single method for partitioning precipitation into
rainfall and snowfall. This method, SINGLE\_TEMPERATURE, is enabled by
default; therefore, no control file or lookup-table entries are required
to invoke the method. This method makes a comparison between a
combination of the minimum and maximum air temperatures and the freezing
point of water (32 degrees Fahrenheit) to partition precipitation into
rainfall and snowfall.

### Snowmelt

SWB includes a single snowmelt method for determining snowmelt volumes.
The TEMPERATURE\_INDEX method assumes that 1.5 millimeters (0.059 inch)
of water-equivalent snow melts per day per average degrees Celsius that
the daily maximum temperature is above freezing. This method also is
enabled by default; therefore, no additional control-file or
lookup-table entries are required to invoke the method.

### Fog Interception

Fog interception is not explicitly modeled within SWB, but estimates of
fog interception may be supplied by means of the GRIDDED data method.
For pilot application of the new code to Maui, Hawaii (discussed in the
appendixes), a set of external grids were developed. These grids express
the intercepted fog as a fraction of the monthly observed rainfall. The
process relies on external grid computations using the aspect,
elevation, and mean monthly rainfall combined with table values of
estimated annual fog-interception rates to yield monthly
fog-interception grids expressed as a fraction of monthly rainfall.
GRIDDED fog interception is not enabled by default.

### Runoff

The following two runoff estimation methods are included: the
CURVE\_NUMBER method and the RUNOFF\_RATIO method.

The CURVE\_NUMBER method (Cronshey and others, 1986) defines runoff in
relation to the difference between precipitation and an initial
abstraction term. User-defined curve numbers are used to describe the
tendency for each land use and soil texture to generate runoff. Runoff
from frozen ground is simulated by introduction of a continuous frozen
ground index, which is used to track frozen ground conditions and modify
runoff conditions accordingly.

Grids containing monthly RUNOFF\_RATIO relative to precipitation may be
used instead of the curve number method. The runoff ratio method relies
on external computations to quantify a rainfall-runoff relation for a
set of user-defined runoff zones. Details on the mechanics of the runoff
ratio method are documented in the appendixes.

### Impervious Surface Runoff

Runoff from impervious surfaces may be simulated in a more detailed
manner by including a gridded dataset defining the proportion of each
grid cell that is comprised of impervious materials. Impervious
surfaces, defined as any grid cell with an impervious surface cover
greater than zero percent, trigger the creation of a fourth storage
reservoir (impervious storage reservoir), with a water balance
calculated for the impervious area within the cell.

### Runoff Routing

In SWB, two methods are included to implement flow routing from grid
cell to grid cell. SWB allows excess water generated on a grid cell to
flow to the next downslope cell using a D8 flow-routing scheme to define
the linkages between cells.

The simplest method, NO\_ROUTING, disables downhill routing altogether.
Cell to cell routing becomes increasingly hard to imagine in a
meaningful way as grid cell sizes exceed about 1 kilometer; in any
system with a well-developed drainage system, overland flow would
commonly meet some type of surface-water feature at this scale. With
flow routing disabled, all cell runoff is assumed to reach a
surface-water feature and leave the model domain.

The DOWNHILL\_ROUTING method allows runoff from one or more cells to
become run-on to downslope cells. All runoff from a cell is assumed to
infiltrate in downslope cells or be routed out of the model domain on
the same day in which the runoff originated as rainfall or snowmelt.

If runoff routing is active, SWB examines the connectivity between each
active cell during model startup. Based on this connectivity, SWB
creates a master list of cell identities and sorts them from upslope to
downslope. When the model solution is calculated each day, the code
begins with the cell furthest upslope, performs all mass-balance
calculations, and then proceeds to perform the same calculation on the
next cell in the list.

### Potential/Reference Evapotranspiration

In SWB, three methods are included to estimate potential or reference
evapotranspiration—JENSEN\_HAISE, HARGREAVES\_SAMANI, and MONTHLY\_GRID.
Evapotranspiration methods developed with evaporation data from unknown
or differing vegetation types are often called potential
evapotranspiration methods, whereas methods developed with evaporation
data for a specific crop type are often called reference
evapotranspiration methods. The Jensen-Haise (1963) method can be called
a potential evapotranspiration method; the method was developed using
evaporation data from a variety of crops grown in the western United
States and is not calibrated to any particular vegetation type. By
contrast, the Hargreaves-Samani (Hargreaves and Samani, 1985) method may
be called a reference evapotranspiration method; the method was
developed using data from weighing lysimeters growing *Festuca*
*altaica* grass.

Evapotranspiration estimation methods can be classified as temperature
based or energy based, or both. The reference evapotranspiration method
of choice is currently thought to be the FAO–56 Penman-Montieth method,
which is a combined temperature and energy-based approach (Allen and
others, 1998; Sentelhas and others, 2010); the FAO Penman-Monteith
method is not currently included in SWB because application of the
method requires gridded datasets for wind speed and relative humidity.
Gridded estimates of relative humidity, when available, are often
estimated from minimum and maximum air temperatures. The
Hargreaves-Samani method included in SWB is a simplified estimation
method recommended for use when not enough data are available to support
the Penman-Monteith approach. The Jensen-Haise approach may be more
applicable to sites in the southwestern United States.

### Soil-Moisture Retention/Actual Evapotranspiration

In SWB, three methods are included to implement the estimation of actual
evapotranspiration from the soil-moisture reservoir—THORNTHWAITE,
FAO-56, and FAO-56\_TWO\_STAGE. Actual evapotranspiration is the soil
moisture that can be extracted from a soil of a given soil-moisture
condition; by definition, actual evapotranspiration will be equal to or
less than the potential evapotranspiration. In the days following a
rainstorm, soil moisture is close to field capacity, and moisture is
evaporated from bare soil and transpired by plants at rates close to the
maximum rate sustainable given climatic conditions. Assuming no further
precipitation, in subsequent days the evaporation and transpiration
rates decrease as remaining soil moisture is held more tightly within
the soil matrix (Dunne and Leopold, 1978).

A technique to simulate decreasing rates of soil-moisture
evapotranspiration is to assume that the actual evapotranspiration is
some function of the potential or reference evapotranspiration and the
current soil-moisture amount (eq. 3).

where

is the actual evapotranspiration, in inches;

is the potential evapotranspiration, in inches;

is the current soil-moisture amount, in inches; and

is the soil field capacity, in inches.

The three soil-moisture retention functions implemented in SWB are
discussed in appendix 1. Of the three functions, one function was
developed by (Thornthwaite, 1948), and the other two functions were
included in the FAO–56 approach (Allen and others, 1998).

### Growing Degree Day

Growing degree-day calculations may be enabled, triggering a growing
degree-day calculation for each grid cell. SWB allows different base and
maximum temperatures to be assigned for each land-use or crop type.
Growing degree-day calculations are needed only if crop coefficients are
used to modulate actual evapotranspiration rates, and then only if any
of the crop-coefficient curves are defined in terms of growing degree
days.

### Crop Coefficients

The FAO–56 methodology links the estimation of actual evapotranspiration
to growth patterns of vegetation and crops by means of a
crop-coefficient curve that changes during the course of a growing
season. The amount of water required by the vegetation or crop at any
point during the growing season is determined by the following crop
evapotranspiration equation:

where

is the crop evapotranspiration amount, in inches;

is the crop coefficient (dimensionless); and

is the reference or potential evapotranspiration, in inches.

The crop evapotranspiration equation (eq. 4) is valid for ideal
conditions; the equation would remain valid if the soil moisture stayed
close to the field capacity regardless of plant water use.

### Rooting Depth

Rooting depth either can be assumed static throughout the simulation or
can be changed dynamically with the assumption that rooting depth is
proportional to the crop-coefficient curve. For purposes of computing
the water balance, the maximum root-zone depth is assumed to define the
size of the soil-moisture reservoir. Specifying a dynamic rooting depth
does not change the size of the soil-moisture reservoir; however,
specifying a dynamic rooting depth does change the total available
water, which is the amount available for plant growth.

### Irrigation Demand and Application

SWB includes a single method for calculating irrigation water demand
based on FAO–56 methodology (Allen and others, 1998). Once crop-water
requirements have been determined, the next step in the process of
simulating irrigation water demand is to apply the water in a realistic
manner. The following four rules are included in the module to describe
when simulated irrigation events take place.

*Restore soil moisture to field capacity*—Complete elimination of
soil-moisture deficit on a cell by cell basis. This option calculates
the amount of water to be applied as the difference between the maximum
soil-moisture value and the soil-moisture value from the previous day.
Thus, this amount ignores the current day’s water-balance components;
the same irrigation amount will be calculated regardless of rainfall
conditions.

*Restore soil moisture to some fraction of field capacity*—Restore soil
moisture to some specified tolerable level of soil-moisture deficit
(deficit irrigation). This option calculates the amount of water to be
applied as the difference between the soil moisture at some preset
deficit amount and the soil-moisture value from the previous day.

*Apply fixed amount of irrigation*—Apply the same, constant amount of
water once the soil-moisture deficit exceeds the maximum allowable
deficit. Many irrigators have sized their equipment to handle
application events of average size; for example, a center-pivot
irrigation setup may only be capable of delivering water within a narrow
range of values. Under this option, a set amount of water is applied to
the cell. If the set amount brings the soil moisture to a value in
excess of field capacity, a net infiltration event will be triggered.

*Apply demand-based amount on a prescribed monthly schedule.* This
option is similar to the “restore soil moisture to field capacity”
option, except that the calculated irrigation amount accounts for the
daily or monthly rainfall and runoff amount and only is applied on a set
schedule. This option was extracted from the Hawaii Water Budget code
and is designed to simulate the unique irrigation conditions in the
Pacific Islands; the calculation is dependent on the monthly, rather
than daily, datasets that generally are available in the Pacific
Islands.

If more control is needed as to which crops receive or do not receive
irrigation water, a supplementary irrigation mask may be supplied. This
irrigation mask can be helpful if the model domain contains a single
crop type, corn, for example, but has areas of irrigated cultivation and
areas of dryland farming. Without an irrigation mask, control over the
simulation of irrigation water application could be accomplished by
including separate land-use codes, one for irrigated corn and another
for dryland corn.

### Rejected Net Infiltration

Specification of maximum daily net-infiltration amounts is a crude but
effective way of preventing SWB from calculating unreasonably high
net-infiltration values. With flow routing enabled, downslope cells can
have significant amounts of water diverted to them. The resulting
calculated net-infiltration values sometimes exceed the values that
might be reasonable because of the soils and underlying geology. Setting
a maximum daily net-infiltration value will prevent these cells from
taking on unrealistic recharge values. Using this method, calculated net
infiltration in excess of the maximum net-infiltration rate will be
moved to the soil reservoir of the next downslope cell. A control-file
directive is not needed, but lookup-table entries defining the maximum
net-infiltration rate are required for each combination of land-use and
soil type to use this method.

# Summary

This report documents the U.S. Geological Survey Soil-Water-Balance
(SWB) code, version 2.0. SWB is designed to estimate net infiltration
and other water budget components by using readily available geographic
information systems (GIS) and gridded climate datasets. SWB is based on
a modified Thornthwaite-Mather soil-water-balance approach, with
components of the soil-water balance calculated at a daily time step.
Net-infiltration calculations are computed by means of a rectangular
grid of computational elements, which allows the calculated infiltration
rates to be imported into grid-based regional groundwater-flow models.
The code can include canopy interception, runoff, evaporation,
transpiration, rainfall, and snowmelt in a basic water budget.
Additional hydrologic components may be added to the simulation as
needed, including fog interception, crop-water demand and irrigation,
and direct additions to soil moisture and net infiltration. Version 2.0
of SWB is written so that additional process methods may be added more
easily and with minimal impact on the workflow of existing SWB 1.0
users.

Appendixes to this report contain additional detail on the methods
incorporated into SWB as well as a basic user’s guide and detail on the
format of required input grids and tables. The appendixes are as
follows:

  - Appendix 1. Method Documentation

  - Appendix 2. User Guide

  - Appendix 3. Input Data, Lookup-Table Entries, and Control-File
    Directives by Method

  - Appendix 4. Example Applications

# Acknowledgments

Primary support for Soil-Water-Balance (SWB) code, version 2.0, was
provided by the U.S. Geological Survey (USGS) Water Availability and Use
Science Program (WAUSP). Additional support was provided in the form of
related project work, which allowed for testing and development as well;
the USGS National Water-Quality Assessment provided support for the
application of SWB 1.0 for the glaciated portions of the conterminous
United States, which also provided opportunities for testing and further
development of SWB version 2.0. The authors thank Paul Barlow (USGS) and
Kevin Dennehey (USGS, retired) for their support of the SWB code. The
authors also thank our USGS colleagues Paul Barlow, Megan Haserodt,
Joseph Hughes, Andrew Leaf, and Rich Signell for their careful reviews
and testing of SWB example problems.

# References Cited

Akinremi, O.O., McGinn, S.M., and Barr, A.G., 1996, Simulation of soil
moisture and other components of the hydrological cycle using a water
budget approach: Canadian Journal of Soil Science, v. 76, no. 2, p.
133–142.

Allen, R.G., Pereira, L.S., Raes, Dirk, and Smith, Martin, 1998, Crop
evapotranspiration—Guidelines for computing crop water requirements:
Rome, Food and Agriculture Organization of the United Nations, FAO
Irrigation and Drainage Paper No. 56, 174 p.

Anderson, M.P., Woessner, W.W., and Hunt, R.J., 2015, Applied
groundwater modeling—Simulation of flow and advective transport (2d
ed.): Academic press, 564 p.

Barker, David, Beuerlein, Jim, Dorrance, Ann, Eckert, Donald, Eisley,
Bruce, Hammond, Ron, Lentz, Ed, Lipps, Pat, Loux, Mark, Mullen, Robert,
Sulc, Mark, Thomison, Peter, and Watson, Maurice, 2005, Ohio agronomy
guide (14th ed.): Columbus, Ohio, Ohio State University Extension,
Bulletin 472, 158 p, accessed August 8, 2017, at
<https://agcrops.osu.edu/sites/agcrops/files/imce/fertility/Ohio_Agronomy_Guide_b472.pdf>.

Batelaan, Okke, and De Smedt, Florimond, 2001, WetSpass—A flexible, GIS
based, distributed recharge methodology for regional groundwater
modelling: *in* Impact of Human Activity on Groundwater Dynamics,
Maastricht, The Netherlands, July 2001, no. 269, p. 11–17, accessed
September 27, 2017 at
<http://www.vub.ac.be/WetSpa/publications/Wetspass%20a%20flexible%20GIS%20based.pdf>.

Bauer, H.H., and Vaccaro, J.J., 1987, Documentation of a deep
percolation model for estimating ground-water recharge: U.S. Geological
Survey Open-File Report 86–536, 186 p., accessed December 16, 2015, at
<https://pubs.er.usgs.gov/publication/ofr86536>.

Bauer, H.H., and Vaccaro, J.J., 1990, Estimates of ground-water recharge
to the Columbia Plateau regional aquifer system, Washington, Oregon, and
Idaho, for predevelopment and current land-use conditions: U.S.
Geological Su[rvey Water-Resources Investigations Report 88–4108, 37,
p.,
acce](file:///C:\\Users\\dressler\\AppData\\Roaming\\Microsoft\\Word\\rvey%20Water-Resources%20Investigations%20Report%2088–4108,%2037,%20p.,%20acce)ssed
December 16, 2015, at
[https://pubs.er.usgs.gov/pubs/wri/wri884108](http://pubs.er.usgs.gov/pubs/wri/wri884108).

Boisvert, J.B., Bootsma, A., Dwyer, L.M., Brewin, D., 1990,
Irrigate—User guide for irrigation management by computer: Technical
Bulletin 1990-2E, Ottawa, Ontario, Agriculture Canada, Research Branch,
65 p., accessed September 27, 2017 at
<https://archive.org/details/irrigateuserguid19902bois>.

Braud, Isabelle, Tilmant, Francois, Samie, Rene, and Le Goff, Isabelle,
2013, Assessment of the SiSPAT SVAT model for irrigation estimation in
south-east France: Procedia Environmental Sciences, Elsevier, v. 19, p.
747–756.

Cronshey, Roger, McCuen, Richard, Miller, Norman, Rawls, Walter,
Robbins, Sam, and Woodward, Don, 1986, Urban hydrology for small
watersheds:U.S. Department of Agriculture, Natural Resources
Conservation Service, Conservation Engineering Division, Technical
Release 55 (2d ed.), accessed August 8, 2017, at
<https://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/16/stelprdb1044171.pdf>.

Dripps, W.R., and Bradbury, K.R., 2007, A simple daily soil–water
balance model for estimating the spatial and temporal distribution of
groundwater recharge in temperate humid areas: Hydrogeology Journal, v.
15, no. 3, p. 433–444.

Dugan, J.T., and Peckenpaugh, J.M., 1985, Effects of climate,
vegetation, and soils on consumptive water use and ground-water recharge
to the Central Midwest Regional aquifer system, Mid-continent United
States: U.S. Geological Survey Water-Resources Investigations Report
85–4236, 78 p., accessed January 11, 2016, at
<https://pubs.er.usgs.gov/usgspubs/wri/wri854236>.

Dunne, Thomas, and Leopold, Luna B., 1978, Water in environmental
planning: W.H. Freeman, 818 p.

Eilers, V.H.M., Carter, R.C., and Rushton, K.R., 2007, A single layer
soil water balance model for estimating deep drainage (potential
recharge)—An application to cropped land in semi-arid North-east
Nigeria: Geoderma, v. 140, nos. 1–2, p. 119–131.

Feinstein, D.T., Hunt, R.J., and Reeves, H.W., 2010, Regional
groundwater-flow model of the Lake Michigan Basin in support of Great
Lakes Basin water availability and use studies: U.S. Geological Survey
Scientific Investigations Report 2010–5109, 379 p., accessed November
22, 2016, at <https://pubs.usgs.gov/sir/2010/5109/>.

Finch, J.W., 2001, Estimating change in direct groundwater recharge
using a spatially distributed soil water balance model: Quarterly
Journal of Engineering Geology and Hydrogeology, v. 34, no. 1, p. 71–83.

Fitzsimons, V.P., and Misstear, B.D.R., 2006, Estimating groundwater
recharge through tills—A sensitivity analysis of soil moisture budgets
and till properties in Ireland: Hydrogeology Journal, v. 14, no. 4, p.
548–561.

Flint, A.L., and Flint, L.E., 2007, Application of the basin
characterization model to estimate in-place recharge and runoff
potential in the Basin and Range carbonate-rock aquifer system, White
Pine County, Nevada, and adjacent areas in Nevada and Utah: U.S.
Geological Survey Scientific Investigations Report 2007–5099, 20 p.,
accessed December 15, 2015, at <https://pubs.usgs.gov/sir/2007/5099/>.

Flint, L.E., Flint, A.L., 2014, California Basin Characterization
Model—A dataset of historical and future hydrologic response to
climate change: U.S. Geological Survey Data Release, accessed September
27, 2017, at https://doi.org/10.5066/F76T0JPB.

Gash, J.H.C., 1979, An analytical model of rainfall interception by
forests: Quarterly Journal of the Royal Meteorological Society, v. 105,
no. 443, p. 43–55.

Gash, J.H.C., Lloyd, C.R., and Lachaud, G., 1995, Estimating sparse
forest rainfall interception with an analytical model: Journal of
Hydrology, v. 170, no. 1, p. 79–86.

Gerrits, A.M.J., 2010, The role of interception in the hydrological
cycle: Delft, Netherlands, Delft University of Technology, Ph.D.
dissertation, 126 p., accessed September 27, 2017 at
<http://repository.tudelft.nl/view/ir/uuid:7dd2523b-2169-4e7e-992c-365d2294d02e/>

Gesch, Dean, Oimoen, Michael, Greenlee, Susan, Nelson, Charles, Steuck,
Michael, and Tyler, Dean, 2002, The national elevation dataset:
Photogrammetric Engineering and Remote Sensing, v. 68, no. 1, p. 5–12.

Hargreaves, G.H., and Samani, Z.A., 1985, Reference crop
evapotranspiration from temperature: Applied Engineering in Agriculture,
v. 1, no. 2, p. 96–99.

Healy, R.W., 2010, Estimating groundwater recharge: Cambridge University
Press, 245 p.

Homer, Colin, Dewitz, John, Yang, Limin, Jin, Suming, Danielson,
Patrick, Xian, George, Coulston, John, Herold, Nathaniel, Wickham,
James, and Megown, Kevin, 2015, Completion of the 2011 National Land
Cover Database for the conterminous United States—Representing a decade
of land cover change information: Photogrammetric Engineering and Remote
Sensing, v. 81, no. 5, p. 345–354.

Horton, R.E., 1919, Monthly weather review—Rainfall interception:
accessed November 1, 2016, at
<http://journals.ametsoc.org/doi/abs/10.1175/1520-0493%281919%2947%3C603%3ARI%3E2.0.CO%3B2>.

Hunt, R.J., Prudic, D.E., Walker, J.F., and Anderson, M.P., 2008,
Importance of unsaturated zone flow for simulating recharge in a humid
climate: Ground Water, v. 46, no. 4, p. 551–560.

Hunt, R.J., Westenbroek, S.M., Walker, J.F., Selbig, W.R., Regan, R.S.,
Leaf, A.T., and Saad, D.A., 2016, Simulation of climate change effects
on streamflow, groundwater, and stream temperature using GSFLOW and
SNTEMP in the Black Earth Creek Watershed, Wisconsin: U.S. Geological
Survey Scientific Investigations Report 2016–5091, 117 p., accessed
October 27, 2016, at <https://pubs.er.usgs.gov/publication/sir20165091>.

Izuka, S.K., Oki, D.S., and Engott, J.A., 2010, Simple method for
estimating groundwater recharge on tropical islands: Journal of
Hydrology, v. 387, no. 1, p. 81–89.

Jensen, M.E., 1969, Scheduling irrigation with computers: Journal of
Soil and Water Conservation, v. 24, no. 5, p. 193–195.

Jensen, M.E., and Haise, H.R., 1963, Estimating evapotranspiration from
solar radiation: American Society of Civil Engineers, Journal of the
Irrigation and Drainage Division, Proceedings, v. 89, p. 15–41.

Jyrkama, M.I., and Sykes, J.F., 2007, The impact of climate change on
spatially varying groundwater recharge in the Grand River watershed
(Ontario): Journal of Hydrology, v. 338, nos. 3–4, p. 237–250.

Jyrkama, M.I., Sykes, J.F., and Normani, S.D., 2002, Recharge estimation
for transient ground water modeling: Groundwater, v. 40, no. 6, p.
638–648.

Kendy, Eloise, Gerard-Marchant, Pierre, Walter, M. Todd, Zhang,
Yongqiang, Liu, Changming, and Steenhuis, Tammo, 2003, A
soil-water-balance approach to quantify groundwater recharge from
irrigated cropland in the North China Plain: Hydrological Processes, v.
17, no. 10, p. 2011–2031.

Lee, Cheng-Haw, Chen, Wei-Ping., and Lee, Ru-Huang, 2006, Estimation of
groundwater recharge using water balance coupled with base-flow-record
estimation and stable-base-flow analysis: Environmental Geology, v. 51,
no. 1, p. 73–82.

Manghi, Fakhri, Mortazavi, Behrooz, Crother, Christie, and Hamdi,
Moshrik, 2009, Estimating regional groundwater recharge using a
hydrological budget method: Water Resources Management, v. 23, no. 12,
p. 2475–2489.

Nimmo, J.R., Healy, R.W., and Stonestrom, D.A., 2005, Aquifer recharge
*in* Anderson, M.G., and Bear, J., eds., Encyclopedia of Hydrological
Science—Part 13, Groundwater: Chichester, United Kingdom, Wiley, v. 4,
p. 2229-2246.

Niswonger, R.G., Prudic, D.E., and Regan, R.S., 2006, Documentation of
the Unsaturated-Zone Flow (UZF1) Package for modeling unsaturated flow
between the land surface and the water table with MODFLOW–2005: U.S.
Geological Survey, Techniques and Methods, book 6, chap. A19, 62 p.

Rushton, K.R., and Ward, C., 1979, The estimation of groundwater
recharge: Journal of Hydrology, v. 41, nos. 3–4, p. 345–361.

Savenije, H.H.G., 2004, The importance of interception and why we should
delete the term evapotranspiration from our vocabulary: Hydrological
Processes, v. 18, no. 8, p. 1507–1511.

Scanlon, B.R., Healy, R.W., and Cook, P.G., 2002, Choosing appropriate
techniques for quantifying groundwater recharge: Hydrogeology Journal,
v. 10, no. 1, p. 18–39.

Schroeder, P.R., Dozier, T.S., Zappi, P.A., McEnroe, B.M., Sjostrom,
J.W., and Peyton, R.L., 1994, The hydrologic evaluation of landfill
performance (HELP) model—Engineering documentation for version 3:
Cincinnati, Ohio, U.S. Environmental Protection Agency Rick Reduction
Engineering Laboratory, 128 p.

Sentelhas, P.C., Gillespie, T.J., and Santos, E.A., 2010, Evaluation of
FAO Penman–Monteith and alternative methods for estimating reference
evapotranspiration with missing data in Southern Ontario, Canada:
Agricultural Water Management, v. 97, no. 5, p. 635–644.

Soil Survey Staff, 2015, Gridded soil survey geographic (gSSURGO)
database for the conterminous United States, accessed September 27, 2017
at https://nrcs.app.box.com/v/soils.

Srikanthan, R., Harrold, T.I., Sharma, A., and McMahon, T.A., 2005,
Comparison of two approaches for generation of daily rainfall data:
Stochastic Environmental Research and Risk Assessment, v. 19, no. 3, p.
215–226.

Srikanthan, R., and McMahon, T.A., 1982, Simulation of annual and
monthly rainfalls—A preliminary study at five Australian stations:
Journal of Applied Meteorology, v. 21, no. 10, p. 1472–1479.

Srikanthan, R., and McMahon, T.A., 1999, Stochastic generation of
annual, monthly and daily climate data—A review: Hydrology and Earth
System Sciences, v. 5, no. 4, p. 653–670.

Thornthwaite, C.W., 1948, An approach toward a rational classification
of climate: Geographical Review, v. 38, no. 1, p. 55–94.

Thornthwaite, C.W., and Mather, J.R., 1955, The water balance:
Publications in Climatology, v. 8, no. 1, p. 185–311.

Thornthwaite, C.W., and Mather, J.R., 1957, Instructions and tables for
computing potential evapotranspiration and the water balance:
Publications in Climatology, v. 10, no. 3, p. 1–104.

Thornton, P.E., Thornton, M.M., Mayer, B.W., Wei, Y., Devarakonda, R.,
Vose, R.S., and Cook, R.B., 2017, Daymet—Daily surface weather data on a
1-km grid for North America, version 3: accessed September 27, 2017, at
<http://dx.doi.org/10.3334/ORNLDAAC/1328>.

Unidata, 2017, NetCDF—Network Common Data Format C API, version 4.4.1:
Boulder, Colo., UCAR/Unidata Program Center, accessed September 27,
2017, at <http://doi.org/10.5065/D6H70CW6>.

U.S. Geological Survey, 2008, Documentation of computer program
INFIL3.0—A distributed-parameter watershed model to estimate net
infiltration below the root zone: U.S. Geological Survey Scientific
Investigations Report 2008–5006, 98 p., accessed August 8, 2017, at
<https://pubs.usgs.gov/sir/2008/5006/>.

Westenbroek, S.M., Kelson, V.A., Dripps, W.R., Hunt, R.J., and Bradbury,
K.R., 2010, SWB—A modified Thornthwaite-Mather Soil-Water-Balance code
for estimating groundwater recharge: U.S. Geological Survey Techniques
and Methods book 6, chap. A31, 60 p.

Appendixes

1.  Utilities Department, City of Bloomington, Indiana.
