### Precipitation ### {#precipitation}

[TOC]

Several options exist for supplying SWB with precipitation data. The most straightforward involves simply reading a set of interpolated values from a gridded data file. If gridded data do not exist, the method of fragments may be used to disaggregate monthly grids into a set of distributed daily precipitation values.

#### Gridded / Tabular

This is the traditional method for supplying SWB with precipitation data. For both SWB 1.0 and SWB 2.0, either gridded or tabular data may be supplied. The details are slightly different between the two methods, and are discussed in the appendixes.

If tabular data are supplied, each daily value of precipitation will be supplied to all gridcells within the model domain. This makes the use of tabular data suitable for application only to small project areas with dimensions of perhaps up to one-hundred square kilometers. Of course, this rule of thumb must be tempered by knowledge of the spatial variability in rainfall, as well as by the project goals. If only annual water budget components are of interest, a single precipitation gage may work fine. On the other hand, if SWB output is to be used at a monthly or daily timestep, gridded data of some sort are probably best, if available.

#### Method of Fragments

The method of fragments creates synthetic sequences of daily rainfall from monthly rainfall by imposing the rainfall pattern from selected rain gages with daily data (see, for example, @srikanthan_stochastic_1999). The synthesized daily rainfall data approximate the long-term average character of daily rainfall, such as frequency, duration, and intensity, but may not necessarily reproduce the actual historical daily rainfall record. Rain gages should be selected on the basis of location, and the length and completeness of daily records. Thiessen polygons are drawn around each of the selected rain gages, and the daily rainfall pattern within each Thiessen polygon is assumed to be the same as the pattern at the rain gage.

Daily rainfall fragments are calculated by dividing each daily rainfall measurement for a particular month by the total rainfall measured at the gage for that month. This results in a set of fragments for that particular month in which the total number of fragments is equal to the number of days in the month. Fragment sets are compiled for every selected gage for every month in which complete daily rainfall measurements are available. Fragment sets are grouped by month of the year and by rain gage. In the water-balance calculation, the fragment set used for a given gage for a given month is selected randomly from among all available sets for that gage for that month. Daily rainfall for a given month is synthesized by multiplying total rainfall for that by each fragment in the set.

{#Fig:fragments_file}

`6  43   1      0 0.0352      0      0      0 0.0282 0.0282 0.0915 0.0352      0 0.0423 …    0.1972`

`6  44   1      0      0      0      0      1      0      0      0      0      0      0 …         0 `

`6  45   1      0      0      0      0      0      0      0      0      0      0 0.1333 …         0 `

`6  46   1      0      0      0      0      0      0      0      0      0      0      0 …         0`

`6  47   1 0.0498   0.01      0 0.0166 0.0066  0.005  0.005      0 0.0083   0.01 0.0449 …    0.0166`

The fragments file ([@fig:fragments_file]) contains columns of data arranged as follows:
* column 1: month number (1 to12)
* column 2: rainfall gage ID number (1 to num rainfall gages)
* column 3: fragment index number (1 to num fragments for the given rainfall gage)
* columns 4-34: daily fragment value

The __sum__ of columns 4 through 34 will, by definition, equal one.
