### Precipitation

Several options exist for supplying SWB with precipitation data. The most straightforward involves simply reading a set of interpolated values from a gridded data file. If gridded data do not exist, the method of fragments may be used to disaggregate monthly grids into a set of distributed daily precipitation values.

#### Gridded / Tabular

#### Method of Fragments

The method of fragments creates synthetic sequences of daily rainfall from monthly rainfall by imposing the rainfall pattern from selected rain gages with daily data (see, for example, @srikanthan_stochastic_1999). The synthesized daily rainfall data approximate the long-term average character of daily rainfall, such as frequency, duration, and intensity, but may not necessarily reproduce the actual historical daily rainfall record. Rain gages should be selected on the basis of location, and the length and completeness of daily records. Thiessen polygons are drawn around each of the selected rain gages, and the daily rainfall pattern within each Thiessen polygon is assumed to be the same as the pattern at the rain gage. 

Daily rainfall fragments are calculated by dividing each daily rainfall measurement for a particular month by the total rainfall measured at the gage for that month. This results in a set of fragments for that particular month in which the total number of fragments is equal to the number of days in the month. Fragment sets are compiled for every selected gage for every month in which complete daily rainfall measurements are available. Fragment sets are grouped by month of the year and by rain gage. In the water-balance calculation, the fragment set used for a given gage for a given month is selected randomly from among all available sets for that gage for that month. Daily rainfall for a given month is synthesized by multiplying total rainfall for that by each fragment in the set.
