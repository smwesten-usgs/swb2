## Tabular data {#tabular_datasets}

In addition to the gridded data, one or more lookup tables must be provided in order to supply parameter values to the SWB modules. Many parameters are specified for specific combinations of landuse categories and hydrologic soils groups. A list of the required parameters for each SWB module may be found in the appendices.

The required parameters for the Lake Michigan Pilot Water Availability Project SWB application used as an example case in Westenbroek and others (2010) are given in the table below.

Module Type	                 |              Module Name   |	        Specified By	|  Parameters
-----------------------------|----------------------------|-----------------------|---------------
precipitation	gridded        |		--
potential evapotranspiration |	Hargreaves-Samani		      | control file          |
runoff                       |	SCS curve number	        | landuse and soil type |	curve number
actual evapotranspiration    |	Thornthwaite              |	landuse and soil type |	rooting depth
canopy interception	         |  bucket                    |	landuse	              | growing and nongrowing season interception values
--                           |	--                        | landuse and soil type |	maximum recharge rate


In the original version of the SWB model, the parameters given above were “hard-wired”; in other words, SWB required the lookup table to be structured such that the parameters were supplied in a non-flexible column order. In this way, parameter values for curve numbers would be supplied in the first columns, followed by the maximum recharge rates, interception values, and rooting depths. In addition, the original version of SWB required that the number of soil types and landuses be specified, and did not allow nor require a table header.

The new version of SWB uses keywords to identify parameter values within the table; the new lookup tables allow parameters to be supplied in any arbitrary column order. A separate column of parameter values must be supplied for each soil type. A snippet of the new table format is given below.

LU code  | Description               |   Surface Storage Max |     CN 1|    CN 2 |     CN 3 |     CN 4
---------|---------------------------|-----------------------|---------|---------|----------|--------
0        | Background                |   0                   |     100 |      100|       100|      100
2        | Pineapple                 |   0                   |     42  |      64 |       76 |       81
3        | Coffee                    |   0                   |     52  |      70 |       80 |       84
4        | Diversified Agriculture   |   0                   |     55  |      72 |       82 |       85
5        | Macadamia                 |   0                   |     44  |      65 |       77 |       82
6        | Fallow_grassland          |   0                   |     37  |      61 |       74 |       79
7        | Developed Open Space      |   0                   |     37  |      61 |       74 |       79
8        | Developed Low intensity   |   0                   |     60  |      75 |       84 |       87
9        | Developed Medium intensity|   0.25                |     70  |      82 |       88 |       91
10       | Developed High intensity  |   0.25                |     81  |      88 |       92 |       94

In SWB 2.0, it is critical that each column be clearly identified so that the proper parameters may be linked to their respective process modules.
