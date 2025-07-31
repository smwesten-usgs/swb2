SWB2 - A Modified Thornthwaite-Mather Soil-Water-Balance Code for Estimating Groundwater Recharge
------------------------------------------------------------------------------------------------

The Soil-Water-Balance (SWB) computer code has been developed to calculate spatial and temporal
 variations in groundwater recharge. The SWB model calculates recharge by use of commonly available
 geographic information system (GIS) data layers in combination with tabular climatological data.
The code is based on a modified Thornthwaite-Mather soil-water-balance approach, with
components of the soil-water balance calculated at a daily timestep.
Recharge calculations are made on a rectangular grid of computational elements that may be
easily imported into a regional groundwater-flow model. Recharge estimates calculated by
 the code may be output as daily, monthly, or annual values.

Official documentation for the code may be found here: https://pubs.er.usgs.gov/publication/tm6A59.

Unofficial online supplemental documentation of the code may be found [here](http://smwesten-usgs.github.io/swb2/).


Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)


Open Source
-----------
This software makes use of several open source projects in order to improve functionality for users. These projects include:

1) PROJ      - https://proj.org/
2) netCDF    - https://www.unidata.ucar.edu/software/netcdf/
3) HDF5      - https://www.hdfgroup.org/solutions/hdf5
4) zlib      - http://zlib.net/

`PROJ` is included in SWB2 as a static snapshot of the code as it existed from when the project was known as `PROJ4`. SWB2 has not been updated to make use of later versions of `PROJ` in order to keep the SWB2 build somewhat simpler; more recent versions of `PROJ` have `sqlite3` as a dependency.

`netCDF`, `HDF5`, and `zlib` are either linked statically or dynamically depending on the platform. Generally static linking is possible using `gfortran` on Windows platforms. Dynamic linking of these libraries is often easier on Macintosh and Linux platforms.

