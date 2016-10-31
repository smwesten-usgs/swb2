# SWB 2.0 [EXPERIMENTAL] A Modified Thornthwaite-Mather Soil-Water-Balance Code for Estimating Groundwater Recharge {#mainpage}

The SWB model calculates recharge by use of commonly available geographic information system (GIS) data layers in combination with tabular climatological data. The code is based on a modified Thornthwaite-Mather soil-water-balance approach, with components of the soil water balance calculated at a daily timestep. Recharge calculations are made on a rectangular grid of computational elements that may be easily imported into a regional groundwater-
flow model. Recharge estimates calculated by the code may be output as daily, monthly, or annual values.

The code is written in modern Fortran (Fortran 95/2003/2008), and has been compiled on Windows, Macintosh, and Linux systems using the gfortran and Intel fortran compilers. The experimental version in this repository incorporates some of the latest Fortran 2003/2008 features. This version is an INCOMPLETE WORK IN PROGRESS.

*This documentation is auto-generated and may contain errors and omissions in content and formatting. You have been warned!*

Source code repository is here: [https://github.com/smwesten-usgs/swb2](https://github.com/smwesten-usgs/swb2)


Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. **The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information.** Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
