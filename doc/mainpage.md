# SWB 2.0 - A Modified Thornthwaite-Mather Soil-Water-Balance Code for Estimating Groundwater Recharge #

The SWB model calculates recharge
by use of commonly available geographic information system
(GIS) data layers in combination with tabular climatological
data. The code is based on a modified Thornthwaite-Mather
soil-water-balance approach, with components of the soilwater
balance calculated at a daily timestep. Recharge
calculations are made on a rectangular grid of computational
elements that may be easily imported into a regional groundwater-
flow model. Recharge estimates calculated by the code
may be output as daily, monthly, or annual values.

The code is written in Fortran 95/2003, and has been compiled
on Windows and Linux systems using the gfortran, g95, and
Intel fortran compilers.

Support is provided for:
- Reading and writing NetCDF files
- Handling gridded data whose projections differ from the model domain projection
- Estimating irrigation amounts required to sustain plant growth

The fortran modules documented here
must be linked against the NetCDF library:
- NetCDF: http://www.unidata.ucar.edu/software/netcdf/

## Documentation ##

Westenbroek, S.M., Kelson, V.A., Dripps, W.R., Hunt, R.J.,
and Bradbury, K.R., 2010, SWB-A modified Thornthwaite-Mather
Soil-Water-Balance code for estimating groundwater recharge:
U.S. Geological Survey Techniques and Methods 6-A31, 60 p. http://pubs.usgs.gov/tm/tm6-a31/

@page Conventions
Conventions to follow when developing SWB modules.

The authors have attempted to follow the conventions described below
in the development of the SWB code.

## Variables ##
 - \b integer: names begin with \em i \n
     example: iCount
 - \b real (float): names begin with \em r or \em f \n
     example: rValue, fValue
 - \b double precision: names begin with \em dp or \em d \n
     example: dpValue, dValue
 - \b logical: names begin with \em l \n
     example: lMatch
 - \b character: names begin with \em s \n
     example: sFileName
 - \b pointer: names begin with \em p; applies to pointer of any type \n
     example: pGrid

## Parameters ##
 - Parameter names are generally entirely UPPERCASE letters
 - Normal parameter names begin with the letters specified above,
   depending on the type of the parameter
 - Constants used throughout the code to specify program options
   are composed of UPPERCASE letters without a type prefix
 - Constants specifying fortran logical unit numbers (for i/o) have prefix \em LU_
 - Parameters of derived type have a suffix of \em _T

## Doxygen ##

Suggested minimum Doxygen elements to be inserted immediately before each
subroutine or function:
- \\@brief
- \\@details
- \\@param[in]
- \\@param[out]
- \\@param[in/out]
- \\@return

@page Licenses

SWB2 makes use of several third-party software libraries. 

# PROJ.4 #

The PROJ.4 library is derived from code originally developed by Gerald Evenden of the USGS. Gerald's code has been extended by a number of individuals and the modified code has been placed under an MIT license:

> Copyright (c) 2000, Frank Warmerdam
> 
> Permission is hereby granted, free of charge, to any person obtaining a
> copy of this software and associated documentation files (the "Software"),
> to deal in the Software without restriction, including without limitation
> the rights to use, copy, modify, merge, publish, distribute, sublicense,
> and/or sell copies of the Software, and to permit persons to whom the
> Software is furnished to do so, subject to the following conditions:
> 
> The above copyright notice and this permission notice shall be included
> in all copies or substantial portions of the Software.
> 
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
> OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
> THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
> LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
> FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
> DEALINGS IN THE SOFTWARE.

 More information about the PROJ.4 code may be found here: http://trac.osgeo.org/proj/ [exit USGS]

# NetCDF #

The NetCDF library has been developed by the University Corporation for Atmospheric Research/Unidata. The library provides SWB2 with the capability to read and write files in both the NetCDF3 and NetCDF4 formats. The NetCDF library license is reproduced below:

> Copyright 1993-2014 University Corporation for Atmospheric Research/Unidata
> 
> Portions of this software were developed by the Unidata Program at the University Corporation for Atmospheric Research.
> 
> Access and use of this software shall impose the following obligations and understandings on the user. The user is granted the right, without any fee or cost, to use, copy, modify, alter, enhance and distribute this software, and any derivative works thereof, and its supporting documentation for any purpose whatsoever, provided that this entire notice appears in all copies of the software, derivative works and supporting documentation. Further, UCAR requests that the user credit UCAR/Unidata in any publications that result from the use of this software or in any product that includes this software, although this is not an obligation. The names UCAR and/or Unidata, however, may not be used in any advertising or publicity to endorse or promote any products or commercial entity unless specific written permission is obtained from UCAR/Unidata. The user also understands that UCAR/Unidata is not obligated to provide the user with any support, consulting, training or assistance of any kind with regard to the use, operation and performance of this software nor to provide the user with any updates, revisions, new versions or "bug fixes."
> 
> THIS SOFTWARE IS PROVIDED BY UCAR/UNIDATA "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UCAR/UNIDATA BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE ACCESS, USE OR PERFORMANCE OF THIS SOFTWARE.

More information about NetCDF may be found here: http://www.unidata.ucar.edu/software/netcdf/ [exit USGS]

@page Changelog

Changes to the SWB2 code are listed here in chronological order with the most recent code changes
listed first.

September 26, 2014:
--------------------

* Initial version released.


