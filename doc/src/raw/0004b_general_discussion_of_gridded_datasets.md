## General Discussion of Gridded Datasets {#general_discussion_of_gridded_datasets}

SWB can ingest gridded data in three formats: Surfer, ESRI Arc ASCII, or netCDF. Often one or more files constituting a time series of gridded data are required in order to perform a simulation. In addition, missing values are often a feature of these gridded datasets. All of these topics are discussed further in the following sections.

### Specifying Grid Filenames

SWB can ingest gridded data in three formats: Surfer, ESRI Arc ASCII, or netCDF. Often one or more files constituting a time series of gridded data are required in order to perform a simulation. The way to do this is to supply a filename 'template' to SWB.

For example, over 43,000 individual Arc/Info ASCII grids were supplied in order to make a 100-year model run for the Lake Michigan Pilot Water Availability Study. The files were all given names with the pattern 'precip-*month*-*day*-*year*.asc', for example, *precip-03-12-1967.asc*.The syntax required to inform SWB of the file naming convention was (for precipitation):

`PRECIPITATION ARC_GRID precip-%0m-%0d-%Y.asc`

The characters immediately following '%' in the filename template shown above represent the following: `%0m`, the month number (1-12), padded by a leading zero, `%0d`, the day of the month, padded by a leading zero, and `%Y`, the 4-digit year value.

: Filename template values understood by SWB. {tbl:filename_template_values}

Template value         |  Meaning
-----------------------|----------------------------------------------
`%Y` or `%y`           | 4-digit year value
`%m`                   | month number, *not* zero padded (1-12)
`%0m`                  | month number, zero padded (01-12)
`%b`                   | abbreviated (3-letter) month name (jan-dec)
`%B`                   | full month name (january-december)
`%d`                   | day of month, *not* zero padded (1-31)
`%0d`                  | day of month, zero padded (01-31)
`#`                    | simple file counter, reset each year (1-n)
`#000`                 | simple file counter with three positions of zero padding, reset each year (1-n)

In addition, three modifiers may be specified in the control file in the event that SWB is being run on a non-Windows platform where capitalization matters:

`_MONTHNAMES_CAPITALIZED`
`_MONTHNAMES_UPPERCASE`
`_MONTHNAMES_LOWERCASE`

The modifiers are to be used in the control file prefixed by the data name. For example, to ensure uppercase monthnames are used in conjunction with precipitation data files, `PRECIPITATION_MONTHNAMES_UPPERCASE` can be added to the control file. These modifiers may be used for precipitation and temperature with the SWB 1.0 code, while they may be used with any known data type with SWB 2.0.

When used together, SWB can find and use a variety of files without required that they be renamed. Some examples:

: Examples showing the use of filename templates. {#tbl:filename_template_examples}

Example filename            | Template                | Control file modifier entry
----------------------------|-------------------------|--------------------------------------
prcp09Jan2010.asc           |   prcp%0m%b%Y.asc       | PRECIPITATION_MONTHNAMES_CAPITALIZED
tmin_2011.nc4               |   tmin_%Y.nc4           | *none*
tasmax-03-23-1977.asc       |   tasmax-%0m-%0d-%Y.asc | *none*
precip_january_1981.nc      |   precip_%B_%Y.nc       | PRECIPITATION_MONTHNAMES_LOWERCASE


### Supported File Types

Three file formats are supported as input to SWB: Surfer ASCII grids, Arc/Info ASCII grids, and NetCDF files. Each format is discussed further in the following sections.

#### Surfer ASCII Grid

Golden Software’s ASCII grid format consists of a 5-line header followed by the data values arranged in a matrix.

```
DSAA
14    5
0.5   7.0
-0.4  0.0
0.0   7.0
0.5   1.   1.5  2.   2.5  3.   3.5  4.   4.5  5.   5.5  6.   6.5  7.0
0.45  0.9  1.4  1.9  2.4  2.9  3.4  3.9  4.4  4.9  5.4  5.9  6.4  6.9
0.4   0.8  1.3  1.8  2.3  2.8  3.3  3.8  4.3  4.8  5.3  5.8  6.3  6.8
0.36  0.72 1.21 1.7  2.2  2.7  3.2  3.7  4.2  4.7  5.2  5.7  6.2  6.7
0.32  0.64 1.13 1.6  2.1  2.6  3.1  3.6  4.1  4.6  5.1  5.6  6.1  6.6
```

The header values contain:

1.	“DSAA”, a label identifying the file format as a Golden Software ASCII grid,
2.	Number of columns (number of X values), number of rows (number of Y values),
3.	Minimum X value, maximum X value,
4.	Minimum Y value, maximum Y value,
5.	Minimum Z value, maximum Z value.

As one might expect, the matrix of values is arranged such that the “x” coordinates increase from lower to higher values as one moves left to right over the columns. The “y” coordinates correspond to the rows of data and decrease from higher values to lower values as one moves down the rows.


#### ESRI ASCII Grid

The publishers of ArcMap and ArcView software, the Environmental Systems Research Institute (ESRI), developed one of the most commonly used raster data formats in use. ESRI’s ASCII grid format is a simple matrix representation of the gridded dataset with a short header tacked to the top of the file.

```
ncols        34
nrows        4
xllcorner    739475.000000000000
yllcorner    2314000.000000000000
cellsize     10.000000000000
NODATA_value -9999
9 9 9 9 9 9 9 9 9 9 9 8 8 8 8 8 8 8 9 9 9 9 9 9 8 8 8 8 8 8 8 8 9 9
7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 6 6 6
7 7 7 7 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
7 7 7 7 7 7 7 7 7 7 6 6 6 7 7 7 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
```

Note that SWB does not really handle the NODATA_value codes as given in the ESRI ASCII grid files; missing values should be handled through the use of user-supplied control file directives, discussed later in this section.


#### NetCDF

NetCDF, or Network Common Data File, is a file format commonly used by researchers in atmospheric and oceanic sciences. One of the key benefits of NetCDF files is that they are designed to be platform-independent; in other words, a NetCDF file generated on a Macintosh by an application compiled with the PGI compiler should be able to be read by an application compiled with the Intel compiler and running on Windows. In addition, NetCDF files are able to store arbitrary combinations of data. This allows for substantial metadata to be stored in the NetCDF file along with the variable of interest.

A set of conventions, known as the Climate and Forecast Metadata Conventions, gives recommendations regarding the kind and nature of metadata to be included along with the primary variable within a NetCDF file [@eaton_netcdf_2011]. SWB outputs written to NetCDF files attempt to adhere to the Climate and Forecast Metadata Conventions version 1.6 (CF 1.6) in order to maximize the number of third-party NetCDF tools that will work with SWB output.

In addition to these benefits of NetCDF file use, the fact that there are dozens of open-source tools available to read, write, and visualize NetCDF files makes them a good candidate for use with SWB. One of the most basic tools, distributed by Unidata, the maintainer of NetCDF file format, is called ncdump—a program to “dump” the contents of a NetCDF file.
If we take a recent Daymet file containing daily precipitation values for 2014, we can see the stored metadata in the file:

```
> ncdump -h prcp_2014.nc4
The following metadata is returned:
netcdf prcp_2014 {
dimensions:
	x = 5268 ;
	y = 4823 ;
	time = UNLIMITED ; // (365 currently)
	nv = 2 ;
variables:
	float x(x) ;
		x:units = "m" ;
		x:long_name = "x coordinate of projection" ;
		x:standard_name = "projection_x_coordinate" ;
	float y(y) ;
		y:units = "m" ;
		y:long_name = "y coordinate of projection" ;
		y:standard_name = "projection_y_coordinate" ;
	float lat(y, x) ;
		lat:units = "degrees_north" ;
		lat:long_name = "latitude coordinate" ;
		lat:standard_name = "latitude" ;
	float lon(y, x) ;
		lon:units = "degrees_east" ;
		lon:long_name = "longitude coordinate" ;
		lon:standard_name = "longitude" ;
	float time(time) ;
		time:long_name = "time" ;
		time:calendar = "standard" ;
		time:units = "days since 1980-01-01 00:00:00 UTC" ;
		time:bounds = "time_bnds" ;
	short yearday(time) ;
		yearday:long_name = "yearday" ;
		yearday:valid_range = 1s, 365s ;
	float time_bnds(time, nv) ;
	short lambert_conformal_conic ;
		lambert_conformal_conic:grid_mapping_name = "lambert_conformal_conic" ;
		lambert_conformal_conic:longitude_of_central_meridian = -100. ;
		lambert_conformal_conic:latitude_of_projection_origin = 42.5 ;
		lambert_conformal_conic:false_easting = 0. ;
		lambert_conformal_conic:false_northing = 0. ;
		lambert_conformal_conic:standard_parallel = 25., 60. ;
	float prcp(time, y, x) ;
		prcp:_FillValue = -9999.f ;
		prcp:cell_methods = "area: sum time: sum" ;
		prcp:coordinates = "lat lon" ;
		prcp:grid_mapping = "lambert_conformal_conic" ;
		prcp:long_name = "daily total precipitation" ;
		prcp:missing_value = -9999.f ;
		prcp:units = "mm/day" ;
		prcp:valid_range = 0.f, 200.f ;

// global attributes:
		:start_year = 2014s ;
		:source = "Daymet Software Version 2.0" ;
		:Version_software = "Daymet Software Version 2.0" ;
		:Version_data = "Daymet Data Version 2.1" ;
		:Conventions = "CF-1.4" ;
		:citation = "Please see http://daymet.ornl.gov/ for current Daymet data citation information" ;
		:references = "Please see http://daymet.ornl.gov/ for current information on Daymet references" ;
}
```

This particular file contains three classes of metadata: dimensions, variables, and global attributes. As can be seen above, the file contains data about 4 “dimensions”: x, y, time, and nv. Nine variables are defined, each of which is references in terms of the dimensions. The key variable in the file is “prcp”—the daily precipitation value. prcp is defined at each time (day) in the file for all values of x and y. Note the way that dates and times are specified in the NetCDF file: as a real-valued number of days since 1980-01-01 00:00:00 UTC.

SWB does not have the ability to make sense of much of this metadata. It is the user’s responsibility to be aware of the physical units that each of the datasets is stored in. Control file directives may be used to convert precipitation in metric units (mm/day) to inches per day. __We recommend examining the SWB output values of air temperature and precipitation in order to verify that any such unit conversions have been done correctly.__ It is particularly easy to get the temperature conversion wrong; SWB will run happily, air temperatures will never make it very high, resulting in unrealistically elevated potential recharge values.

In addition, SWB cannot parse the variables and attributes associated with any map projection that may have been used when the NetCDF file was created. The user needs to be aware of the geographic projection (if any) that was used. If the gridded data do not match the SWB project bounds exactly, a “PROJ4 string” must be provided to enable SWB to translate between project coordinates and the NetCDF file coordinates.

### Geographic Projections

Later versions of SWB are linked to the PROJ4 library, originally written by a USGS scientist [@evenden_cartographic_1990]. PROJ4 is used by many commonly used mapping applications, both open source and proprietary. The library provides a set of routines that may be used to calculate forward and backward coordinate transformations. SWB make use of this library in order to translate coordinates found in external data files to the coordinates in use by the SWB project.

### Treatment of Missing Values
