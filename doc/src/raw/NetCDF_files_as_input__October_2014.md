# Using NetCDF files as input to SWB #
###* October, 2014 *###

----------


Version 1.0 of SWB could read either Surfer or Arc ASCII grid files in order to supply SWB with spatially varied climate data. SWB version 1.2 (Beta) includes several features that make data input much easier that the methods available with the initial version of SWB. Specifically, SWB Version 1.2( Beta) has the following new capabilities:


1. **NetCDF4 compatibility** means that many  of the gridded climate data products coming online are accessible by SWB. If desired, data may be accessed directly via the Internet.
2. ** Map projections** are now required information for a SWB run, because if SWB knows the projection of the base grid, all other input grids can be reprojected on the fly to conform to the base grid extents and projection.    

An interesting and useful side effect of the addition of these two capabilities is that the resolution of the base grid can be changed with a single control file entry. No other changes to SWB input are required.

### SWB Control File Entries for Specific Datasets ###

One of the ideas behind the development of the NetCDF was to create a file format that is platform-independent and as self-describing as possible. Use of the NetCDF file format along with conventions, such as the Climate and Forecast Conventions (CF-1.6 as of this writing, see: http://cfconventions.org/index.html), allow these datasets to be easily shared, used, and understood by researchers.

However, occasionally one still encounters problems regarding the use of NetCDF files. For example, if the software used to create a NetCDF file writes out latitude and longitude in a nonstandard way, client software (such as a visualization package) may end up plotting the data in a mirrored or inverted fashion. A telltale sign that this is the case is when a SWB run for a location in the northern hemisphere generates more snowmelt in the southern end of the model grid relative to the northern end.

Nearly a dozen keywords have been added to SWB in order to allow the data in a NetCDF file to be used as input. These keywords allow the user to specify the following NetCDF file attributes:


1. Variable names
2. Geographic projection
3. Offset and scale factors
4. Missing value codes and treatments
5. Reversal of horizontal or vertical axes

##### Variable names (NetCDF only)

The box below shows the control file directives that deal with variable names. Often the variables names for the x, y, and time variables are, simply, "x", "y", and "time". If they are not so named, use a control file directive to specify what the variable name should be. Replace "PRECIP" with "TMAX" or"TMIN" as appropriate to specify NetCDF variable names for SWB air temperature inputs.  
```
NETCDF_PRECIP_X_VAR
NETCDF_PRECIP_Y_VAR
NETCDF_PRECIP_Z_VAR
NETCDF_PRECIP_TIME_VAR
```

##### Geographic projection (ASCII grids _or_ NetCDF)

The format for this control file directive is shown below.
```
PRECIPITATION_GRID_PROJECTION_DEFINITION
```
As before, replace "PRECIPITATION" with "TMIN" or "TMAX" in order to specify the projection definition for air temperature files.

SWB incorporates a piece of software called PROJ.4 to perform geographic transformations between various map projections. PROJ.4 was originally written by Gerald Evenden of the USGS. Documentation for the original version may be found here: http://pubs.er.usgs.gov/publication/ofr90284.

The specific attributes of a projection are defined by supplying SWB with a PROJ.4 string. A PROJ.4 string may specify attribute such as those shown below.

Parameter  | Definition
-----------|----------------------------------------------
+a         | Semimajor radius of the ellipsoid axis
+alpha     | ? Used with Oblique Mercator and possibly a few others
+axis      | Axis orientation (new in 4.8.0)
+b         | Semiminor radius of the ellipsoid axis
+datum     | Datum name (see `proj -ld`)
+ellps     | Ellipsoid name (see `proj -le`)
+k         | Scaling factor (old name)
+k_0       | Scaling factor (new name)
+lat_0     | Latitude of origin
+lat_1     | Latitude of first standard parallel
+lat_2     | Latitude of second standard parallel
+lat_ts    | Latitude of true scale
+lon_0     | Central meridian
+lonc      | ? Longitude used with Oblique Mercator and possibly a few others
+lon_wrap  | Center longitude to use for wrapping (see below)
+nadgrids  | Filename of NTv2 grid file to use for datum transforms (see below)
+no_defs   | Don't use the /usr/share/proj/proj_def.dat defaults file
+over      | Allow longitude output outside -180 to 180 range, disables wrapping (see below)
+pm        | Alternate prime meridian (typically a city name, see below)
+proj      | Projection name (see `proj -l`)
+south     | Denotes southern hemisphere UTM zone
+to_meter  | Multiplier to convert map units to 1.0m
+towgs84   | 3 or 7 term datum transform parameters (see below)
+units     | meters, US survey feet, etc.
+vto_meter | vertical conversion to meters.
+vunits    | vertical units.
+x_0       | False easting
+y_0       | False northing
+zone      | UTM zone




### Statistically Downscaled Climate Projections for the US and southern Canada east of the Rocky Mountains.

http://cida.usgs.gov/thredds/catalog.html?dataset=wicci

**
```
PRECIPITATION NETCDF http://cida.usgs.gov/thredds/dodsC/wicci/cmip3/sres_early
PRECIPITATION_GRID_PROJECTION_DEFINITION +proj=lonlat +ellps=WGS84 +datum=WGS84 +no_defs
NETCDF_PRECIP_X_VAR lon
NETCDF_PRECIP_Y_VAR lat
NETCDF_PRECIP_Z_VAR sresa1b-cccma_cgcm3_1-prcp-01
NETCDF_PRECIP_TIME_VAR time
NETCDF_PRECIP_FLIP_VERTICAL

# conversion factor is 1/25.4; converting mm to inches
PRECIPITATION_CONVERSION_FACTOR 0.03937008
PRECIPITATION_MISSING_VALUES_CODE -32768.
PRECIPITATION_MISSING_VALUES_OPERATOR <=
PRECIPITATION_MISSING_VALUES_ACTION ZERO
```
```
TEMPERATURE NETCDF http://cida.usgs.gov/thredds/dodsC/wicci/cmip3/sres_early http://cida.usgs.gov/thredds/dodsC/wicci/cmip3/sres_early

TMAX_GRID_PROJECTION_DEFINITION +proj=lonlat +ellps=WGS84 +datum=WGS84 +no_defs
NETCDF_TMAX_X_VAR lon
NETCDF_TMAX_Y_VAR lat
NETCDF_TMAX_Z_VAR sresa1b-cccma_cgcm3_1-tmax-01
NETCDF_TMAX_TIME_VAR time
NETCDF_TMAX_FLIP_VERTICAL
TMAX_MISSING_VALUES_CODE -32768.
TMAX_MISSING_VALUES_OPERATOR <=
TMAX_MISSING_VALUES_ACTION MEAN

# convert from degrees C to degrees F
TMAX_SCALE_FACTOR 1.8
TMAX_ADD_OFFSET 32
```

The control file entries for the minimum air temperatures (TMIN) are identical to those shown for TMAX, except that TMIN is substituted everywhere a TMAX is present in the lines above.
