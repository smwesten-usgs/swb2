## Cartographic projections and resampling {#projections_and_resampling}

One significant feature added to SWB since the initial release is the ability to use datasets that vary from the base grid in gridcell size, cartographic projection, and geographic extent. To accomplish this SWB incorporates a piece of software called PROJ4 to perform transformations between various map projections. PROJ4 was originally written by Gerald Evenden of the USGS [evenden_cartographic_1990].

*Specification of a coordinate transformation for the base grid is optional!* There are two modes of operation with respect to coordinate transformations:

1.	__No transformations or resampling__. This is how SWB 1.0 operated. All grids must be of the same dimension, cell size, and geographic projection of the project grid.

2.	__Transformation and resampling__. This is triggered if a PROJ4 string is supplied for the base model grid definition. If no PROJ4 string is supplied for a data grid, SWB assumes that the projection is the same as the base grid.

The specific attributes of a projection are defined by supplying SWB with a PROJ4 string. A PROJ4 string may be assembled by specifying.

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


: PROJ4 strings for some common cartographic projections. {tbl:proj4_for_common_projections}

Projection name                         | PROJ4 string
----------------------------------------|----------------------------
<unprojected> (geographic coordinates)	| +proj=lonlat +datum=WGS84 +no_defs
Universal Transverse Mercator (UTM)	    |+proj=utm +zone=18 +north +ellps=GRS80 +datum=NAD83 +units=m +no_defs
Wisconsin Transverse Mercator (WTM)	    | +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m
Lambert Conformal Conic	                |+proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
USA Contiguous Albers Equal Area (USGS version) |	+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
USA Contiguous Albers Equal Area	+             | proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs

If transformation and resampling have been activated by supplying a PROJ4 string to the base model grid, SWB takes the following additional steps in order to supply values for computation:

1.	Creates an array of coordinates for the data grid in native projected coordinates;

2.	Transforms the native projected coordinates to SWB base coordinates;

3.	Finds the indices or the data gridcell closest to each of the SWB base coordinates;

4.	Obtain the data grid values for the set of indices found in step 3;

5.	Return an array of all the values obtained in step 4.

The process outlined above is essentially a nearest neighbor resampling scheme. Anything much more complex results in much slower execution times. The SWB user must judge whether or not such a procedure is acceptable.

If, for example, the data grid contains precipitation data at a 4km grid resolution, and the underlying SWB base resolution is 200m, there is nothing lost applying a simple nearest neighbor approach. Interpolating this type of data could be done, but would provide only the illusion of greater accuracy—a smoother precipitation surface.

However, if the SWB base grid is 1km, and the underlying data grid contains landuse data at a 90m resolution, the nearest neighbor approach may or may not be acceptable. A simple majority filter may be invoked for integer grids, but will still characterize the landuses found in a tiny subset of the data gridcells corresponding to the SWB base gridcell. In this case, it may be preferable to use an external GIS procedure to resample the landuse to the SWB base grid resolution.

Specification of a cartographic projection for an SWB model is accomplished with the `BASE_PROJECTION_DEFINITION` control file statement. For example, to specify that the coordinates of a model grid be interpreted by means of the Wisconsin Transverse Mercator projection, the following control file statement would be added:  

`BASE_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m`
