## General Discussion of Gridded Datassets {#general_discussion_of_gridded_datasets}

SWB can ingest gridded data in three formats: Surfer, ESRI Arc ASCII, or netCDF.


### Supported File Types

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

### Geographic Projections

### Treatment of Missing Values
