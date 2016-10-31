# Changes from Previous Versions {#changes_from_previous_versions}

The core functionality of SWB has not changed since its initial release [@westenbroek_swbmodified_2010]. The code still performs a modified Thornthwaite-Mather soil water balance at each grid point within the model domain. However, the scope of recent process module additions and modifications of the input and output file structures are significant enough to warrant a new major SWB release along with a new publication.

Many of the changes to SWB have been "under the hood"; most users will never appreciate the difference between the initial and the current codes. Some of these changes will be very apparent to anyone familiar with the original SWB code. These changes have to do with model input and output, and include:

1.	Elimination of swbstats,
2.	Elimination of internally generated graphics,
3.	Elimination of the custom swb binary output files,
4.	Addition of PROJ.4 library (allows SWB to read grids with differing geographic projections),
5.	Upgrading NetCDF input and output to NetCDF version 4,
6.	Modification of internal structure to make adding new modules easier,
7.	Addition of facility allowing for more flexible tabular data and parameter input, and
8.	Rearrangement of internal data structures to more efficiently simulate non-rectangular model domains.

Many of the changes outlined above were made in response to frustrations relating to the difficulty of aligning and resampling input grids; SWB 1.0 required that every grid supplied to it be in exactly the same geographic projection, cover the same extents as the SWB project grid, at the same gridcell resolution. This requirement resulted in much needless data manipulation and consumed project time that would better have been spent on more worthwhile project tasks. In addition, SWB 1.0 stored results in a custom-programmed binary file format. Following a SWB 1.0 model run, a little program called ‘swbstats’ could be run to extract daily, monthly, annual, or period grids as well as generate grids and plots.

When the opportunity to modify SWB 1.0 came, the authors decided to overhaul some of the input and output code. SWB 2.0 stores all gridded output in a common gridded and widely used format: NetCDF [@unidata_netcdf_2014]. The NetCDF file format is in common use amongst climate scientists and meteorologists, and has been slowly gaining favor in other scientific fields. The benefit of switching to a well-known binary file format is that rather than relying on a single program, swbstats, to handle post-processing, there are dozens of active, maintained open-source tools designed to make post-processing of NetCDF files easier.

Other changes since the initial SWB release add or modify the actual hydrologic processes simulated by SWB. These changes include the addition of the following modules:

1.	Irrigation demand,
2.	FAO-56 soil-moisture retention,
3.	Interception: a) Horton, b) Gash,
4.	Fog interception,
5.	Rainfall: method of fragments,
6.	Pervious/impervious subgrid characterization, and
7.	Runoff: monthly runoff ratio.
