# Changes from Previous Versions {#changes_from_previous_versions}

The core functionality of SWB has not changed since its initial release in 2010 [@westenbroek_swbmodified_2010]. The scope of recent additions and modifications to the code are significant enough to warrant a new official SWB release along with a new publication.

Many of the changes to SWB have been "under the hood"; most users will never appreciate the difference between the initial and the current codes. Some of these changes will be very apparent to anyone familiar with the original SWB code. These changes have to do with model input and output, and include:

1. Elimination of swbstats;
2. Elimination of internally generated graphics;
3. Elimination of the custom swb binary output files;
4. Addition of PROJ.4 library (allows SWB to read grids with differing geographic projections);
5. Upgrading NetCDF input and output to NetCDF version 4;
6. Modification of internal structure to make adding new modules easier;
7. Addition of facility allowing for more flexible tabular data and parameter input;
8. Rearrangement of internal data structures to more efficiently simulate non-rectangular model domains.

Other changes since the initial SWB release add or modify the actual hydrologic processes simulated by SWB. These changes include the addition of the following modules:

1. Irrigation demand;
2. FAO-56 soil-moisture retention;
3. Interception: a) Horton, b) Gash;
4. Fog interception;
5. Rainfall: method of fragments;
6. Pervious/impervious subgrid characterization;
7. Runoff: monthly runoff ratio.
