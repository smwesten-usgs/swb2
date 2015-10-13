# Control File Directives {#appendix_2_control_file_directives_original_swb}

[TOC]

This section provides a complete list of the control file statements understood by SWB, version 1.x. 

# Control File Directives: Gridded Datasets

For each of the three major climate datasets (precipiration, minimum and maximum air temperature), a standard set of suffixes may be added to the dataset name to control how SWB treats the dataset. The list of suffixes understood by SWB is long:

| Suffix                             | Argument         | Description                             |
|------------------------------------|------------------|-----------------------------------------|
| _SCALE_FACTOR                      |  *real value*    | amount to multiply raw grid value by prior to use |
| _ADD_OFFSET                        |  *real value*    | amount to add to the raw grid value following application of the scale factor, if any |
| _NETCDF_X_VAR                      |  *string*        | name of the variable to be used as the "x" axis |
| _NETCDF_Y_VAR                      |  *string*        | name of the variable to be used as the "y" axis |
| _NETCDF_Z_VAR                      |  *string*        | name of the variable to be used as the "z" (value) axis |
| _NETCDF_TIME_VAR                   |  *string*        | name of the variable to be used as the "time" axis |
| _NETCDF_VARIABLE_ORDER             |  "xyt or txy"    | description of the order in which the gridded data were written |
| _NETCDF_FLIP_VERTICAL              |  **none**        | if present, all gridded data will be "flipped" around the vertical axis. |
| _NETCDF_FLIP_HORIZONTAL            |  **none**        | if present, all gridded data will be "flipped" around the horizontal axis  |
| _NETCDF_MAKE_LOCAL_ARCHIVE         |  |
| _PROJECTION_DEFINITION             |  | PROJ.4 string describing the geographic projection of the dataset
| _MINIMUM_ALLOWED_VALUE             | *real value* | ceiling to be applied to the data; data above this value will be reset to this amount
| _MAXIMUM_ALLOWED_VALUE             | *real value* | floor to be applied to the data; data beneath this value will be reset to this amount   
| _MISSING_VALUES_CODE               | *real or integer value* | value   
| _MISSING_VALUES_OPERATOR           | "<", "<=", ">", ">=" | 
| _MISSING_VALUES_ACTION             | "mean" or "zero" | "mean" will substitute the mean value calculated over the remaining valid cells; "zero" will substitute a value of 0.0 in place of missing values
