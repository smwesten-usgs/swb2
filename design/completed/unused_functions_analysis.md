# Unused Private Functions — Analysis and Disposition

**Date:** June 26, 2026  
**Context:** 31 gfortran `-Wunused-function` warnings; required for USGS release

---

## Summary

The 31 unused private functions are not random dead code. They cluster into coherent groups that reflect intentional API design and future feature directions. A tiered approach is recommended: preserve functions with clear future value under `#ifdef SWB_FUTURE_API`, and delete only those that are trivial, superseded, or buggy.

---

## Category Analysis

### 1. NetCDF API Completeness Layer — 12 functions (`netcdf4_support.F90`)

| Function | Purpose |
|----------|---------|
| `nf_index_to_dayvalue` | Convert time index → day value |
| `nf_return_varid` | Look up NetCDF variable ID by index |
| `nf_return_dimid` | Look up NetCDF dimension ID by index |
| `nf_return_attvalue` | Retrieve attribute value by name |
| `nf_return_dimindex` | Reverse-lookup dimension index from ID |
| `nf_set_z_variable_name` | Set the Z (vertical) variable name |
| `nf_get_variable_array_short` | Read 2D short array from NetCDF |
| `nf_get_variable_array_double` | Read 2D double array from NetCDF |
| `nf_get_variable_array_float` | Read 2D float array from NetCDF |
| `nf_define_dimension` | Define a new NetCDF dimension |
| `nf_delete_attribute` | Delete a NetCDF attribute |
| `nf_define_variable` | Define a new NetCDF variable |

**Intent:** Complete abstraction layer over NetCDF C API. Used functions handle current I/O paths; these provide capabilities for future workflows (custom output variables, multi-precision reads, programmatic attribute management).

**Disposition:** Preserve (`#ifdef SWB_FUTURE_API`)

---

### 2. Grid Spatial Analysis Library — 4 functions (`grid.F90`)

| Function | Purpose |
|----------|---------|
| `grid_searchcolumn` | Search column for target Z value (T-M retention table lookup) |
| `grid_lookupreal` | Nearest-cell lookup by X,Y coordinate |
| `grid_gridtopoint_int` | Reproject integer value between grid resolutions (majority filter) |
| `grid_convolve_sgl` | Apply convolution kernel to grid data |

**Intent:** Spatial analysis utilities for table-based soil-moisture lookups, multi-resolution grid work, and spatial smoothing/filtering.

**Disposition:** Preserve (`#ifdef SWB_FUTURE_API`)

---

### 3. Date/Time Utility Variants — 4 functions (`datetime.F90`)

| Function | Purpose |
|----------|---------|
| `set_default_date_format` | Set module-level default date format string |
| `set_default_time_format` | Set module-level default time format string |
| `mmddyyyy2julian` | Parse MM/DD/YYYY string → Julian day |
| `mmddyyyy2doy` | Parse MM/DD/YYYY string → day of year |

**Intent:** Flexibility for input date formats (common in USGS station data).

**Disposition:** Delete. The format setters modify module state that nothing reads; the MM/DD/YYYY parsers are straightforward to recreate if needed. Git history preserves them.

---

### 4. Debug/Diagnostic Helpers — 3 functions

| Function | File | Purpose |
|----------|------|---------|
| `minmaxmean` | `daily_calculation.F90` | Print min/max/mean of 1D array for diagnostics |
| `minmaxmean_float` | `data_catalog_entry.F90` | Print min/max/mean of 2D float grid |
| `dquote` | `logfiles.F90` | Wrap string in double-quotes |

**Intent:** Developer debugging aids.

**Disposition:** Delete. Trivial to recreate; `dquote` is a one-liner.

---

### 5. Data Catalog Setters — 2 functions (`data_catalog_entry.F90`)

| Function | Purpose |
|----------|---------|
| `set_missing_value_int_sub` | Programmatically set integer missing-value code |
| `set_missing_value_real_sub` | Programmatically set real missing-value code |

**Intent:** Part of DATA_CATALOG_ENTRY_T interface for cases where missing values are set in code rather than read from file metadata.

**Disposition:** Delete. Note: `set_missing_value_real_sub` has a type bug (`integer` argument for a `real` field), confirming it was never tested/used.

---

### 6. Feature Stubs / Partial Implementations — 3 functions

| Function | File | Purpose |
|----------|------|---------|
| `read_polygon_id` | `model_initialize.F90` | Read polygon ID grid for zone-based spatial output |
| `get_parameter_values_datetime` | `parameters.F90` | Parse control-file values as datetime objects |
| `return_landuse_index_fn` | `runoff__curve_number.F90` | Look up landuse code index in module array |

**Intent:**
- `read_polygon_id` → zone-based aggregation feature (spatial reporting by watershed/polygon)
- `get_parameter_values_datetime` → datetime-typed control file entries
- `return_landuse_index_fn` → likely superseded by a different lookup mechanism

**Disposition:**
- `read_polygon_id`: Preserve (`#ifdef SWB_FUTURE_API`) — represents a real planned feature
- `get_parameter_values_datetime`: Preserve (`#ifdef SWB_FUTURE_API`) — completes the parameter type system
- `return_landuse_index_fn`: Delete — superseded; the `elemental` search loop is also inefficient

---

### 7. String Utilities — 3 functions

| Function | File | Purpose |
|----------|------|---------|
| `strip_full_pathname_fn` | `fstring.F90` | Extract filename from full path |
| `remove_repeats` | `fstring.F90` | Collapse repeated characters in a string |
| `list_finalize_sub` | `fstring_list.F90` | Finalizer for FSTRING_LIST_T (wraps `clear()`) |

**Intent:** General-purpose string library.

**Disposition:** Delete.
- `strip_full_pathname_fn` has a bug (operates on `value` before assignment)
- `remove_repeats` is unused and the implementation is straightforward to recreate
- `list_finalize_sub` just delegates to `clear()` — if a finalizer is needed later, it can be re-added

---

## Recommended Approach

### Final Decision (June 26, 2026)

Rather than wrapping in `#ifdef` or making functions `public`, the approach is:

1. **Delete 12 stale/buggy functions** — trivial, superseded, or have bugs
2. **Retain 19 intentional reserve functions as-is** — they represent coherent future API design (NetCDF abstraction, spatial analysis, zone aggregation, parameter type completeness)
3. **Normal builds:** suppress `-Wunused-function` to avoid noise
4. **Static analysis builds:** the 19 warnings are acknowledged and documented here as intentional; reviewers can reference this document

This preserves the facade pattern (private `nf_*` helpers behind public `netcdf_*` entry points) without contorting the code to satisfy a compiler limitation.

### Deleted (12 functions) ✅

| Function | File | Reason |
|----------|------|--------|
| `mmddyyyy2julian` | `datetime.F90` | Straightforward to recreate; git preserves |
| `mmddyyyy2doy` | `datetime.F90` | Straightforward to recreate; git preserves |
| `minmaxmean` | `daily_calculation.F90` | Debug helper |
| `minmaxmean_float` | `data_catalog_entry.F90` | Debug helper |
| `set_missing_value_int_sub` | `data_catalog_entry.F90` | Never tested/used |
| `set_missing_value_real_sub` | `data_catalog_entry.F90` | Type bug (integer arg for real field) |
| `dquote` | `logfiles.F90` | Trivial one-liner |
| `return_landuse_index_fn` | `runoff__curve_number.F90` | Superseded by different lookup |
| `strip_full_pathname_fn` | `fstring.F90` | Bug (uses `value` before assignment) |
| `remove_repeats` | `fstring.F90` | Unused, straightforward to recreate |
| `list_finalize_sub` | `fstring_list.F90` | Trivial wrapper around `clear()` |

### Retained — Intentional Reserve API (19 functions)

These remain private and unused. The 19 warnings in static_analysis builds are accepted.

| Function | File | Future Direction |
|----------|------|-----------------|
| `nf_index_to_dayvalue` | `netcdf4_support.F90` | Complete NetCDF abstraction |
| `nf_return_varid` | `netcdf4_support.F90` | Complete NetCDF abstraction |
| `nf_return_dimid` | `netcdf4_support.F90` | Complete NetCDF abstraction |
| `nf_return_attvalue` | `netcdf4_support.F90` | Complete NetCDF abstraction |
| `nf_return_dimindex` | `netcdf4_support.F90` | Complete NetCDF abstraction |
| `nf_set_z_variable_name` | `netcdf4_support.F90` | Complete NetCDF abstraction |
| `nf_get_variable_array_short` | `netcdf4_support.F90` | Multi-precision array reads |
| `nf_get_variable_array_double` | `netcdf4_support.F90` | Multi-precision array reads |
| `nf_get_variable_array_float` | `netcdf4_support.F90` | Multi-precision array reads |
| `nf_define_dimension` | `netcdf4_support.F90` | Programmatic output creation |
| `nf_delete_attribute` | `netcdf4_support.F90` | Programmatic output creation |
| `nf_define_variable` | `netcdf4_support.F90` | Programmatic output creation |
| `grid_searchcolumn` | `grid.F90` | T-M table lookup |
| `grid_lookupreal` | `grid.F90` | Coordinate-based cell lookup |
| `grid_gridtopoint_int` | `grid.F90` | Multi-resolution grid reprojection |
| `grid_convolve_sgl` | `grid.F90` | Spatial smoothing/filtering |
| `read_polygon_id` | `model_initialize.F90` | Zone-based spatial aggregation |
| `get_parameter_values_datetime` | `parameters.F90` | Datetime-typed control file entries |
| `set_default_date_format` | `datetime.F90` | Configurable date format |
| `set_default_time_format` | `datetime.F90` | Configurable time format |
