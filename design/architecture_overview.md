# SWB2 Codebase Architecture Overview

**Last updated:** July 2026  
**SWB2 version:** v2.4.1  

---

## High-Level Execution Flow

```
main.F90
  │
  ├─ Parse command-line arguments (output_dir, data_dir, lookup_dir)
  ├─ LOGS%initialize() — open log files
  ├─ read_control_file() → PARAMS_DICT dictionary
  │
  ├─ initialize_all() [model_initialize.F90]
  │     ├─ Initialize simulation dates (SIM_DT)
  │     ├─ Initialize grid (MODEL%initialize_grid)
  │     ├─ Read static grids (land use, soils, flow direction, AWC, etc.)
  │     ├─ Set method pointers from control file directives
  │     ├─ Initialize ancillary values (phenology determines growth methods per LU)
  │     ├─ Initialize each method (crop coefficients defer to phenology for Kcb method)
  │     └─ Allocate all state arrays in MODEL
  │
  ├─ iterate_over_simulation_days(MODEL) [model_iterate.F90]
  │     ├─ initialize_output() — create NetCDF output files
  │     └─ DO WHILE (SIM_DT%curr <= SIM_DT%end)
  │           ├─ get_weather_data() — read precip, tmin, tmax
  │           ├─ perform_daily_calculation(MODEL) [daily_calculation.F90]
  │           ├─ write_output(MODEL) — write to NetCDF
  │           └─ SIM_DT%addDay()
  │
  └─ finalize_output() — close NetCDF files
```

---

## Daily Water Balance (per cell)

```
gross_precip ──┬── rainfall (T > threshold)
               └── snowfall (T ≤ threshold) → snow_storage
                                                    │
                                              snowmelt (T-index)
                                                    │
rainfall + snowmelt + fog + runon = INFLOW
         │
         ├── interception (canopy) → interception_storage
         │
         ├── net_rainfall = rainfall - interception
         │
         ├── runoff (Curve Number, adjusted for frozen ground)
         │
         ├── impervious fraction → surface_storage
         │
         └── infiltration = (inflow - runoff) × pervious_frac
                    │
                    ├── actual_et (FAO-56 or Thornthwaite-Mather)
                    │
                    └── SOIL MASS BALANCE:
                          soil_storage += infiltration - actual_et
                          IF soil_storage > max:
                              net_infiltration = excess  ← RECHARGE
                          ELSE:
                              net_infiltration = 0

         + direct_net_infiltration (septic, leakage)

         TOTAL RECHARGE = net_infiltration + direct_net_infiltration
```

If D8 routing is enabled, cells are processed upstream→downstream and `runoff` from each cell becomes `runon` to the downstream cell.

---

## Key Derived Types

### MODEL_DOMAIN_T (model_domain.F90) — The Central State Container

Contains ALL grid-cell state as 1D arrays (packed by active cells):
- **Climate inputs:** `gross_precip`, `tmin`, `tmax`, `rainfall`, `snowfall`
- **Storage states:** `soil_storage`, `snow_storage`, `surface_storage`, `interception_storage`
- **Fluxes:** `runoff`, `infiltration`, `net_infiltration`, `actual_et`, `reference_et0`
- **Properties:** `landuse_code`, `soil_group`, `awc`, `curve_num_adj`, `pervious_fraction`
- **~40 procedure pointers** (e.g., `calc_runoff`, `calc_actual_et`, `calc_snowmelt`) set at initialization

### GENERAL_GRID_T (grid.F90)
2D grid with coordinate system (PROJ4 string), cell size, extents. Used for reading/writing Arc ASCII grids.

### DATA_CATALOG_ENTRY_T (data_catalog_entry.F90)
Describes one input dataset: filename template, source/target projections, data type, scaling. Handles static grids, dynamic (time-varying) grids, NetCDF files, and tabular data.

### DATA_CATALOG_T (data_catalog.F90)
Linked list of DATA_CATALOG_ENTRY_T objects with add/find/delete methods.

### DATETIME_T / DATE_RANGE_T (datetime.F90, simulation_datetime.F90)
Date arithmetic, parsing, comparison operators. `SIM_DT` is the global simulation date range.

### PARAMETERS_T / DICT_T (parameters.F90, dictionary.F90)
Key-value store for control file directives and lookup tables.

---

## Module Dependency Graph

```
constants_and_conversions  ← (used by nearly everything)
    ↑
logfiles ← exceptions ← fstring ← fstring_list ← dictionary
    ↑           ↑
datetime ← simulation_datetime
    ↑
grid ← (constants, exceptions, fstring, logfiles)
    ↑
netcdf4_support ← (grid, constants, exceptions, logfiles, fstring, proj4_support, datetime)
    ↑
data_catalog_entry ← (grid, netcdf4_support, constants, datetime, exceptions, fstring, parameters)
    ↑
data_catalog ← (data_catalog_entry, logfiles, fstring)
    ↑
model_domain ← (constants, data_catalog, data_catalog_entry, datetime, exceptions,
                 logfiles, simulation_datetime, fstring_list, grid, parameters, netcdf4_support)
    ↑
model_initialize ← (constants, datetime, data_catalog, data_catalog_entry, dictionary,
                     exceptions, file_operations, grid, logfiles, model_domain, output,
                     parameters, simulation_datetime, fstring, fstring_list)
    ↑
daily_calculation ← (model_domain, exceptions, mass_balance modules, simulation_datetime)
    ↑
model_iterate ← (constants, daily_calculation, logfiles, model_domain, simulation_datetime, output)
    ↑
main ← (constants, logfiles, model_initialize, model_domain, model_iterate, fstring, timer)
```

---

## Complete Module Catalog

### Core Orchestration

| Module | File | Description |
|--------|------|-------------|
| (program) | `main.F90` | Entry point: CLI args, control file, initialize, iterate |
| `model_domain` | `model_domain.F90` | Central state container (163KB — largest file) |
| `model_initialize` | `model_initialize.F90` | Control file parsing, grid/method setup |
| `model_iterate` | `model_iterate.F90` | Daily time-stepping loop |
| `model_iterate_multiple_simulations` | `model_iterate_multiple_simulations.F90` | Monte Carlo precipitation variant |
| `daily_calculation` | `daily_calculation.F90` | One day's water-balance orchestration |

### Data I/O

| Module | File | Description |
|--------|------|-------------|
| `data_catalog` | `data_catalog.F90` | Linked-list container of input datasets |
| `data_catalog_entry` | `data_catalog_entry.F90` | Single input dataset descriptor (87KB) |
| `netcdf4_support` | `netcdf4_support.F90` | NetCDF-4 read/write wrappers (155KB) |
| `netcdf_c_api_interfaces` | `netcdf_c_api_interfaces.F90` | Raw C-binding interfaces to libnetcdf |
| `output` | `output.F90` | NetCDF output file management, 32 output variables |
| `grid` | `grid.F90` | 2D grid type, ASCII grid I/O, PROJ4 transforms (100KB) |
| `weather_data_tabular` | `weather_data_tabular.F90` | Tabular weather station data reader |

### Infrastructure / Utilities

| Module | File | Description |
|--------|------|-------------|
| `constants_and_conversions` | `constants_and_conversions.F90` | Physical constants, unit conversions, globals |
| `exceptions` | `exceptions.F90` | assert/die/warn error handling |
| `logfiles` | `logfiles.F90` | Dual-file logging with configurable verbosity |
| `parameters` | `parameters.F90` | Lookup table reader, key-value parameter store |
| `dictionary` | `dictionary.F90` | Linked-list key→string_list dictionary |
| `fstring` | `fstring.F90` | String utilities and operators |
| `fstring_list` | `fstring_list.F90` | Dynamic string list |
| `datetime` | `datetime.F90` | Date/time type with Julian day arithmetic |
| `simulation_datetime` | `simulation_datetime.F90` | Simulation date range (global SIM_DT) |
| `file_operations` | `file_operations.F90` | ASCII file line-by-line reader |
| `timer` | `timer.F90` | Wall-clock timing |
| `proj4_support` | `proj4_support.F90` | PROJ4 coordinate system wrapper |
| `solar_calculations` | `solar_calculations.F90` | Day length, solar radiation geometry |
| `meteorological_calculations` | `meteorological_calculations.F90` | Derived met variables |
| `running_grid_stats` | `running_grid_stats.F90` | Running statistics on grid values |
| `kiss_random_number_generator` | `kiss_random_number_generator.F90` | KISS PRNG |
| `summary_statistics` | `summary_statistics.F90` | Summary stats output |

### Process Modules (Pluggable via Procedure Pointers)

| Module | File | Method Category |
|--------|------|-----------------|
| `actual_et__thornthwaite_mather` | `actual_et__thornthwaite_mather.F90` | Actual ET |
| `actual_et__fao56` | `actual_et__fao56.F90` | Actual ET |
| `actual_et__fao56__two_stage` | `actual_et__fao56__two_stage.F90` | Actual ET |
| `actual_et__gridded_values` | `actual_et__gridded_values.F90` | Actual ET |
| `et__hargreaves_samani` | `et__hargreaves_samani.F90` | Reference ET₀ |
| `et__jensen_haise` | `et__jensen_haise.F90` | Reference ET₀ |
| `et__gridded_values` | `et__gridded_values.F90` | Reference ET₀ |
| `et__zone_values` | `et__zone_values.F90` | Reference ET₀ |
| `crop_coefficients__fao56` | `crop_coefficients__fao56.F90` | Crop coefficients (defers to phenology for method selection) |
| `runoff__curve_number` | `runoff__curve_number.F90` | Runoff |
| `runoff__gridded_values` | `runoff__gridded_values.F90` | Runoff |
| `snowfall__original` | `snowfall__original.F90` | Snow partitioning |
| `snowmelt__original` | `snowmelt__original.F90` | Snowmelt |
| `interception__bucket` | `interception__bucket.F90` | Interception |
| `interception__gash` | `interception__gash.F90` | Interception |
| `routing__D8` | `routing__D8.F90` | Flow routing |
| `irrigation` | `irrigation.F90` | Irrigation demand |
| `fog__monthly_grid` | `fog__monthly_grid.F90` | Fog drip |
| `growing_degree_day` | `growing_degree_day.F90` | GDD |
| `growing_degree_day_baskerville_emin` | `growing_degree_day_baskerville_emin.F90` | GDD |
| `phenology` | `phenology.F90` | Vegetation phenology (growth stage, growing season) |
| `awc__depth_integrated` | `awc__depth_integrated.F90` | Available water capacity |
| `awc__gridded_values` | `awc__gridded_values.F90` | Available water capacity |
| `rooting_depth__FAO56` | `rooting_depth__FAO56.F90` | Rooting depth |
| `continuous_frozen_ground_index` | `continuous_frozen_ground_index.F90` | Frozen ground |
| `maximum_net_infiltration` | `maximum_net_infiltration.F90` | Infiltration cap |
| `precipitation__method_of_fragments` | `precipitation__method_of_fragments.F90` | Stochastic precip |
| `direct_net_infiltration__gridded_data` | `direct_net_infiltration__gridded_data.F90` | Direct recharge |
| `direct_soil_moisture__gridded_data` | `direct_soil_moisture__gridded_data.F90` | Direct soil moisture |
| `storm_drain_capture` | `storm_drain_capture.F90` | Storm drain capture |

### Mass Balance Modules

| Module | File | Description |
|--------|------|-------------|
| `mass_balance__snow` | `mass_balance__snow.F90` | Snow storage conservation |
| `mass_balance__interception` | `mass_balance__interception.F90` | Interception storage conservation |
| `mass_balance__impervious_surface` | `mass_balance__impervious_surface.F90` | Impervious surface water balance |
| `mass_balance__soil` | `mass_balance__soil.F90` | Soil zone → produces net_infiltration (recharge) |

---

## Key Design Patterns

1. **Strategy pattern via procedure pointers**: MODEL_DOMAIN_T has ~40 procedure pointers set at initialization based on control file directives. This allows swapping algorithms without conditional logic in the daily loop.

2. **1D packed arrays**: All state variables are 1D arrays indexed by active cell number (not 2D row×col), improving cache performance. A `sort_order` array maps routing sequence to cell indices.

3. **Linked-list data catalogs**: Input datasets managed as a linked list allowing dynamic addition of data sources from control file directives.

4. **Separation of mass balances**: Four distinct mass-balance modules (snow, interception, impervious, soil) each enforce conservation independently.

5. **Coordinate-system-aware I/O**: Input grids can be in any projection; PROJ4 handles reprojection to the model grid's coordinate system transparently.

6. **Phenology-first initialization**: The phenology module reads growth timing columns (GDD thresholds, stage lengths, planting dates) and determines `PHENOLOGY_METHOD_INDEX` per land use. Downstream modules (crop coefficients, interception) defer to this determination rather than independently re-reading and validating the same columns.

---

## Build Structure (src/meson.build)

The source is compiled as a static library (`swb_library`) which is then linked into:
- `swb2` — the main executable
- `swbstats2` — a post-processing statistics utility
- `swbtest` — the unit test executable

External dependencies: netCDF, HDF5, zlib (linked statically on Windows, dynamically on Linux/macOS).
Bundled dependency: PROJ4 (static snapshot compiled as a separate static library).
