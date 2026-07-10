# Doxygen Documentation Coverage Plan

**Date:** July 2026
**Current coverage:** 12.9% (105 of 817 routines documented)
**Module-level docs:** 26 of 69 files (37.7%)

---

## Documentation Style

All Fortran documentation should use Doxygen Javadoc style (per `design/expectations_for_code.md`):

```fortran
!> @brief Brief one-line description.
!!
!! Extended description if needed.
!!
!! @param[in]  param_name  Description of input parameter.
!! @param[out] result      Description of output parameter.
!! @retval     value       Description of function return value.
!!
!! @note Implementation follows equation XX, Reference (Year).
```

Module-level documentation:

```fortran
!> @file
!! Contains module @ref module_name.

!> Brief module description.
!!
!! Extended description of what this module provides.
module module_name
```

---

## Priority 1: Quick Wins (1–3 undocumented routines per file)

Small files that can be fully documented in minutes. Completing these brings many files to 100%.

- [ ] `daily_calculation.F90` (1 routine)
- [ ] `model_iterate.F90` (1 routine)
- [ ] `mass_balance__interception.F90` (1 routine)
- [ ] `mass_balance__soil.F90` (1 routine)
- [ ] `mass_balance__snow.F90` (1 routine)
- [ ] `mass_balance__impervious_surface.F90` (1 routine)
- [ ] `snowfall__original.F90` (1 routine)
- [ ] `snowmelt__original.F90` (1 routine)
- [ ] `proj4_support.F90` (1 routine)
- [ ] `et__jensen_haise.F90` (1 routine)
- [ ] `actual_et__thornthwaite_mather.F90` (1 routine)
- [ ] `interception__bucket.F90` (2 routines)
- [ ] `growing_season.F90` (2 routines)
- [ ] `growing_degree_day_baskerville_emin.F90` (2 routines)
- [ ] `storm_drain_capture.F90` (2 routines)
- [ ] `disclaimers.F90` (2 routines)
- [ ] `awc__depth_integrated.F90` (2 routines)
- [ ] `meteorological_calculations.F90` (3 routines)
- [ ] `growing_degree_day.F90` (3 routines)
- [ ] `et__hargreaves_samani.F90` (3 routines)

**Estimated effort:** 1–2 hours total for all Priority 1 files.

---

## Priority 2: High-Impact Public API Modules

These modules are used heavily by other parts of the code (or will be by the phenology work). Clear documentation here pays dividends.

- [ ] `parameters.F90` — 10 routines (file loading, munging, retrieval)
- [ ] `exceptions.F90` — 11 routines (die, warn, assert, check_for_fatal_warnings)
- [ ] `datetime.F90` — 46 routines (date parsing, arithmetic, comparisons, Julian day)
- [ ] `logfiles.F90` — 10 routines (logging infrastructure)
- [ ] `simulation_datetime.F90` — 7 routines (SIM_DT management)
- [ ] `timer.F90` — 9 routines (TIMER_T type)
- [ ] `file_operations.F90` — 16 routines (ASCII_FILE_T for reading input files)

**Estimated effort:** 1–2 days for all Priority 2 files.

---

## Priority 3: Core Science Modules

These implement the key algorithms. Documentation should include equation references.

- [ ] `crop_coefficients__fao56.F90` — 6 undocumented of 8 (partially done)
- [ ] `actual_et__fao56__two_stage.F90` — 4 undocumented of 10 (partially done)
- [ ] `runoff__curve_number.F90` — 6 undocumented of 7
- [ ] `interception__gash.F90` — 2 undocumented of 3 (partially done)
- [ ] `routing__D8.F90` — 7 routines
- [ ] `precipitation__method_of_fragments.F90` — 4 undocumented of 8 (partially done)
- [ ] `irrigation.F90` — 2 undocumented of 3
- [ ] `continuous_frozen_ground_index.F90` — 1 undocumented of 2

**Estimated effort:** 1–2 days for all Priority 3 files.

---

## Priority 4: Infrastructure / Utility Modules

Heavily used internally but less likely to be read by end users.

- [ ] `fstring.F90` — 30 undocumented of 31
- [ ] `fstring_list.F90` — 29 undocumented of 29
- [ ] `dictionary.F90` — 20 undocumented of 25
- [ ] `constants_and_conversions.F90` — 10 undocumented of 48 (already 79%)
- [ ] `running_grid_stats.F90` — 7 routines
- [ ] `summary_statistics.F90` — 9 routines
- [ ] `kiss_random_number_generator.F90` — 5 routines
- [ ] `weather_data_tabular.F90` — 5 routines

**Estimated effort:** 2–3 days for all Priority 4 files.

---

## Priority 5: Large Internal Modules (Document Opportunistically)

These are the largest files. Document when touching them for other work rather than as a standalone effort.

- [ ] `model_domain.F90` — 123 undocumented of 124 (largest file, strategy pattern hub)
- [ ] `netcdf4_support.F90` — 81 undocumented of 83 (I/O layer)
- [ ] `data_catalog_entry.F90` — 55 undocumented of 55 (data management)
- [ ] `grid.F90` — 35 undocumented of 39 (grid operations)
- [ ] `model_initialize.F90` — 21 undocumented of 24 (startup/config)
- [ ] `swbstats2_support.F90` — 28 undocumented of 28 (post-processor)
- [ ] `output.F90` — 11 undocumented of 11
- [ ] `model_iterate_multiple_simulations.F90` — 6 undocumented of 6

**Estimated effort:** Ongoing — document as you refactor.

---

## Priority 6: Skip / Low Value

- [ ] `netcdf_c_api_interfaces.F90` — 47 routines, all C binding interfaces. Minimal value from Doxygen docs; the C API is documented elsewhere.
- [ ] `test_params.F90`, `test_kiss.F90` — test utilities, not production code

---

## Module-Level Documentation Needed (43 files)

Files currently missing `!> @file` or module-level `!!` documentation:

- [ ] `model_domain.F90`
- [ ] `data_catalog_entry.F90`
- [ ] `fstring.F90`
- [ ] `fstring_list.F90`
- [ ] `swbstats2_support.F90`
- [ ] `model_initialize.F90`
- [ ] `dictionary.F90`
- [ ] `file_operations.F90`
- [ ] `output.F90`
- [ ] `exceptions.F90`
- [ ] `logfiles.F90`
- [ ] `parameters.F90`
- [ ] `summary_statistics.F90`
- [ ] `timer.F90`
- [ ] `routing__D8.F90`
- [ ] `simulation_datetime.F90`
- [ ] `running_grid_stats.F90`
- [ ] `model_iterate_multiple_simulations.F90`
- [ ] `runoff__curve_number.F90`
- [ ] `weather_data_tabular.F90`
- [ ] `kiss_random_number_generator.F90`
- [ ] `meteorological_calculations.F90`
- [ ] `growing_degree_day.F90`
- [ ] `disclaimers.F90`
- [ ] `interception__gash.F90`
- [ ] `interception__bucket.F90`
- [ ] `growing_season.F90`
- [ ] `growing_degree_day_baskerville_emin.F90`
- [ ] `storm_drain_capture.F90`
- [ ] `daily_calculation.F90`
- [ ] `continuous_frozen_ground_index.F90`
- [ ] `snowfall__original.F90`
- [ ] `snowmelt__original.F90`
- [ ] `model_iterate.F90`
- [ ] `mass_balance__interception.F90`
- [ ] `mass_balance__soil.F90`
- [ ] `mass_balance__snow.F90`
- [ ] `mass_balance__impervious_surface.F90`
- [ ] `proj4_support.F90`
- [ ] `solar_calculations.F90` (routines are documented but module header missing)

---

## Files Already at 100% (No Action Needed)

- ✅ `solar_calculations.F90` — 17/17 routines documented
- ✅ `data_catalog.F90` — 9/9 routines documented

---

## Tracking Progress

After each documentation session, update this file:
- Mark checkboxes as completed
- Update the coverage numbers at the top
- Note any issues discovered while documenting (e.g., confusing logic, potential bugs)
