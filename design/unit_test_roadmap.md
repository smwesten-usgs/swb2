# Unit Test Roadmap

**Created:** 2026-07-15
**Purpose:** Track which modules have unit tests, which need them, and what infrastructure changes (PARAMS dependency injection) should accompany new test development.

---

## Pattern for Adding Tests to a Module

When writing unit tests for a module that calls `PARAMS%get_parameters`:

1. Add `type(PARAMETERS_T), intent(inout) :: params` argument to the initialize routine
2. Replace internal `PARAMS%get_parameters(...)` with `params%get_parameters(...)`
3. Remove `use parameters, only: PARAMS` from the module (if nothing else uses it)
4. Update caller in `model_domain.F90` / `model_initialize.F90` to pass `PARAMS`
5. In the test, create a local `PARAMETERS_T` instance with only the data needed

This ensures each test suite is self-contained and order-independent.

---

## Current Coverage

| Module | Unit Tests | PARAMS Injected | Notes |
|--------|:----------:|:---------------:|-------|
| `phenology.F90` | ✅ 37 tests | ✅ | DOY, GDD, FAO56_DATES, FAO56_GDD, dispatch, init |
| `crop_coefficients__fao56.F90` | ✅ 10 tests | ✅ | Kcb interpolation, method detection, Eq. 72, Example 35 |
| `growing_degree_day.F90` | ✅ 1 test | ❌ | Basic GDD calculation only. Needs per-landuse base/max tests. |
| `datetime.F90` | ✅ 28 tests | N/A | No PARAMS usage |
| `fstring_list.F90` | ✅ 19 tests | N/A | No PARAMS usage |
| `constants_and_conversions.F90` | ✅ 25 tests | N/A | No PARAMS usage |
| `parameters.F90` | ✅ 9 tests | N/A | Tests PARAMETERS_T itself |
| `exceptions.F90` | ✅ 6 tests | N/A | No PARAMS usage |
| `interception__gash.F90` | ✅ 2 tests | ❌ | Basic Gash interception case |
| `solar_calculations.F90` | ✅ 15 tests | N/A | No PARAMS usage |
| `timer.F90` | ✅ 7 tests | N/A | No PARAMS usage |

---

## Modules Needing Unit Tests

### High Priority (Phase 4 — will be modified soon)

| Module | What to Test | PARAMS Columns Used |
|--------|-------------|---------------------|
| `interception__bucket.F90` | Storage max interpolation with `growth_fraction`; binary vs continuous mode; growing/nongrowing switch | `Interception_storage_max_growing`, `Interception_storage_max_nongrowing` |
| `rooting_depth__FAO56.F90` | Zr interpolation from `growth_fraction`; min/max bounds; zero growth_fraction case | `Maximum_rooting_depth`, soil group table |
| `actual_et__fao56__two_stage.F90` | Plant height from `growth_fraction`; few calculation; Kr/Ke stages; interaction with Kcb | REW/TEW tables, plant height, `Kcb_max` |

### Medium Priority (core model logic, no tests)

| Module | What to Test | PARAMS Columns Used |
|--------|-------------|---------------------|
| `growing_degree_day.F90` | Per-landuse `GDD_Base`/`GDD_Max` override; Baskerville-Emin vs simple; edge cases (Tmin > Tmax, all below base) | `GDD_Base`, `GDD_Max` |
| `growing_degree_day_baskerville_emin.F90` | Same as above but with sine-curve approximation | `GDD_Base`, `GDD_Max` |
| `runoff__curve_number.F90` | CN selection for growing vs dormant; soil group mapping; frozen ground adjustment | CN table (A/B/C/D × growing/dormant) |
| `irrigation.F90` | Trigger logic; max rate; application timing; interaction with soil moisture | `Max_irrigation_rate`, `Irrigation_trigger`, etc. |
| `et__hargreaves_samani.F90` | Reference ET calculation; comparison to published values | Latitude-based parameters |
| `awc__depth_integrated.F90` | AWC integration over rooting depth; soil layer handling | AWC table by soil group |

### Low Priority (stable, rarely modified)

| Module | What to Test | Notes |
|--------|-------------|-------|
| `precipitation__method_of_fragments.F90` | Fragment disaggregation; monthly distribution | Complex, would need synthetic data |
| `direct_net_infiltration__gridded_data.F90` | Grid reading, scaling | Mostly I/O |
| `direct_soil_moisture__gridded_data.F90` | Grid reading, scaling | Mostly I/O |
| `fog__monthly_grid.F90` | Monthly fog interception | Simple lookup |
| `storm_drain_capture.F90` | Impervious fraction routing | Simple math |
| `maximum_net_infiltration.F90` | Cap on infiltration | Simple clamp |
| `weather_data_tabular.F90` | Tabular weather parsing | I/O focused |
| `interception__gash.F90` | Already has 2 tests; could add edge cases | Low urgency |
| `actual_et__fao56.F90` | Single crop coefficient ET | Simpler version of two-stage |

---

## Modules That Do NOT Need PARAMS Injection

These modules don't read from lookup tables (or only read grid/climate data via DATA_CATALOG):

- `datetime.F90`, `fstring.F90`, `fstring_list.F90`, `dictionary.F90`
- `constants_and_conversions.F90`, `timer.F90`, `exceptions.F90`
- `solar_calculations.F90`, `meteorological_calculations.F90`
- `simulation_datetime.F90`, `logfiles.F90`, `file_operations.F90`
- `grid.F90`, `netcdf4_support.F90`, `data_catalog.F90`, `data_catalog_entry.F90`
- `output.F90`, `summary_statistics.F90`, `running_grid_stats.F90`
- `mass_balance__*.F90` (soil, interception, snow, impervious)
- `routing__D8.F90`, `snowmelt__original.F90`, `snowfall__original.F90`
- `continuous_frozen_ground_index.F90`

---

## Integration Tests

| Test | Status | What It Exercises |
|------|--------|-------------------|
| `cs/` (Central Sands FAO-56) | ✅ Passing | Full model: FAO-56 Kcb, irrigation, two-stage ET, GDD |
| `cs_phenology/` (T-M mode) | ✅ Passing | DOY_BASED, GDD_THRESHOLD, winter crop phenology |
| FAO56_GDD focused test | Missing | GDD-driven stage transitions end-to-end |
| Continuous `growth_fraction` | Missing | Phase 4 — verify interception/rooting depth respond to growth_fraction |

---

## Relationship to `test_cleanup_plan.md`

That document covers:
- Auditing and rationalizing the test *directory structure*
- Removing dead tests (`old_fruit_tests/`, legacy regression comparisons)
- Updating control files for new phenology directives

This document covers:
- Which *modules* need unit tests and what to test
- The PARAMS injection pattern for enabling test isolation
- Priority ordering tied to the development roadmap
