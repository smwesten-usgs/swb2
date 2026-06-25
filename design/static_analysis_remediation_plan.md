# Static Analysis Remediation Plan

**Date:** June 2026  
**Goal:** Warning-free builds under both `gfortran -std=f2018` and `ifx /warn:all /stand:f18`  
**Context:** Required for official USGS release

---

## Current Status (June 25, 2026)

| Metric | Value |
|--------|-------|
| gfortran build | ✅ Compiles + links (zero errors) |
| gfortran Fortran warnings | **176** (down from 540) |
| ifx build | ✅ Compiles + links |
| ifx Fortran diagnostics | ~420 est. (down from 487) |

### Completed

- ✅ Mixed-kind min/max (18 instances) — all fixed
- ✅ Impure function warnings — 64 of 69 eliminated (5 remain in `fstring_list`)
- ✅ Conversion warnings in `constants_and_conversions.F90` — 8 fixed
- ✅ Conversion warnings in `snowmelt__original.F90` — 2 fixed
- ✅ Conversion warning in `kiss_random_number_generator.F90` — 1 fixed
- ✅ **Unused variables — ALL Fortran instances eliminated** (289 → 0)
- ✅ Meson build refactored (DRY source lists, `profile` option, Cray paths removed)

---

## Remaining gfortran Warnings (191)

### 1. Implicit narrowing `-Wconversion` — 89 instances

Double-to-float or int64-to-int32 assignments without explicit conversion.

| File | Count | Pattern |
|------|-------|---------|
| `datetime.F90` | 18 | `c_int` assigned from `c_short` member arithmetic |
| `grid.F90` | 17 | `c_double` coordinates → `c_float` locals |
| `netcdf4_support.F90` | 16 | `c_size_t` → `c_int`, Julian day calcs |
| `simulation_datetime.F90` | 5 | Date arithmetic (`c_double`) → `c_int` |
| `crop_coefficients__fao56.F90` | 5 | `c_double` interpolation → `c_float` Kcb |
| `model_initialize.F90` | 4 | Grid cell/coordinate calcs |
| `model_domain.F90` | 4 | Coordinate/date conversions |
| `data_catalog_entry.F90` | 4 | Scale/offset applied to float grids |
| `test_FAO56_functions.F90` | 4 | Test calculations |
| `mass_balance__soil.F90` | 3 | `c_double` soil_storage → `c_float` delta |
| Others (7 files) | 9 | 1–2 each |

**Fix:** Wrap RHS with `real(..., c_float)` or `int(..., c_int)`.  
**Effort:** 2–3 hours  
**Risk:** Low (making implicit behavior explicit)

### 2. Unused private functions `-Wunused-function` — 31 instances

Private module functions defined but never called.

| File | Count | Notes |
|------|-------|-------|
| `netcdf4_support.F90` | 12 | Utility functions for unused NetCDF operations |
| `grid.F90` | 4 | Lookup/interpolation functions |
| `datetime.F90` | 4 | Date conversion helpers |
| `data_catalog_entry.F90` | 3 | Setter/getter functions |
| `fstring.F90` | 2 | `strip_full_pathname_fn`, `remove_repeats` |
| Others (6 files) | 6 | 1 each |

**Fix:** Delete if truly dead code, or mark `public` if intended for future use.  
**Effort:** 1 hour (verify no callers, then delete)  
**Risk:** Low

### 3. ~~Unused variables `-Wunused-variable`~~ — ✅ DONE

All Fortran unused variable warnings eliminated. Remaining 3 in build output are from bundled C/proj4 code.

### 4. Maybe-uninitialized `-Wmaybe-uninitialized` — 13 instances

Variables that may reach a use point without assignment on all code paths.

| File | Notes |
|------|-------|
| `solar_calculations.F90` | `dAlpha` — real bug (function result never assigned on main path) |
| `netcdf4_support.F90` | `pnc_dim`, `ncol`, `nrow`, `iIndex` — path-dependent |
| `model_initialize.F90` | `rGridCellSize`, `rX1`, `rY1` — control-file dependent |
| `dictionary.F90` | `pDict` pointer |
| `swbstats2_support.F90` | `sum_val_comp` (2) |
| `data_catalog_entry.F90` | `iFileType` |
| `proj4_support.F90` | `standard_parallels` |

**Fix:** Initialize at declaration or add explicit assignment on all paths.  
**Effort:** 1 hour  
**Risk:** Medium (some may reveal actual bugs — especially `solar_calculations.F90`)

### 5. Unused module-level variables `-Wunused-value` — 11 instances

Private module variables that are declared but never referenced.

**Fix:** Delete or comment out.  
**Effort:** 15 min  
**Risk:** None

### 6. Stack-to-static `-Wsurprising` — 10 instances

Large `ASCII_FILE_T` local variables moved from stack to static. Affects reentrancy.

**Fix:** Declare as `allocatable` + `allocate`/`deallocate`.  
**Effort:** 30 min  
**Risk:** Low

### 7. Float equality comparisons `-Wcompare-reals` — 10 instances

All in `grid.F90` — comparing against `rNoData` sentinel value.

**Fix:** These are intentional (exact bit-pattern comparison for NODATA). Add a named helper function or suppress with a comment. Alternatively, compare with a tolerance or use IEEE NaN as NODATA.  
**Effort:** 20 min  
**Risk:** None (behavior is intentional)

### 8. Remaining impure function elimination — 5 instances

`return_count_of_matching_strings_fn` in `fstring_list.F90` — requires making the list `get` method `pure` with `intent(in)`.

**Fix:** Deeper refactor of `FSTRING_LIST_T`.  
**Effort:** 30 min–1 hour  
**Risk:** Low

### 9. Unused imported parameters — 2 instances

Explicitly imported module parameters that aren't used.

**Fix:** Remove from `use ... only:` list.  
**Effort:** 5 min

### 10. Character truncation — 1 instance

String assignment where RHS is longer than LHS.

**Fix:** Increase LHS length or use allocatable.  
**Effort:** 5 min

### 11. Definite uninitialized — 1 instance

`solar_calculations.F90` — `dAlpha` used without assignment (overlaps with #4).

---

## ifx-Specific Remaining Issues

These are in addition to items shared with gfortran:

| Issue | Count | Fix |
|-------|-------|-----|
| `/MDd` vs `/MT` conflict (#10121) | 76 | Add `b_vscrt=mt` to meson default_options |
| INQUIRE with `logical(c_bool)` (#6048) | 16 | Use default `logical` for INQUIRE specifiers |
| Lines > 132 chars (#5268) | 5 | Wrap or delete |
| Unused `this`/`indx` in stubs (#7712) | ~25 | `associate` pattern |

---

## Recommended Next Steps (by ROI)

| # | Task | Warnings eliminated | Effort |
|---|------|--------------------:|--------|
| 1 | Fix `-Wconversion` (89) | 89 | 2–3 hr |
| 2 | Delete unused functions (31) | 31 | 1 hr |
| 3 | Fix maybe-uninitialized (13) | 13 | 1 hr |
| 4 | Delete unused module vars (11) | 11 | 15 min |
| 5 | Fix stack-to-static (10) | 10 | 30 min |
| 6 | Address float comparisons (10) | 10 | 20 min |
| 7 | Remaining small items (12) | 12 | 30 min |
| | **Total** | **176** | **~6 hr** |

---

## Progress Summary

| Date | Warnings (gfortran) | Key changes |
|------|--------------------:|-------------|
| June 23 | 540 (build failed) | Initial static_analysis profile created |
| June 24 AM | 540 → 476 | Fixed 18 min/max, marked 5 functions pure |
| June 24 PM | 476 → 191 | Fixed 11 conversions, removed 271 unused vars |
| June 25 | **176** | All Fortran unused variables eliminated (289 → 0) |

---

## Change Log

| Date | Changes |
|------|---------|
| June 23, 2026 | Initial plan based on ifx build |
| June 23, 2026 | Revised: added gfortran results, documented min/max as fatal |
| June 24, 2026 | Fixed min/max, pure functions, conversions, bulk unused var removal |
| June 25, 2026 | Unused variables fully eliminated; 176 warnings remain across 7 categories |
