# Static Analysis Remediation Plan

**Date:** June 2026  
**Goal:** Warning-free builds under both `gfortran -std=f2018` and `ifx /warn:all /stand:f18`  
**Context:** Required for official USGS release

---

## Current Status (July 2, 2026)

| Metric | Value |
|--------|-------|
| gfortran build | ✅ Compiles + links (zero errors) |
| gfortran Fortran warnings | **41** — all non-actionable (see breakdown below). **Zero actionable.** |
| ifx build | ✅ Compiles + links (zero errors) |
| ifx warnings | **0** — zero actual warnings |
| ifx remarks | 83 — unused dummy arguments (informational only) |

### Breakdown of 41 gfortran warnings (static_analysis profile, gfortran 15.2)

| Category | Count | Verdict |
|----------|------:|---------|
| `-Wunused-function` (intentional reserve API) | 20 | Private functions retained for planned features |
| `-Wunused-dummy-argument` (interface conformance) | 16 | Strategy pattern: procedure pointer signatures require matching arguments across all implementations |
| `-Wmaybe-uninitialized` (false positive on allocatable) | 5 | gfortran cannot prove allocatable metadata is set from function result assignment — not a real bug |

**Note:** `-Wno-unused-dummy-argument` is specified in the static_analysis profile
flags, but gfortran 15.2 does not suppress these warnings when `-Wextra` is also
active. This appears to be a gfortran flag-ordering quirk. The warnings remain
non-actionable regardless.

### Completed

- ✅ Mixed-kind min/max (18 instances) — all fixed
- ✅ **Impure function warnings — ALL eliminated** (69 → 0)
- ✅ **Implicit narrowing `-Wconversion` — ALL 89 instances eliminated**
- ✅ **Unused variables — ALL Fortran instances eliminated** (289 → 0)
- ✅ Meson build refactored (DRY source lists, `profile` option, Cray paths removed)
- ✅ **Unused functions — 12 stale functions deleted; 20 retained as intentional reserve API**
- ✅ **Unused module variables and imports — ALL 16 instances eliminated**
- ✅ **Maybe-uninitialized — ALL 13 instances fixed** (found 3 real bugs)
- ✅ **Float comparisons — ALL 10 instances fixed** (inequality + product test)
- ✅ **Stack-to-static — ALL 10 instances fixed** (allocatable)
- ✅ **Character truncation — fixed** (widened local variable)
- ✅ **Argument aliasing — fixed** (intent(inout))
- ✅ **ifx `/MDd` vs `/MT` conflict — fixed** (b_vscrt in Meson)
- ✅ **ifx INQUIRE `logical(c_bool)` — ALL 16 fixed** (default logical)
- ✅ **ifx long lines — ALL 6 fixed**
- ✅ **gfortran actionable warnings: ZERO remaining**
- ✅ **ifx actionable warnings: ZERO remaining**

---

## Remaining gfortran Warnings (~108 actionable + 20 acknowledged)

### 1. ~~Implicit narrowing `-Wconversion`~~ — ✅ DONE (89 → 0)

All 89 instances eliminated across 17 files. Fixes applied explicit `real(..., c_float)`, `int(..., c_int)`, or `int(..., c_size_t)` wrappers to make intentional precision reductions explicit.
### 2. Unused private functions `-Wunused-function` — 31 → 19 instances

Private module functions defined but never called.

| File | Count | Status |
|------|-------|--------|
| `netcdf4_support.F90` | 12 | **Retained** — intentional reserve API |
| `grid.F90` | 4 | **Retained** — spatial analysis library |
| `datetime.F90` | 4 → 2 | 2 deleted (stale parsers), 2 retained (format setters) |
| `data_catalog_entry.F90` | 3 → 0 | **All deleted** (debug helper + buggy setters) |
| `fstring.F90` | 2 → 0 | **All deleted** (buggy + unused) |
| `fstring_list.F90` | 1 → 0 | **Deleted** (trivial wrapper) |
| `logfiles.F90` | 1 → 0 | **Deleted** (trivial one-liner) |
| `model_initialize.F90` | 1 | **Retained** — planned zone aggregation feature |
| `parameters.F90` | 1 | **Retained** — completes parameter type system |
| `runoff__curve_number.F90` | 1 → 0 | **Deleted** (superseded) |
| `daily_calculation.F90` | 1 → 0 | **Deleted** (debug helper) |

**Approach (decided June 26):**
- 12 stale/buggy functions deleted outright
- 19 intentional reserve functions retained as-is (facade pattern: private helpers for future public entry points)
- Normal builds: suppress `-Wunused-function` (noise reduction)
- Static analysis builds: 19 warnings acknowledged and documented as intentional in `unused_functions_analysis.md`
- No `#ifdef` wrapping, no `public` promotion — design intent preserved

**Status:** ✅ DONE — 12 deleted, 19 documented as intentional

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

### 5. ~~Unused module-level variables `-Wunused-value`~~ — ✅ DONE

All 11 private module variables and 5 unused imports eliminated.

**Fix applied:** Deleted declarations (`pNCFILE` ×5, `GROWTH_STAGE_SHIFT_DAYS`, `LU_SOILS_CSV`, `pIRRIGATION_MASK`, `DATE_OF_LAST_RETRIEVAL`, `pFRAGMENTS_SEQUENCE`, `pRAINFALL_ADJUST_FACTOR`) and removed unused names from `use ... only:` statements (`PARAMS_DICT` ×2, `BNDS`, `DATATYPE_INT`, `DATATYPE_FLOAT`).

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

### 9. ~~Unused imported parameters~~ — ✅ DONE (merged into #5 fix)

Removed from `use ... only:` lists.

### 10. Character truncation — 1 instance

String assignment where RHS is longer than LHS.

**Fix:** Increase LHS length or use allocatable.  
**Effort:** 5 min

### 11. Definite uninitialized — 1 instance

`solar_calculations.F90` — `dAlpha` used without assignment (overlaps with #4).

---

## ifx-Specific Remaining Issues

| Issue | Count | Fix |
|-------|-------|-----|
| `/MDd` vs `/MT` conflict (#10121) | 76 | Add `b_vscrt=mt` to meson default_options |
| Unused dummy args (#7712) | 83 | Remarks only (informational); same as gfortran `-Wno-unused-dummy-argument` |
| INQUIRE with `logical(c_bool)` (#6048) | 16 | Use default `logical` for INQUIRE specifiers |
| Lines > 132 chars (#5268) | 6 | Wrap or shorten |

---

## Recommended Next Steps (by ROI)

**gfortran: COMPLETE** — zero actionable warnings.  
**ifx: COMPLETE** — zero actual warnings (83 informational remarks only).

### Remaining Diagnostics — By Design

**gfortran (41 warnings in static_analysis profile, gfortran 15.2):**

1. **`-Wunused-function` (20 warnings):** Private module functions retained as intentional reserve API — part of a NetCDF abstraction layer, spatial analysis library, and planned features (zone aggregation, configurable date formats). They follow a facade design pattern: private helpers intended for future public entry points. Each has been reviewed and documented in `unused_functions_analysis.md`. Suppressed in normal (non-static-analysis) builds.

2. **`-Wunused-dummy-argument` (16 warnings):** These arise from the strategy pattern used throughout SWB2. Procedure pointers (e.g., `model_calculate_irrigation`, `model_calculate_direct_net_infiltration`) require all implementations to match a common interface signature. Stub implementations (`_none` variants) and specific algorithm implementations that don't need every argument produce these warnings. This is structurally identical to Python's `def handler(self, event, **kwargs)` where a handler ignores some arguments.

   | File | Arguments | Reason |
   |------|-----------|--------|
   | `model_domain.F90` | `indx` ×4 | `_none` stub implementations |
   | `grid.F90` | `rnodatavalue`, `inodatavalue`, `pgrd`, `sfilename` | Functions also flagged as unused (reserve API) |
   | `fog__monthly_grid.F90` | `lactive`, `nodata_fill_value` | Interface conformance |
   | `direct_net_infiltration__gridded_data.F90` | `nodata_fill_value` | Interface conformance |
   | `maximum_net_infiltration.F90` | `landuse_index` | Interface conformance |
   | `actual_et__fao56__two_stage.F90` | `landuse_index` | Reserved for future use in elemental function |
   | `mass_balance__impervious_surface.F90` | `runon` | Interface conformance |
   | `interception__bucket.F90` | `active_cells` | Interface conformance |
   | `data_catalog_entry.F90` | `sdescription` | Interface conformance |
   | `et__gridded_values.F90` | `lactive` | Interface conformance |
   | `swbstats2_support.F90` | `date_range_string` | Interface conformance |

3. **`-Wmaybe-uninitialized` (5 warnings, all same variable in `swbstats2_support.F90`):** The variable `zone_values` is an allocatable array assigned from function result `unique_zone_list%get_integer()`. gfortran cannot prove that the allocatable metadata (.offset, .dim[0].lbound, .dim[0].ubound) is initialized before the assignment, but it always is. This is a known gfortran limitation with allocatable assignment from derived-type method results.

**ifx (83 `#7712` remarks):** Unused dummy arguments in type-bound procedure stubs and interface implementations. These are inherent to Fortran OOP — a base-type method signature must match across all implementations even when a particular implementation doesn't use every argument. ifx correctly classifies these as remarks (informational), not warnings.

---

## Progress Summary

| Date | Warnings (gfortran) | Key changes |
|------|--------------------:|-------------|
| June 23 | 540 (build failed) | Initial static_analysis profile created |
| June 24 AM | 540 → 476 | Fixed 18 min/max, marked 5 functions pure |
| June 24 PM | 476 → 191 | Fixed 11 conversions, removed 271 unused vars |
| June 25 AM | 191 → 176 | All Fortran unused variables eliminated (289 → 0) |
| June 25 PM | 176 → **~140** | All 89 `-Wconversion` instances eliminated |
| June 26 | **540 → 20 (gfortran), 487 → 0 (ifx)** | All actionable warnings eliminated across both compilers. 12 stale functions deleted; 20 intentional reserve API functions retained by design. Fixed 3 real bugs (uninitialized variable, typo in Kahan summation). Meson CRT linkage fixed. All INQUIRE type conformance issues resolved. **DONE.** |
| July 2 | **41 total (gfortran 15.2)** | Full recount with pixi-managed gfortran 15.2: 20 unused-function + 16 unused-dummy-argument + 5 false-positive maybe-uninitialized. All non-actionable. |

---

## Change Log

| Date | Changes |
|------|---------|
| June 23, 2026 | Initial plan based on ifx build |
| June 23, 2026 | Revised: added gfortran results, documented min/max as fatal |
| June 24, 2026 | Fixed min/max, pure functions, conversions, bulk unused var removal |
| June 25, 2026 | Unused variables fully eliminated; all 89 `-Wconversion` warnings eliminated |
| June 26, 2026 | Tiered unused-function approach: 12 stale deleted, 20 intentional retained |
| June 26, 2026 | All unused module variables and imports eliminated (16) |
| June 26, 2026 | All maybe-uninitialized fixed (13); found 3 real bugs |
| June 26, 2026 | Float comparisons fixed (10): inequality test + product-based sign change |
| June 26, 2026 | Stack-to-static fixed (10): changed to allocatable |
| June 26, 2026 | Impure function warnings fixed (5): marked functions `pure` |
| June 26, 2026 | Character truncation fixed; argument aliasing fixed (intent(inout)) |
| June 26, 2026 | ifx: `/MDd` vs `/MT` conflict resolved via `b_vscrt` in Meson (76 warnings) |
| June 26, 2026 | ifx: INQUIRE `logical(c_bool)` → default `logical` (16 warnings) |
| June 26, 2026 | ifx: Long lines >132 chars wrapped (6 warnings) |
| June 26, 2026 | **COMPLETE**: zero actionable warnings on both compilers |
| July 2, 2026 | Revalidated with pixi-managed gfortran 15.2 on Windows. Full recount: 41 total non-actionable warnings (20 unused-function, 16 unused-dummy-argument, 5 false-positive maybe-uninitialized). Documented unused-dummy-argument breakdown by file. Noted `-Wno-unused-dummy-argument` flag ineffective when `-Wextra` also active in gfortran 15.2. |
