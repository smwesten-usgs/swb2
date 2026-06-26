# Static Analysis Remediation Plan

**Date:** June 2026  
**Goal:** Warning-free builds under both `gfortran -std=f2018` and `ifx /warn:all /stand:f18`  
**Context:** Required for official USGS release

---

## Current Status (June 26, 2026)

| Metric | Value |
|--------|-------|
| gfortran build | ✅ Compiles + links (zero errors) |
| gfortran Fortran warnings | **20** — all intentional reserve API. **Zero actionable.** |
| ifx build | ✅ Compiles + links (zero errors) |
| ifx diagnostics | **181** (down from 487): 76 build-config, 83 remarks, 16 INQUIRE type, 6 long lines |

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
- ✅ **gfortran actionable warnings: ZERO remaining**

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

**gfortran: COMPLETE** — zero actionable warnings remain.

**ifx remaining work:**

| # | Task | Diagnostics eliminated | Effort |
|---|------|--------------------:|--------|
| 1 | Fix `/MDd` vs `/MT` meson config | 76 | 5 min |
| 2 | Fix INQUIRE `logical(c_bool)` → default logical | 16 | 20 min |
| 3 | Wrap lines >132 chars | 6 | 10 min |
| 4 | (Optional) Suppress unused dummy arg remarks | 83 | Build flag or leave as remarks |
| | **Total** | **181** | **~35 min** + optional |

**Note:** 20 `-Wunused-function` warnings (gfortran) are acknowledged intentional reserve API (documented in `unused_functions_analysis.md`). Suppressed in normal builds.

---

## Progress Summary

| Date | Warnings (gfortran) | Key changes |
|------|--------------------:|-------------|
| June 23 | 540 (build failed) | Initial static_analysis profile created |
| June 24 AM | 540 → 476 | Fixed 18 min/max, marked 5 functions pure |
| June 24 PM | 476 → 191 | Fixed 11 conversions, removed 271 unused vars |
| June 25 AM | 191 → 176 | All Fortran unused variables eliminated (289 → 0) |
| June 25 PM | 176 → **~140** | All 89 `-Wconversion` instances eliminated |
| June 26 | ~140 → **20** (0 actionable) | 12 stale functions deleted; unused module vars/imports eliminated; all maybe-uninitialized fixed (3 real bugs found); float comparisons fixed; stack-to-static fixed; impure functions fixed; character truncation fixed. **gfortran complete.** |

---

## Change Log

| Date | Changes |
|------|---------|
| June 23, 2026 | Initial plan based on ifx build |
| June 23, 2026 | Revised: added gfortran results, documented min/max as fatal |
| June 24, 2026 | Fixed min/max, pure functions, conversions, bulk unused var removal |
| June 25, 2026 | Unused variables fully eliminated; all 89 `-Wconversion` warnings eliminated |
| June 26, 2026 | Tiered unused-function approach: 12 stale deleted, 19 intentional retained; normal builds suppress `-Wunused-function` |
