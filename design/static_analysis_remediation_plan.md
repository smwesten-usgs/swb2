# Static Analysis Remediation Plan

**Date:** June 2026  
**Goal:** Warning-free builds under both `gfortran -std=f2018` and `ifx /warn:all /stand:f18`  
**Context:** Required for official USGS release

---

## Current Status (June 26, 2026)

| Metric | Value |
|--------|-------|
| gfortran build | ✅ Compiles + links (zero errors) |
| gfortran Fortran warnings | **~61** total; **~41** actionable (20 are acknowledged reserve API) |
| ifx build | ✅ Compiles + links |
| ifx Fortran diagnostics | ~400 est. (down from 487) |

### Completed

- ✅ Mixed-kind min/max (18 instances) — all fixed
- ✅ Impure function warnings — 64 of 69 eliminated (5 remain in `fstring_list`)
- ✅ **Implicit narrowing `-Wconversion` — ALL 89 instances eliminated**
- ✅ **Unused variables — ALL Fortran instances eliminated** (289 → 0)
- ✅ Meson build refactored (DRY source lists, `profile` option, Cray paths removed)
- ✅ **Unused functions — 12 stale functions deleted; 20 retained as intentional reserve API**
- ✅ **Unused module variables and imports — ALL 16 instances eliminated**

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
| 1 | ~~Fix `-Wconversion` (89)~~ | ✅ 89 | Done |
| 2 | ~~Delete stale unused functions (12 of 31)~~ | ✅ 12 | Done |
| 3 | ~~Delete unused module vars / imports (16)~~ | ✅ 16 | Done |
| 4 | Fix maybe-uninitialized (13) | 13 | 1 hr |
| 5 | Fix stack-to-static (10) | 10 | 30 min |
| 6 | Address float comparisons (10) | 10 | 20 min |
| 7 | Remaining impure function (5) | 5 | 30 min |
| 8 | Fix character truncation (1) + argument aliasing (2) | 3 | 10 min |
| | **Total actionable remaining** | **~41** | **~2.5 hr** |

**Note:** 20 `-Wunused-function` warnings are acknowledged intentional reserve API (documented in `unused_functions_analysis.md`). These will be suppressed in normal builds.

---

## Progress Summary

| Date | Warnings (gfortran) | Key changes |
|------|--------------------:|-------------|
| June 23 | 540 (build failed) | Initial static_analysis profile created |
| June 24 AM | 540 → 476 | Fixed 18 min/max, marked 5 functions pure |
| June 24 PM | 476 → 191 | Fixed 11 conversions, removed 271 unused vars |
| June 25 AM | 191 → 176 | All Fortran unused variables eliminated (289 → 0) |
| June 25 PM | 176 → **~140** | All 89 `-Wconversion` instances eliminated |
| June 26 | ~140 → **~61** | 12 stale unused functions deleted; 19 retained as intentional reserve API; all 16 unused module vars/imports eliminated |

---

## Change Log

| Date | Changes |
|------|---------|
| June 23, 2026 | Initial plan based on ifx build |
| June 23, 2026 | Revised: added gfortran results, documented min/max as fatal |
| June 24, 2026 | Fixed min/max, pure functions, conversions, bulk unused var removal |
| June 25, 2026 | Unused variables fully eliminated; all 89 `-Wconversion` warnings eliminated |
| June 26, 2026 | Tiered unused-function approach: 12 stale deleted, 19 intentional retained; normal builds suppress `-Wunused-function` |
