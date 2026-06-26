# Maybe-Uninitialized Fixes — Findings and Corrections

**Date:** June 26, 2026  
**Scope:** 13 gfortran `-Wmaybe-uninitialized` / `-Wuninitialized` warnings in Fortran source

---

## Summary

All 13 warnings addressed. Three were **real bugs** (producing incorrect results or operating on garbage data). The remaining 10 were false positives where the compiler couldn't prove that an error-terminating else clause (via `assert(FALSE, ...)`, `die()`, or `warn(..., lFatal=TRUE)`) makes the uninitialized path unreachable.

**Fix pattern:** Assign an initial value via executable statement immediately after declarations (not at declaration, which would imply `SAVE` in Fortran).

---

## Real Bugs Found

### 1. `dAlpha` in `solar_calculations.F90` (line 531) — **WRONG RESULT**

```fortran
! BEFORE (used uninitialized result variable in its own computation):
dAlpha = HALFPI - dAlpha

! AFTER (correct — solar altitude = π/2 - zenith angle):
dAlpha = HALFPI - dTheta_z
```

**Impact:** Function `solar_altitude__alpha` returned garbage. Any code path calling this function produced incorrect solar altitude angles.

### 2. `sum_val_comp` in `swbstats2_support.F90` (lines 337, 410) — **TYPO**

```fortran
! BEFORE (typo — assigned sum_val twice, sum_val_comp never initialized):
sum_val = 0.0; sum_val = 0.0

! AFTER:
sum_val = 0.0; sum_val_comp = 0.0
```

**Impact:** Kahan summation compensator started with garbage value, corrupting the accumulated sum in zone-based statistics. Appeared in two nearly identical subroutines.

---

## False Positives (compiler can't prove termination)

These all follow the same pattern: a variable is assigned inside an if-elseif chain where the else clause calls a fatal error handler. The compiler cannot prove the else never returns, so it flags the variable as potentially uninitialized.

| Variable | File | Fix |
|----------|------|-----|
| `pDict` | `dictionary.F90` | `pDict => null()` |
| `pGrd` | `grid.F90` | `pGrd => null()` |
| `pNC_DIM` | `netcdf4_support.F90` | `pNC_DIM => null()` |
| `iIndex` | `netcdf4_support.F90` | `iIndex = -1` |
| `ncol`, `nrow` | `netcdf4_support.F90` | `ncol = 0; nrow = 0` |
| `iFileType` | `data_catalog_entry.F90` | `iFileType = -1` |
| `rGridCellSize`, `rX1`, `rY1` | `model_initialize.F90` | `= 0.0_c_double` |
| `standard_parallels` | `proj4_support.F90` | `standard_parallels = ""` |

---

## Why Not Initialize at Declaration?

In Fortran, initializing a local variable at declaration:
```fortran
integer :: x = 0  ! implies SAVE attribute!
```
implicitly gives it the `SAVE` attribute, meaning it retains its value between calls. This is almost never the intended behavior and makes the procedure unsafe for concurrent or recursive use.

The correct pattern is assignment as an executable statement:
```fortran
integer :: x
x = 0
```

---

## Files Modified

- `solar_calculations.F90` — fixed real bug (wrong variable in expression)
- `swbstats2_support.F90` — fixed typo in two subroutines
- `dictionary.F90` — nullified pointer result
- `grid.F90` — nullified pointer result
- `netcdf4_support.F90` — initialized `pNC_DIM`, `iIndex`, `ncol`, `nrow`
- `data_catalog_entry.F90` — initialized `iFileType`
- `model_initialize.F90` — initialized `rGridCellSize`, `rX1`, `rY1`
- `proj4_support.F90` — initialized `standard_parallels`
