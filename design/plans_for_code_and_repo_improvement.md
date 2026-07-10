# SWB2 Code and Repository Improvement Plan

**Reference:** MODFLOW6 (https://github.com/MODFLOW-ORG/modflow6) for CI/testing best practices  
**Current SWB2 version:** v2.4.1  
**Build system:** Meson ≥1.6.0  

---

## Current State Assessment

### Fortran 2018 Compliance Audit Summary

The SWB2 codebase is **remarkably clean modern Fortran** — it avoids nearly all deprecated/obsolescent features. However, it explicitly targets GNU extensions mode (`-std=gnu`) rather than any Fortran standard.

**Clean (no issues found):**
- No `REAL*8`, `INTEGER*4`, `CHARACTER*N`
- `IMPLICIT NONE` in all modules and programs
- No `INCLUDE` statements — all dependencies via module `USE`
- No COMMON, EQUIVALENCE, arithmetic IF, computed GOTO, PAUSE, alternate RETURN, ENTRY, BLOCK DATA
- No labeled DO loops or `EXTERNAL` declarations
- Consistent use of `iso_c_binding` kinds throughout
- Modern OOP: type-bound procedures, generic interfaces, allocatable components

**Issues found:**

| Category | Count | Severity | Status |
|----------|-------|----------|--------|
| `-std=gnu` instead of `-std=f2018` | 1 | High | ✅ Fixed — `static_analysis` profile uses `-std=f2018`; code compiles clean |
| `-fallow-argument-mismatch` flag | 1 | High | ✅ Fixed — flag removed from active profiles |
| `-fall-intrinsics` flag | 1 | Medium | ✅ Fixed — flag removed |
| `stop` instead of `error stop` | 9 instances in 6 files | Medium | ✅ Fixed (June 2026) |
| Lines >132 characters | 17 lines in 5 files | Low | ✅ Fixed (June 2026) |
| `SAVE` variables (hidden mutable state) | 7 instances in 5 files | Low | Open |
| `__FILE__`/`__LINE__` preprocessor macros | 545 uses in 37 files | Accepted deviation | N/A |
| `#ifdef __GFORTRAN__`/`__INTEL_COMPILER__` | in `main.F90` | Low | Open |
| Duplicate `USE` statement | 1 in `netcdf4_support.F90` | Trivial | ✅ Fixed |
| Bare `integer` without kind | ~1 in `grid.F90` | Trivial | ✅ Fixed |
| Intel suppressed diagnostics (7416, 7025, 6048) | 3 | Medium | Open |
| Unused functions | 19 in 5 files | Low | Accepted — intentional reserve API. See `design/unused_functions_analysis.md` |
| Maybe-uninitialized (false positives) | 10 in 3 files | Trivial | N/A — gfortran false positives on allocatables |

### Current Compiler Warning Summary (July 2026, gfortran `-std=f2018 -Wall -Wextra -pedantic`)

**Fortran (SWB2 source): ~30 warnings**
- 19 unused functions (grid, netcdf4_support, datetime, parameters, model_initialize)
- 10 maybe-uninitialized (false positives on allocatable arrays — testdrive, swbstats2_support, solar_calculations)
- 1 intentional float equality comparison in test code

**C (bundled PROJ4 — third-party, not maintained): ~30 warnings**
- Unused variables, missing braces, format overflow, misleading indentation
- Will be eliminated when PROJ4 is replaced with modern PROJ library (Phase 6.1)

### Current Testing Infrastructure

- **Unit tests:** ✅ test-drive framework (10 suites, 144 tests)
- **Test driver:** ✅ `tester.F90` with CLI suite/test selection, ANSI output
- **Integration/regression tests:** Ad-hoc, no CI automation
- **CI:** ✅ GitHub Actions (documentation deployment); build/test CI pending
- **Formatting:** Not enforced
- **Spelling:** Not checked

---

## MODFLOW6 Practices Worth Adopting

MODFLOW6 provides an excellent reference for a mature Fortran scientific code with modern CI/CD.

### Directly Applicable to SWB2

| Practice | MODFLOW6 Implementation |
|----------|------------------------|
| Fortran standard enforcement | `fortran_std=f2018` in `meson.build` |
| Unit test framework | `test-drive` (fortran-lang project) |
| Source formatting | `fprettify` with `.fprettify.yaml` config, CI-enforced |
| Task runner | `pixi.toml` wrapping meson commands |
| CI pipeline | GitHub Actions with tiered jobs (lint → build → smoke → full) |
| Multi-compiler testing | gfortran 11-14, Intel ifx, Intel classic (matrix) |
| Multi-OS testing | Ubuntu, Windows, macOS |
| Integration tests | pytest with markers (`smoke`, `regression`, `slow`, `external`) |
| Spelling | `codespell` with `.codespellrc` |
| Concurrency | `cancel-in-progress` for same-branch pushes |
| Failure debugging | Artifact upload of build logs and failed test output |
| Static link verification | Confirm no unexpected dynamic dependencies |

### MODFLOW-Specific (Not Applicable)

- PETSc/MPI parallel build variants
- flopy Python package integration
- External test model repositories
- BMI shared library build
- IDM code generation from definition files
- LaTeX documentation builds

---

## Implementation Plan

### Phase 0: Repository & Release Housekeeping

| # | Task | Status | Notes |
|---|------|--------|-------|
| 0.1 | Update `code.json` for provisional release | ✅ Done (June 2026) | Points to code.usgs.gov as authoritative |
| 0.2 | Submit provisional release issue on CHS GitLab | Pending | Requires push to gitlab remote first |
| 0.3 | Remove CMakeLists.txt (deprecated) | Pending | Deprecation notice added; remove once Meson is verified cross-platform |
| 0.4 | Consolidate DEVELOPMENT_GUIDELINES.md into CONTRIBUTING.md | ✅ Done (June 2026) | Delete DEVELOPMENT_GUIDELINES.md |
| 0.5 | Commit and push build helper scripts (sanitized) | Pending | Shell/cmd scripts that capture working meson invocations per platform |
| 0.6 | Make DOI-USGS/swb2 and code.usgs.gov repos public | Pending | Required for provisional release |
| 0.7 | Switch gitlab remote from SSH to HTTPS+PAT | Pending | SSH blocked on code.usgs.gov as of June 2026 |

### Phase 1: Quick Wins (1-2 days)

| # | Task | Files Affected |
|---|------|----------------|
| 1.1 | Replace all `stop` with `error stop` | `exceptions.F90`, `main.F90`, `logfiles.F90`, `swbstats2.F90`, `simulation_datetime.F90`, `test_kiss.F90` |
| 1.2 | Fix lines >132 characters | `swbstats2.F90` (9), `netcdf4_support.F90` (4), `swbstats2_support.F90` (2), `actual_et__fao56__two_stage.F90` (1), `grid.F90` (1) |
| 1.3 | Remove duplicate `use netcdf_c_api_interfaces` | `netcdf4_support.F90` |
| 1.4 | Add explicit kind to bare `integer` declarations | `grid.F90` |
| 1.5 | Remove commented-out legacy code (`INTEGER*8`, `goto 440`) | `kiss_random_number_generator.F90`, `awc__depth_integrated.F90` |

### Phase 2: Build System Modernization (1 week)

| # | Task | Notes |
|---|------|-------|
| 2.1 | Verify Meson build works from clean checkout on Windows (gfortran), Linux, macOS | ✅ Windows gfortran verified. Linux/macOS pending CI |
| 2.2 | Add `fortran_std=f2018` to `meson.build` default_options | Matches MODFLOW6 approach |
| 2.3 | Remove `-fallow-argument-mismatch` — fix underlying type mismatches | Likely in netCDF wrapper or FRUIT |
| 2.4 | Remove `-fall-intrinsics` — verify no non-standard intrinsics are called | Search found none in active code |
| 2.5 | Remove `-std=gnu` (superseded by meson's `fortran_std`) | |
| 2.6 | Address Intel suppressed diagnostics (7416, 7025, 6048) | Fix `c_bool` vs default logical issues |
| 2.7 | Add `fprettify` configuration (`.fprettify.yaml`) and format all source | MODFLOW6 uses: indent=2, line-length=82 |
| 2.8 | Add `pixi.toml` as task runner for build/test commands | ✅ Done (June 2026). Wraps `meson setup`, `meson compile`, `meson test`, `docs` |
| 2.9 | Remove CMakeLists.txt and cmake/ directory | After 2.1 is confirmed |

### Phase 3: Testing Framework Overhaul (1-2 weeks)

| # | Task | Notes |
|---|------|-------|
| 3.1 | Replace FRUIT with `test-drive` for Fortran unit tests | ✅ Done (July 2026). Vendored `testdrive.F90` compiled directly into test executable |
| 3.2 | Port existing FRUIT tests to test-drive format | ✅ Done (July 2026). 10 suites, 144 tests covering timer, allocatable_string, exceptions, gash, datetime, fao56, constants, fstring_list, parameters, solar |
| 3.3 | Add test-drive as a meson dependency | ✅ Done (July 2026). Vendored single-file approach (no subproject/wrap needed) |
| 3.4 | Restructure test `meson.build` with `foreach` pattern | ✅ Done (July 2026). Individual test source files compiled together via meson |
| 3.5 | Add pytest-based integration tests | Python scripts run SWB2 executable, validate output |
| 3.6 | Separate test data with clear regression baselines | Structured `test/` directory with expected outputs |
| 3.7 | Add pytest markers for test categorization | `smoke`, `regression`, `slow` |

### Phase 4: CI/CD Pipeline (1 week)

| # | Task | Notes |
|---|------|-------|
| 4.1 | Create `.github/workflows/ci.yml` with tiered jobs | lint → build → smoke → full test (with `needs:` gates) |
| 4.1a | Create `.github/workflows/docs.yml` for Doxygen → GitHub Pages | ✅ Done (July 2026). Auto-deploys on push to main via pixi + setup-pixi@v0.9.6 |
| 4.2 | Add format-check CI step using `fprettify` | Fail PR if formatting differs |
| 4.3 | Add `codespell` for typo checking | |
| 4.4 | Multi-compiler matrix: gfortran 12/13/14, Intel ifx | Use `fortran-lang/setup-fortran@v1` |
| 4.5 | Multi-OS testing: Ubuntu, Windows, macOS | |
| 4.6 | Add concurrency control (`cancel-in-progress`) | Prevent redundant runs on rapid pushes |
| 4.7 | Upload build logs and failed test artifacts on failure | |
| 4.8 | Add static-link verification step | Confirm no unexpected dynamic deps on Windows/Linux |
| 4.9 | Add `paths-ignore` for docs/images | Avoid unnecessary CI runs |
| 4.10 | Add DOI-USGS sync workflow | Auto-push to DOI-USGS on successful main build |

### Phase 5: Ongoing Code Modernization

| # | Task | Priority |
|---|------|----------|
| 5.1 | Reduce `#ifdef` compiler blocks — use `COMPILER_VERSION()` | Low |
| 5.2 | Document `__FILE__`/`__LINE__` as accepted non-standard dependency | Low |
| 5.3 | Refactor `SAVE` variables where feasible (7 instances) | Low |
| 5.4 | Modernize fixed-length strings to `character(len=:), allocatable` | Low |
| 5.5 | Evaluate `iso_c_binding` vs `iso_fortran_env` kinds for non-C-interop modules | Very Low |
| 5.6 | Add INTENT to C interface arguments in `grid.F90` | Low |
| 5.7 | `datetime.F90`: Remove redundant standalone `operator(<)` / `operator(>)` interfaces — use type-bound generic operators only. Currently both are defined, which confuses compiler resolution and prevents using the operators directly in generic contexts (e.g., test-drive's `check`). | Low |
| 5.8 | `fstring.F90` / `constants_and_conversions.F90`: Consolidate duplicate `keepnumeric` implementations. Both modules contain nearly identical private copies. Consider extracting to a shared utility module or eliminating one. | Very Low |

### Phase 6: Feature Wishlist (longer-term)

| # | Task | Notes |
|---|------|-------|
| 6.1 | Replace bundled PROJ4 with modern PROJ via pixi/system library | See `design/feature_consideration__adopt_modern_PROJ_library.md` |
| 6.2 | Add GDAL/GeoTIFF support for direct raster I/O | See `design/feature_consideration__reading_writing_geotiffs_gdal.md` |
| 6.3 | Unit-agnostic operation (metric/imperial flexibility) | See `design/feature_consideration__unit_agnostic_operation.md` |
| 6.4 | Adopt pixi for library/dependency management | ✅ Done (June-July 2026). See `pixi.toml`; manages gfortran, netcdf, hdf5, doxygen, graphviz |

---

## Recommended Implementation Order

1. **Phase 0** — release housekeeping (in progress)
2. **Phase 1** — zero risk, immediate cleanup
3. **Phase 2.1** — verify cross-platform Meson build (gate for everything else)
4. **Phase 2.7-2.8** — establish fprettify + pixi workflow
5. **Phase 4.1-4.2** — get basic CI feedback loop working
6. **Phase 3.1-3.4** — migrate to test-drive
7. **Phase 2.2-2.6** — enable strict F2018 (CI now catches regressions)
8. **Phase 4.3-4.10** — expand CI coverage
9. **Phase 3.5-3.7** — pytest integration tests
10. **Phase 5** — ongoing modernization
11. **Phase 6** — feature work as bandwidth allows

---

## Architecture Comparison

| Aspect | SWB2 Today | Target |
|--------|-----------|--------|
| Fortran standard | `-std=gnu` (extensions) | `fortran_std=f2018` |
| Build system | Meson (working) + CMake (deprecated) | Meson only |
| Formatting | None enforced | `fprettify` with CI check |
| Unit test framework | ✅ `test-drive` (10 suites, 144 tests) | Expand coverage |
| Integration tests | Manual/ad-hoc | pytest with markers |
| CI | ✅ GitHub Actions (docs deployment) | Expand: build, test, multi-compiler |
| Task runner | ✅ `pixi` tasks (setup, build, test, docs, etc.) | Complete |
| Spelling | None | `codespell` |
| Test discovery | ✅ test-drive collection + meson build | Complete |
| Failure artifacts | None | Uploaded to GitHub Actions |
| Repo sync | Manual push to 3 remotes | CI auto-sync to DOI-USGS; manual push to code.usgs.gov at release |
| Documentation | ✅ Doxygen via GitHub Actions → Pages | Complete |
| Line endings | ✅ `.gitattributes` (LF normalized) | Complete |

---

## Key Files to Create/Modify

### New files:
- `.github/workflows/ci.yml`
- `.github/workflows/sync-doi-usgs.yml`
- `.fprettify.yaml`
- `.codespellrc`
- `pixi.toml`
- `test/unit_tests/tester.f90` (test-drive driver)
- `test/integration_tests/conftest.py`

### Modified files:
- `meson.build` (add `fortran_std=f2018`, remove non-standard flags)
- `src/meson.build` (if needed for library target)
- `test/unit_tests/meson.build` (test-drive integration)
- All 9 files with `stop` → `error stop`
- All 5 files with >132 char lines

### Files to remove:
- `CMakeLists.txt` (after Phase 2.1 verified)
- `cmake/` directory
- `DEVELOPMENT_GUIDELINES.md` (absorbed into CONTRIBUTING.md)
- `test/unit_tests/fruit.F90` (after test-drive migration)
- `test/unit_tests/fruit_util.F90`
- `test/unit_tests/fruit_driver.F90`
- `test/unit_tests/FRUIT.py`
- `test/unit_tests/make_test_suite.py`

---

## References

- MODFLOW6 repository: https://github.com/MODFLOW-ORG/modflow6
- test-drive: https://github.com/fortran-lang/test-drive
- fprettify: https://github.com/pseewald/fprettify
- pixi: https://pixi.sh
- fortran-lang/setup-fortran: https://github.com/fortran-lang/setup-fortran
- Fortran 2018 standard (ISO/IEC 1539-1:2018)

---

## Change Log

| Date | Changes |
|------|---------|
| May 2026 | Initial plan created |
| June 2026 | Added Phase 0 (release housekeeping), Phase 6 (feature wishlist). Moved cross-platform build verification to top of Phase 2. Added DOI-USGS sync workflow to Phase 4. Added CMake removal to Phase 2. Added change log. |
| July 2026 | Marked Phase 3.1–3.4 complete (test-drive migration, 10 suites, 144 tests). Marked 2.1 partial (Windows verified), 2.8 complete (pixi.toml), 6.4 complete (pixi adoption). Added 4.1a (Doxygen → GitHub Pages CI deployment). Removed `docs/` from git tracking. Added `.gitattributes` for cross-platform line endings. |


---

## Issue: Incomplete Weather Data Files (Leap Year Edge Case)

**Discovered:** 2026-07-10
**Severity:** Minor (workaround: adjust END_DATE), but poor user experience
**File:** `src/data_catalog_entry.F90`, `getvalues_dynamic_netcdf_sub`, line ~1575

**Problem:** When a NetCDF climate file doesn't cover the full simulation period (e.g., Daymet 2012 file has 365 records but 2012 is a leap year with 366 days), SWB halts with a fatal error rather than padding the missing day(s).

**Root cause (investigated):** The existing padding logic in `getvalues_dynamic_netcdf_sub` has two paths that handle missing data:

1. **`test_for_need_to_pad_values` (~line 1400):** Returns TRUE only when the *file doesn't exist* at year-end. Assumes a missing file = missing data. Does NOT help when the file exists but is short by a day.

2. **`netcdf_update_time_starting_index` returns FALSE (~line 1615):** Sets `lPadValues = TRUE` for missing timesteps *within* a file's date range. But this path is only reached if `netcdf_date_within_range` passes first.

The gap: when the file **exists and opens successfully** but the requested date is **past the file's last date**, the code hits the outer `netcdf_date_within_range` check (line ~1550), finds the date is out of range, sees no `#` in the filename (not a sequential series), and calls `assert(FALSE, ...)` — a hard stop.

**The fix would be:** At the else-branch around line ~1565 (where the fatal error is), instead of dying, set `lPadValues = TRUE` and emit a warning:
```fortran
! Instead of assert(FALSE, ...):
this%lPadValues = TRUE
call LOGS%write("WARNING: Current simulation date is beyond the end of NetCDF file "  &
  //dquote(this%sSourceFilename)//". Padding with previous/zero values.", &
  iLogLevel=LOG_ALL, lEcho=TRUE)
```

This would make the behavior consistent: missing days at file boundaries get the same padding treatment as missing files at year boundaries.

**Why it works fine with multi-year series (e.g., `prcp_%y.nc`):** When the filename template contains `%y`, SWB closes the 2012 file at year-end and attempts to open a 2013 file. If that file doesn't exist, `test_for_need_to_pad_values` catches it. The problem is specific to single-file runs or the last file in a series where the file exists but is short.

**Workaround:** Set `END_DATE` to match the last date in the NetCDF file (e.g., `12/30/2012` for Daymet leap years).

**Future work:** Implement the fix above, add a test case exercising this edge case.
