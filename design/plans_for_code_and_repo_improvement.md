# SWB2 Code and Repository Improvement Plan

**Reference:** MODFLOW6 (https://github.com/MODFLOW-ORG/modflow6) for CI/testing best practices  
**Current SWB2 version:** v2.3.5-rc0  
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

| Category | Count | Severity |
|----------|-------|----------|
| `-std=gnu` instead of `-std=f2018` | 1 | High |
| `-fallow-argument-mismatch` flag | 1 | High |
| `-fall-intrinsics` flag | 1 | Medium |
| `stop` instead of `error stop` | 9 instances in 6 files | Medium |
| Lines >132 characters | 17 lines in 5 files | Low |
| `SAVE` variables (hidden mutable state) | 7 instances in 5 files | Low |
| `__FILE__`/`__LINE__` preprocessor macros | 545 uses in 37 files | Accepted deviation |
| `#ifdef __GFORTRAN__`/`__INTEL_COMPILER__` | in `main.F90` | Low |
| Duplicate `USE` statement | 1 in `netcdf4_support.F90` | Trivial |
| Bare `integer` without kind | ~1 in `grid.F90` | Trivial |
| Intel suppressed diagnostics (7416, 7025, 6048) | 3 | Medium |

### Current Testing Infrastructure

- **Unit tests:** FRUIT framework (vendored, circa 2005-2013)
- **Test driver:** Hand-maintained `fruit_driver.F90` with manual test registration
- **Integration/regression tests:** Ad-hoc, no CI automation
- **CI:** None
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
| 2.1 | Verify Meson build works from clean checkout on Windows (gfortran), Linux, macOS | The priority — must work before removing CMake |
| 2.2 | Add `fortran_std=f2018` to `meson.build` default_options | Matches MODFLOW6 approach |
| 2.3 | Remove `-fallow-argument-mismatch` — fix underlying type mismatches | Likely in netCDF wrapper or FRUIT |
| 2.4 | Remove `-fall-intrinsics` — verify no non-standard intrinsics are called | Search found none in active code |
| 2.5 | Remove `-std=gnu` (superseded by meson's `fortran_std`) | |
| 2.6 | Address Intel suppressed diagnostics (7416, 7025, 6048) | Fix `c_bool` vs default logical issues |
| 2.7 | Add `fprettify` configuration (`.fprettify.yaml`) and format all source | MODFLOW6 uses: indent=2, line-length=82 |
| 2.8 | Add `pixi.toml` as task runner for build/test commands | Wraps `meson setup`, `meson compile`, `meson test` |
| 2.9 | Remove CMakeLists.txt and cmake/ directory | After 2.1 is confirmed |

### Phase 3: Testing Framework Overhaul (1-2 weeks)

| # | Task | Notes |
|---|------|-------|
| 3.1 | Replace FRUIT with `test-drive` for Fortran unit tests | `test-drive` is actively maintained by fortran-lang |
| 3.2 | Port existing FRUIT tests to test-drive format | 6 test modules: allocatable_string, datetime, exceptions, FAO56, gash, timer |
| 3.3 | Add test-drive as a meson dependency | Via pkg-config or meson subproject/wrap |
| 3.4 | Restructure test `meson.build` with `foreach` pattern | Register individual test suites like MODFLOW6 |
| 3.5 | Add pytest-based integration tests | Python scripts run SWB2 executable, validate output |
| 3.6 | Separate test data with clear regression baselines | Structured `test/` directory with expected outputs |
| 3.7 | Add pytest markers for test categorization | `smoke`, `regression`, `slow` |

### Phase 4: CI/CD Pipeline (1 week)

| # | Task | Notes |
|---|------|-------|
| 4.1 | Create `.github/workflows/ci.yml` with tiered jobs | lint → build → smoke → full test (with `needs:` gates) |
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

### Phase 6: Feature Wishlist (longer-term)

| # | Task | Notes |
|---|------|-------|
| 6.1 | Replace bundled PROJ4 with modern PROJ via pixi/system library | See `design/feature_consideration__adopt_modern_PROJ_library.md` |
| 6.2 | Add GDAL/GeoTIFF support for direct raster I/O | See `design/feature_consideration__reading_writing_geotiffs_gdal.md` |
| 6.3 | Unit-agnostic operation (metric/imperial flexibility) | See `design/feature_consideration__unit_agnostic_operation.md` |
| 6.4 | Adopt pixi for library/dependency management | See `design/feature_consideration__adopt_pixi_for_library_management.md` |

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
| Unit test framework | FRUIT (vendored, legacy) | `test-drive` (modern, maintained) |
| Integration tests | Manual/ad-hoc | pytest with markers |
| CI | None | GitHub Actions, multi-compiler, multi-OS |
| Task runner | Manual meson commands / shell scripts | `pixi` tasks |
| Spelling | None | `codespell` |
| Test discovery | Manual registration | test-drive collection + meson `foreach` |
| Failure artifacts | None | Uploaded to GitHub Actions |
| Repo sync | Manual push to 3 remotes | CI auto-sync to DOI-USGS; manual push to code.usgs.gov at release |

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
