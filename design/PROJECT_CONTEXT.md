# SWB2 Project Context for AI Assistants

## What This Is
SWB2 (Soil-Water-Balance v2) is a USGS Fortran code that estimates groundwater recharge on a gridded domain using a modified Thornthwaite-Mather approach at a daily timestep. It reads climate grids (precip, temperature) and static property grids (land use, soils, flow direction), runs a daily water balance, and outputs recharge estimates as NetCDF files.

## Key Facts
- **Language:** Fortran 2003+ (targets GNU extensions currently; goal is F2018)
- **Build system:** Meson ≥1.6.0 (also has legacy CMakeLists.txt, may be outdated)
- **Version:** v2.3.5-rc0
- **Compilers tested:** gfortran 10+, Intel ifx (oneAPI)
- **Dependencies:** NetCDF-C, HDF5, zlib (external); PROJ4 (bundled in src/proj4/)
- **Kind system:** Uses `iso_c_binding` kinds everywhere (c_int, c_float, c_double, c_bool)
- **Executables:** `swb2` (main model), `swbstats2` (post-processor), `swbtest` (unit tests)

## Architecture (see design/architecture_overview.md for full details)
- **Central type:** `MODEL_DOMAIN_T` in `model_domain.F90` — holds all state as 1D packed arrays
- **Strategy pattern:** ~40 procedure pointers on MODEL_DOMAIN_T select algorithms at runtime
- **Flow:** main → model_initialize → model_iterate (daily loop) → daily_calculation → output
- **Largest files:** model_domain.F90 (163KB), netcdf4_support.F90 (155KB), grid.F90 (100KB)

## Current State & Planned Improvements (May 2026)
See `design/plans_for_code_and_repo_improvement__may_2026.md` for the full plan. Summary:
1. Code is clean (no COMMON, GOTO, REAL*8, etc.) but uses `-std=gnu` not `-std=f2018`
2. Unit tests use vendored FRUIT framework — planned migration to `test-drive`
3. No CI currently — planned GitHub Actions with multi-compiler matrix
4. No formatting enforcement — planned `fprettify` adoption
5. 9 instances of `stop` should be `error stop`
6. `-fallow-argument-mismatch` flag needs investigation and removal

## Design Documentation
- `design/architecture_overview.md` — module map, data flow, key types
- `design/developer_quickstart.md` — build/test/contribute instructions
- `design/test_drive_migration_guide.md` — FRUIT→test-drive with code examples
- `design/plans_for_code_and_repo_improvement__may_2026.md` — full improvement roadmap

## Build Quick Reference
```
meson setup builddir                          # configure (release)
meson setup builddir -Doptimization=0         # configure (develop, with runtime checks)
meson setup builddir -Dnetcdf_root=<path>     # custom netcdf location
meson compile -C builddir                     # build
meson test -C builddir                        # run unit tests
```

## Conventions
- Commit messages: Conventional Commits (fix:, feat:, refactor:, etc.)
- Version: Semantic Versioning (MAJOR.MINOR.PATCH)
- Error handling: `call die(message, __FILE__, __LINE__)`
- Logging: `LOGS%write(message)` (global LOGFILE_T)
- Boolean constants: `TRUE`/`FALSE` from `constants_and_conversions`
