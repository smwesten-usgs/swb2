# SWB2 Developer Quickstart Guide

**Last updated:** May 2026  
**SWB2 version:** v2.4.1  

---

## Prerequisites

### Required
- **Fortran compiler**: gfortran ≥10 (GCC) or Intel ifx (oneAPI)
- **C/C++ compiler**: gcc/g++ or MSVC (for PROJ4 and meson C detection)
- **Meson** ≥1.6.0 (`pip install meson`)
- **Ninja** build system (`pip install ninja`)
- **NetCDF-C library** (with HDF5 and zlib)

### Platform-Specific NetCDF Locations (defaults)
| Platform | Default `netcdf_root` |
|----------|----------------------|
| Windows | `C:/Program Files/netCDF 4.9.3` |
| macOS | `/opt/homebrew/opt/netcdf` |
| Linux | `/usr/local` |

Override with: `meson setup builddir -Dnetcdf_root=/path/to/netcdf`

### Windows Setup (gfortran)
1. Install MSYS2 or use standalone MinGW-w64 with gfortran
2. Install NetCDF-C (pre-built binaries from Unidata)
3. Ensure `netcdf.lib`, `hdf5.lib`, `hdf5_hl.lib`, `zlib.lib` are in `netcdf_root/lib`

### Windows Setup (Intel ifx)
1. Install Intel oneAPI Base + HPC Toolkit
2. Run from oneAPI command prompt (setvars.bat)
3. NetCDF must be built with Intel compiler for ABI compatibility

### Linux/macOS Setup
```bash
# Ubuntu/Debian
sudo apt install gfortran libnetcdf-dev libhdf5-dev

# macOS (Homebrew)
brew install gcc netcdf
```

---

## Building

### Quick Build (release mode)

```bash
# Configure (from repo root)
meson setup builddir

# Compile
meson compile -C builddir

# Executables appear in builddir/
#   - swb2       (main model)
#   - swbstats2  (post-processing statistics)
#   - swbtest    (unit tests)
```

### Development Build (with runtime checks)

```bash
meson setup builddir_dev -Doptimization=0
meson compile -C builddir_dev
```

This enables `-fcheck=all`, `-finit-real=nan`, and floating-point exception traps.

### Custom NetCDF Location

```bash
meson setup builddir -Dnetcdf_root=E:/libs/netcdf-4.9.3
```

### Reconfigure

```bash
meson configure builddir -Doptimization=2
meson compile -C builddir
```

### Clean Rebuild

```bash
# Remove build directory and start fresh
rm -rf builddir
meson setup builddir
meson compile -C builddir
```

---

## Build Profiles

| Profile | Triggered by | Flags (gfortran) |
|---------|-------------|------------------|
| release | `-Doptimization=2` (default) | `-ffpe-trap=overflow,zero -g -fbacktrace` |
| develop | `-Doptimization=0` or `1` | `-fcheck=all -finit-real=nan -ffpe-trap=overflow,zero -g -fbacktrace` |

---

## Running Tests

### Unit Tests (FRUIT framework)

```bash
meson test -C builddir
```

Or run directly:
```bash
builddir/swbtest
```

The test executable runs all registered FRUIT test cases and reports pass/fail counts.

### Test Structure

```
test/
├── unit_tests/          # FRUIT-based Fortran unit tests
│   ├── fruit.F90        # FRUIT framework (vendored)
│   ├── fruit_util.F90   # FRUIT utilities
│   ├── fruit_driver.F90 # Test driver (manual registration)
│   ├── test_allocatable_string.F90
│   ├── test_datetime.F90
│   ├── test_exceptions__index_values_valid.F90
│   ├── test_FAO56_functions.F90
│   ├── test_gash.F90
│   └── test_timer.F90
├── integration_tests/   # Full model runs
├── regression_tests/    # Output comparison tests
├── feature_tests/       # Feature-specific model runs
├── bugfix_tests/        # Regression tests for specific bugs
└── test_data/           # Shared test input data
```

---

## Project Structure

```
swb2/
├── src/                 # Fortran source (67 .F90 files + proj4/ C library)
│   ├── main.F90         # Entry point
│   ├── model_domain.F90 # Central state container (largest file)
│   ├── proj4/           # Bundled PROJ4 C library
│   └── meson.build      # Source build definition
├── test/                # All test categories
├── build/               # Build output (gitignored)
├── design/              # Architecture and planning docs
├── doc_src/             # Doxygen source documentation
├── docs/                # Generated Doxygen HTML
├── share/               # Standard lookup tables
├── docker/              # Docker build environment
├── doxygen/             # Doxygen configuration
├── meson.build          # Top-level build configuration
├── meson_options.txt    # Build options (netcdf_root)
├── CMakeLists.txt       # Legacy CMake build (may be outdated)
└── DEVELOPMENT_GUIDELINES.md  # Commit conventions
```

---

## Executables Produced

| Executable | Purpose |
|-----------|---------|
| `swb2` | Main soil-water-balance model |
| `swbstats2` | Post-processing: reads SWB2 NetCDF output, computes zonal statistics |
| `swbtest` | Unit test runner |

---

## Compiler Support

| Compiler | Platform | Status |
|----------|----------|--------|
| gfortran 10+ | Windows, Linux, macOS | Primary development compiler |
| Intel ifx (oneAPI) | Windows, Linux | Supported, tested |
| Intel ifort (classic) | Windows, Linux | Legacy support |

---

## Contributing

### Commit Messages
Follow [Conventional Commits](https://www.conventionalcommits.org/):
- `fix:` — bug fix (PATCH version bump)
- `feat:` — new feature (MINOR version bump)
- `refactor:` — code restructuring
- `test:` — test changes
- `docs:` — documentation
- `build:` — build system changes
- `BREAKING CHANGE:` — in commit body for MAJOR version bumps

### Version Numbering
[Semantic Versioning](https://semver.org/): `MAJOR.MINOR.PATCH`

### Adding a New Process Module

1. Create `src/new_method__variant.F90` with a module containing:
   - An initialization subroutine (reads parameters, opens data files)
   - A calculation subroutine matching the expected procedure pointer interface
2. Add the file to `swb_src` list in `src/meson.build`
3. Add a `USE` statement in `model_initialize.F90`
4. Add a case to the method-selection logic in `model_initialize.F90`
5. Write unit tests in `test/unit_tests/test_new_method.F90`
6. Add the test file to `test/unit_tests/meson.build`

### Key Conventions in the Codebase
- All types use `iso_c_binding` kinds: `integer(c_int)`, `real(c_float)`, `real(c_double)`, `logical(c_bool)`
- Boolean constants: `TRUE` and `FALSE` from `constants_and_conversions`
- Error handling: call `call die(message, __FILE__, __LINE__)` for fatal errors
- Logging: use `LOGS%write(message)` (global LOGFILE_T instance)
- String operations: use `fstring` module operators (`.contains.`, `.strequal.`)

---

## Debugging Tips

### Enable Full Runtime Checks
```bash
meson setup builddir_debug -Doptimization=0
```
This enables array bounds checking, uninitialized variable detection (NaN initialization), and full floating-point exception trapping.

### Common Issues

| Symptom | Likely Cause |
|---------|-------------|
| Link error: `undefined reference to nc_*` | NetCDF library not found. Check `-Dnetcdf_root` |
| Segfault on startup | Control file path wrong or missing required grid files |
| NaN in output | Check that all required input grids cover the model domain |
| `SIGFPE` in release mode | Overflow or divide-by-zero in a calculation module |

### Useful Meson Commands
```bash
meson introspect builddir --targets    # List build targets
meson introspect builddir --tests      # List registered tests
meson configure builddir               # Show current options
meson compile -C builddir --clean      # Clean and rebuild
```
