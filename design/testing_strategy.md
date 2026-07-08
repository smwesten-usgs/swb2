# SWB2 Testing Strategy

**Date:** July 2026
**Replaces:** `test_drive_migration_guide.md` (May 2026)
**Status:** Tier 1 (Fortran unit tests) — IMPLEMENTED. Tier 2 (Python integration) — Proposed.

---

## Overview

SWB2 adopts a two-tier testing strategy inspired by MODFLOW6's approach:

1. **Fortran unit tests (test-drive):** Fast, compiled-with-the-code tests for individual functions and subroutines.
2. **Python integration/regression tests (pytest):** Full model runs on small domains, output verification, parametric sweeps.

The Fortran tier tests correctness of computational logic. The Python tier tests correctness of model behavior, configuration handling, and output.

---

## Why Two Tiers?

| Concern | Fortran unit tests handle it | Python integration tests handle it |
|---------|------------------------------|-------------------------------------|
| Does `calculate_Ks()` return correct values? | ✓ | |
| Does phenology stage transition at the right GDD? | ✓ | |
| Does a full run with `PHENOLOGY_METHOD FAO56_GDD` produce reasonable recharge? | | ✓ |
| Does the model error cleanly with a missing required column? | | ✓ |
| Did my refactoring change the output of an existing test case? | | ✓ (regression) |
| Is the Kcb interpolation formula correct? | ✓ | |
| Do all the modules wire together correctly at runtime? | | ✓ |

Neither tier alone is sufficient. Unit tests can't catch integration bugs (wrong pointer assignment, missing initialization). Integration tests can't pinpoint which function is broken — they just show that output changed.

---

## Tier 1: Fortran Unit Tests (test-drive)

### Framework: test-drive

- Repository: https://github.com/fortran-lang/test-drive
- Version: v0.5.0
- Pure Fortran 2008+, actively maintained (fortran-lang project)
- Meson-native dependency
- Individual test execution support

### Setup

Create `subprojects/test-drive.wrap`:
```ini
[wrap-git]
url = https://github.com/fortran-lang/test-drive.git
revision = v0.5.0
```

### Conversion Pattern: FRUIT → test-drive

| FRUIT | test-drive |
|-------|-----------|
| `use fruit` | `use testdrive, only: check, error_type, new_unittest, unittest_type` |
| `subroutine test_foo` | `subroutine test_foo(error)` with `type(error_type), allocatable, intent(out) :: error` |
| `call assert_equals(expected, actual)` | `call check(error, actual == expected, "message")` |
| `call assert_equals(exp, act, delta=tol)` | `call check(error, abs(actual - expected) < tol, "message")` |
| `call assert_true(condition)` | `call check(error, condition, "message")` |
| (no early return) | `if (allocated(error)) return` after each check |
| `call init_fruit` / `call fruit_summary` | `call run_testsuite(collect_fn, error_unit, stat)` |
| Manual registration in driver | Collected via subroutine arrays |

### What to Test at This Tier

- Pure/elemental computational functions (`calculate_water_stress_coefficient_ks`, `calculate_evaporation_reduction_coefficient_Kr`, `calculate_surface_evap_coefficient_ke`, `crop_coefficients_FAO56_calculate_Kcb_Max`)
- Phenology stage transitions (given inputs, what stage/growth_fraction results?)
- GDD accumulation logic
- Date arithmetic (already tested)
- Kcb interpolation (date-based and GDD-based)
- TAW/RAW calculation
- Any new function added during the phenology refactoring

### Test Driver Structure

```fortran
program tester
  use, intrinsic :: iso_fortran_env, only: error_unit
  use testdrive, only: run_testsuite, new_testsuite, testsuite_type, &
                       select_suite, run_selected, get_argument
  use test_timer, only: collect_timer
  use test_datetime, only: collect_datetime
  use test_gash, only: collect_gash
  use test_fao56, only: collect_fao56
  use test_exceptions, only: collect_exceptions
  use test_allocatable_string, only: collect_allocatable_string
  use test_phenology, only: collect_phenology
  implicit none

  integer :: stat, is
  character(len=:), allocatable :: suite_name, test_name
  type(testsuite_type), allocatable :: testsuites(:)

  stat = 0

  testsuites = [ &
    new_testsuite("timer", collect_timer), &
    new_testsuite("datetime", collect_datetime), &
    new_testsuite("gash", collect_gash), &
    new_testsuite("fao56", collect_fao56), &
    new_testsuite("exceptions", collect_exceptions), &
    new_testsuite("allocatable_string", collect_allocatable_string), &
    new_testsuite("phenology", collect_phenology) &
  ]

  call get_argument(1, suite_name)
  call get_argument(2, test_name)

  if (allocated(suite_name)) then
    is = select_suite(testsuites, suite_name)
    if (is > 0 .and. is <= size(testsuites)) then
      if (allocated(test_name)) then
        write(error_unit, '("# Suite: ", a)') testsuites(is)%name
        call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
        if (stat < 0) error stop 1
      else
        write(error_unit, '("# Testing: ", a)') testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
      end if
    else
      write(error_unit, '("# Available testsuites:")')
      do is = 1, size(testsuites)
        write(error_unit, '("#  - ", a)') testsuites(is)%name
      end do
      error stop 1
    end if
  else
    do is = 1, size(testsuites)
      write(error_unit, '("# Testing: ", a)') testsuites(is)%name
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do
  end if

  if (stat > 0) then
    write(error_unit, '(i0, " test(s) failed!")') stat
    error stop 1
  end if
end program tester
```

### Example: Converting test_timer.F90

**BEFORE (FRUIT):**
```fortran
module test_timer
  use fruit
  use timer
  implicit none
contains
  subroutine test_timer_basic
    type (TIMER_T) :: tmr
    tmr%starttime = 0.0_c_double
    tmr%stoptime = tmr%starttime + 60.*3. + 5.
    call tmr%calc_elapsed()
    call assert_equals(3, tmr%minutes)
    call assert_equals(5, tmr%seconds)
  end subroutine test_timer_basic
end module test_timer
```

**AFTER (test-drive):**
```fortran
module test_timer
  use iso_c_binding, only: c_double
  use testdrive, only: check, error_type, new_unittest, unittest_type
  use timer
  implicit none
  private
  public :: collect_timer
contains

  subroutine collect_timer(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("basic", test_timer_basic), &
      new_unittest("split", test_timer_split), &
      new_unittest("prettyprint", test_timer_prettyprint) &
    ]
  end subroutine collect_timer

  subroutine test_timer_basic(error)
    type(error_type), allocatable, intent(out) :: error
    type(TIMER_T) :: tmr

    tmr%starttime = 0.0_c_double
    tmr%stoptime = tmr%starttime + 60.*3. + 5.
    call tmr%calc_elapsed()

    call check(error, tmr%minutes == 3, "expected 3 minutes")
    if (allocated(error)) return
    call check(error, tmr%seconds == 5, "expected 5 seconds")
  end subroutine test_timer_basic

end module test_timer
```

### Example: Floating-Point Tolerance (test_gash.F90)

```fortran
subroutine test_gash_psat_calculation(error)
  type(error_type), allocatable, intent(out) :: error
  real(c_float) :: PSat
  real(c_float), parameter :: tol = 1.0e-4

  PSat = precipitation_at_saturation(E_div_P=0.5, &
         canopy_storage_capacity=0.05, canopy_cover_fraction=0.58)

  call check(error, abs(PSat - 0.119508) < tol, &
             "PSat mismatch: expected 0.119508")
end subroutine test_gash_psat_calculation
```

### Meson Build for Fortran Tests

```meson
test_drive_dep = dependency('test-drive', fallback: ['test-drive', 'testdrive_dep'])

test_srcs = files(
  'tester.f90',
  'test_timer.F90',
  'test_datetime.F90',
  'test_gash.F90',
  'test_fao56.F90',
  'test_exceptions.F90',
  'test_allocatable_string.F90',
  'test_phenology.F90',
)

tester = executable('swbtest',
  sources: test_srcs,
  link_with: [swb_library],
  link_language: 'fortran',
  dependencies: swb_deps + [test_drive_dep],
  install: true)

# Register all tests at once
test('All unit tests', tester)

# Also register individual suites for selective running
foreach t : ['timer', 'datetime', 'gash', 'fao56', 'exceptions',
             'allocatable_string', 'phenology']
  test(t, tester, args: t)
endforeach
```

### Running Fortran Tests

```bash
# Run all tests
meson test -C builddir

# Run all tests with verbose output
meson test -C builddir --verbose

# Run a specific test suite
builddir/swbtest timer

# Run a specific test within a suite
builddir/swbtest timer basic
```

---

## Tier 2: Python Integration/Regression Tests (pytest)

### Framework: pytest

- Standard Python test framework
- Fixture-based setup/teardown
- Parametrization (`@pytest.mark.parametrize`)
- Markers for filtering test subsets
- Rich plugin ecosystem (pytest-xdist for parallel, pytest-timeout, etc.)

### Dependencies

Using the local `py313` environment:

```
pytest
numpy
xarray
netCDF4
```

No custom packages needed. No equivalent to FloPy required — SWB2 input files are simple text that Python can generate directly.

### Directory Structure

```
test/
├── unit_tests/              ← Fortran (test-drive)
│   ├── meson.build
│   ├── tester.f90
│   ├── test_timer.F90
│   ├── test_datetime.F90
│   ├── test_fao56.F90
│   ├── test_phenology.F90
│   └── ...
├── integration/             ← Python (pytest)
│   ├── conftest.py          ← fixtures (swb2 runner, tmp dirs)
│   ├── pytest.ini
│   ├── test_basic_run.py
│   ├── test_phenology_methods.py
│   ├── test_fao56_two_stage.py
│   ├── test_error_messages.py
│   └── data/                ← minimal test grids, lookup tables
│       ├── grids/
│       └── tables/
├── regression/              ← existing regression test domains
│   ├── cs/
│   ├── or/
│   └── sar/
└── ...
```

### Core Fixtures (conftest.py)

```python
"""SWB2 integration test fixtures."""

import subprocess
import shutil
from pathlib import Path
from typing import Callable

import pytest


@pytest.fixture(scope="session")
def swb2_exe() -> Path:
    """Path to the compiled swb2 executable."""
    # Look in standard Meson build directory
    candidates = [
        Path(__file__).parents[2] / "builddir" / "src" / "swb2.exe",
        Path(__file__).parents[2] / "builddir" / "src" / "swb2",
    ]
    for p in candidates:
        if p.is_file():
            return p
    pytest.skip("swb2 executable not found — build first with `meson compile`")


@pytest.fixture
def run_swb2(swb2_exe: Path, tmp_path: Path) -> Callable:
    """
    Factory fixture that runs SWB2 in a temporary directory.

    Usage:
        result = run_swb2(control_file_text, lookup_table_text, grids={"landuse.asc": ...})
        assert result.returncode == 0
    """

    def _run(
        control_file: str,
        lookup_table: str | None = None,
        grids: dict[str, str] | None = None,
        grid_dir: Path | None = None,
        timeout: int = 120,
    ) -> subprocess.CompletedProcess:
        # Write control file
        (tmp_path / "swb2.ctl").write_text(control_file)

        # Write lookup table if provided
        if lookup_table:
            (tmp_path / "lookup.txt").write_text(lookup_table)

        # Write grid files if provided as strings
        if grids:
            for name, content in grids.items():
                (tmp_path / name).write_text(content)

        # Copy grid directory if provided
        if grid_dir and grid_dir.is_dir():
            for f in grid_dir.iterdir():
                shutil.copy2(f, tmp_path / f.name)

        # Run SWB2
        result = subprocess.run(
            [str(swb2_exe)],
            cwd=tmp_path,
            capture_output=True,
            text=True,
            timeout=timeout,
        )

        return result

    return _run


@pytest.fixture
def workspace(tmp_path: Path) -> Path:
    """The temporary workspace for the current test."""
    return tmp_path


@pytest.fixture(scope="session")
def test_data_dir() -> Path:
    """Path to shared test data (grids, tables)."""
    return Path(__file__).parent / "data"
```

### Example: Testing a Complete Model Run

```python
"""test_basic_run.py — Verify SWB2 runs to completion on a minimal domain."""

import numpy as np
import pytest
import xarray as xr


def test_basic_thornthwaite_mather_run(run_swb2, workspace, test_data_dir):
    """A 3x3 domain with T-M soil moisture and DOY-based growing season."""
    control = """\
    GRID 3 3 100 100 0 0
    GROWING_SEASON DOY_BASED
    ...
    """
    result = run_swb2(control, grid_dir=test_data_dir / "grids" / "minimal_3x3")
    assert result.returncode == 0, f"SWB2 failed:\n{result.stderr}"

    # Verify output exists and has reasonable values
    nc_files = list(workspace.glob("*.nc"))
    assert len(nc_files) > 0, "No NetCDF output produced"

    ds = xr.open_dataset(nc_files[0])
    assert "net_infiltration" in ds.data_vars
    assert np.all(np.isfinite(ds["net_infiltration"].values))
    assert np.all(ds["net_infiltration"].values >= 0)
```

### Example: Testing Error Messages

```python
"""test_error_messages.py — Verify the model errors clearly on bad input."""

import pytest


def test_missing_phenology_method_with_fao56(run_swb2):
    """FAO56 crop coefficients without PHENOLOGY_METHOD should error."""
    control = """\
    CROP_COEFFICIENT  FAO56
    """
    result = run_swb2(control)
    assert result.returncode != 0
    assert "PHENOLOGY_METHOD" in result.stderr or "PHENOLOGY_METHOD" in result.stdout


def test_missing_required_column(run_swb2):
    """PHENOLOGY_METHOD FAO56_DATES without L_dev column should error clearly."""
    control = """\
    PHENOLOGY_METHOD  FAO56_DATES
    CROP_COEFFICIENT  FAO56
    """
    lookup = """\
    LU_Code  Planting_date  L_ini  L_mid  L_late  Kcb_ini  Kcb_mid  Kcb_end  Kcb_min
    1        05/01          30     50     30      0.15     1.10     0.50     0.10
    """
    result = run_swb2(control, lookup)
    assert result.returncode != 0
    assert "L_dev" in result.stderr or "L_dev" in result.stdout
```

### Example: Parametric Testing of Phenology Methods

```python
"""test_phenology_methods.py — Verify different phenology methods produce expected behavior."""

import numpy as np
import pytest
import xarray as xr

PHENOLOGY_CONFIGS = [
    ("DOY_BASED", {"Growing_season_start": "04/15", "Growing_season_end": "10/15"}),
    ("FAO56_DATES", {"Planting_date": "05/01", "L_ini": "30", "L_dev": "40",
                     "L_mid": "50", "L_late": "30", "L_fallow": "215"}),
]


@pytest.mark.parametrize("method,params", PHENOLOGY_CONFIGS)
def test_growing_season_activates(run_swb2, workspace, method, params):
    """Each phenology method should produce some days with growing_season=1."""
    # ... build control file and lookup table from method + params ...
    result = run_swb2(control, lookup)
    assert result.returncode == 0

    ds = xr.open_dataset(workspace / "output.nc")
    growing = ds["growing_season"].values
    assert np.any(growing > 0), f"No growing season days found for {method}"
    assert np.any(growing == 0), f"Growing season never turns off for {method}"
```

### Example: Regression Testing

```python
"""test_regression.py — Verify output hasn't changed from a known baseline."""

import numpy as np
import pytest
import xarray as xr


@pytest.fixture(scope="session")
def baseline_dir() -> Path:
    """Directory containing known-good baseline outputs."""
    return Path(__file__).parent / "baselines"


def test_cs_domain_regression(run_swb2, workspace, baseline_dir):
    """Run the 'cs' test domain and compare against stored baseline."""
    # ... set up and run ...
    result = run_swb2(control, grid_dir=test_data_dir / "cs")
    assert result.returncode == 0

    actual = xr.open_dataset(workspace / "output.nc")
    expected = xr.open_dataset(baseline_dir / "cs_baseline.nc")

    # Compare net infiltration within tolerance
    np.testing.assert_allclose(
        actual["net_infiltration"].values,
        expected["net_infiltration"].values,
        rtol=1e-5,
        atol=1e-8,
        err_msg="Net infiltration regression failure"
    )
```

### pytest Configuration (pytest.ini)

```ini
[pytest]
testpaths = test/integration
markers =
    slow: marks tests that run full model simulations (deselect with '-m "not slow"')
    regression: marks regression tests against stored baselines
    phenology: marks tests related to the phenology module
```

### Running Python Tests

```bash
# Run all integration tests
pytest test/integration/

# Run with verbose output
pytest test/integration/ -v

# Run only phenology-related tests
pytest test/integration/ -m phenology

# Run only fast tests (skip full model runs)
pytest test/integration/ -m "not slow"

# Run a specific test file
pytest test/integration/test_phenology_methods.py

# Run a specific parametrized case
pytest test/integration/test_phenology_methods.py -k "FAO56_GDD"

# Run with parallel workers (requires pytest-xdist)
pytest test/integration/ -n auto
```

---

## Comparison with MODFLOW6's Approach

### What MODFLOW6 Does

| Layer | Framework | What it tests |
|-------|-----------|---------------|
| Fortran unit tests (`autotest/Test*.f90`) | test-drive | Array handlers, math utilities, hash tables, smoothing, geometry — isolated functions |
| Python integration tests (`autotest/test_*.py`) | pytest | Full model runs: build with FloPy → run mf6 → compare output. ~200+ test files covering every package and feature |
| Supporting infrastructure | `modflow_devtools`, `flopy`, `framework.py` | Fixtures for binary discovery, comparison utilities, parallel testing |

### What SWB2 Needs (Simpler)

| Layer | Framework | What it tests |
|-------|-----------|---------------|
| Fortran unit tests (`test/unit_tests/`) | test-drive | Kcb interpolation, phenology stages, Kr/Ks/Ke, date math — isolated functions |
| Python integration tests (`test/integration/`) | pytest | Full model runs: write control file → run swb2 → read NetCDF output → assert |
| Supporting infrastructure | `conftest.py` only | ~50 lines of fixtures. No equivalent to FloPy needed. |

SWB2 doesn't need:
- FloPy (SWB2 input is simple text — no object model needed)
- `modflow_devtools` (one `conftest.py` with a few fixtures suffices)
- `framework.py` class (SWB2 runs are simple: one executable, one control file, deterministic output)
- Comparison against multiple legacy programs (just compare against previous SWB2 output)

---

## Alternatives Considered and Rejected

### Python wrapping via ctypes/f2py (Rejected for Now)

Calling individual Fortran functions from Python would allow pytest's parametrization for unit-level tests. However:

- SWB2 functions rely heavily on module-level state (`PARAMS`, `SIM_DT`, allocatable arrays initialized at startup). Isolating individual functions for ctypes/f2py calling would require either:
  - Adding ISO_C_BINDING wrapper functions that set up and tear down state (significant effort)
  - Refactoring modules to eliminate global state (a much larger change)
- test-drive provides adequate unit testing capability within Fortran itself
- The effort-to-benefit ratio is poor for the current codebase

**Revisit when:** If SWB2 ever exposes a BMI (Basic Model Interface) — as discussed in `design/feature_consideration__bmi_for_swb2_modflow_coupling.md` — then Python-callable interfaces come for free and this option becomes attractive.

### pytest Only, No Fortran Tests (Rejected)

Running the full model for every test is slow and makes it hard to isolate failures. When a test fails, you want to know "which function is wrong?" not just "the output changed." Fortran unit tests provide that localization.

### FRUIT (Rejected — Current State)

Unmaintained since ~2013. Manual test registration. No individual test execution. Non-standard Fortran (`double precision`). No Meson integration.

---

## Migration Checklist: FRUIT → test-drive

**Status: ✅ COMPLETE (2026-07-08)**

For each existing test module (all done):

- [x] Replace `use fruit` with `use testdrive, only: check, error_type, new_unittest, unittest_type`
- [x] Add `private` / `public :: collect_<name>` to module
- [x] Create `collect_<name>(testsuite)` subroutine that builds the test array
- [x] Add `type(error_type), allocatable, intent(out) :: error` to each test subroutine
- [x] Convert `assert_equals(exp, act)` → `call check(error, act == exp, "msg")`
- [x] Convert `assert_equals(exp, act, delta=tol)` → `call check(error, abs(act - exp) < tol, "msg")`
- [x] Convert `assert_true(cond)` → `call check(error, cond, "msg")`
- [x] Add `if (allocated(error)) return` after each `check` call
- [x] Update `tester.F90` to USE the new module and add to testsuites array
- [x] Update `meson.build`
- [x] Verify test passes: `pixi run test`

Order of conversion (completed):
1. ✅ `test_timer.F90`
2. ✅ `test_allocatable_string.F90`
3. ✅ `test_exceptions__index_values_valid.F90`
4. ✅ `test_datetime.F90`
5. ✅ `test_gash.F90`
6. ✅ `test_FAO56_functions.F90`

Additional test modules added (new coverage):
7. `test_constants_and_conversions.F90`
8. `test_fstring_list.F90`
9. `test_parameters.F90`
10. `test_solar_calculations.F90`

**Implementation notes:**
- test-drive vendored as single file (`testdrive.F90`) — not a subproject (avoids DLL issues on Windows)
- `test_fixtures.F90` provides tiered environment setup analogous to pytest fixtures
- Old FRUIT files in `test/unit_tests/old_fruit_tests/` — delete when confident
- `logical(c_bool)` results must be wrapped with `logical()` for test-drive's `check` generic

---

## Summary: What Gets Run When

```
Developer workflow:
  meson test -C builddir          ← runs all Fortran unit tests (~seconds)
  pytest test/integration/ -m "not slow"  ← runs fast integration checks (~seconds)
  pytest test/integration/        ← runs all integration tests including full model runs (~minutes)

CI workflow (GitHub Actions):
  1. meson setup + compile
  2. meson test                   ← Fortran unit tests (gate: must pass before proceeding)
  3. pytest test/integration/     ← full integration + regression suite
```
