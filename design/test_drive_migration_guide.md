# FRUIT → test-drive Migration Guide

**Last updated:** May 2026  
**Purpose:** Concrete code examples for converting SWB2 unit tests from FRUIT to test-drive  
**Reference:** https://github.com/fortran-lang/test-drive  

---

## Why Migrate?

| Aspect | FRUIT | test-drive |
|--------|-------|-----------|
| Maintained | Last update ~2013 | Active (fortran-lang project) |
| Test discovery | Manual registration in driver | Collected via subroutine arrays |
| Assertion style | `call assert_equals(expected, actual)` | `call check(error, condition, message)` |
| Error propagation | Global counter | Allocatable `error_type` (early return) |
| Meson integration | Custom executable | Built-in `dependency('test-drive')` |
| Individual test runs | Not supported | `tester SuiteName TestName` |
| Standard compliance | Uses `double precision` | Pure Fortran 2008+ |

---

## Setup: Adding test-drive to SWB2

### Option A: Meson subproject (self-contained)

Create `subprojects/test-drive.wrap`:
```ini
[wrap-git]
url = https://github.com/fortran-lang/test-drive.git
revision = v0.5.0
```

In `test/unit_tests/meson.build`:
```meson
test_drive_dep = dependency('test-drive', fallback: ['test-drive', 'testdrive_dep'])
```

### Option B: System dependency (via pkg-config or conda)

```meson
test_drive_dep = dependency('test-drive', required: true)
```

---

## Conversion Pattern: FRUIT → test-drive

### Key Differences

| FRUIT | test-drive |
|-------|-----------|
| `use fruit` | `use testdrive, only: check, error_type, new_unittest, unittest_type` |
| `subroutine test_foo` | `subroutine test_foo(error)` with `type(error_type), allocatable, intent(out) :: error` |
| `call assert_equals(expected, actual)` | `call check(error, actual == expected, "message")` |
| `call assert_equals(exp, act, delta=tol)` | `call check(error, abs(actual - expected) < tol, "message")` |
| `call assert_true(condition)` | `call check(error, condition, "message")` |
| (no early return) | `if (allocated(error)) return` after each check |
| `call init_fruit` / `call fruit_summary` | `call run_testsuite(collect_fn, error_unit, stat)` |

---

## Example 1: test_timer.F90

### BEFORE (FRUIT)

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
    call assert_equals(-9999, tmr%hours)
    call assert_equals(-9999, tmr%days)
  end subroutine test_timer_basic

  subroutine test_timer_split
    type (TIMER_T) :: tmr
    tmr%starttime = 0.0_c_double
    tmr%stoptime = tmr%starttime + 60.*3. + 5.
    call tmr%calc_split()
    call tmr%calc_elapsed()
    call assert_equals(5, tmr%seconds)
    call assert_equals(3, tmr%minutes)
    tmr%stoptime = tmr%starttime + 3600.*4 + 60.*6. + 42.
    call tmr%calc_split()
    call assert_equals(42, tmr%seconds)
    call assert_equals(6, tmr%minutes)
    call assert_equals(4, tmr%hours)
  end subroutine test_timer_split

  subroutine test_timer_prettyprint
    type (TIMER_T) :: tmr
    tmr%starttime = 0.0_c_double
    call tmr%reset()
    tmr%stoptime = tmr%starttime + 86400.*3. + 3600.*4. + 60.*6. + 42.
    call tmr%calc_elapsed()
    call assert_equals(3, tmr%days)
    call assert_equals(4, tmr%hours)
    call assert_equals(6, tmr%minutes)
    call assert_equals(42, tmr%seconds)
    call assert_equals("3 days, 4 hours, 6 minutes, 42 seconds.", tmr%get_pretty())
  end subroutine test_timer_prettyprint

end module test_timer
```

### AFTER (test-drive)

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
    if (allocated(error)) return
    call check(error, tmr%hours == -9999, "expected hours unset")
    if (allocated(error)) return
    call check(error, tmr%days == -9999, "expected days unset")
  end subroutine test_timer_basic

  subroutine test_timer_split(error)
    type(error_type), allocatable, intent(out) :: error
    type(TIMER_T) :: tmr

    tmr%starttime = 0.0_c_double
    tmr%stoptime = tmr%starttime + 60.*3. + 5.
    call tmr%calc_split()
    call tmr%calc_elapsed()

    call check(error, tmr%seconds == 5, "expected 5 seconds")
    if (allocated(error)) return
    call check(error, tmr%minutes == 3, "expected 3 minutes")
    if (allocated(error)) return

    tmr%stoptime = tmr%starttime + 3600.*4 + 60.*6. + 42.
    call tmr%calc_split()

    call check(error, tmr%seconds == 42, "expected 42 seconds")
    if (allocated(error)) return
    call check(error, tmr%minutes == 6, "expected 6 minutes")
    if (allocated(error)) return
    call check(error, tmr%hours == 4, "expected 4 hours")
  end subroutine test_timer_split

  subroutine test_timer_prettyprint(error)
    type(error_type), allocatable, intent(out) :: error
    type(TIMER_T) :: tmr

    tmr%starttime = 0.0_c_double
    call tmr%reset()
    tmr%stoptime = tmr%starttime + 86400.*3. + 3600.*4. + 60.*6. + 42.
    call tmr%calc_elapsed()

    call check(error, tmr%days == 3, "expected 3 days")
    if (allocated(error)) return
    call check(error, tmr%hours == 4, "expected 4 hours")
    if (allocated(error)) return
    call check(error, tmr%minutes == 6, "expected 6 minutes")
    if (allocated(error)) return
    call check(error, tmr%seconds == 42, "expected 42 seconds")
    if (allocated(error)) return
    call check(error, tmr%get_pretty() == "3 days, 4 hours, 6 minutes, 42 seconds.", &
               "pretty print mismatch")
  end subroutine test_timer_prettyprint

end module test_timer
```

---

## Example 2: test_gash.F90 (floating-point tolerance)

### BEFORE (FRUIT)

```fortran
module test__gash
  use iso_c_binding, only: c_int, c_float, c_bool
  use constants_and_conversions
  use fruit
  use interception__gash
  implicit none

  real(c_float), parameter :: maximum_difference = 1.0e-4

contains

  subroutine test_gash_psat_calculation
    real(c_float) :: PSat
    PSat = precipitation_at_saturation(E_div_P=0.5, &
           canopy_storage_capacity=0.05, canopy_cover_fraction=0.58)
    call assert_equals(0.119508, PSat, delta=maximum_difference)
  end subroutine test_gash_psat_calculation

end module test__gash
```

### AFTER (test-drive)

```fortran
module test_gash
  use iso_c_binding, only: c_float
  use testdrive, only: check, error_type, new_unittest, unittest_type
  use interception__gash
  implicit none
  private
  public :: collect_gash

  real(c_float), parameter :: tol = 1.0e-4

contains

  subroutine collect_gash(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("psat_calculation", test_gash_psat_calculation), &
      new_unittest("full_calculation", test_gash_calculation) &
    ]
  end subroutine collect_gash

  subroutine test_gash_psat_calculation(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: PSat

    PSat = precipitation_at_saturation(E_div_P=0.5, &
           canopy_storage_capacity=0.05, canopy_cover_fraction=0.58)

    call check(error, abs(PSat - 0.119508) < tol, &
               "PSat mismatch: expected 0.119508")
  end subroutine test_gash_psat_calculation

  subroutine test_gash_calculation(error)
    type(error_type), allocatable, intent(out) :: error
    ! ... (port the loop-based test similarly)
  end subroutine test_gash_calculation

end module test_gash
```

---

## Example 3: test_datetime.F90 (testing error conditions)

### BEFORE (FRUIT)

```fortran
subroutine test_datetime_illegal_values
  type(DATETIME_T) :: dt
  call start_supressing_fatal_errors()
  call dt%parseDate("2/29/2001", sFilename=trim(__FILE__), iLineNumber=__LINE__)
  call assert_true(dt%dJulianDate < 0.)
  call stop_supressing_fatal_errors()
end subroutine test_datetime_illegal_values
```

### AFTER (test-drive)

```fortran
subroutine test_datetime_illegal_values(error)
  type(error_type), allocatable, intent(out) :: error
  type(DATETIME_T) :: dt

  ! Suppress fatal errors for this test
  HALT_UPON_FATAL_ERROR = FALSE
  call dt%parseDate("2/29/2001", sFilename=trim(__FILE__), iLineNumber=__LINE__)
  HALT_UPON_FATAL_ERROR = TRUE

  call check(error, dt%dJulianDate < 0., &
             "expected negative Julian date for invalid date 2/29/2001")
end subroutine test_datetime_illegal_values
```

---

## The Test Driver (tester.f90)

### BEFORE (fruit_driver.F90)

```fortran
program tests
  use fruit
  use test_timer
  use test_datetime
  ! ... more USE statements
  implicit none

  call init_fruit
  call run_test_case(test_timer_basic, "timer: basic")
  call run_test_case(test_timer_split, "timer: split")
  ! ... manually register every test
  call fruit_summary
  call fruit_finalize
end program tests
```

### AFTER (tester.f90)

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
    new_testsuite("allocatable_string", collect_allocatable_string) &
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

---

## Updated meson.build for Tests

### BEFORE

```meson
swbtest_src = files(
  'fruit.F90',
  'fruit_util.F90',
  'test_allocatable_string.F90',
  'test_datetime.F90',
  'test_exceptions__index_values_valid.F90',
  'test_FAO56_functions.F90',
  'test_gash.F90',
  'test_timer.F90',
  'fruit_driver.F90'
)

executable('swbtest', 
            swbtest_src,
            link_with: [proj4, swb_library], 
            link_language: 'fortran',
            dependencies: [zlib, hdf5, hdf5_hl, netcdf],
            install: true)
```

### AFTER

```meson
test_drive_dep = dependency('test-drive', required: true)

tests = [
  'timer',
  'datetime',
  'gash',
  'fao56',
  'exceptions',
  'allocatable_string',
]

test_srcs = files('tester.f90')
foreach t : tests
  test_srcs += files('test_@0@.F90'.format(t))
endforeach

tester = executable('swbtest',
  sources: test_srcs,
  link_with: [proj4, swb_library],
  link_language: 'fortran',
  dependencies: [zlib, hdf5, hdf5_hl, netcdf, test_drive_dep],
  install: true)

# Register all tests at once
test('All unit tests', tester)

# Also register individual suites for selective running
foreach t : tests
  test(t, tester, args: t)
endforeach
```

---

## Migration Checklist

For each test module:

- [ ] Replace `use fruit` with `use testdrive, only: check, error_type, new_unittest, unittest_type`
- [ ] Add `private` / `public :: collect_<name>` to module
- [ ] Create `collect_<name>(testsuite)` subroutine that builds the test array
- [ ] Add `type(error_type), allocatable, intent(out) :: error` to each test subroutine
- [ ] Convert `assert_equals(exp, act)` → `call check(error, act == exp, "msg")`
- [ ] Convert `assert_equals(exp, act, delta=tol)` → `call check(error, abs(act - exp) < tol, "msg")`
- [ ] Convert `assert_true(cond)` → `call check(error, cond, "msg")`
- [ ] Add `if (allocated(error)) return` after each `check` call
- [ ] Update `tester.f90` to USE the new module and add to testsuites array
- [ ] Update `meson.build` to reference new file names
- [ ] Remove `fruit.F90`, `fruit_util.F90`, `fruit_driver.F90` once all tests are ported

---

## Running Tests After Migration

```bash
# Run all tests
meson test -C builddir

# Run all tests with verbose output
meson test -C builddir --verbose

# Run a specific test suite
builddir/swbtest timer

# Run a specific test within a suite
builddir/swbtest timer basic

# List available suites (pass invalid name)
builddir/swbtest --help
```
