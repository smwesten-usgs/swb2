program tester
  !! Test driver for SWB2 unit tests using the test-drive framework.
  !!
  !! Usage:
  !!   swbtest              — run all test suites
  !!   swbtest timer        — run only the 'timer' suite
  !!   swbtest timer basic  — run only 'basic' test within 'timer' suite

  use, intrinsic :: iso_fortran_env, only: error_unit
  use testdrive, only: run_testsuite, new_testsuite, testsuite_type, &
                       select_suite, run_selected, get_argument, &
                       unittest_type
  use test_timer, only: collect_timer
  use test_allocatable_string, only: collect_allocatable_string
  use test_exceptions, only: collect_exceptions
  use test_gash, only: collect_gash
  use test_datetime, only: collect_datetime
  use test_fao56, only: collect_fao56
  use test_constants_and_conversions, only: collect_constants_and_conversions
  use test_fstring_list, only: collect_fstring_list
  use test_parameters, only: collect_parameters
  use test_solar_calculations, only: collect_solar_calculations
  use test_phenology, only: collect_phenology
  use test_interception_bucket, only: collect_interception_bucket
  implicit none

  integer :: stat, suite_index, total_tests
  character(len=:), allocatable :: suite_name, test_name
  type(testsuite_type), allocatable :: testsuites(:)
  type(unittest_type), allocatable :: temp_suite(:)
  character(len=*), parameter :: fmt = '("#", *(1x, a))'

  ! ANSI color codes
  character(len=*), parameter :: YELLOW = achar(27) // "[33m"
  character(len=*), parameter :: GREEN  = achar(27) // "[32m"
  character(len=*), parameter :: RED    = achar(27) // "[31m"
  character(len=*), parameter :: RESET  = achar(27) // "[0m"

  stat = 0

  testsuites = [ &
    new_testsuite("timer", collect_timer), &
    new_testsuite("allocatable_string", collect_allocatable_string), &
    new_testsuite("exceptions", collect_exceptions), &
    new_testsuite("gash", collect_gash), &
    new_testsuite("datetime", collect_datetime), &
    new_testsuite("phenology", collect_phenology), &
    new_testsuite("interception_bucket", collect_interception_bucket), &
    new_testsuite("fao56", collect_fao56), &
    new_testsuite("constants", collect_constants_and_conversions), &
    new_testsuite("fstring_list", collect_fstring_list), &
    new_testsuite("parameters", collect_parameters), &
    new_testsuite("solar", collect_solar_calculations) &
  ]

  call get_argument(1, suite_name)
  call get_argument(2, test_name)

  if (allocated(suite_name)) then
    suite_index = select_suite(testsuites, suite_name)
    if (suite_index > 0 .and. suite_index <= size(testsuites)) then
      if (allocated(test_name)) then
        write(error_unit, fmt) "Suite:", testsuites(suite_index)%name
        call run_selected(testsuites(suite_index)%collect, test_name, error_unit, stat)
        if (stat < 0) then
          error stop 1, quiet=.true.
        end if
      else
        write(error_unit, fmt) YELLOW // "Testing:", testsuites(suite_index)%name // RESET
        call run_testsuite(testsuites(suite_index)%collect, error_unit, stat)
      end if
    else
      write(error_unit, fmt) "Available testsuites:"
      do suite_index = 1, size(testsuites)
        write(error_unit, fmt) " -", testsuites(suite_index)%name
      end do
      error stop 1, quiet=.true.
    end if
  else
    do suite_index = 1, size(testsuites)
      write(error_unit, fmt) YELLOW // "Testing:", testsuites(suite_index)%name // RESET
      call run_testsuite(testsuites(suite_index)%collect, error_unit, stat)
    end do
  end if

  ! Count total tests across all suites
  total_tests = 0
  do suite_index = 1, size(testsuites)
    call testsuites(suite_index)%collect(temp_suite)
    total_tests = total_tests + size(temp_suite)
    deallocate(temp_suite)
  end do

  if (stat > 0) then
    write(error_unit, '(a, i0, " of ", i0, " test(s) in ", i0, " suite(s) failed!", a)') &
      RED, stat, total_tests, size(testsuites), RESET
    error stop 1, quiet=.true.
  else
    write(error_unit, '(a, i0, " tests in ", i0, " suites: All passed.", a)') &
      GREEN, total_tests, size(testsuites), RESET
  end if

end program tester
