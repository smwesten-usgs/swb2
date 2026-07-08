program tester
  !! Test driver for SWB2 unit tests using the test-drive framework.
  !!
  !! Usage:
  !!   swbtest              — run all test suites
  !!   swbtest timer        — run only the 'timer' suite
  !!   swbtest timer basic  — run only 'basic' test within 'timer' suite

  use, intrinsic :: iso_fortran_env, only: error_unit
  use testdrive, only: run_testsuite, new_testsuite, testsuite_type, &
                       select_suite, run_selected, get_argument
  use test_timer, only: collect_timer
  use test_allocatable_string, only: collect_allocatable_string
  use test_exceptions, only: collect_exceptions
  use test_gash, only: collect_gash
  use test_datetime, only: collect_datetime
  use test_fao56, only: collect_fao56
  use test_constants_and_conversions, only: collect_constants_and_conversions
  implicit none

  integer :: stat, suite_index
  character(len=:), allocatable :: suite_name, test_name
  type(testsuite_type), allocatable :: testsuites(:)
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
    new_testsuite("fao56", collect_fao56), &
    new_testsuite("constants", collect_constants_and_conversions) &
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

  if (stat > 0) then
    write(error_unit, '(a, i0, 1x, a, a)') RED, stat, "test(s) failed!", RESET
    error stop 1, quiet=.true.
  else
    write(error_unit, '(a, a, a)') GREEN, "All tests passed.", RESET
  end if

end program tester
