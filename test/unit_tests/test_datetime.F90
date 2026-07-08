module test_datetime
  !! Unit tests for the DATETIME_T type and related date arithmetic.
  !!
  !! Tests date parsing (various formats), date addition, Julian day
  !! calculation, leap day counting, and error handling for illegal values.
  !!
  !! Uses setup_common() from test_fixtures for logging initialization
  !! (needed by the error-suppression tests).

  use iso_c_binding, only: c_int, c_bool
  use testdrive, only: check, error_type, new_unittest, unittest_type
  use datetime, only: DATETIME_T, count_leap_days_between_dates
  use exceptions, only: HALT_UPON_FATAL_ERROR
  use constants_and_conversions, only: FALSE, TRUE, iTINYVAL
  use test_fixtures, only: setup_common
  implicit none
  private

  public :: collect_datetime

contains

  !---------------------------------------------------------------------------
  !> @brief Register all datetime tests into the test suite array.
  !---------------------------------------------------------------------------
  subroutine collect_datetime(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    call setup_common()

    testsuite = [ &
      ! --- Date parsing ---
      new_unittest("parse_default_format", test_parse_default_format), &
      new_unittest("parse_mangled_format", test_parse_mangled_format), &
      new_unittest("parse_custom_format", test_parse_custom_format), &
      ! --- Date arithmetic ---
      new_unittest("add_days_across_month", test_add_days_across_month), &
      new_unittest("addition_operator", test_addition_operator), &
      ! --- Leap day counting ---
      new_unittest("leap_days_within_year", test_leap_days_within_year), &
      new_unittest("leap_days_before_feb29", test_leap_days_before_feb29), &
      new_unittest("leap_days_multi_year", test_leap_days_multi_year), &
      new_unittest("leap_days_1950_to_2020", test_leap_days_1950_to_2020), &
      new_unittest("leap_days_1900_to_2020", test_leap_days_1900_to_2020), &
      ! --- Error handling (illegal values) ---
      new_unittest("parse_illegal_date", test_parse_illegal_date), &
      new_unittest("julian_illegal_month", test_julian_illegal_month), &
      new_unittest("julian_illegal_day", test_julian_illegal_day), &
      new_unittest("julian_illegal_month_and_day", test_julian_illegal_month_and_day) &
    ]
  end subroutine collect_datetime

  !---------------------------------------------------------------------------
  ! Helper: suppress fatal errors during tests that intentionally trigger them
  !---------------------------------------------------------------------------
  subroutine suppress_fatal_errors()
    use, intrinsic :: iso_fortran_env, only: error_unit
    HALT_UPON_FATAL_ERROR = FALSE
    write(error_unit, '(a)') "  [NOTE: Any error message is EXPECTED — testing error handling]"
  end subroutine suppress_fatal_errors

  subroutine restore_fatal_errors()
    use, intrinsic :: iso_fortran_env, only: error_unit
    HALT_UPON_FATAL_ERROR = TRUE
    write(error_unit, '(a)') "  [END of any expected error message]"
  end subroutine restore_fatal_errors

  !---------------------------------------------------------------------------
  ! DATE PARSING TESTS
  !---------------------------------------------------------------------------

  !> @brief Parse "03/15/2011" with default MM/DD/YYYY format.
  subroutine test_parse_default_format(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt

    call dt%parseDate("03/15/2011", sFilename=trim(__FILE__), iLineNumber=__LINE__)

    call check(error, int(dt%iMonth) == 3, "expected month = 3")
    if (allocated(error)) return
    call check(error, int(dt%iDay) == 15, "expected day = 15")
    if (allocated(error)) return
    call check(error, int(dt%iYear) == 2011, "expected year = 2011")
  end subroutine test_parse_default_format

  !> @brief Parse "3/2/2011" — missing leading zeros should still work.
  subroutine test_parse_mangled_format(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt

    call dt%parseDate("3/2/2011", sFilename=trim(__FILE__), iLineNumber=__LINE__)

    call check(error, int(dt%iMonth) == 3, "expected month = 3")
    if (allocated(error)) return
    call check(error, int(dt%iDay) == 2, "expected day = 2")
    if (allocated(error)) return
    call check(error, int(dt%iYear) == 2011, "expected year = 2011")
  end subroutine test_parse_mangled_format

  !> @brief Parse "1776-07-4" with custom YYYY-MM-DD format.
  subroutine test_parse_custom_format(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt

    call dt%setDateFormat("YYYY-MM-DD")
    call dt%parseDate("1776-07-4", sFilename=trim(__FILE__), iLineNumber=__LINE__)

    call check(error, int(dt%iMonth) == 7, "expected month = 7")
    if (allocated(error)) return
    call check(error, int(dt%iDay) == 4, "expected day = 4")
    if (allocated(error)) return
    call check(error, int(dt%iYear) == 1776, "expected year = 1776")
  end subroutine test_parse_custom_format

  !---------------------------------------------------------------------------
  ! DATE ARITHMETIC TESTS
  !---------------------------------------------------------------------------

  !> @brief Add 5 days to Feb 29, 2000 → March 5, 2000.
  subroutine test_add_days_across_month(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt
    integer :: i

    call dt%calcJulianDay(iMonth=2, iDay=29, iYear=2000)
    do i = 1, 5
      call dt%addDay()
    end do

    call check(error, int(dt%iMonth) == 3, "expected month = 3")
    if (allocated(error)) return
    call check(error, int(dt%iDay) == 5, "expected day = 5")
    if (allocated(error)) return
    call check(error, int(dt%iYear) == 2000, "expected year = 2000")
  end subroutine test_add_days_across_month

  !> @brief 1950-01-01 + 11323.5 days = 1981-01-01.
  subroutine test_addition_operator(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt_min, dt_new

    call dt_min%parseDate("1950-01-01", sFilename=trim(__FILE__), iLineNumber=__LINE__)
    dt_new = dt_min + 11323.5
    call dt_new%calcGregorianDate()

    call check(error, int(dt_new%iMonth) == 1, "expected month = 1")
    if (allocated(error)) return
    call check(error, int(dt_new%iDay) == 1, "expected day = 1")
    if (allocated(error)) return
    call check(error, int(dt_new%iYear) == 1981, "expected year = 1981")
  end subroutine test_addition_operator

  !---------------------------------------------------------------------------
  ! LEAP DAY COUNTING TESTS
  !---------------------------------------------------------------------------

  !> @brief 2000-01-01 to 2000-03-01: 1 leap day (Feb 29, 2000).
  subroutine test_leap_days_within_year(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt_min, dt_max
    integer :: num_leap_days

    call dt_min%setDateFormat("YYYY-MM-DD")
    call dt_min%parseDate("2000-01-01", sFilename=trim(__FILE__), iLineNumber=__LINE__)
    call dt_max%setDateFormat("YYYY-MM-DD")
    call dt_max%parseDate("2000-03-01", sFilename=trim(__FILE__), iLineNumber=__LINE__)

    num_leap_days = count_leap_days_between_dates(date_min=dt_min, date_max=dt_max)
    call check(error, num_leap_days == 1, "expected 1 leap day between 2000-01-01 and 2000-03-01")
  end subroutine test_leap_days_within_year

  !> @brief 2000-01-01 to 2000-02-28: 0 leap days (haven't reached Feb 29 yet).
  subroutine test_leap_days_before_feb29(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt_min, dt_max
    integer :: num_leap_days

    call dt_min%setDateFormat("YYYY-MM-DD")
    call dt_min%parseDate("2000-01-01", sFilename=trim(__FILE__), iLineNumber=__LINE__)
    call dt_max%setDateFormat("YYYY-MM-DD")
    call dt_max%parseDate("2000-02-28", sFilename=trim(__FILE__), iLineNumber=__LINE__)

    num_leap_days = count_leap_days_between_dates(date_min=dt_min, date_max=dt_max)
    call check(error, num_leap_days == 0, "expected 0 leap days between 2000-01-01 and 2000-02-28")
  end subroutine test_leap_days_before_feb29

  !> @brief 2000-01-01 to 2004-02-29: 2 leap days (2000 and 2004).
  subroutine test_leap_days_multi_year(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt_min, dt_max
    integer :: num_leap_days

    call dt_min%setDateFormat("YYYY-MM-DD")
    call dt_min%parseDate("2000-01-01", sFilename=trim(__FILE__), iLineNumber=__LINE__)
    call dt_max%setDateFormat("YYYY-MM-DD")
    call dt_max%parseDate("2004-02-29", sFilename=trim(__FILE__), iLineNumber=__LINE__)

    num_leap_days = count_leap_days_between_dates(date_min=dt_min, date_max=dt_max)
    call check(error, num_leap_days == 2, "expected 2 leap days between 2000-01-01 and 2004-02-29")
  end subroutine test_leap_days_multi_year

  !> @brief 1950-01-01 to 2020-12-31: 18 leap days.
  subroutine test_leap_days_1950_to_2020(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt_min, dt_max
    integer :: num_leap_days

    call dt_min%setDateFormat("YYYY-MM-DD")
    call dt_min%parseDate("1950-01-01", sFilename=trim(__FILE__), iLineNumber=__LINE__)
    call dt_max%setDateFormat("YYYY-MM-DD")
    call dt_max%parseDate("2020-12-31", sFilename=trim(__FILE__), iLineNumber=__LINE__)

    num_leap_days = count_leap_days_between_dates(date_min=dt_min, date_max=dt_max)
    call check(error, num_leap_days == 18, "expected 18 leap days between 1950 and 2020")
  end subroutine test_leap_days_1950_to_2020

  !> @brief 1900-01-01 to 2020-12-31: 30 leap days.
  subroutine test_leap_days_1900_to_2020(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt_min, dt_max
    integer :: num_leap_days

    call dt_min%setDateFormat("YYYY-MM-DD")
    call dt_min%parseDate("1900-01-01", sFilename=trim(__FILE__), iLineNumber=__LINE__)
    call dt_max%setDateFormat("YYYY-MM-DD")
    call dt_max%parseDate("2020-12-31", sFilename=trim(__FILE__), iLineNumber=__LINE__)

    num_leap_days = count_leap_days_between_dates(date_min=dt_min, date_max=dt_max)
    call check(error, num_leap_days == 30, "expected 30 leap days between 1900 and 2020")
  end subroutine test_leap_days_1900_to_2020

  !---------------------------------------------------------------------------
  ! ERROR HANDLING TESTS (illegal values)
  !---------------------------------------------------------------------------

  !> @brief Parsing Feb 29, 2001 (not a leap year) should flag an error.
  subroutine test_parse_illegal_date(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt

    call suppress_fatal_errors()
    call dt%parseDate("2/29/2001", sFilename=trim(__FILE__), iLineNumber=__LINE__)
    call restore_fatal_errors()

    call check(error, dt%dJulianDate < 0.0, &
               "parsing 2/29/2001 should yield negative Julian date (error flag)")
  end subroutine test_parse_illegal_date

  !> @brief Julian day with month=0 should return iTINYVAL.
  subroutine test_julian_illegal_month(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt

    call suppress_fatal_errors()
    call dt%calcJulianDay(iMonth=0, iDay=28, iYear=2000)
    call restore_fatal_errors()

    call check(error, int(dt%iJulianDay) == iTINYVAL, &
               "month=0 should yield iJulianDay == iTINYVAL")
  end subroutine test_julian_illegal_month

  !> @brief Julian day with day=0 should return iTINYVAL.
  subroutine test_julian_illegal_day(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt

    call suppress_fatal_errors()
    call dt%calcJulianDay(iMonth=2, iDay=0, iYear=2000)
    call restore_fatal_errors()

    call check(error, int(dt%iJulianDay) == iTINYVAL, &
               "day=0 should yield iJulianDay == iTINYVAL")
  end subroutine test_julian_illegal_day

  !> @brief Julian day with month=13 and day=0 should return iTINYVAL.
  subroutine test_julian_illegal_month_and_day(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt

    call suppress_fatal_errors()
    call dt%calcJulianDay(iMonth=13, iDay=0, iYear=2000)
    call restore_fatal_errors()

    call check(error, int(dt%iJulianDay) == iTINYVAL, &
               "month=13, day=0 should yield iJulianDay == iTINYVAL")
  end subroutine test_julian_illegal_month_and_day

end module test_datetime
