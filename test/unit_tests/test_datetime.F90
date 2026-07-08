module test_datetime
  !! Unit tests for the DATETIME_T type and related date arithmetic.
  !!
  !! Tests date parsing (various formats), date addition, Julian day
  !! calculation, leap day counting, and error handling for illegal values.
  !!
  !! Uses setup_common() from test_fixtures for logging initialization
  !! (needed by the error-suppression tests).

  use iso_c_binding, only: c_int, c_float, c_double, c_bool
  use testdrive, only: check, error_type, new_unittest, unittest_type
  use datetime, only: DATETIME_T, count_leap_days_between_dates, isLeap
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
      new_unittest("julian_illegal_month_and_day", test_julian_illegal_month_and_day), &
      ! --- Leap year detection ---
      new_unittest("leap_year_2000", test_leap_year_2000), &
      new_unittest("leap_year_1900_not_leap", test_leap_year_1900_not_leap), &
      new_unittest("leap_year_2024", test_leap_year_2024), &
      new_unittest("leap_year_2023_not_leap", test_leap_year_2023_not_leap), &
      ! --- Year boundary crossing ---
      new_unittest("addDay_dec31_to_jan1", test_addDay_dec31_to_jan1), &
      new_unittest("addDay_feb28_leap_year", test_addDay_feb28_leap_year), &
      new_unittest("addDay_feb28_nonleap_year", test_addDay_feb28_nonleap_year), &
      ! --- Day of year ---
      new_unittest("doy_jan1", test_doy_jan1), &
      new_unittest("doy_dec31_nonleap", test_doy_dec31_nonleap), &
      new_unittest("doy_dec31_leap", test_doy_dec31_leap), &
      ! --- Date comparisons ---
      new_unittest("compare_less_than", test_compare_less_than), &
      new_unittest("compare_greater_than", test_compare_greater_than), &
      new_unittest("compare_equal", test_compare_equal), &
      ! --- Date subtraction ---
      new_unittest("subtract_dates", test_subtract_dates) &
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

  !---------------------------------------------------------------------------
  ! LEAP YEAR DETECTION
  !---------------------------------------------------------------------------

  !> @brief 2000 is a leap year (divisible by 400).
  subroutine test_leap_year_2000(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, logical(isLeap(2000)), &
               "2000 should be a leap year (div by 400)")
  end subroutine test_leap_year_2000

  !> @brief 1900 is NOT a leap year (divisible by 100 but not 400).
  subroutine test_leap_year_1900_not_leap(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, .not. logical(isLeap(1900)), &
               "1900 should NOT be a leap year (div by 100 not 400)")
  end subroutine test_leap_year_1900_not_leap

  !> @brief 2024 is a leap year (divisible by 4, not by 100).
  subroutine test_leap_year_2024(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, logical(isLeap(2024)), &
               "2024 should be a leap year (div by 4)")
  end subroutine test_leap_year_2024

  !> @brief 2023 is NOT a leap year.
  subroutine test_leap_year_2023_not_leap(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, .not. logical(isLeap(2023)), &
               "2023 should NOT be a leap year")
  end subroutine test_leap_year_2023_not_leap

  !---------------------------------------------------------------------------
  ! YEAR BOUNDARY CROSSING
  !---------------------------------------------------------------------------

  !> @brief Adding a day to Dec 31 yields Jan 1 of the next year.
  subroutine test_addDay_dec31_to_jan1(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt

    call dt%calcJulianDay(iMonth=12, iDay=31, iYear=2023)
    call dt%addDay()

    call check(error, int(dt%iMonth) == 1, "Dec 31 + 1 day: expected month = 1")
    if (allocated(error)) return
    call check(error, int(dt%iDay) == 1, "Dec 31 + 1 day: expected day = 1")
    if (allocated(error)) return
    call check(error, int(dt%iYear) == 2024, "Dec 31 2023 + 1 day: expected year = 2024")
  end subroutine test_addDay_dec31_to_jan1

  !> @brief Feb 28 + 1 day in a leap year = Feb 29.
  subroutine test_addDay_feb28_leap_year(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt

    call dt%calcJulianDay(iMonth=2, iDay=28, iYear=2000)
    call dt%addDay()

    call check(error, int(dt%iMonth) == 2, "Feb 28 2000 + 1 day: expected month = 2")
    if (allocated(error)) return
    call check(error, int(dt%iDay) == 29, "Feb 28 2000 + 1 day: expected day = 29 (leap year)")
  end subroutine test_addDay_feb28_leap_year

  !> @brief Feb 28 + 1 day in a non-leap year = March 1.
  subroutine test_addDay_feb28_nonleap_year(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt

    call dt%calcJulianDay(iMonth=2, iDay=28, iYear=2023)
    call dt%addDay()

    call check(error, int(dt%iMonth) == 3, "Feb 28 2023 + 1 day: expected month = 3")
    if (allocated(error)) return
    call check(error, int(dt%iDay) == 1, "Feb 28 2023 + 1 day: expected day = 1 (non-leap)")
  end subroutine test_addDay_feb28_nonleap_year

  !---------------------------------------------------------------------------
  ! DAY OF YEAR
  !---------------------------------------------------------------------------

  !> @brief Jan 1 is day-of-year 1.
  subroutine test_doy_jan1(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt
    real(c_float) :: doy

    call dt%calcJulianDay(iMonth=1, iDay=1, iYear=2023)
    doy = dt%getDayOfYear()

    call check(error, abs(doy - 1.0) < 0.5, &
               "Jan 1 should be DOY 1")
  end subroutine test_doy_jan1

  !> @brief Dec 31 in a non-leap year is day-of-year 365.
  subroutine test_doy_dec31_nonleap(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt
    real(c_float) :: doy

    call dt%calcJulianDay(iMonth=12, iDay=31, iYear=2023)
    doy = dt%getDayOfYear()

    call check(error, abs(doy - 365.0) < 0.5, &
               "Dec 31 non-leap should be DOY 365")
  end subroutine test_doy_dec31_nonleap

  !> @brief Dec 31 in a leap year is day-of-year 366.
  subroutine test_doy_dec31_leap(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt
    real(c_float) :: doy

    call dt%calcJulianDay(iMonth=12, iDay=31, iYear=2000)
    doy = dt%getDayOfYear()

    call check(error, abs(doy - 366.0) < 0.5, &
               "Dec 31 leap year should be DOY 366")
  end subroutine test_doy_dec31_leap

  !---------------------------------------------------------------------------
  ! DATE COMPARISONS
  !---------------------------------------------------------------------------

  !> @brief Earlier date is less than later date.
  subroutine test_compare_less_than(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt1, dt2

    call dt1%calcJulianDay(iMonth=6, iDay=15, iYear=2020)
    call dt2%calcJulianDay(iMonth=7, iDay=1, iYear=2020)

    call check(error, dt1%dJulianDate < dt2%dJulianDate, &
               "June 15 should be less than July 1")
  end subroutine test_compare_less_than

  !> @brief Later date is greater than earlier date.
  subroutine test_compare_greater_than(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt1, dt2

    call dt1%calcJulianDay(iMonth=12, iDay=25, iYear=2020)
    call dt2%calcJulianDay(iMonth=1, iDay=1, iYear=2020)

    call check(error, dt1%dJulianDate > dt2%dJulianDate, &
               "Dec 25 should be greater than Jan 1")
  end subroutine test_compare_greater_than

  !> @brief Same date is equal.
  subroutine test_compare_equal(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt1, dt2

    call dt1%calcJulianDay(iMonth=3, iDay=14, iYear=2020)
    call dt2%calcJulianDay(iMonth=3, iDay=14, iYear=2020)

    call check(error, abs(dt1%dJulianDate - dt2%dJulianDate) < 0.5_c_double, &
               "Same date should have equal Julian dates")
  end subroutine test_compare_equal

  !---------------------------------------------------------------------------
  ! DATE SUBTRACTION
  !---------------------------------------------------------------------------

  !> @brief Subtracting two dates gives the correct number of days.
  subroutine test_subtract_dates(error)
    type(error_type), allocatable, intent(out) :: error
    type(DATETIME_T) :: dt1, dt2
    real(c_double) :: diff

    call dt1%calcJulianDay(iMonth=1, iDay=1, iYear=2020)
    call dt2%calcJulianDay(iMonth=1, iDay=31, iYear=2020)

    diff = dt2 - dt1

    call check(error, abs(diff - 30.0_c_double) < 0.5_c_double, &
               "Jan 31 - Jan 1 should be 30 days")
  end subroutine test_subtract_dates

end module test_datetime
