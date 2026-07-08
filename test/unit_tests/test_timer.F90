module test_timer
  !! Unit tests for the TIMER_T type.
  !!
  !! Tests basic elapsed time calculation, split timing, and
  !! human-readable pretty-print output. No environment setup needed —
  !! timer is pure local computation.

  use iso_c_binding, only: c_double, c_int
  use testdrive, only: check, error_type, new_unittest, unittest_type
  use timer, only: TIMER_T
  implicit none
  private

  public :: collect_timer

contains

  !---------------------------------------------------------------------------
  !> @brief Register all timer tests into the test suite array.
  !---------------------------------------------------------------------------
  subroutine collect_timer(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      new_unittest("elapsed_minutes_seconds", test_elapsed_minutes_seconds), &
      new_unittest("elapsed_hours_not_set", test_elapsed_hours_not_set), &
      new_unittest("split_then_elapsed", test_split_then_elapsed), &
      new_unittest("split_hours_minutes_seconds", test_split_hours_minutes_seconds), &
      new_unittest("cumulative_elapsed", test_cumulative_elapsed), &
      new_unittest("prettyprint_days", test_prettyprint_days), &
      new_unittest("prettyprint_string", test_prettyprint_string) &
    ]
  end subroutine collect_timer

  !---------------------------------------------------------------------------
  !> @brief 3 minutes + 5 seconds yields correct minutes and seconds.
  !---------------------------------------------------------------------------
  subroutine test_elapsed_minutes_seconds(error)
    type(error_type), allocatable, intent(out) :: error
    type(TIMER_T) :: tmr

    tmr%starttime = 0.0_c_double
    tmr%stoptime = tmr%starttime + 60.0_c_double * 3.0_c_double + 5.0_c_double
    call tmr%calc_elapsed()

    call check(error, tmr%minutes == 3, "expected 3 minutes")
    if (allocated(error)) return
    call check(error, tmr%seconds == 5, "expected 5 seconds")
  end subroutine test_elapsed_minutes_seconds

  !---------------------------------------------------------------------------
  !> @brief When elapsed time < 1 hour, hours and days remain at -9999.
  !---------------------------------------------------------------------------
  subroutine test_elapsed_hours_not_set(error)
    type(error_type), allocatable, intent(out) :: error
    type(TIMER_T) :: tmr

    tmr%starttime = 0.0_c_double
    tmr%stoptime = tmr%starttime + 60.0_c_double * 3.0_c_double + 5.0_c_double
    call tmr%calc_elapsed()

    call check(error, tmr%hours == -9999, "hours should be -9999 when < 1 hour")
    if (allocated(error)) return
    call check(error, tmr%days == -9999, "days should be -9999 when < 1 day")
  end subroutine test_elapsed_hours_not_set

  !---------------------------------------------------------------------------
  !> @brief calc_split followed by calc_elapsed gives consistent results.
  !---------------------------------------------------------------------------
  subroutine test_split_then_elapsed(error)
    type(error_type), allocatable, intent(out) :: error
    type(TIMER_T) :: tmr

    tmr%starttime = 0.0_c_double
    tmr%stoptime = tmr%starttime + 60.0_c_double * 3.0_c_double + 5.0_c_double
    call tmr%calc_split()
    call tmr%calc_elapsed()

    call check(error, tmr%seconds == 5, "expected 5 seconds after split+elapsed")
    if (allocated(error)) return
    call check(error, tmr%minutes == 3, "expected 3 minutes after split+elapsed")
  end subroutine test_split_then_elapsed

  !---------------------------------------------------------------------------
  !> @brief Split of 4h 6m 42s decomposes correctly.
  !---------------------------------------------------------------------------
  subroutine test_split_hours_minutes_seconds(error)
    type(error_type), allocatable, intent(out) :: error
    type(TIMER_T) :: tmr

    tmr%starttime = 0.0_c_double
    ! First split to establish the baseline
    tmr%stoptime = tmr%starttime + 60.0_c_double * 3.0_c_double + 5.0_c_double
    call tmr%calc_split()

    ! Second split: 4 hours, 6 minutes, 42 seconds from start
    tmr%stoptime = tmr%starttime + 3600.0_c_double * 4.0_c_double &
                   + 60.0_c_double * 6.0_c_double + 42.0_c_double
    call tmr%calc_split()

    call check(error, tmr%seconds == 42, "expected 42 seconds in split")
    if (allocated(error)) return
    call check(error, tmr%minutes == 6, "expected 6 minutes in split")
    if (allocated(error)) return
    call check(error, tmr%hours == 4, "expected 4 hours in split")
    if (allocated(error)) return
    call check(error, tmr%days == -9999, "days should be -9999 when < 1 day")
  end subroutine test_split_hours_minutes_seconds

  !---------------------------------------------------------------------------
  !> @brief Cumulative elapsed time after two intervals sums correctly.
  !---------------------------------------------------------------------------
  subroutine test_cumulative_elapsed(error)
    type(error_type), allocatable, intent(out) :: error
    type(TIMER_T) :: tmr

    tmr%starttime = 0.0_c_double

    ! First interval: 3m 5s — accumulate into elapsed
    tmr%stoptime = tmr%starttime + 60.0_c_double * 3.0_c_double + 5.0_c_double
    call tmr%calc_split()
    call tmr%calc_elapsed()

    ! Second interval: 4h 6m 42s from start — accumulate into elapsed
    tmr%stoptime = tmr%starttime + 3600.0_c_double * 4.0_c_double &
                   + 60.0_c_double * 6.0_c_double + 42.0_c_double
    call tmr%calc_split()

    ! Elapsed should be total: (185 + 14802) = 14987s = 4h 9m 47s
    call tmr%calc_elapsed()

    call check(error, tmr%seconds == 47, "expected 47 seconds cumulative")
    if (allocated(error)) return
    call check(error, tmr%minutes == 9, "expected 9 minutes cumulative")
    if (allocated(error)) return
    call check(error, tmr%hours == 4, "expected 4 hours cumulative")
    if (allocated(error)) return
    call check(error, tmr%days == -9999, "days should be -9999")
  end subroutine test_cumulative_elapsed

  !---------------------------------------------------------------------------
  !> @brief 3 days + 4 hours + 6 minutes + 42 seconds decomposes correctly.
  !---------------------------------------------------------------------------
  subroutine test_prettyprint_days(error)
    type(error_type), allocatable, intent(out) :: error
    type(TIMER_T) :: tmr

    tmr%starttime = 0.0_c_double
    call tmr%reset()

    tmr%stoptime = tmr%starttime             &
                   + (86400.0_c_double * 3.0_c_double)  &
                   + (3600.0_c_double * 4.0_c_double)   &
                   + (60.0_c_double * 6.0_c_double)     &
                   + 42.0_c_double
    call tmr%calc_elapsed()

    call check(error, tmr%days == 3, "expected 3 days")
    if (allocated(error)) return
    call check(error, tmr%hours == 4, "expected 4 hours")
    if (allocated(error)) return
    call check(error, tmr%minutes == 6, "expected 6 minutes")
    if (allocated(error)) return
    call check(error, tmr%seconds == 42, "expected 42 seconds")
  end subroutine test_prettyprint_days

  !---------------------------------------------------------------------------
  !> @brief get_pretty() returns correctly formatted human-readable string.
  !---------------------------------------------------------------------------
  subroutine test_prettyprint_string(error)
    type(error_type), allocatable, intent(out) :: error
    type(TIMER_T) :: tmr

    tmr%starttime = 0.0_c_double
    call tmr%reset()

    tmr%stoptime = tmr%starttime             &
                   + (86400.0_c_double * 3.0_c_double)  &
                   + (3600.0_c_double * 4.0_c_double)   &
                   + (60.0_c_double * 6.0_c_double)     &
                   + 42.0_c_double
    call tmr%calc_elapsed()

    call check(error, tmr%get_pretty() == "3 days, 4 hours, 6 minutes, 42 seconds.", &
               "pretty-print mismatch")
  end subroutine test_prettyprint_string

end module test_timer
