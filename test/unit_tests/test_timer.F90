module test_timer

  use fruit
  use timer
  implicit none

contains

  subroutine test_timer_basic
  ! timer: test basic functionality - convert seconds to hms
    type (TIMER_T) :: tmr

    tmr%starttime = 0.0_c_double

    ! add 3 minutes, 5 seconds to timer
    tmr%stoptime = tmr%starttime + 60.*3. + 5.
    call tmr%calc_elapsed()
    call assert_equals (3, tmr%minutes)
    call assert_equals (5, tmr%seconds)
    call assert_equals (-9999, tmr%hours)
    call assert_equals (-9999, tmr%days)

  end subroutine test_timer_basic

  subroutine test_timer_split
  ! timer: test split and elapsed timer functionality
    type (TIMER_T) :: tmr

    tmr%starttime = 0.0_c_double

    ! add 3 minutes, 5 seconds to timer
    tmr%stoptime = tmr%starttime + 60.*3. + 5.
    call tmr%calc_split()
    call tmr%calc_elapsed()
    call assert_equals (5, tmr%seconds)
    call assert_equals (3, tmr%minutes)
    call assert_equals (-9999, tmr%hours)
    call assert_equals (-9999, tmr%days)

    ! add 4 hours, 6 minutes, 42 seconds to timer
    tmr%stoptime = tmr%starttime + 3600. * 4 + 60.*6. + 42.
    call tmr%calc_split()
    call assert_equals (42, tmr%seconds)
    call assert_equals (6, tmr%minutes)
    call assert_equals (4, tmr%hours)
    call assert_equals (-9999, tmr%days)

    ! now check the total elapsed time: 4 hours, 9 minutes, 47 seconds
    call tmr%calc_elapsed()
    call assert_equals (47, tmr%seconds)
    call assert_equals (9, tmr%minutes)
    call assert_equals (4, tmr%hours)
    call assert_equals (-9999, tmr%days)

  end subroutine test_timer_split

  subroutine test_timer_prettyprint
  ! timer: test ability to output in human-readable form
    type (TIMER_T) :: tmr

    tmr%starttime = 0.0_c_double

    call tmr%reset()

    ! add 3 days, 4 hours, 6 minutes, and 42 seconds to timer
    tmr%stoptime = tmr%starttime           &
                   + (86400. * 3.)         &
                   + (3600. * 4.)          &
                   + (60. * 6.)            &
                   + 42.

    call tmr%calc_elapsed()
    call assert_equals (3, tmr%days)
    call assert_equals (4, tmr%hours)
    call assert_equals (6, tmr%minutes)
    call assert_equals (42, tmr%seconds)

    call assert_equals ("3 days, 4 hours, 6 minutes, 42 seconds.", &
                        tmr%get_pretty())

  end subroutine test_timer_prettyprint

end module test_timer
