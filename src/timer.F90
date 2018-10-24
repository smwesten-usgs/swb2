module timer

  use strings, only   : asCharacter
  use iso_c_binding
  implicit none

  type TIMER_T

    real(c_double)   :: starttime
    real(c_double)   :: stoptime
    real (c_double)  :: elapsedtime = 0.0_c_double
    real (c_double)  :: splittime = 0.0_c_double

    integer (c_int)  :: seconds
    integer (c_int)  :: minutes
    integer (c_int)  :: hours
    integer (c_int)  :: days

  contains
    generic       :: reset => reset_timer_sub
    procedure     :: reset_timer_sub
    generic       :: start => start_timer_sub
    procedure     :: start_timer_sub
    generic       :: stop => stop_timer_sub
    procedure     :: stop_timer_sub

    generic       :: calc_elapsed => calc_elapsed_time_sub
    procedure     :: calc_elapsed_time_sub

    generic       :: calc_split => calc_split_time_sub
    procedure     :: calc_split_time_sub

    generic       :: get_elapsed => get_elapsed_seconds_fn
    procedure     :: get_elapsed_seconds_fn

    generic       :: get_split => get_split_seconds_fn
    procedure     :: get_split_seconds_fn

    generic       :: calc_time_values => calc_time_values_sub
    procedure     :: calc_time_values_sub

    generic       :: get_pretty => get_timer_pretty_fn
    procedure     :: get_timer_pretty_fn

  end type TIMER_T


contains

  subroutine start_timer_sub(this)

    class (TIMER_T) :: this

    call cpu_time(this%starttime)

  end subroutine start_timer_sub

!------------------------------------------------------------------------------

  subroutine stop_timer_sub(this)

    class (TIMER_T) :: this

    call cpu_time(this%stoptime)
    this%splittime = this%stoptime - this%starttime
    this%elapsedtime = this%elapsedtime + (this%stoptime - this%starttime)

  end subroutine stop_timer_sub

!------------------------------------------------------------------------------

  subroutine calc_elapsed_time_sub(this)

    class (TIMER_T) :: this

    this%elapsedtime = this%elapsedtime + (this%stoptime - this%starttime)
    call this%calc_time_values("elapsed")

  end subroutine calc_elapsed_time_sub

!------------------------------------------------------------------------------

  subroutine calc_split_time_sub(this)

    class (TIMER_T) :: this

    this%splittime = this%stoptime - this%starttime
    call this%calc_time_values("split")

end subroutine calc_split_time_sub

!------------------------------------------------------------------------------

  subroutine reset_timer_sub(this)

    class (TIMER_T) :: this

    this%elapsedtime = 0.0_c_double

  end subroutine reset_timer_sub

!------------------------------------------------------------------------------

  function get_elapsed_seconds_fn(this)    result(elapsed_seconds)

    class (TIMER_T) :: this
    integer (c_int) :: elapsed_seconds

    elapsed_seconds = int(this%elapsedtime, kind=c_int)

end function get_elapsed_seconds_fn

!------------------------------------------------------------------------------

function get_split_seconds_fn(this)    result(split_seconds)

  class (TIMER_T) :: this
  integer (c_int) :: split_seconds

  split_seconds = int(this%splittime, kind=c_int)

end function get_split_seconds_fn

!------------------------------------------------------------------------------

  subroutine calc_time_values_sub(this, timer_name)

    class (TIMER_T)                :: this
    character (len=*), intent(in)  :: timer_name

    ! [ LOCALS ]
    real (c_double) :: remainder

    select case(timer_name)
      case("split")
        remainder = this%splittime
      case("elapsed")
        remainder = this%elapsedtime
      case default
        remainder = this%elapsedtime
    end select

    this%days    = -9999
    this%hours   = -9999
    this%minutes = -9999
    this%seconds = -9999

    if ( int(remainder) >= 86400 ) then
      this%days = int( remainder / 86400.0_c_double )
      remainder = modulo(remainder, 86400.0_c_double)
    endif

    if ( int(remainder) >= 3600) then
      this%hours = int( remainder / 3600.0_c_double )
      remainder = modulo(remainder, 3600.0_c_double)
    endif

    if ( int(remainder) >= 60) then
      this%minutes = int( remainder / 60.0_c_double )
      remainder = modulo(remainder, 60.0_c_double)
    endif

    this%seconds = int( remainder )

  end subroutine calc_time_values_sub

!------------------------------------------------------------------------------

  function get_timer_pretty_fn(this)     result(time_txt)

    class (TIMER_T)                 :: this
    character (len=:), allocatable  :: time_txt

    ! [ LOCALS ]
    character (len=256) :: str

    str = ""

    if ( this%days >= 0 )    str = asCharacter(this%days)//" days,"
    if ( this%hours >= 0 )   str = trim(str)//" "//asCharacter(this%hours)//" hours,"
    if ( this%minutes >= 0 ) str = trim(str)//" "//asCharacter(this%minutes)//" minutes,"
    str = trim(str)//" "//asCharacter(this%seconds)//" seconds."

    time_txt = trim(adjustl(str))

  end function get_timer_pretty_fn

end module timer
