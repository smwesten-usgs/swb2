module simulation_datetime

  use iso_c_binding
  use datetime
  implicit none

  private

  type, public :: DATE_RANGE_T
    type (DATETIME_T)       :: start
    type (DATETIME_T)       :: end
    type (DATETIME_T)       :: curr
    integer (c_int)    :: iDOY
    integer (c_int)    :: iDaysInMonth
    integer (c_int)    :: iDaysInYear
    integer (c_int)    :: iYearOfSimulation = 1
    logical (c_bool)   :: lIsLeapYear
    integer (c_int)    :: iNumDaysFromOrigin = 0
    integer (c_int)    :: iDayOfSimulation = 0

  contains

    procedure :: initialize_datetimes_sub
    generic   :: initialize => initialize_datetimes_sub

    procedure :: days_from_origin_fn
    generic   :: days_from_origin => days_from_origin_fn

    procedure :: increment_by_one_day_sub
    generic   :: addDay => increment_by_one_day_sub

    procedure :: advance_curr_to_last_day_of_year_sub
    generic   :: advance_to_last_doy => advance_curr_to_last_day_of_year_sub

    procedure :: advance_curr_to_last_day_of_month_sub
    generic   :: advance_to_last_day_of_month => advance_curr_to_last_day_of_month_sub

    procedure :: set_curr_to_arbitrary_date_sub
    generic   :: set_current_date => set_curr_to_arbitrary_date_sub

    procedure :: percent_complete => percent_complete_fn

  end type DATE_RANGE_T

  type (DATE_RANGE_T), public :: SIM_DT

contains

  subroutine initialize_datetimes_sub( this, start_date, end_date )

    class (DATE_RANGE_T), intent(inout)      :: this
    type (DATETIME_T), intent(inout)         :: start_date
    type (DATETIME_T), intent(inout)         :: end_date

    this%start = start_date
    this%end = end_date
    this%curr = start_date

  end subroutine initialize_datetimes_sub

!------------------------------------------------------------------------------

  subroutine set_curr_to_arbitrary_date_sub(this, new_current_date)

    class (DATE_RANGE_T), intent(inout)   :: this
    type (DATETIME_T), intent(in)         :: new_current_date

    if (( new_current_date > this%end ) .or. ( new_current_date < this%start ))      &
      stop ( "Attempted to set current date to one outside of start and end date." )

    this%curr = new_current_date  
    this%iNumDaysFromOrigin = this%days_from_origin( new_current_date )
    this%iDaysInMonth = this%curr%dayspermonth()
    this%iDaysInYear = this%curr%daysperyear()
    this%lIsLeapYear = this%curr%isLeapYear()
    this%iDOY = day_of_year( this%curr%getJulianDay() )
    this%iDayOfSimulation = this%curr - this%start + 1

  end subroutine set_curr_to_arbitrary_date_sub

!------------------------------------------------------------------------------

  subroutine advance_curr_to_last_day_of_year_sub(this)

    class (DATE_RANGE_T), intent(inout)   :: this

    this%curr%iMonth = 12
    this%curr%iDay = 31

    call this%curr%calcJulianDay()

    this%iDaysInMonth = this%curr%dayspermonth()
    this%iDaysInYear = this%curr%daysperyear()
    this%lIsLeapYear = this%curr%isLeapYear()
    this%iDOY = day_of_year( this%curr%getJulianDay() )
    this%iYearOfSimulation = this%curr%iYear - this%start%iYear + 1
    this%iNumDaysFromOrigin = this%iNumDaysFromOrigin + 1
    this%iDayOfSimulation = this%curr - this%start + 1

  end subroutine advance_curr_to_last_day_of_year_sub

!------------------------------------------------------------------------------

  subroutine advance_curr_to_last_day_of_month_sub(this)

    class (DATE_RANGE_T), intent(inout)   :: this

    call this%curr%advanceLastDayOfMonth()

!    this%iDaysInMonth = this%curr%dayspermonth()
!    this%iDaysInYear = this%curr%daysperyear()
!    this%lIsLeapYear = this%curr%isLeapYear()
    this%iDOY = day_of_year( this%curr%getJulianDay() )
    this%iYearOfSimulation = this%curr%iYear - this%start%iYear + 1
    this%iNumDaysFromOrigin = this%iNumDaysFromOrigin + 1
    this%iDayOfSimulation = this%curr - this%start + 1

  end subroutine advance_curr_to_last_day_of_month_sub

!------------------------------------------------------------------------------

  function days_from_origin_fn(this, datetime )    result( num_days_from_origin )

    class (DATE_RANGE_T), intent(inout)   :: this
    type (DATETIME_T), intent(in)         :: datetime
    real (c_double)                  :: num_days_from_origin

    num_days_from_origin = real( datetime - this%start, c_double)

  end function days_from_origin_fn

  function percent_complete_fn( this )   result( percent_complete )

    class (DATE_RANGE_T), intent(inout)   :: this
    real (c_float)                   :: percent_complete

    percent_complete = real(this%curr - this%start) / real( this%end - this%start ) * 100.

  end function percent_complete_fn

  !------------------------------------------------------------------------------

  subroutine increment_by_one_day_sub(this)

    class (DATE_RANGE_T), intent(inout)   :: this

    call this%curr%addDay()

    this%iDaysInMonth = this%curr%dayspermonth()
    this%iDaysInYear = this%curr%daysperyear()
    this%lIsLeapYear = this%curr%isLeapYear()
    this%iDOY = day_of_year( this%curr%getJulianDay() )
    this%iYearOfSimulation = this%curr%iYear - this%start%iYear + 1
    this%iNumDaysFromOrigin = this%iNumDaysFromOrigin + 1
    this%iDayOfSimulation = this%curr - this%start + 1

  end subroutine increment_by_one_day_sub

end module simulation_datetime
