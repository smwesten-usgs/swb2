module simulation_datetime

  use iso_c_binding
  use datetime
  implicit none

  private

  type, public :: DATE_RANGE_T
    type (DATETIME_T)       :: start
    type (DATETIME_T)       :: end
    type (DATETIME_T)       :: curr
    integer (kind=c_int)    :: iDOY
    integer (kind=c_int)    :: iDaysInMonth
    integer (kind=c_int)    :: iDaysInYear
    integer (kind=c_int)    :: iYearOfSimulation = 1
    logical (kind=c_bool)   :: lIsLeapYear
    integer (kind=c_int)    :: iNumDaysFromOrigin = 0

  contains

    procedure :: increment_by_one_day_sub
    generic   :: addDay => increment_by_one_day_sub

    procedure :: percent_complete => percent_complete_fn

  end type DATE_RANGE_T

  type (DATE_RANGE_T), public :: SIM_DT

contains

  function percent_complete_fn( this )   result( percent_complete )

    class (DATE_RANGE_T), intent(inout)   :: this    
    real (kind=c_float)                   :: percent_complete

    percent_complete = real(this%curr - this%start) / real( this%end - this%start + 1) * 100.

  end function percent_complete_fn


  subroutine increment_by_one_day_sub(this)

    class (DATE_RANGE_T), intent(inout)   :: this

    call this%curr%addDay()

    this%iDaysInMonth = this%curr%dayspermonth()
    this%iDaysInYear = this%curr%daysperyear()
    this%lIsLeapYear = this%curr%isLeapYear()
    this%iDOY = day_of_year( this%curr%getJulianDay() )
    this%iYearOfSimulation = this%curr%iYear - this%start%iYear + 1
    this%iNumDaysFromOrigin = this%iNumDaysFromOrigin + 1

  end subroutine increment_by_one_day_sub


end module simulation_datetime