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
    logical (kind=c_bool)   :: lIsLeapYear
    integer (kind=c_int)    :: iNumDaysFromOrigin = 0

  contains

    procedure :: increment_by_one_day_sub
    generic   :: addDay => increment_by_one_day_sub

  end type DATE_RANGE_T

  type (DATE_RANGE_T), public :: SIM_DT

contains


  subroutine increment_by_one_day_sub(this)

    class (DATE_RANGE_T), intent(inout)   :: this

    call this%curr%addDay()

    this%iDaysInMonth = this%curr%dayspermonth()
    this%iDaysInYear = this%curr%daysperyear()
    this%lIsLeapYear = this%curr%isLeapYear()
    this%iDOY = day_of_year( this%curr%getJulianDay() )
    this%iNumDaysFromOrigin = this%iNumDaysFromOrigin + 1

  end subroutine increment_by_one_day_sub


end module simulation_datetime