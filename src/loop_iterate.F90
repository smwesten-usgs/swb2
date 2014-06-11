module loop_iterate

  use iso_c_binding
  use constants_and_conversions
  use cell_collection
  use data_catalog_entry
  use datetime
  use logfiles
  implicit none


  type, public :: DATE_RANGE_T
    type (DATETIME_T)       :: start
    type (DATETIME_T)       :: end
    type (DATETIME_T)       :: curr
    integer (kind=c_int)    :: iDOY
    integer (kind=c_int)    :: iDaysInMonth
    integer (kind=c_int)    :: iDaysInYear
    logical (kind=c_bool)   :: lIsLeapYear

  contains

    procedure :: increment_by_one_day_sub
    generic   :: addDay => increment_by_one_day_sub

  end type DATE_RANGE_T

  type (DATE_RANGE_T), public :: SIM_DT

contains

  !! need to iterate over the model domain and over a set time domain

  subroutine iterate_over_simulation_days()

    do while ( SIM_DT%curr <= SIM_DT%end )

      call LOGS%write("Calculating: "//SIM_DT%curr%prettydate(), iLogLevel=LOG_ALL, lEcho=.true._c_bool )
      call get_required_climate_data()
      call CELLS%solve()
      call SIM_DT%addDay()

    enddo 

  end subroutine iterate_over_simulation_days



  subroutine get_required_climate_data()

    ! [ LOCALS ]
    integer (kind=c_int) :: iJulianDay
    integer (kind=c_int) ::iMonth
    integer (kind=c_int) ::iDay
    integer (kind=c_int) ::iYear


    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear


      call PRCP%getvalues( iMonth, iDay, iYear, iJulianDay )
      call TMIN%getvalues( iMonth, iDay, iYear, iJulianDay )
      call TMAX%getvalues( iMonth, iDay, iYear, iJulianDay )

    end associate

  end subroutine get_required_climate_data



  subroutine increment_by_one_day_sub(this)

    class (DATE_RANGE_T), intent(inout)   :: this

    call this%curr%addDay()

    this%iDaysInMonth = this%curr%dayspermonth()
    this%iDaysInYear = this%curr%daysperyear()
    this%lIsLeapYear = this%curr%isLeapYear()
    this%iDOY = day_of_year( this%curr%getJulianDay() )
    SIM_DT%iDOY = this%iDOY

  end subroutine increment_by_one_day_sub

end module loop_iterate