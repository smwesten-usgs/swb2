module loop_iterate

  use iso_c_binding
  use constants_and_conversions
  use cell_collection
  use data_catalog_entry
  use datetime
  use simulation_datetime, only : SIM_DT
  use logfiles
  implicit none

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

      CELLS%cell%fGrossPrecip = PRCP%pGrdBase%rData
      CELLS%cell%fTMax = TMAX%pGrdBase%rData
      CELLS%cell%fTMin = TMIN%pGrdBase%rData

    end associate

  end subroutine get_required_climate_data


end module loop_iterate