module model_iterate

  use iso_c_binding, only             : c_bool, c_float, c_int
  use constants_and_conversions, only : TRUE, BNDS
  use daily_calculation, only         : perform_daily_calculation
  use file_operations, only           : ASCII_FILE_T
  use logfiles, only                  : LOGS, LOG_ALL
  use model_domain, only              : MODEL_DOMAIN_T
  use simulation_datetime, only       : SIM_DT
  use output, only                    : write_output, initialize_output, finalize_output
!  use summary_statistics, only        : perform_polygon_summarize
  use grid
  implicit none

  private

  public :: iterate_over_simulation_days

contains

  subroutine iterate_over_simulation_days(cells)

    class (MODEL_DOMAIN_T), intent(inout)  :: cells

    type (GENERAL_GRID_T), pointer  :: pTempGrid


    ! open and prepare NetCDF files for output
    ! (this statement moved here so that files are opened and initialized
    ! after all other routines have initialized; this was needed in order to
    ! make the multiple simulation outputs work)
    call initialize_output( cells )

    do while ( SIM_DT%curr <= SIM_DT%end )

      call LOGS%write("Calculating: "//SIM_DT%curr%prettydate(), iLogLevel=LOG_ALL, lEcho=.true._c_bool )
      call cells%update_landuse_codes()
      call cells%update_irrigation_mask()

      call cells%get_weather_data( )
      call perform_daily_calculation( cells )
      call write_output( cells )
!      call perform_polygon_summarize( cells )

      call cells%dump_variables( )

      call SIM_DT%addDay( )

    enddo

    call finalize_output( cells )

  end subroutine iterate_over_simulation_days

end module model_iterate
