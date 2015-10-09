module model_iterate

  use iso_c_binding, only         : c_bool
  use daily_calculation, only     : perform_daily_calculation
  use file_operations, only       : ASCII_FILE_T
  use logfiles, only              : LOGS, LOG_ALL
  use model_domain, only          : MODEL_DOMAIN_T
  use simulation_datetime, only   : SIM_DT
  use string_list, only           : STRING_LIST_T
  use strings, only               : asCharacter
  use parameters, only            : PARAMS, PARAMS_DICT 
  use netcdf4_support, only       : NC_FILL_FLOAT
  use output, only                : write_output, OUTPUT_DIRECTORY_NAME
  use polygon_summarize, only     : perform_polygon_summarize
  implicit none

  private

  public :: iterate_over_simulation_days

  type ( ASCII_FILE_T ) :: PROGRESS_FILE

contains

  subroutine iterate_over_simulation_days(cells)

    class (MODEL_DOMAIN_T), intent(inout)  :: cells

    ! [ LOCALS ]
    character (len=8) :: progress_txt

    call PROGRESS_FILE%open( trim( OUTPUT_DIRECTORY_NAME )//"run_progress.txt" )

    do while ( SIM_DT%curr <= SIM_DT%end )

      call LOGS%write("Calculating: "//SIM_DT%curr%prettydate(), iLogLevel=LOG_ALL, lEcho=.true._c_bool )

      call cells%get_climate_data( )
      call perform_daily_calculation( cells )
      call write_output( cells )
      call perform_polygon_summarize( cells )

      progress_txt = asCharacter( SIM_DT%percent_complete() )
      call PROGRESS_FILE%writeLine( sText= progress_txt, lAdvance=.false._c_bool )
      call SIM_DT%addDay( )

    enddo 

  end subroutine iterate_over_simulation_days

end module model_iterate