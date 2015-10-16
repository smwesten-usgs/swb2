module model_iterate

  use iso_c_binding, only             : c_bool, c_float, c_int
  use constants_and_conversions, only : lTRUE
  use daily_calculation, only         : perform_daily_calculation
  use file_operations, only           : ASCII_FILE_T
  use logfiles, only                  : LOGS, LOG_ALL
  use model_domain, only              : MODEL_DOMAIN_T
  use simulation_datetime, only       : SIM_DT
  use string_list, only               : STRING_LIST_T
  use strings, only                   : asCharacter
  use parameters, only                : PARAMS, PARAMS_DICT 
  use netcdf4_support, only           : NC_FILL_FLOAT
  use output, only                    : write_output, OUTPUT_DIRECTORY_NAME
  use polygon_summarize, only         : perform_polygon_summarize
  implicit none

  private

  public :: iterate_over_simulation_days

  type ( ASCII_FILE_T ) :: PROGRESS_FILE

contains

  subroutine iterate_over_simulation_days(cells)

    class (MODEL_DOMAIN_T), intent(inout)  :: cells

    ! [ LOCALS ]
    character (len=64)    :: progress_txt
    real (kind=c_float)   :: progress
    integer (kind=c_int)  :: progress_int

    call PROGRESS_FILE%open( sFilename=trim( OUTPUT_DIRECTORY_NAME )//"run_progress.txt", lQuiet=lTRUE )

    do while ( SIM_DT%curr <= SIM_DT%end )

      call LOGS%write("Calculating: "//SIM_DT%curr%prettydate(), iLogLevel=LOG_ALL, lEcho=.true._c_bool )

      call cells%get_climate_data( )
      call perform_daily_calculation( cells )
      call write_output( cells )
      call perform_polygon_summarize( cells )

      progress = SIM_DT%percent_complete()
      progress_int = int( progress / 2.0 )

      progress_txt = "|"//repeat("-", progress_int)//repeat(" ", 50 - progress_int)//"| " &
        //asCharacter( int(progress) )//"%"
      call PROGRESS_FILE%open( sFilename=trim( OUTPUT_DIRECTORY_NAME )//"run_progress.txt", lQuiet=lTRUE )
      call PROGRESS_FILE%writeLine( sText= progress_txt )
      call PROGRESS_FILE%close()
      
      call SIM_DT%addDay( )

    enddo 

    progress_txt = "Done."
    call PROGRESS_FILE%open( sFIlename=trim( OUTPUT_DIRECTORY_NAME )//"run_progress.txt", lQuiet=lTRUE )
    call PROGRESS_FILE%writeLine( sText= progress_txt )
    call PROGRESS_FILE%close()



  end subroutine iterate_over_simulation_days

end module model_iterate