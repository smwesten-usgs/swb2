module model_iterate

  use iso_c_binding, only         : c_bool
  use daily_calculation, only     : perform_daily_calculation
  use logfiles, only              : LOGS, LOG_ALL
  use model_domain, only          : MODEL_DOMAIN_T
  use simulation_datetime, only   : SIM_DT
  use string_list, only           : STRING_LIST_T
  use parameters, only            : PARAMS, PARAMS_DICT 
  use netcdf4_support, only       : NC_FILL_FLOAT
  use output, only                : write_output
  implicit none

  private

  public :: iterate_over_simulation_days

contains

  subroutine iterate_over_simulation_days(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    do while ( SIM_DT%curr <= SIM_DT%end )

      call LOGS%write("Calculating: "//SIM_DT%curr%prettydate(), iLogLevel=LOG_ALL, lEcho=.true._c_bool )

      call this%get_climate_data()
      call perform_daily_calculation(this)
      call write_output(this)
      call SIM_DT%addDay()

    enddo 

  end subroutine iterate_over_simulation_days

end module model_iterate