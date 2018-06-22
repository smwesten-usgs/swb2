module model_iterate_multiple_simulations

  use iso_c_binding, only             : c_bool, c_float, c_int, c_double
  use constants_and_conversions, only : lTRUE, BNDS, TRUE
  use daily_calculation, only         : perform_daily_calculation
  use file_operations, only           : ASCII_FILE_T
  use logfiles, only                  : LOGS, LOG_ALL
  use model_domain, only              : MODEL_DOMAIN_T
  use simulation_datetime, only       : SIM_DT
  use output, only                    : write_output,                          &
                                        initialize_output,                     &
                                        initialize_multiple_sim_output,        &
                                        write_multi_sim_output,                &
                                        OUTSPECS, NCDF_NET_INFILTRATION,       &
                                        NCDF_ACTUAL_ET

!  use summary_statistics, only        : perform_polygon_summarize
  use grid
  implicit none

  private

  public :: iterate_over_multiple_simulation_days

  real (kind=c_double), target, allocatable   :: SOIL_MOISTURE_STORAGE_PER_SIM(:,:)
  real (kind=c_float), target, allocatable    :: SNOW_STORAGE_PER_SIM(:,:)
  real (kind=c_double), target, allocatable   :: SURFACE_STORAGE_PER_SIM(:,:)
  real (kind=c_float), target, allocatable    :: INTERCEPTION_STORAGE_PER_SIM(:,:)
  real (kind=c_float), target, allocatable    :: NET_INFILTRATION_PER_SIM(:,:)
  real (kind=c_double), target, allocatable   :: ACTUAL_ET_PER_SIM(:,:)



! Concept: remap pointers to key state variables for each of X simulations,
!          run the daily simulation given current landuse, soil type, etc.
!          update rainfall by means of method of fragments
!
contains

  subroutine allocate_space_for_simulation_storage_state_variables(cells, number_of_simulations)

    class (MODEL_DOMAIN_T), intent(inout)     :: cells
    integer (kind=c_int), intent(in)          :: number_of_simulations

    ! [ LOCALS ]
    integer (kind=c_int) :: length
    integer (kind=c_int) :: indx

    length = size( cells%soil_storage, 1)

    allocate( SOIL_MOISTURE_STORAGE_PER_SIM(length, number_of_simulations))
    allocate( SNOW_STORAGE_PER_SIM(length, number_of_simulations))
    allocate( SURFACE_STORAGE_PER_SIM(length, number_of_simulations))
    allocate( INTERCEPTION_STORAGE_PER_SIM(length, number_of_simulations))
    allocate( NET_INFILTRATION_PER_SIM(length, number_of_simulations))
    allocate( ACTUAL_ET_PER_SIM(length, number_of_simulations))

    ! don't want or need this memory since we're going to swap out a
    ! different set of arrays for each simulation number
    if (associated(cells%snow_storage)) then
      do indx=1,number_of_simulations
        SNOW_STORAGE_PER_SIM(:,indx) = cells%snow_storage
      enddo
      deallocate(cells%snow_storage)
    endif

    if (associated(cells%soil_storage)) then
      do indx=1,number_of_simulations
        SOIL_MOISTURE_STORAGE_PER_SIM(:,indx) = cells%soil_storage
      enddo
      deallocate(cells%soil_storage)
    endif

    if (associated(cells%surface_storage)) then
      do indx=1,number_of_simulations
        SURFACE_STORAGE_PER_SIM(:,indx) = cells%surface_storage
      enddo
      deallocate(cells%surface_storage)
    endif

    if (associated(cells%interception_storage)) then
      do indx=1,number_of_simulations
        INTERCEPTION_STORAGE_PER_SIM(:,indx) = cells%interception_storage
      enddo
      deallocate(cells%interception_storage)
    endif

    if (associated(cells%net_infiltration)) then
      do indx=1,number_of_simulations
        NET_INFILTRATION_PER_SIM(:,indx) = cells%net_infiltration
      enddo
      deallocate(cells%net_infiltration)
    endif

    if (associated(cells%actual_et)) then
      do indx=1,number_of_simulations
        ACTUAL_ET_PER_SIM(:,indx) = cells%actual_et
      enddo
      deallocate(cells%actual_et)
    endif

  end subroutine allocate_space_for_simulation_storage_state_variables

!------------------------------------------------------------------------------------------------

  subroutine iterate_over_multiple_simulation_days(cells, number_of_simulations)

    use precipitation__method_of_fragments, only  : SIMULATION_NUMBER

    class (MODEL_DOMAIN_T), intent(inout)     :: cells
    integer (kind=c_int), intent(inout)       :: number_of_simulations

    ! [ LOCALS ]
    integer (kind=c_int) :: sim_number

    ! set flags in output mod to allow multiple simulation output to be generated
    OUTSPECS(NCDF_NET_INFILTRATION)%multisim_outputs = TRUE
    OUTSPECS(NCDF_ACTUAL_ET)%multisim_outputs = TRUE

    call allocate_space_for_simulation_storage_state_variables(cells, number_of_simulations)

    ! open and prepare NetCDF files for output
    call initialize_output( cells )
    call initialize_multiple_sim_output(cells, number_of_simulations)

    do while ( SIM_DT%curr <= SIM_DT%end )

      call LOGS%write("Calculating: "//SIM_DT%curr%prettydate(), iLogLevel=LOG_ALL, lEcho=.true._c_bool )

      call cells%update_landuse_codes()

      write(*,fmt="(a)", advance="no") "  running multiple simulations: "

      do sim_number=1,number_of_simulations

        SIMULATION_NUMBER = sim_number

        write(*,fmt="(a)", advance="no") "."

        call cells%get_climate_data( )

        cells%snow_storage => SNOW_STORAGE_PER_SIM(:,sim_number)
        cells%soil_storage => SOIL_MOISTURE_STORAGE_PER_SIM(:,sim_number)
        cells%interception_storage => INTERCEPTION_STORAGE_PER_SIM(:,sim_number)
        cells%surface_storage => SURFACE_STORAGE_PER_SIM(:,sim_number)
        cells%net_infiltration => NET_INFILTRATION_PER_SIM(:,sim_number)
        cells%actual_et => ACTUAL_ET_PER_SIM(:,sim_number)

        call perform_daily_calculation( cells )
        call write_output( cells )
        call write_multi_sim_output( cells, sim_number)
  !      call perform_polygon_summarize( cells )

        call cells%dump_variables( )

      enddo

      write(*,fmt="(/)")

      call SIM_DT%addDay( )

    enddo

  end subroutine iterate_over_multiple_simulation_days

end module model_iterate_multiple_simulations
