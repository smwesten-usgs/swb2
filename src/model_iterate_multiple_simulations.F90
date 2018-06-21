module model_iterate_multiple_simulations

  use iso_c_binding, only             : c_bool, c_float, c_int, c_double
  use constants_and_conversions, only : lTRUE, BNDS
  use daily_calculation, only         : perform_daily_calculation
  use file_operations, only           : ASCII_FILE_T
  use logfiles, only                  : LOGS, LOG_ALL
  use model_domain, only              : MODEL_DOMAIN_T
  use simulation_datetime, only       : SIM_DT
  use output, only                    : write_output
!  use summary_statistics, only        : perform_polygon_summarize
  use grid
  implicit none

  private

  public :: iterate_over_multiple_simulation_days

  real (kind=c_double), target, allocatable   :: SOIL_MOISTURE_STORAGE_PER_SIM(:,:)
  real (kind=c_float), target, allocatable    :: SNOW_STORAGE_PER_SIM(:,:)
  real (kind=c_double), target, allocatable   :: IMPERVIOUS_SURFACE_STORAGE_PER_SIM(:,:)
  real (kind=c_double), target, allocatable   :: INTERCEPTION_STORAGE_PER_SIM(:,:)

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
    allocate( IMPERVIOUS_SURFACE_STORAGE_PER_SIM(length, number_of_simulations))
    allocate( INTERCEPTION_STORAGE_PER_SIM(length, number_of_simulations))

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

  end subroutine allocate_space_for_simulation_storage_state_variables

!------------------------------------------------------------------------------------------------

  subroutine iterate_over_multiple_simulation_days(cells, number_of_simulations)

    use precipitation__method_of_fragments, only  : SIMULATION_NUMBER

    class (MODEL_DOMAIN_T), intent(inout)     :: cells
    integer (kind=c_int), intent(inout)       :: number_of_simulations

    ! [ LOCALS ]
    integer (kind=c_int) :: sim_number

    call allocate_space_for_simulation_storage_state_variables(cells, number_of_simulations)

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

        call perform_daily_calculation( cells )
        call write_output( cells )
  !      call perform_polygon_summarize( cells )

        call cells%dump_variables( )

      enddo

      write(*,fmt="(/)")

      call SIM_DT%addDay( )

    enddo

  end subroutine iterate_over_multiple_simulation_days

end module model_iterate_multiple_simulations
