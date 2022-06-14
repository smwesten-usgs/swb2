module model_iterate_multiple_simulations

  use iso_c_binding, only             : c_bool, c_float, c_int, c_double
  use constants_and_conversions, only : TRUE, BNDS, TRUE
  use datetime, only                  : MONTHS
  use daily_calculation, only         : perform_daily_calculation
  use file_operations, only           : ASCII_FILE_T
  use grid
  use logfiles, only                  : LOGS, LOG_ALL
  use model_domain, only              : MODEL_DOMAIN_T
  use simulation_datetime, only       : SIM_DT
  use fstring, only                   : asCharacter
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

  real (c_float), allocatable   :: MONTHLY_NET_INFILTRATION_STATS(:,:)
  real (c_float), allocatable   :: ANNUAL_NET_INFILTRATION_STATS(:)
  real (c_float), allocatable   :: MONTHLY_ACTUAL_ET_STATS(:,:)
  real (c_float), allocatable   :: ANNUAL_ACTUAL_ET_STATS(:)
  real (c_float), allocatable   :: MONTHLY_RAINFALL_STATS(:,:)
  real (c_float), allocatable   :: ANNUAL_RAINFALL_STATS(:)
  integer (c_int)               :: DAY_COUNT_BY_MONTH(12)

  real (c_double), target, allocatable   :: SOIL_MOISTURE_STORAGE_PER_SIM(:,:)
  real (c_float), target, allocatable    :: SNOW_STORAGE_PER_SIM(:,:)
  real (c_double), target, allocatable   :: SURFACE_STORAGE_PER_SIM(:,:)
  real (c_float), target, allocatable    :: INTERCEPTION_STORAGE_PER_SIM(:,:)
  real (c_float), target, allocatable    :: NET_INFILTRATION_PER_SIM(:,:)
  real (c_double), target, allocatable   :: ACTUAL_ET_PER_SIM(:,:)
  real (c_float), target, allocatable    :: FOG_PER_SIM(:,:)

! Concept: remap pointers to key state variables for each of X simulations,
!          run the daily simulation given current landuse, soil type, etc.
!          update rainfall by means of method of fragments
!
contains

  subroutine allocate_space_for_simulation_storage_state_variables(cells, number_of_simulations)

    class (MODEL_DOMAIN_T), intent(inout)     :: cells
    integer (c_int), intent(in)          :: number_of_simulations

    ! [ LOCALS ]
    integer (c_int) :: length
    integer (c_int) :: indx

    length = size( cells%soil_storage, 1)

    allocate( SOIL_MOISTURE_STORAGE_PER_SIM(length, number_of_simulations))
    allocate( SNOW_STORAGE_PER_SIM(length, number_of_simulations))
    allocate( SURFACE_STORAGE_PER_SIM(length, number_of_simulations))
    allocate( INTERCEPTION_STORAGE_PER_SIM(length, number_of_simulations))
    allocate( NET_INFILTRATION_PER_SIM(length, number_of_simulations))
    allocate( ACTUAL_ET_PER_SIM(length, number_of_simulations))
    allocate( FOG_PER_SIM(length, number_of_simulations))

    allocate( MONTHLY_NET_INFILTRATION_STATS(length, 12) )
    allocate( ANNUAL_NET_INFILTRATION_STATS(length) )
    allocate( MONTHLY_ACTUAL_ET_STATS(length, 12) )
    allocate( ANNUAL_ACTUAL_ET_STATS(length) )
    allocate( MONTHLY_RAINFALL_STATS(length, 12) )
    allocate( ANNUAL_RAINFALL_STATS(length) )

    ! do not want or need this memory since we are going to swap out a
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

    if (associated(cells%fog)) then
      do indx=1,number_of_simulations
        FOG_PER_SIM(:,indx) = cells%fog
      enddo
      deallocate(cells%fog)
    endif

  end subroutine allocate_space_for_simulation_storage_state_variables

!------------------------------------------------------------------------------------------------

  subroutine iterate_over_multiple_simulation_days(cells, number_of_simulations)

    use precipitation__method_of_fragments, only  : SIMULATION_NUMBER

    class (MODEL_DOMAIN_T), intent(inout)     :: cells
    integer (c_int), intent(inout)       :: number_of_simulations

    ! [ LOCALS ]
    integer (c_int) :: sim_number

    ! set flags in output mod to allow multiple simulation output to be generated
    OUTSPECS(NCDF_NET_INFILTRATION)%multisim_outputs = TRUE
    OUTSPECS(NCDF_ACTUAL_ET)%multisim_outputs = TRUE

    call allocate_space_for_simulation_storage_state_variables(cells, number_of_simulations)

    call reset_annual_accumulators()
    call reset_monthly_accumulators()

    !call initialize_multiple_sim_output(cells, number_of_simulations)

    do while ( SIM_DT%curr <= SIM_DT%end )

      call cells%update_landuse_codes()
      call cells%update_irrigation_mask()

      do sim_number=1,number_of_simulations

        ! modifying module variable from precipitation__method_of_fragments
        SIMULATION_NUMBER = sim_number

        call cells%get_climate_data( )

        if (sim_number==1) then
          call LOGS%write("Calculating: "//SIM_DT%curr%prettydate(),  &
                          iLogLevel=LOG_ALL, lEcho=.true._c_bool )
          write(*,"(a,i0,a)", advance="no") "  running ",number_of_simulations, &
                                            " simulations: "
        endif

        write(*,fmt="(a)", advance="no") "."

        cells%snow_storage => SNOW_STORAGE_PER_SIM(:,sim_number)
        cells%soil_storage => SOIL_MOISTURE_STORAGE_PER_SIM(:,sim_number)
        cells%interception_storage => INTERCEPTION_STORAGE_PER_SIM(:,sim_number)
        cells%surface_storage => SURFACE_STORAGE_PER_SIM(:,sim_number)
        cells%net_infiltration => NET_INFILTRATION_PER_SIM(:,sim_number)
        cells%actual_et => ACTUAL_ET_PER_SIM(:,sim_number)
        cells%fog => FOG_PER_SIM(:,sim_number)

        call perform_daily_calculation( cells )

        call write_output( cells )
        call update_accumulators( cells, sim_number )
!        call write_multi_sim_output( cells, sim_number)
  !      call perform_polygon_summarize( cells )

        call cells%dump_variables( )

      enddo

      write(*,fmt="(/)")

      call SIM_DT%addDay( )

    enddo

    call finalize_accumulators( cells, number_of_simulations )

  end subroutine iterate_over_multiple_simulation_days

!------------------------------------------------------------------------------------------------

  subroutine reset_monthly_accumulators( )

    MONTHLY_ACTUAL_ET_STATS = 0.0_c_float
    MONTHLY_RAINFALL_STATS = 0.0_c_float
    MONTHLY_NET_INFILTRATION_STATS = 0.0_c_float
    DAY_COUNT_BY_MONTH(:) = 0

  end subroutine reset_monthly_accumulators

!------------------------------------------------------------------------------------------------

  subroutine reset_annual_accumulators( )

    ANNUAL_ACTUAL_ET_STATS = 0.0_c_float
    ANNUAL_RAINFALL_STATS = 0.0_c_float
    ANNUAL_NET_INFILTRATION_STATS = 0.0_c_float

  end subroutine reset_annual_accumulators

!------------------------------------------------------------------------------------------------

  subroutine update_accumulators( cells, simulation_number )

    class (MODEL_DOMAIN_T), intent(inout)     :: cells
    integer (c_int), intent(in)          :: simulation_number

    ![ LOCALS ]
    integer (c_int)  :: month

    month = SIM_DT%curr%iMonth

    if (simulation_number == 1)                                                &
      DAY_COUNT_BY_MONTH(month) = DAY_COUNT_BY_MONTH(month) + 1

    ! called each day of a simulation for nsim number of simulations
    MONTHLY_NET_INFILTRATION_STATS(:,month) = MONTHLY_NET_INFILTRATION_STATS(:,month)    &
                                               + cells%net_infiltration

    MONTHLY_ACTUAL_ET_STATS(:,month) = MONTHLY_ACTUAL_ET_STATS(:,month)    &
                                               + cells%actual_et

    MONTHLY_RAINFALL_STATS(:,month) = MONTHLY_RAINFALL_STATS(:,month)    &
                                               + cells%rainfall

    ! ANNUAL_NET_INFILTRATION_STATS(:,simulation_number) =                             &
    !    ANNUAL_NET_INFILTRATION_STATS(:,simulation_number) + cells%net_infiltration
    !
    ! ANNUAL_ACTUAL_ET_STATS(:,simulation_number) =                                    &
    !    ANNUAL_ACTUAL_ET_STATS(:,simulation_number) + cells%actual_et

  end subroutine update_accumulators

!------------------------------------------------------------------------------------------------

  subroutine finalize_accumulators(cells, number_of_simulations)

    class (MODEL_DOMAIN_T), intent(inout)     :: cells
    integer (c_int), intent(in)          :: number_of_simulations

    ![ LOCALS ]
    real (c_float)   :: number_of_simulation_days
    integer (c_int)  :: month
    integer (c_int)  :: simulation_number
    character (len=256)   :: filename, file_suffix
    integer (c_int)  :: start_year, end_year
    integer (c_int)  :: nx, ny
    character (len=3)     :: month_abbrev

    type (GENERAL_GRID_T), pointer  :: pTempGrid

    start_year = SIM_DT%start%iYear
    end_year = SIM_DT%end%iYear
    nx = cells%number_of_columns
    ny = cells%number_of_rows

    file_suffix = "__"//trim(asCharacter(number_of_simulations))//"-simulations__"              &
                  //trim(asCharacter(ny))//"_cols_by_"//trim(asCharacter(nx))//"_rows__"        &
                  //trim(asCharacter(start_year))//"_to_"//trim(asCharacter(end_year))//".asc"

    pTempGrid => grid_Create( iNX=cells%number_of_columns, iNY=cells%number_of_rows, &
        rX0=cells%X_ll, rY0=cells%Y_ll, &
        rGridCellSize=cells%gridcellsize, iDataType=GRID_DATATYPE_REAL )

    call grid_set_nodata_value(pTempGrid, fValue=cells%nodata_fill_value(1,1))

    number_of_simulation_days = real( SIM_DT%end - SIM_DT%start + 1.0, c_float)

    do month=1,12

      month_abbrev = MONTHS(month)%sName

      ANNUAL_NET_INFILTRATION_STATS(:) = ANNUAL_NET_INFILTRATION_STATS(:)            &
                                         + MONTHLY_NET_INFILTRATION_STATS(:,month)

      ANNUAL_ACTUAL_ET_STATS(:) = ANNUAL_ACTUAL_ET_STATS(:)                          &
                                  + MONTHLY_ACTUAL_ET_STATS(:,month)

      ANNUAL_RAINFALL_STATS(:) = ANNUAL_RAINFALL_STATS(:)                            &
                                  + MONTHLY_RAINFALL_STATS(:,month)

      MONTHLY_NET_INFILTRATION_STATS(:,month) = MONTHLY_NET_INFILTRATION_STATS(:,month)          &
                                                / real(number_of_simulations, c_float)      &
                                                * real(MONTHS(month)%iNumDays, c_float)     &
                                                / real(DAY_COUNT_BY_MONTH(month), c_float)

      MONTHLY_ACTUAL_ET_STATS(:,month) = MONTHLY_ACTUAL_ET_STATS(:,month)                  &
                                          / real(number_of_simulations, c_float)      &
                                          * real(MONTHS(month)%iNumDays, c_float)     &
                                          / real(DAY_COUNT_BY_MONTH(month), c_float)

      MONTHLY_RAINFALL_STATS(:,month) = MONTHLY_RAINFALL_STATS(:,month)                    &
                                          / real(number_of_simulations, c_float)      &
                                          * real(MONTHS(month)%iNumDays, c_float)     &
                                          / real(DAY_COUNT_BY_MONTH(month), c_float)

      pTempGrid%rData = unpack( MONTHLY_ACTUAL_ET_STATS(:,month), cells%active, cells%nodata_fill_value )
      filename = "MEAN_MONTHLY_SUM-"//trim(month_abbrev)//"__actual_evapotranspiration"//trim(file_suffix)
      call grid_WriteArcGrid( sFilename=filename, pGrd=pTempGrid )

      pTempGrid%rData = unpack( MONTHLY_NET_INFILTRATION_STATS(:,month), cells%active, cells%nodata_fill_value )
      filename = "MEAN_MONTHLY_SUM-"//trim(month_abbrev)//"__net_infiltration"//trim(file_suffix)
      call grid_WriteArcGrid( sFilename=filename, pGrd=pTempGrid )

      pTempGrid%rData = unpack( MONTHLY_RAINFALL_STATS(:,month), cells%active, cells%nodata_fill_value )
      filename = "MEAN_MONTHLY_SUM-"//trim(month_abbrev)//"__rainfall"//trim(file_suffix)
      call grid_WriteArcGrid( sFilename=filename, pGrd=pTempGrid )

     enddo

     ANNUAL_NET_INFILTRATION_STATS(:) = ANNUAL_NET_INFILTRATION_STATS(:)                   &
                                         / real(number_of_simulations, c_float)       &
                                         * 365.0_c_float                                   &
                                         / real(number_of_simulation_days, c_float)

     ANNUAL_ACTUAL_ET_STATS(:) = ANNUAL_ACTUAL_ET_STATS(:)                         &
                                 / real(number_of_simulations, c_float)       &
                                 * 365.0_c_float                                   &
                                 / real(number_of_simulation_days, c_float)

     ANNUAL_RAINFALL_STATS(:) = ANNUAL_RAINFALL_STATS(:)                           &
                                 / real(number_of_simulations, c_float)       &
                                 * 365.0_c_float                                   &
                                 / real(number_of_simulation_days, c_float)

     pTempGrid%rData = unpack( ANNUAL_ACTUAL_ET_STATS, cells%active, cells%nodata_fill_value )
     filename = "MEAN_ANNUAL_SUM__actual_evapotranspiration"//trim(file_suffix)
     call grid_WriteArcGrid( sFilename=filename, pGrd=pTempGrid )

     pTempGrid%rData = unpack( ANNUAL_NET_INFILTRATION_STATS, cells%active, cells%nodata_fill_value )
     filename = "MEAN_ANNUAL_SUM__net_infiltration"//trim(file_suffix)
     call grid_WriteArcGrid( sFilename=filename, pGrd=pTempGrid )

     pTempGrid%rData = unpack( ANNUAL_RAINFALL_STATS, cells%active, cells%nodata_fill_value )
     filename = "MEAN_ANNUAL_SUM__rainfall"//trim(file_suffix)
     call grid_WriteArcGrid( sFilename=filename, pGrd=pTempGrid )

     call grid_Destroy( pTempGrid )

  end subroutine finalize_accumulators


end module model_iterate_multiple_simulations
