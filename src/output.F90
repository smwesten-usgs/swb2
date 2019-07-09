module output

  use constants_and_conversions
  use exceptions, only            : warn, assert, die
  use model_domain, only          : MODEL_DOMAIN_T
  use iso_c_binding
  use netcdf4_support
  use simulation_datetime, only   : SIM_DT
  use fstring, only               : asCharacter
  implicit none

  real( c_double ), allocatable      :: RECHARGE_ARRAY(:)

  type, public :: NETCDF_FILE_COLLECTION_T
    type (T_NETCDF4_FILE), pointer :: ncfile
  end type NETCDF_FILE_COLLECTION_T

  type (NETCDF_FILE_COLLECTION_T), allocatable, public :: NC_OUT(:)
  type (NETCDF_FILE_COLLECTION_T), allocatable, public :: NC_MULTI_SIM_OUT(:,:)


  integer (c_int), parameter   :: NCDF_NUM_OUTPUTS = 29

  type OUTPUT_SPECS_T
    character (len=27)          :: variable_name
    character (len=21)          :: variable_units
    real (c_float)         :: valid_minimum
    real (c_float)         :: valid_maximum
    logical (c_bool)       :: is_active
    logical (c_bool)       :: multisim_outputs
  end type OUTPUT_SPECS_T

  type (OUTPUT_SPECS_T)    :: OUTSPECS(NCDF_NUM_OUTPUTS) = [                                              &
    OUTPUT_SPECS_T( "gross_precipitation        ", "inches               ", 0.0, 60.0, TRUE, FALSE ),     &
    OUTPUT_SPECS_T( "rainfall                   ", "inches               ", 0.0, 60.0, TRUE, FALSE  ),    &
    OUTPUT_SPECS_T( "snowfall                   ", "inches               ", 0.0, 60.0, TRUE, FALSE  ),    &
    OUTPUT_SPECS_T( "interception               ", "inches               ", 0.0, 60.0, TRUE, FALSE  ),    &
    OUTPUT_SPECS_T( "interception_storage       ", "inches               ", 0.0, 60.0, FALSE, FALSE  ),   &
    OUTPUT_SPECS_T( "runon                      ", "inches               ", 0.0, 10000.0, TRUE, FALSE  ), &
    OUTPUT_SPECS_T( "runoff                     ", "inches               ", 0.0, 10000.0, TRUE, FALSE  ), &
    OUTPUT_SPECS_T( "snow_storage               ", "inches               ", 0.0, 2000.0, FALSE, FALSE ),  &
    OUTPUT_SPECS_T( "surface_storage            ", "inches               ", 0.0, 2000.0, FALSE, FALSE ),  &
    OUTPUT_SPECS_T( "soil_storage               ", "inches               ", 0.0, 2000.0, FALSE, FALSE ),  &
    OUTPUT_SPECS_T( "delta_soil_storage         ", "inches               ", -5.0, 5.0, FALSE, FALSE  ),   &
    OUTPUT_SPECS_T( "reference_ET0              ", "inches               ", 0.0, 2000.0, TRUE, FALSE  ),  &
    OUTPUT_SPECS_T( "actual_et                  ", "inches               ", 0.0, 2000.0, TRUE, FALSE  ),  &
    OUTPUT_SPECS_T( "snowmelt                   ", "inches               ", 0.0, 2000.0, FALSE, FALSE ),  &
    OUTPUT_SPECS_T( "tmin                       ", "degrees_fahrenheit   ", -100.0, 150.0, TRUE, FALSE  ),&
    OUTPUT_SPECS_T( "tmax                       ", "degrees_fahrenheit   ", -100.0, 150.0, TRUE, FALSE  ),&
    OUTPUT_SPECS_T( "net_infiltration           ", "inches               ", 0.0, 2000.0, TRUE, FALSE  ),  &
    OUTPUT_SPECS_T( "rejected_net_infiltration  ", "inches               ", 0.0, 5000.0, TRUE, FALSE  ),  &
    OUTPUT_SPECS_T( "infiltration               ", "inches               ", 0.0, 2000.0, FALSE, FALSE  ), &
    OUTPUT_SPECS_T( "irrigation                 ", "inches               ", 0.0, 2000.0, TRUE, FALSE  ),  &
    OUTPUT_SPECS_T( "runoff_outside             ", "inches               ", 0.0, 10000.0, TRUE, FALSE  ), &
    OUTPUT_SPECS_T( "crop_et                    ", "inches               ", 0.0, 10000.0, FALSE, FALSE ), &
    OUTPUT_SPECS_T( "bare_soil_evaporation      ", "inches               ", 0.0, 10000.0, FALSE, FALSE ), &
    OUTPUT_SPECS_T( "growing_degree_day         ", "degree_day_fahrenheit", 0.0, 10000.0, FALSE, FALSE ), &
    OUTPUT_SPECS_T( "direct_net_infiltation     ", "inches               ", 0.0, 100.0, FALSE, FALSE ),   &
    OUTPUT_SPECS_T( "direct_soil_moisture       ", "inches               ", 0.0, 100.0, FALSE, FALSE ),   &
    OUTPUT_SPECS_T( "storm_drain_capture        ", "inches               ", 0.0, 100.0, FALSE, FALSE ),   &
    OUTPUT_SPECS_T( "growing_season             ", "0_no__1_yes          ", 0.0, 1.0, FALSE, FALSE ),     &
    OUTPUT_SPECS_T( "fog                        ", "inches               ", 0.0, 100.0, FALSE, FALSE)      ]

  enum, bind(c)
    enumerator :: NCDF_GROSS_PRECIPITATION=1, NCDF_RAINFALL, NCDF_SNOWFALL,   &
                  NCDF_INTERCEPTION, NCDF_INTERCEPTION_STORAGE,               &
                  NCDF_RUNON, NCDF_RUNOFF,                                    &
                  NCDF_SNOW_STORAGE, NCDF_SURFACE_STORAGE, NCDF_SOIL_STORAGE, &
                  NCDF_DELTA_SOIL_STORAGE,                                    &
                  NCDF_REFERENCE_ET0,                                         &
                  NCDF_ACTUAL_ET, NCDF_SNOWMELT, NCDF_TMIN, NCDF_TMAX,        &
                  NCDF_NET_INFILTRATION, NCDF_REJECTED_NET_INFILTRATION,      &
                  NCDF_INFILTRATION,                                          &
                  NCDF_IRRIGATION, NCDF_RUNOFF_OUTSIDE,                       &
                  NCDF_CROP_ET, NCDF_BARE_SOIL_EVAP, NCDF_GDD,                &
                  NCDF_DIRECT_NET_INFILTRATION, NCDF_DIRECT_SOIL_MOISTURE,    &
                  NCDF_STORM_DRAIN_CAPTURE, NCDF_GROWING_SEASON, NCDF_FOG
  end enum

  logical ( c_bool ) :: OUTPUT_INCLUDES_LATLON = lTRUE

contains

  subroutine set_output_latlon_option( output_includes_latlon_l )

    logical (c_bool) :: output_includes_latlon_l

    OUTPUT_INCLUDES_LATLON = output_includes_latlon_l

  end subroutine set_output_latlon_option

!--------------------------------------------------------------------------------------------------

  subroutine set_output_directory( output_dir_name )

    character (len=*), intent(in)  :: output_dir_name

    OUTPUT_DIRECTORY_NAME = trim( output_dir_name )

  end subroutine set_output_directory

!--------------------------------------------------------------------------------------------------

  subroutine set_output_prefix( output_prefix )

    character (len=*), intent(in)  :: output_prefix

    OUTPUT_PREFIX_NAME = trim( output_prefix )

  end subroutine set_output_prefix

!--------------------------------------------------------------------------------------------------

  subroutine initialize_output(cells)

    class (MODEL_DOMAIN_T), intent(inout)   :: cells

    ! [ LOCALS ]
    integer (c_int) :: iStat
    integer (c_int) :: iIndex

    allocate ( NC_OUT( NCDF_NUM_OUTPUTS ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    if ( .not. allocated( OUTPUT_DIRECTORY_NAME ) ) OUTPUT_DIRECTORY_NAME = ""
    if ( .not. allocated( OUTPUT_PREFIX_NAME ) ) OUTPUT_PREFIX_NAME       = ""

    ! overrides to prevent unused variables from being written to output
    if ( .not. associated(cells%fog) ) OUTSPECS( NCDF_FOG)%is_active = FALSE

    if ( OUTPUT_INCLUDES_LATLON ) then

      do iIndex = 1, ubound(NC_OUT, 1)

        ! do not initialize a single output file if a series of outputs is desired
        if ( OUTSPECS(iIndex)%multisim_outputs ) cycle

        if ( OUTSPECS(iIndex)%is_active ) then

          allocate ( NC_OUT(iIndex)%ncfile )

          call netcdf_open_and_prepare_as_output(                                        &
                NCFILE=NC_OUT( iIndex )%ncfile,                                          &
                sVariableName=trim( OUTSPECS( iIndex )%variable_name ),                  &
                sVariableUnits=trim( OUTSPECS( iIndex )%variable_units ),                &
                iNX=cells%number_of_columns,                                             &
                iNY=cells%number_of_rows,                                                &
                fX=cells%X,                                                              &
                fY=cells%Y,                                                              &
                StartDate=SIM_DT%start,                                                  &
                EndDate=SIM_DT%end,                                                      &
                PROJ4_string=cells%PROJ4_string,                                         &
                dpLat=cells%Y_lat,                                                       &
                dpLon=cells%X_lon,                                                       &
                fValidMin=OUTSPECS( iIndex )%valid_minimum,                              &
                fValidMax=OUTSPECS( iIndex )%valid_maximum )

        endif

      enddo

    else

      do iIndex = 1, ubound(NC_OUT, 1)

        ! do not initialize a single output file if a series of outputs is desired
        if ( OUTSPECS(iIndex)%multisim_outputs ) cycle

        if ( OUTSPECS(iIndex)%is_active ) then

          allocate ( NC_OUT(iIndex)%ncfile )

          call netcdf_open_and_prepare_as_output(                                        &
                NCFILE=NC_OUT( iIndex )%ncfile,                                          &
                sVariableName=trim( OUTSPECS( iIndex )%variable_name ),                  &
                sVariableUnits=trim( OUTSPECS( iIndex )%variable_units ),                &
                iNX=cells%number_of_columns,                                             &
                iNY=cells%number_of_rows,                                                &
                fX=cells%X,                                                              &
                fY=cells%Y,                                                              &
                StartDate=SIM_DT%start,                                                  &
                EndDate=SIM_DT%end,                                                      &
                PROJ4_string=cells%PROJ4_string,                                         &
                fValidMin=OUTSPECS( iIndex )%valid_minimum,                              &
                fValidMax=OUTSPECS( iIndex )%valid_maximum )

        endif

      enddo

    endif

    cells%nodata_fill_value = NC_FILL_FLOAT

  end subroutine initialize_output

  !--------------------------------------------------------------------------------------------------

  subroutine initialize_multiple_sim_output(cells, number_of_simulations)

    class (MODEL_DOMAIN_T), intent(inout)   :: cells
    integer (c_int), intent(in)        :: number_of_simulations

    ! [ LOCALS ]
    integer (c_int) :: iStat
    integer (c_int) :: noutput, nsim

    allocate ( NC_MULTI_SIM_OUT( NCDF_NUM_OUTPUTS, number_of_simulations ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    if ( .not. allocated( OUTPUT_DIRECTORY_NAME ) ) OUTPUT_DIRECTORY_NAME = ""
    if ( .not. allocated( OUTPUT_PREFIX_NAME ) ) OUTPUT_PREFIX_NAME       = ""

    if ( OUTPUT_INCLUDES_LATLON ) then

      do noutput = 1, ubound(NC_MULTI_SIM_OUT, 1)

        ! do not initialize a series of output files unless explicitly called for
        if ( .not. OUTSPECS(noutput)%multisim_outputs ) cycle

        if ( OUTSPECS(noutput)%is_active ) then

          do nsim=1, number_of_simulations

            allocate ( NC_MULTI_SIM_OUT(noutput, nsim)%ncfile )

            call netcdf_open_and_prepare_as_output(                                        &
                  NCFILE=NC_MULTI_SIM_OUT( noutput, nsim )%ncfile,                         &
                  sVariableName=trim( OUTSPECS( noutput )%variable_name ),                 &
                  sVariableUnits=trim( OUTSPECS( noutput )%variable_units ),               &
                  iNX=cells%number_of_columns,                                             &
                  iNY=cells%number_of_rows,                                                &
                  fX=cells%X,                                                              &
                  fY=cells%Y,                                                              &
                  StartDate=SIM_DT%start,                                                  &
                  EndDate=SIM_DT%end,                                                      &
                  PROJ4_string=cells%PROJ4_string,                                         &
                  dpLat=cells%Y_lat,                                                       &
                  dpLon=cells%X_lon,                                                       &
                  fValidMin=OUTSPECS( noutput )%valid_minimum,                             &
                  fValidMax=OUTSPECS( noutput )%valid_maximum,                             &
                  filename_modifier="simulation_"//trim(asCharacter(nsim)) )

          enddo

        endif

      enddo

    else

      do noutput = 1, ubound(NC_MULTI_SIM_OUT, 1)

        ! do not initialize a series of output files unless explicitly called for
        if ( .not. OUTSPECS(noutput)%multisim_outputs ) cycle

        if ( OUTSPECS(noutput)%is_active ) then

          do nsim=1, number_of_simulations

            allocate ( NC_MULTI_SIM_OUT(noutput, nsim)%ncfile )

            call netcdf_open_and_prepare_as_output(                                        &
                  NCFILE=NC_MULTI_SIM_OUT( noutput, nsim )%ncfile,                         &
                  sVariableName=trim( OUTSPECS( noutput )%variable_name ),                 &
                  sVariableUnits=trim( OUTSPECS( noutput )%variable_units ),               &
                  iNX=cells%number_of_columns,                                             &
                  iNY=cells%number_of_rows,                                                &
                  fX=cells%X,                                                              &
                  fY=cells%Y,                                                              &
                  StartDate=SIM_DT%start,                                                  &
                  EndDate=SIM_DT%end,                                                      &
                  PROJ4_string=cells%PROJ4_string,                                         &
                  fValidMin=OUTSPECS( noutput )%valid_minimum,                             &
                  fValidMax=OUTSPECS( noutput )%valid_maximum,                             &
                  filename_modifier="simulation_"//trim(asCharacter(nsim)) )

          enddo

        endif

      enddo

    endif

  end subroutine initialize_multiple_sim_output

!--------------------------------------------------------------------------------------------------

  subroutine output_2D_float_array( ncfile_ptr, values, cells )

    type (T_NETCDF4_FILE), pointer      :: ncfile_ptr
    real (c_float), intent(in)     :: values(:)
    class (MODEL_DOMAIN_T), intent(in)  :: cells

    call netcdf_put_variable_vector(NCFILE=ncfile_ptr,                         &
       iVarID=ncfile_ptr%iVarID(NC_TIME),                                      &
       iStart=[int(SIM_DT%iNumDaysFromOrigin, c_size_t)],                 &
       iCount=[1_c_size_t],                                                    &
       iStride=[1_c_size_t],                                                &
       dpValues=[real(SIM_DT%iNumDaysFromOrigin, c_double)])

  call netcdf_put_packed_variable_array(NCFILE=ncfile_ptr,                     &
        iVarID=ncfile_ptr%iVarID(NC_Z),                                        &
        iStart=[ int(SIM_DT%iNumDaysFromOrigin, c_size_t),                &
                0_c_size_t, 0_c_size_t ],                                      &
        iCount=[ 1_c_size_t, int(cells%number_of_rows, c_size_t),         &
                int(cells%number_of_columns, c_size_t) ],                 &
        iStride=[ 1_c_size_t, 1_c_size_t, 1_c_size_t ],               &
        lMask=cells%active,                                                    &
        rValues=values,                                                        &
        rField=cells%nodata_fill_value )

  end subroutine output_2D_float_array

!--------------------------------------------------------------------------------------------------

  subroutine write_output(cells)

    class (MODEL_DOMAIN_T), intent(inout)  :: cells

    ! [ LOCALS ]
    integer (c_int)           :: iIndex
    type (T_NETCDF4_FILE), pointer :: ncfile_ptr

    do



      if (.not. allocated(NC_OUT) ) exit

      ! first put out the current time variable for all open NetCDF files
      do iIndex = 1, ubound( NC_OUT, 1 )

        if ( OUTSPECS(iIndex)%multisim_outputs ) cycle

        if ( OUTSPECS(iIndex)%is_active ) then

          call netcdf_put_variable_vector(NCFILE=NC_OUT(iIndex)%ncfile, &
             iVarID=NC_OUT(iIndex)%ncfile%iVarID(NC_TIME), &
             iStart=[int(SIM_DT%iNumDaysFromOrigin, c_size_t)], &
             iCount=[1_c_size_t], &
             iStride=[1_c_size_t], &
             dpValues=[real(SIM_DT%iNumDaysFromOrigin, c_double)])

        endif

      enddo



      if ( OUTSPECS( NCDF_GROSS_PRECIPITATION )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_GROSS_PRECIPITATION )%ncfile,      &
                                    values=cells%gross_precip,                                 &
                                    cells=cells )

      if ( OUTSPECS( NCDF_RAINFALL )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_RAINFALL )%ncfile,       &
                                    values=cells%rainfall,                           &
                                    cells=cells )

      if ( OUTSPECS( NCDF_INTERCEPTION )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_INTERCEPTION )%ncfile,   &
                                    values=cells%interception,                       &
                                    cells=cells )

      if ( OUTSPECS( NCDF_INTERCEPTION )%is_active                                &
           .and. (.not. OUTSPECS( NCDF_INTERCEPTION_STORAGE )%multisim_outputs ))  &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_INTERCEPTION )%ncfile,   &
                                    values=cells%interception,                       &
                                    cells=cells )

      if ( OUTSPECS( NCDF_RUNOFF )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_RUNOFF )%ncfile,       &
                                    values=cells%runoff,                           &
                                    cells=cells )

      if ( OUTSPECS( NCDF_RUNOFF_OUTSIDE )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_RUNOFF_OUTSIDE )%ncfile,   &
                                    values=cells%runoff_outside,                       &
                                    cells=cells )

      if ( OUTSPECS( NCDF_RUNON )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_RUNON )%ncfile,       &
                                    values=cells%runon,                           &
                                    cells=cells )

      if ( OUTSPECS( NCDF_INFILTRATION )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_INFILTRATION )%ncfile,       &
                                    values=cells%infiltration,                           &
                                    cells=cells )

      if ( OUTSPECS( NCDF_SNOWFALL )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_SNOWFALL )%ncfile,       &
                                    values=cells%snowfall,                           &
                                    cells=cells )

      if ( OUTSPECS( NCDF_SNOWMELT )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_SNOWMELT )%ncfile,       &
                                    values=cells%snowmelt,                           &
                                    cells=cells )

      if ( OUTSPECS( NCDF_SNOW_STORAGE )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_SNOW_STORAGE )%ncfile,  &
                                    values=cells%snow_storage,                      &
                                    cells=cells )

      if ( OUTSPECS( NCDF_SURFACE_STORAGE )%is_active                            &
           .and. (.not. OUTSPECS( NCDF_SURFACE_STORAGE )%multisim_outputs) )     &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_SURFACE_STORAGE )%ncfile,  &
                                    values=real(cells%surface_storage, c_float),  &
                                    cells=cells )

      if ( OUTSPECS( NCDF_SOIL_STORAGE )%is_active                             &
           .and. (.not. OUTSPECS( NCDF_SOIL_STORAGE )%multisim_outputs) )   &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_SOIL_STORAGE )%ncfile,  &
                                    values=real(cells%soil_storage, c_float),  &
                                    cells=cells )

      if ( OUTSPECS( NCDF_DELTA_SOIL_STORAGE )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_DELTA_SOIL_STORAGE )%ncfile,  &
                                    values=cells%delta_soil_storage,                      &
                                    cells=cells )

      if ( OUTSPECS( NCDF_NET_INFILTRATION )%is_active                           &
           .and. (.not. OUTSPECS( NCDF_NET_INFILTRATION )%multisim_outputs) )    &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_NET_INFILTRATION )%ncfile,  &
                                    values=cells%net_infiltration,                      &
                                    cells=cells )

      if ( OUTSPECS( NCDF_REFERENCE_ET0 )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_REFERENCE_ET0 )%ncfile,  &
                                    values=cells%reference_ET0,                      &
                                    cells=cells )

      if ( OUTSPECS( NCDF_ACTUAL_ET )%is_active                                 &
           .and. (.not. OUTSPECS( NCDF_ACTUAL_ET )%multisim_outputs) )          &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_ACTUAL_ET )%ncfile,  &
                                    values=real(cells%actual_et, c_float),  &
                                    cells=cells )

      if ( OUTSPECS( NCDF_TMIN )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_TMIN )%ncfile,       &
                                    values=cells%tmin,                           &
                                    cells=cells )

      if ( OUTSPECS( NCDF_TMAX )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_TMAX )%ncfile,       &
                                    values=cells%tmax,                           &
                                    cells=cells )

      if ( OUTSPECS( NCDF_IRRIGATION )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_IRRIGATION )%ncfile,       &
                                    values=cells%irrigation,                           &
                                    cells=cells )

      if ( OUTSPECS( NCDF_GDD )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_GDD )%ncfile,       &
                                    values=cells%gdd,                           &
                                    cells=cells )

      if ( OUTSPECS( NCDF_REJECTED_NET_INFILTRATION )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_REJECTED_NET_INFILTRATION )%ncfile,       &
                                    values=cells%rejected_net_infiltration,                           &
                                    cells=cells )

      if ( OUTSPECS( NCDF_CROP_ET )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_CROP_ET )%ncfile,       &
                                    values=cells%crop_etc,                          &
                                    cells=cells )

      if ( OUTSPECS( NCDF_BARE_SOIL_EVAP )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_BARE_SOIL_EVAP )%ncfile,       &
                                    values=cells%bare_soil_evap,                           &
                                    cells=cells )

      if ( OUTSPECS( NCDF_DIRECT_NET_INFILTRATION )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_DIRECT_NET_INFILTRATION )%ncfile,   &
                                    values=cells%direct_net_infiltration,                       &
                                    cells=cells )

      if ( OUTSPECS( NCDF_DIRECT_SOIL_MOISTURE )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_DIRECT_SOIL_MOISTURE )%ncfile,       &
                                    values=cells%direct_soil_moisture,                           &
                                    cells=cells )

      if ( OUTSPECS( NCDF_STORM_DRAIN_CAPTURE )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_STORM_DRAIN_CAPTURE )%ncfile,        &
                                    values=cells%storm_drain_capture,                            &
                                    cells=cells )

      if ( OUTSPECS( NCDF_GROWING_SEASON )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_GROWING_SEASON )%ncfile,        &
                                    values=asFloat(cells%it_is_growing_season),             &
                                    cells=cells )

      if ( OUTSPECS( NCDF_FOG )%is_active ) &

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_FOG )%ncfile,        &
                                    values=cells%fog,                            &
                                    cells=cells )



      exit

    enddo

  end subroutine write_output

!-------------------------------------------------------------------------------

  subroutine write_multi_sim_output(cells, simulation_number)

    class (MODEL_DOMAIN_T), intent(inout)  :: cells
    integer (c_int), intent(in)       :: simulation_number

    ! [ LOCALS ]
    integer (c_int)           :: iIndex
    type (T_NETCDF4_FILE), pointer :: ncfile_ptr



    ! first put out the current time variable for all open NetCDF files
    do iIndex = 1, ubound( NC_MULTI_SIM_OUT, 1 )

      ! do not write output unless multisim flag has been set
      if ( .not. OUTSPECS(iIndex)%multisim_outputs ) cycle

      if ( OUTSPECS(iIndex)%is_active ) then


        call netcdf_put_variable_vector(NCFILE=                                        &
           NC_MULTI_SIM_OUT(iIndex,simulation_number)%ncfile,                          &
           iVarID=NC_MULTI_SIM_OUT(iIndex, simulation_number)%ncfile%iVarID(NC_TIME),  &
           iStart=[int(SIM_DT%iNumDaysFromOrigin, c_size_t)],                     &
           iCount=[1_c_size_t],                                                        &
           iStride=[1_c_size_t],                                                    &
           dpValues=[real(SIM_DT%iNumDaysFromOrigin, c_double)])

      endif

    enddo

    if ( OUTSPECS( NCDF_SNOW_STORAGE )%is_active                    &
       .and. OUTSPECS(NCDF_SNOW_STORAGE)%multisim_outputs )         &

      call output_2D_float_array(                                                    &
        ncfile_ptr=NC_MULTI_SIM_OUT( NCDF_SNOW_STORAGE, simulation_number )%ncfile,  &
        values=cells%snow_storage,                                                   &
        cells=cells )

    if ( OUTSPECS( NCDF_SOIL_STORAGE )%is_active                    &
       .and. OUTSPECS(NCDF_SOIL_STORAGE)%multisim_outputs )         &

      call output_2D_float_array(                                                    &
        ncfile_ptr=NC_MULTI_SIM_OUT( NCDF_SOIL_STORAGE, simulation_number )%ncfile,  &
        values=real(cells%soil_storage, c_float),                               &
        cells=cells )

    if ( OUTSPECS( NCDF_NET_INFILTRATION )%is_active                    &
       .and. OUTSPECS(NCDF_NET_INFILTRATION)%multisim_outputs )         &

      call output_2D_float_array(                                                        &
        ncfile_ptr=NC_MULTI_SIM_OUT( NCDF_NET_INFILTRATION, simulation_number )%ncfile,  &
        values=cells%net_infiltration,                                                   &
        cells=cells )

    if ( OUTSPECS( NCDF_ACTUAL_ET )%is_active                    &
       .and. OUTSPECS(NCDF_ACTUAL_ET)%multisim_outputs )         &

      call output_2D_float_array(                                                   &
        ncfile_ptr=NC_MULTI_SIM_OUT( NCDF_ACTUAL_ET, simulation_number )%ncfile,    &
        values=real(cells%actual_et, c_float),                                 &
        cells=cells )

    if ( OUTSPECS( NCDF_INTERCEPTION_STORAGE )%is_active                    &
       .and. OUTSPECS(NCDF_INTERCEPTION_STORAGE)%multisim_outputs )         &

      call output_2D_float_array(                                                            &
        ncfile_ptr=NC_MULTI_SIM_OUT( NCDF_INTERCEPTION_STORAGE, simulation_number )%ncfile,  &
        values=real(cells%interception_storage, c_float),                               &
        cells=cells )

    if ( OUTSPECS( NCDF_SURFACE_STORAGE )%is_active                    &
       .and. OUTSPECS(NCDF_SURFACE_STORAGE)%multisim_outputs )         &

      call output_2D_float_array(                                                         &
        ncfile_ptr=NC_MULTI_SIM_OUT( NCDF_SURFACE_STORAGE, simulation_number )%ncfile,    &
        values=real(cells%surface_storage, c_float),                                 &
        cells=cells )

  end subroutine write_multi_sim_output

end module output
