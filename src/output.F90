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


  integer (c_int), parameter   :: NCDF_NUM_OUTPUTS = 31

  type OUTPUT_SPECS_T
    character (len=27)          :: variable_name
    character (len=21)          :: variable_units
    real (c_float)              :: valid_minimum
    real (c_float)              :: valid_maximum
    logical (c_bool)            :: is_active
    logical (c_bool)            :: multisim_outputs
  end type OUTPUT_SPECS_T

  type (OUTPUT_SPECS_T)    :: OUTSPECS(NCDF_NUM_OUTPUTS) = [                                                  &
    OUTPUT_SPECS_T( "gross_precipitation        ", "inches               ", 2000.0, 0.0, TRUE, FALSE ),       &
    OUTPUT_SPECS_T( "rainfall                   ", "inches               ", 2000.0, 0.0, TRUE, FALSE  ),      &
    OUTPUT_SPECS_T( "snowfall                   ", "inches               ", 2000.0, 0.0, TRUE, FALSE  ),      &
    OUTPUT_SPECS_T( "interception               ", "inches               ", 2000.0, 0.0, TRUE, FALSE  ),      &
    OUTPUT_SPECS_T( "interception_storage       ", "inches               ", 2000.0, 0.0, FALSE, FALSE  ),     &
    OUTPUT_SPECS_T( "runon                      ", "inches               ", 2000.0, 0.0, TRUE, FALSE  ),      &
    OUTPUT_SPECS_T( "runoff                     ", "inches               ", 2000.0, 0.0, TRUE, FALSE  ),      &
    OUTPUT_SPECS_T( "snow_storage               ", "inches               ", 2000.0, 0.0, FALSE, FALSE ),      &
    OUTPUT_SPECS_T( "surface_storage            ", "inches               ", 2000.0, 0.0, FALSE, FALSE ),      &
    OUTPUT_SPECS_T( "soil_storage               ", "inches               ", 2000.0, 0.0, FALSE, FALSE ),      &
    OUTPUT_SPECS_T( "delta_soil_storage         ", "inches               ", 2000.0, -2000.0, FALSE, FALSE  ), &
    OUTPUT_SPECS_T( "reference_ET0              ", "inches               ", 2000.0, 0.0, TRUE, FALSE  ),      &
    OUTPUT_SPECS_T( "actual_et                  ", "inches               ", 2000.0, 0.0, TRUE, FALSE  ),      &
    OUTPUT_SPECS_T( "climatic_deficit           ", "inches               ", 2000.0, 0.0, FALSE, FALSE  ),     &
    OUTPUT_SPECS_T( "snowmelt                   ", "inches               ", 2000.0, 0.0, FALSE, FALSE ),      &
    OUTPUT_SPECS_T( "tmin                       ", "degrees_fahrenheit   ", 2000.0, -2000.0, TRUE, FALSE  ),  &
    OUTPUT_SPECS_T( "tmax                       ", "degrees_fahrenheit   ", 2000.0, -2000.0, TRUE, FALSE  ),  &
    OUTPUT_SPECS_T( "tmax_minus_tmin            ", "degrees_fahrenheit   ", 2000.0, -2000.0, FALSE, FALSE  ), &
    OUTPUT_SPECS_T( "net_infiltration           ", "inches               ", 2000.0, 0.0, TRUE, FALSE  ),      &
    OUTPUT_SPECS_T( "rejected_net_infiltration  ", "inches               ", 2000.0, 0.0, TRUE, FALSE  ),      &
    OUTPUT_SPECS_T( "infiltration               ", "inches               ", 2000.0, 0.0, FALSE, FALSE  ),     &
    OUTPUT_SPECS_T( "irrigation                 ", "inches               ", 2000.0, 0.0, TRUE, FALSE  ),      &
    OUTPUT_SPECS_T( "runoff_outside             ", "inches               ", 2000.0, 0.0, TRUE, FALSE  ),      &
    OUTPUT_SPECS_T( "crop_et                    ", "inches               ", 2000.0, 0.0, FALSE, FALSE ),      &
    OUTPUT_SPECS_T( "bare_soil_evaporation      ", "inches               ", 2000.0, 0.0, FALSE, FALSE ),      &
    OUTPUT_SPECS_T( "growing_degree_day         ", "degree_day_fahrenheit", 2000.0, 0.0, FALSE, FALSE ),      &
    OUTPUT_SPECS_T( "direct_net_infiltation     ", "inches               ", 2000.0, 0.0, FALSE, FALSE ),      &
    OUTPUT_SPECS_T( "direct_soil_moisture       ", "inches               ", 2000.0, 0.0, FALSE, FALSE ),      &
    OUTPUT_SPECS_T( "storm_drain_capture        ", "inches               ", 2000.0, 0.0, FALSE, FALSE ),      &
    OUTPUT_SPECS_T( "growing_season             ", "0_no__1_yes          ", 2000.0, 0.0, FALSE, FALSE ),      &
    OUTPUT_SPECS_T( "fog                        ", "inches               ", 2000.0, 0.0, FALSE, FALSE)      ]

  enum, bind(c)
    enumerator :: NCDF_GROSS_PRECIPITATION=1, NCDF_RAINFALL, NCDF_SNOWFALL,   &
                  NCDF_INTERCEPTION, NCDF_INTERCEPTION_STORAGE,               &
                  NCDF_RUNON, NCDF_RUNOFF,                                    &
                  NCDF_SNOW_STORAGE, NCDF_SURFACE_STORAGE, NCDF_SOIL_STORAGE, &
                  NCDF_DELTA_SOIL_STORAGE,                                    &
                  NCDF_REFERENCE_ET0,                                         &
                  NCDF_ACTUAL_ET, NCDF_CLIMATIC_DEFICIT,                        &
                  NCDF_SNOWMELT, NCDF_TMIN, NCDF_TMAX,                        &
                  NCDF_TMAX_MINUS_TMIN,                                       &
                  NCDF_NET_INFILTRATION, NCDF_REJECTED_NET_INFILTRATION,      &
                  NCDF_INFILTRATION,                                          &
                  NCDF_IRRIGATION, NCDF_RUNOFF_OUTSIDE,                       &
                  NCDF_CROP_ET, NCDF_BARE_SOIL_EVAP, NCDF_GDD,                &
                  NCDF_DIRECT_NET_INFILTRATION, NCDF_DIRECT_SOIL_MOISTURE,    &
                  NCDF_STORM_DRAIN_CAPTURE, NCDF_GROWING_SEASON, NCDF_FOG
  end enum

  logical ( c_bool ) :: OUTPUT_INCLUDES_LATLON = TRUE

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
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

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
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

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

  subroutine finalize_output(cells)

    class (MODEL_DOMAIN_T), intent(inout)   :: cells

    ! [ LOCALS ]
    integer (c_int) :: iStat
    integer (c_int) :: iIndex

    do iIndex = 1, ubound(NC_OUT, 1)

      ! do not initialize a single output file if a series of outputs is desired
      if ( OUTSPECS(iIndex)%multisim_outputs ) cycle

      if ( OUTSPECS(iIndex)%is_active ) then

        call netcdf_rewrite_attribute( NCFILE=NC_OUT( iIndex )%ncfile,                           &
                                       sVariableName=trim( OUTSPECS( iIndex )%variable_name ),   &
                                       sAttributeName='valid_min',                               &
                                       rAttributeValue=[OUTSPECS(iIndex)%valid_minimum])

        call netcdf_rewrite_attribute( NCFILE=NC_OUT( iIndex )%ncfile,                           &
                                       sVariableName=trim( OUTSPECS( iIndex )%variable_name ),   &
                                       sAttributeName='valid_max',                               &
                                       rAttributeValue=[OUTSPECS(iIndex)%valid_maximum])

        call netcdf_rewrite_attribute( NCFILE=NC_OUT( iIndex )%ncfile,                                                  &
                                       sVariableName=trim( OUTSPECS( iIndex )%variable_name ),                          &
                                       sAttributeName='valid_range',                                                    &
                                       rAttributeValue=[OUTSPECS(iIndex)%valid_minimum, OUTSPECS(iIndex)%valid_maximum])
                                       
        call netcdf_close_file( NCFILE=NC_OUT( iIndex )%ncfile )

      endif

    enddo

  end subroutine finalize_output

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
       rValues=[real(SIM_DT%iNumDaysFromOrigin, c_float)])

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
             rValues=[real(SIM_DT%iNumDaysFromOrigin, c_float)])

        endif

      enddo


      if ( OUTSPECS( NCDF_GROSS_PRECIPITATION )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_GROSS_PRECIPITATION )%ncfile,      &
                                    values=cells%gross_precip,                                 &
                                    cells=cells )
        OUTSPECS(NCDF_GROSS_PRECIPITATION)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_GROSS_PRECIPITATION)%valid_maximum, cells%gross_precip)
        OUTSPECS(NCDF_GROSS_PRECIPITATION)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_GROSS_PRECIPITATION)%valid_minimum, cells%gross_precip)

      endif

      if ( OUTSPECS( NCDF_RAINFALL )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_RAINFALL )%ncfile,       &
                                    values=cells%rainfall,                           &
                                    cells=cells )
        OUTSPECS(NCDF_RAINFALL)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_RAINFALL)%valid_maximum, cells%rainfall)
        OUTSPECS(NCDF_RAINFALL)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_RAINFALL)%valid_minimum, cells%rainfall)
                          
      endif
                                    
      if ( OUTSPECS( NCDF_INTERCEPTION )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_INTERCEPTION )%ncfile,   &
                                    values=cells%interception,                       &
                                    cells=cells )
        OUTSPECS(NCDF_INTERCEPTION)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_INTERCEPTION)%valid_maximum, cells%interception)
        OUTSPECS(NCDF_INTERCEPTION)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_INTERCEPTION)%valid_minimum, cells%interception)
                          
      endif

      if ( OUTSPECS( NCDF_INTERCEPTION_STORAGE )%is_active                               &
           .and. (.not. OUTSPECS( NCDF_INTERCEPTION_STORAGE )%multisim_outputs ))  then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_INTERCEPTION_STORAGE )%ncfile,   &
                                    values=cells%interception_storage,                       &
                                    cells=cells )
        OUTSPECS(NCDF_INTERCEPTION_STORAGE)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_INTERCEPTION_STORAGE)%valid_maximum, cells%interception_storage)
        OUTSPECS(NCDF_INTERCEPTION_STORAGE)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_INTERCEPTION_STORAGE)%valid_minimum, cells%interception_storage)
                          
      endif


      if ( OUTSPECS( NCDF_RUNOFF )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_RUNOFF )%ncfile,       &
                                    values=cells%runoff,                           &
                                    cells=cells )
        OUTSPECS(NCDF_RUNOFF)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_RUNOFF)%valid_maximum, cells%runoff)
        OUTSPECS(NCDF_RUNOFF)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_RUNOFF)%valid_minimum, cells%runoff)
                          
      endif


      if ( OUTSPECS( NCDF_RUNOFF_OUTSIDE )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_RUNOFF_OUTSIDE )%ncfile,   &
                                    values=cells%runoff_outside,                       &
                                    cells=cells )
        OUTSPECS(NCDF_RUNOFF_OUTSIDE)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_RUNOFF_OUTSIDE)%valid_maximum, cells%runoff_outside)
        OUTSPECS(NCDF_RUNOFF_OUTSIDE)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_RUNOFF_OUTSIDE)%valid_minimum, cells%runoff_outside)
                          
      endif


      if ( OUTSPECS( NCDF_RUNON )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_RUNON )%ncfile,       &
                                    values=cells%runon,                           &
                                    cells=cells )
        OUTSPECS(NCDF_RUNON)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_RUNON)%valid_maximum, cells%runon)
        OUTSPECS(NCDF_RUNON)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_RUNON)%valid_minimum, cells%runon)
                          
      endif


      if ( OUTSPECS( NCDF_INFILTRATION )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_INFILTRATION )%ncfile,       &
                                    values=cells%infiltration,                           &
                                    cells=cells )
        OUTSPECS(NCDF_INFILTRATION)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_INFILTRATION)%valid_maximum, cells%infiltration)
        OUTSPECS(NCDF_INFILTRATION)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_INFILTRATION)%valid_minimum, cells%infiltration)
                          
      endif

      if ( OUTSPECS( NCDF_SNOWFALL )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_SNOWFALL )%ncfile,       &
                                    values=cells%snowfall,                           &
                                    cells=cells )
        OUTSPECS(NCDF_SNOWFALL)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_SNOWFALL)%valid_maximum, cells%snowfall)
        OUTSPECS(NCDF_SNOWFALL)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_SNOWFALL)%valid_minimum, cells%snowfall)
                          
      endif

      if ( OUTSPECS( NCDF_SNOWMELT )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_SNOWMELT )%ncfile,       &
                                    values=cells%snowmelt,                           &
                                    cells=cells )
        OUTSPECS(NCDF_SNOWMELT)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_SNOWMELT)%valid_maximum, cells%snowmelt)
        OUTSPECS(NCDF_SNOWMELT)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_SNOWMELT)%valid_minimum, cells%snowmelt)
                          
      endif

      if ( OUTSPECS( NCDF_SNOW_STORAGE )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_SNOW_STORAGE )%ncfile,  &
                                    values=cells%snow_storage,                      &
                                    cells=cells )
        OUTSPECS(NCDF_SNOW_STORAGE)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_SNOW_STORAGE)%valid_maximum, cells%snow_storage)
        OUTSPECS(NCDF_SNOW_STORAGE)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_SNOW_STORAGE)%valid_minimum, cells%snow_storage)
                          
      endif

      if ( OUTSPECS( NCDF_SURFACE_STORAGE )%is_active                            &
           .and. (.not. OUTSPECS( NCDF_SURFACE_STORAGE )%multisim_outputs) ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_SURFACE_STORAGE )%ncfile,  &
                                    values=real(cells%surface_storage, c_float),  &
                                    cells=cells )
        OUTSPECS(NCDF_SURFACE_STORAGE)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_SURFACE_STORAGE)%valid_maximum, real(cells%surface_storage, c_float))
        OUTSPECS(NCDF_SURFACE_STORAGE)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_SURFACE_STORAGE)%valid_minimum, real(cells%surface_storage, c_float))
                          
      endif

      if ( OUTSPECS( NCDF_SOIL_STORAGE )%is_active                             &
           .and. (.not. OUTSPECS( NCDF_SOIL_STORAGE )%multisim_outputs) ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_SOIL_STORAGE )%ncfile,  &
                                    values=real(cells%soil_storage, c_float),  &
                                    cells=cells )
        OUTSPECS(NCDF_SOIL_STORAGE)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_SOIL_STORAGE)%valid_maximum, real(cells%soil_storage, c_float))
        OUTSPECS(NCDF_SOIL_STORAGE)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_SOIL_STORAGE)%valid_minimum, real(cells%soil_storage, c_float))
                          
      endif

      if ( OUTSPECS( NCDF_DELTA_SOIL_STORAGE )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_DELTA_SOIL_STORAGE )%ncfile,  &
                                    values=cells%delta_soil_storage,                      &
                                    cells=cells )
        OUTSPECS(NCDF_DELTA_SOIL_STORAGE)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_DELTA_SOIL_STORAGE)%valid_maximum, cells%delta_soil_storage)
        OUTSPECS(NCDF_DELTA_SOIL_STORAGE)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_DELTA_SOIL_STORAGE)%valid_minimum, cells%delta_soil_storage)
                          
      endif

      if ( OUTSPECS( NCDF_NET_INFILTRATION )%is_active                             &
           .and. (.not. OUTSPECS( NCDF_NET_INFILTRATION )%multisim_outputs) ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_NET_INFILTRATION )%ncfile,  &
                                    values=cells%net_infiltration,                      &
                                    cells=cells )
        OUTSPECS(NCDF_NET_INFILTRATION)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_NET_INFILTRATION)%valid_maximum, cells%net_infiltration)
        OUTSPECS(NCDF_NET_INFILTRATION)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_NET_INFILTRATION)%valid_minimum, cells%net_infiltration)
                          
      endif

      if ( OUTSPECS( NCDF_REFERENCE_ET0 )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_REFERENCE_ET0 )%ncfile,    &
                                    values=real(cells%reference_ET0, c_float),         &
                                    cells=cells )
        OUTSPECS(NCDF_REFERENCE_ET0)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_REFERENCE_ET0)%valid_maximum, real(cells%reference_et0, c_float))
        OUTSPECS(NCDF_REFERENCE_ET0)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_REFERENCE_ET0)%valid_minimum, real(cells%reference_et0, c_float))
                          
      endif

      if ( OUTSPECS( NCDF_ACTUAL_ET )%is_active                                 &
           .and. (.not. OUTSPECS( NCDF_ACTUAL_ET )%multisim_outputs) ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_ACTUAL_ET )%ncfile,  &
                                    values=real(cells%actual_et, c_float),       &
                                    cells=cells )
        OUTSPECS(NCDF_ACTUAL_ET)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_ACTUAL_ET)%valid_maximum, real(cells%actual_et, c_float))
        OUTSPECS(NCDF_ACTUAL_ET)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_ACTUAL_ET)%valid_minimum, real(cells%actual_et, c_float))
                          
      endif

      if ( OUTSPECS( NCDF_CLIMATIC_DEFICIT )%is_active                                 &
           .and. (.not. OUTSPECS( NCDF_CLIMATIC_DEFICIT )%multisim_outputs) ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_CLIMATIC_DEFICIT )%ncfile,  &
                                    values=real(cells%climatic_deficit, c_float),       &
                                    cells=cells )
        OUTSPECS(NCDF_CLIMATIC_DEFICIT)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_CLIMATIC_DEFICIT)%valid_maximum, real(cells%climatic_deficit, c_float))
        OUTSPECS(NCDF_CLIMATIC_DEFICIT)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_CLIMATIC_DEFICIT)%valid_minimum, real(cells%climatic_deficit, c_float))
                          
      endif

      if ( OUTSPECS( NCDF_TMIN )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_TMIN )%ncfile,       &
                                    values=cells%tmin,                           &
                                    cells=cells )
        OUTSPECS(NCDF_TMIN)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_TMIN)%valid_maximum, cells%tmin)
        OUTSPECS(NCDF_TMIN)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_TMIN)%valid_minimum, cells%tmin)
                          
      endif

      if ( OUTSPECS( NCDF_TMAX )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_TMAX )%ncfile,       &
                                    values=cells%tmax,                           &
                                    cells=cells )
        OUTSPECS(NCDF_TMAX)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_TMAX)%valid_maximum, cells%tmax)
        OUTSPECS(NCDF_TMAX)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_TMAX)%valid_minimum, cells%tmax)
                          
      endif

      if ( OUTSPECS( NCDF_TMAX_MINUS_TMIN )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_TMAX_MINUS_TMIN )%ncfile,  &
                                    values=cells%tmax_minus_tmin,                           &
                                    cells=cells )
        OUTSPECS(NCDF_TMAX)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_TMAX)%valid_maximum, cells%tmax_minus_tmin)
        OUTSPECS(NCDF_TMAX)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_TMAX)%valid_minimum, cells%tmax_minus_tmin)
                          
      endif

      if ( OUTSPECS( NCDF_IRRIGATION )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_IRRIGATION )%ncfile,       &
                                    values=cells%irrigation,                           &
                                    cells=cells )
        OUTSPECS(NCDF_IRRIGATION)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_IRRIGATION)%valid_maximum, cells%irrigation)
        OUTSPECS(NCDF_IRRIGATION)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_IRRIGATION)%valid_minimum, cells%irrigation)
                          
      endif


      if ( OUTSPECS( NCDF_GDD )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_GDD )%ncfile,       &
                                    values=cells%gdd,                           &
                                    cells=cells )
        OUTSPECS(NCDF_GDD)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_GDD)%valid_maximum, cells%gdd)
        OUTSPECS(NCDF_GDD)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_GDD)%valid_minimum, cells%gdd)
                          
      endif

      if ( OUTSPECS( NCDF_REJECTED_NET_INFILTRATION )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_REJECTED_NET_INFILTRATION )%ncfile,       &
                                    values=cells%rejected_net_infiltration,                           &
                                    cells=cells )
        OUTSPECS(NCDF_REJECTED_NET_INFILTRATION)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_REJECTED_NET_INFILTRATION)%valid_maximum, cells%rejected_net_infiltration)
        OUTSPECS(NCDF_REJECTED_NET_INFILTRATION)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_REJECTED_NET_INFILTRATION)%valid_minimum, cells%rejected_net_infiltration)
                          
      endif

      if ( OUTSPECS( NCDF_CROP_ET )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_CROP_ET )%ncfile,       &
                                    values=cells%crop_etc,                          &
                                    cells=cells )
        OUTSPECS(NCDF_CROP_ET)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_CROP_ET)%valid_maximum, cells%crop_etc)
        OUTSPECS(NCDF_CROP_ET)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_CROP_ET)%valid_minimum, cells%crop_etc)
                          
      endif

      if ( OUTSPECS( NCDF_BARE_SOIL_EVAP )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_BARE_SOIL_EVAP )%ncfile,       &
                                    values=cells%bare_soil_evap,                           &
                                    cells=cells )
        OUTSPECS(NCDF_BARE_SOIL_EVAP)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_BARE_SOIL_EVAP)%valid_maximum, cells%bare_soil_evap)
        OUTSPECS(NCDF_BARE_SOIL_EVAP)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_BARE_SOIL_EVAP)%valid_minimum, cells%bare_soil_evap)
                          
      endif

      if ( OUTSPECS( NCDF_DIRECT_NET_INFILTRATION )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_DIRECT_NET_INFILTRATION )%ncfile,   &
                                    values=cells%direct_net_infiltration,                       &
                                    cells=cells )
        OUTSPECS(NCDF_DIRECT_NET_INFILTRATION)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_DIRECT_NET_INFILTRATION)%valid_maximum, cells%direct_net_infiltration)
        OUTSPECS(NCDF_DIRECT_NET_INFILTRATION)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_DIRECT_NET_INFILTRATION)%valid_minimum, cells%direct_net_infiltration)
                          
      endif

      if ( OUTSPECS( NCDF_DIRECT_SOIL_MOISTURE )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_DIRECT_SOIL_MOISTURE )%ncfile,       &
                                    values=cells%direct_soil_moisture,                           &
                                    cells=cells )
        OUTSPECS(NCDF_DIRECT_SOIL_MOISTURE)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_DIRECT_SOIL_MOISTURE)%valid_maximum, cells%direct_soil_moisture)
        OUTSPECS(NCDF_DIRECT_SOIL_MOISTURE)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_DIRECT_SOIL_MOISTURE)%valid_minimum, cells%direct_soil_moisture)
                          
      endif

      if ( OUTSPECS( NCDF_STORM_DRAIN_CAPTURE )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_STORM_DRAIN_CAPTURE )%ncfile,        &
                                    values=cells%storm_drain_capture,                            &
                                    cells=cells )
        OUTSPECS(NCDF_STORM_DRAIN_CAPTURE)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_STORM_DRAIN_CAPTURE)%valid_maximum, cells%storm_drain_capture)
        OUTSPECS(NCDF_STORM_DRAIN_CAPTURE)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_STORM_DRAIN_CAPTURE)%valid_minimum, cells%storm_drain_capture)
                          
      endif

      if ( OUTSPECS( NCDF_GROWING_SEASON )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_GROWING_SEASON )%ncfile,        &
                                    values=asFloat(cells%it_is_growing_season),             &
                                    cells=cells )
        OUTSPECS(NCDF_GROWING_SEASON)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_GROWING_SEASON)%valid_maximum, asFloat(cells%it_is_growing_season))
        OUTSPECS(NCDF_GROWING_SEASON)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_GROWING_SEASON)%valid_minimum, asFloat(cells%it_is_growing_season))
                          
      endif


      if ( OUTSPECS( NCDF_FOG )%is_active ) then

        call output_2D_float_array( ncfile_ptr=NC_OUT( NCDF_FOG )%ncfile,        &
                                    values=cells%fog,                            &
                                    cells=cells )
        OUTSPECS(NCDF_FOG)%valid_maximum =         &
          update_maximum_value(OUTSPECS(NCDF_FOG)%valid_maximum, cells%fog)
        OUTSPECS(NCDF_FOG)%valid_minimum =         &
          update_minimum_value(OUTSPECS(NCDF_FOG)%valid_minimum, cells%fog)

      endif

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
           rValues=[real(SIM_DT%iNumDaysFromOrigin, c_float)])

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

  pure function update_minimum_value(current_minimum, values)    result(new_minimum)

    real (c_float), intent(in)             :: current_minimum
    real (c_float), intent(in)             :: values(:)

    real (c_float)                         :: new_minimum

    new_minimum = min(current_minimum, minval(values))

  end function update_minimum_value

!------------------------------------------------------------------------------

  pure function update_maximum_value(current_maximum, values)    result(new_maximum)

    real (c_float), intent(in)             :: current_maximum
    real (c_float), intent(in)             :: values(:)

    real (c_float)                         :: new_maximum

    new_maximum = max(current_maximum, maxval(values))

  end function update_maximum_value

end module output
