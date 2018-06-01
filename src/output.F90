module output

  use constants_and_conversions
  use exceptions, only            : warn, assert, die
  use model_domain, only          : MODEL_DOMAIN_T
  use iso_c_binding
  use netcdf4_support
  use simulation_datetime, only   : SIM_DT
  implicit none

  private

  public :: initialize_output, set_output_directory, write_output, set_output_prefix, &
            set_output_latlon_option, RECHARGE_ARRAY, OUTSPECS

! supply apparently missing parameter values from Intel implementation of ISO_C_BINDING
#ifdef __INTEL_COMPILER
  integer, parameter :: c_ptrdiff_t = 8
  integer, parameter :: c_diff_t = 8
#endif

  real( kind=c_double ), allocatable      :: RECHARGE_ARRAY(:)

  type, public :: NETCDF_FILE_COLLECTION_T
    type (T_NETCDF4_FILE), pointer, public :: ncfile
  end type NETCDF_FILE_COLLECTION_T

  type (NETCDF_FILE_COLLECTION_T), allocatable, public :: NC_OUT(:)

  integer (kind=c_int), parameter   :: NCDF_NUM_OUTPUTS = 24

  type OUTPUT_SPECS_T
    character (len=27)          :: variable_name
    character (len=21)          :: variable_units
    real (kind=c_float)         :: valid_minimum
    real (kind=c_float)         :: valid_maximum
    logical (kind=c_bool)       :: is_active
  end type OUTPUT_SPECS_T

  type (OUTPUT_SPECS_T)    :: OUTSPECS(NCDF_NUM_OUTPUTS) = [                                       &
    OUTPUT_SPECS_T( "gross_precipitation        ", "inches               ", 0.0, 60.0, TRUE ),     &
    OUTPUT_SPECS_T( "rainfall                   ", "inches               ", 0.0, 60.0, TRUE  ),    &
    OUTPUT_SPECS_T( "snowfall                   ", "inches               ", 0.0, 60.0, TRUE  ),    &
    OUTPUT_SPECS_T( "interception               ", "inches               ", 0.0, 60.0, TRUE  ),    &
    OUTPUT_SPECS_T( "runon                      ", "inches               ", 0.0, 10000.0, TRUE  ), &
    OUTPUT_SPECS_T( "runoff                     ", "inches               ", 0.0, 10000.0, TRUE  ), &
    OUTPUT_SPECS_T( "snow_storage               ", "inches               ", 0.0, 2000.0, FALSE ),  &
    OUTPUT_SPECS_T( "soil_storage               ", "inches               ", 0.0, 2000.0, FALSE ),  &
    OUTPUT_SPECS_T( "delta_soil_storage         ", "inches               ", 0.0, 5.0, FALSE  ),    &
    OUTPUT_SPECS_T( "reference_ET0              ", "inches               ", 0.0, 2000.0, TRUE  ),  &
    OUTPUT_SPECS_T( "actual_et                  ", "inches               ", 0.0, 2000.0, TRUE  ),  &
    OUTPUT_SPECS_T( "snowmelt                   ", "inches               ", 0.0, 2000.0, FALSE ),  &
    OUTPUT_SPECS_T( "tmin                       ", "degrees_fahrenheit   ", -100.0, 150.0, TRUE  ),&
    OUTPUT_SPECS_T( "tmax                       ", "degrees_fahrenheit   ", -100.0, 150.0, TRUE  ),&
    OUTPUT_SPECS_T( "net_infiltration           ", "inches               ", 0.0, 2000.0, TRUE  ),  &
    OUTPUT_SPECS_T( "rejected_net_infiltration  ", "inches               ", 0.0, 5000.0, TRUE  ),  &
    OUTPUT_SPECS_T( "infiltration               ", "inches               ", 0.0, 2000.0, FALSE  ), &
    OUTPUT_SPECS_T( "irrigation                 ", "inches               ", 0.0, 2000.0, TRUE  ),  &
    OUTPUT_SPECS_T( "runoff_outside             ", "inches               ", 0.0, 10000.0, TRUE  ), &
    OUTPUT_SPECS_T( "crop_et                    ", "inches               ", 0.0, 10000.0, FALSE ), &
    OUTPUT_SPECS_T( "gdd                        ", "degree_day_fahrenheit", 0.0, 10000.0, FALSE ), &
    OUTPUT_SPECS_T( "direct_net_infiltation     ", "inches               ", 0.0, 100.0, FALSE ),   &
    OUTPUT_SPECS_T( "direct_soil_moisture       ", "inches               ", 0.0, 100.0, FALSE ),   &
    OUTPUT_SPECS_T( "storm_drain_capture        ", "inches               ", 0.0, 100.0, FALSE )      ]

  enum, bind(c)
    enumerator :: NCDF_GROSS_PRECIPITATION=1, NCDF_RAINFALL, NCDF_SNOWFALL,   &
                  NCDF_INTERCEPTION, NCDF_RUNON, NCDF_RUNOFF,                 &
                  NCDF_SNOW_STORAGE, NCDF_SOIL_STORAGE,                       &
                  NCDF_DELTA_SOIL_STORAGE,                                    &
                  NCDF_REFERENCE_ET0,                                         &
                  NCDF_ACTUAL_ET, NCDF_SNOWMELT, NCDF_TMIN, NCDF_TMAX,        &
                  NCDF_NET_INFILTRATION, NCDF_REJECTED_NET_INFILTRATION,      &
                  NCDF_INFILTRATION,                                          &
                  NCDF_IRRIGATION, NCDF_RUNOFF_OUTSIDE,                       &
                  NCDF_CROP_ET, NCDF_GDD, NCDF_DIRECT_NET_INFILTRATION,       &
                  NCDF_DIRECT_SOIL_MOISTURE, NCDF_STORM_DRAIN_CAPTURE
  end enum

  logical ( kind=c_bool ) :: OUTPUT_INCLUDES_LATLON = lTRUE

contains

  subroutine set_output_latlon_option( output_includes_latlon_l )

    logical (kind=c_bool) :: output_includes_latlon_l

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
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iIndex

    allocate ( NC_OUT( NCDF_NUM_OUTPUTS ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    if ( .not. allocated( OUTPUT_DIRECTORY_NAME ) ) OUTPUT_DIRECTORY_NAME = ""
    if ( .not. allocated( OUTPUT_PREFIX_NAME ) ) OUTPUT_PREFIX_NAME       = ""

    if ( OUTPUT_INCLUDES_LATLON ) then

      do iIndex = 1, ubound(NC_OUT, 1)

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

    allocate( RECHARGE_ARRAY( size( cells%net_infiltration, 1 ) ) )

  end subroutine initialize_output

!--------------------------------------------------------------------------------------------------

  subroutine write_output(cells)

    class (MODEL_DOMAIN_T), intent(inout)  :: cells

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    ! first put out the current time variable for all open NetCDF files
    do iIndex = 1, ubound( NC_OUT, 1 )

      if ( OUTSPECS(iIndex)%is_active ) then

        call netcdf_put_variable_vector(NCFILE=NC_OUT(iIndex)%ncfile, &
           iVarID=NC_OUT(iIndex)%ncfile%iVarID(NC_TIME), &
           iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t)], &
           iCount=[1_c_size_t], &
           iStride=[1_c_ptrdiff_t], &
           dpValues=[real(SIM_DT%iNumDaysFromOrigin, kind=c_double)])

      endif

    enddo

    if ( OUTSPECS( NCDF_GROSS_PRECIPITATION )%is_active ) &
      call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_GROSS_PRECIPITATION )%ncfile,   &
            iVarID=NC_OUT( NCDF_GROSS_PRECIPITATION )%ncfile%iVarID(NC_Z),                      &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                      &
                                int(cells%number_of_columns, kind=c_size_t) ],                  &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                 &
            rValues=cells%gross_precip,                                                         &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_RAINFALL )%is_active ) &
      call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_RAINFALL )%ncfile,              &
            iVarID=NC_OUT( NCDF_RAINFALL )%ncfile%iVarID(NC_Z),                                 &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                      &
                                int(cells%number_of_columns, kind=c_size_t) ],                  &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                 &
            rValues=cells%rainfall,                                                             &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_INTERCEPTION )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_INTERCEPTION )%ncfile,            &
            iVarID=NC_OUT( NCDF_INTERCEPTION )%ncfile%iVarID(NC_Z),                             &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%interception,                                                          &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_RUNOFF )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_RUNOFF )%ncfile,                  &
            iVarID=NC_OUT( NCDF_RUNOFF )%ncfile%iVarID(NC_Z),                                   &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%runoff,                                                                &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_RUNOFF_OUTSIDE )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_RUNOFF_OUTSIDE )%ncfile,          &
            iVarID=NC_OUT( NCDF_RUNOFF_OUTSIDE )%ncfile%iVarID(NC_Z),                           &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%runoff_outside,                                                        &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_RUNON )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_RUNON )%ncfile,                   &
            iVarID=NC_OUT( NCDF_RUNON )%ncfile%iVarID(NC_Z),                                    &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%runon,                                                                 &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_INFILTRATION )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_INFILTRATION )%ncfile,            &
            iVarID=NC_OUT( NCDF_INFILTRATION )%ncfile%iVarID(NC_Z),                             &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%infiltration,                                                          &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_SNOWFALL )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_SNOWFALL )%ncfile,                &
            iVarID=NC_OUT( NCDF_SNOWFALL )%ncfile%iVarID(NC_Z),                                 &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%snowfall,                                                              &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_SNOWMELT )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_SNOWMELT )%ncfile,                &
            iVarID=NC_OUT( NCDF_SNOWMELT )%ncfile%iVarID(NC_Z),                                 &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%snowmelt,                                                              &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_SNOW_STORAGE )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_SNOW_STORAGE )%ncfile,            &
            iVarID=NC_OUT( NCDF_SNOW_STORAGE )%ncfile%iVarID(NC_Z),                             &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%snow_storage,                                                          &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_SOIL_STORAGE )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_SOIL_STORAGE )%ncfile,             &
            iVarID=NC_OUT( NCDF_SOIL_STORAGE )%ncfile%iVarID(NC_Z),                              &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],     &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                             &
            lMask=cells%active,                                                                  &
            rValues=real(cells%soil_storage, kind=c_float),                                      &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_DELTA_SOIL_STORAGE )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_DELTA_SOIL_STORAGE )%ncfile,       &
            iVarID=NC_OUT( NCDF_SOIL_STORAGE )%ncfile%iVarID(NC_Z),                              &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],     &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                             &
            lMask=cells%active,                                                                  &
            rValues=cells%delta_soil_storage,                                                    &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_NET_INFILTRATION )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_NET_INFILTRATION )%ncfile,      &
            iVarID=NC_OUT( NCDF_NET_INFILTRATION )%ncfile%iVarID(NC_Z),                       &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%net_infiltration,                                                    &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_REFERENCE_ET0 )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_REFERENCE_ET0 )%ncfile,           &
            iVarID=NC_OUT( NCDF_REFERENCE_ET0 )%ncfile%iVarID(NC_Z),                            &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%reference_ET0,                                                         &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_ACTUAL_ET )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_ACTUAL_ET )%ncfile,               &
            iVarID=NC_OUT( NCDF_ACTUAL_ET )%ncfile%iVarID(NC_Z),                                &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%actual_et,                                                             &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_TMIN )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_TMIN )%ncfile,                    &
            iVarID=NC_OUT( NCDF_TMIN )%ncfile%iVarID(NC_Z),                                     &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%tmin,                                                                  &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_TMAX )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_TMAX )%ncfile,                    &
            iVarID=NC_OUT( NCDF_TMAX )%ncfile%iVarID(NC_Z),                                     &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%tmax,                                                                  &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_IRRIGATION )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_IRRIGATION )%ncfile,              &
            iVarID=NC_OUT( NCDF_IRRIGATION )%ncfile%iVarID(NC_Z),                               &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%irrigation,                                                            &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_GDD )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_GDD )%ncfile,                      &
            iVarID=NC_OUT( NCDF_GDD )%ncfile%iVarID(NC_Z),                                       &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],     &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                             &
            lMask=cells%active,                                                                  &
            rValues=cells%gdd,                                                                   &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_REJECTED_NET_INFILTRATION )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_REJECTED_NET_INFILTRATION )%ncfile, &
            iVarID=NC_OUT( NCDF_REJECTED_NET_INFILTRATION )%ncfile%iVarID(NC_Z),                  &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],        &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                          &
                                int(cells%number_of_columns, kind=c_size_t) ],                      &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                                &
            lMask=cells%active,                                                                     &
            rValues=cells%rejected_net_infiltration,                                              &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_CROP_ET )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_CROP_ET )%ncfile,                     &
            iVarID=NC_OUT( NCDF_CROP_ET )%ncfile%iVarID(NC_Z),                                      &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],        &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                          &
                                int(cells%number_of_columns, kind=c_size_t) ],                      &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                                &
            lMask=cells%active,                                                                     &
            rValues=cells%crop_etc,                                                                 &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_DIRECT_NET_INFILTRATION )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_DIRECT_NET_INFILTRATION )%ncfile,     &
            iVarID=NC_OUT( NCDF_DIRECT_NET_INFILTRATION)%ncfile%iVarID(NC_Z),                       &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],        &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                          &
                                int(cells%number_of_columns, kind=c_size_t) ],                      &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                                &
            lMask=cells%active,                                                                     &
            rValues=cells%direct_net_infiltration,                                                  &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_DIRECT_SOIL_MOISTURE )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_DIRECT_SOIL_MOISTURE )%ncfile,        &
            iVarID=NC_OUT( NCDF_DIRECT_SOIL_MOISTURE )%ncfile%iVarID(NC_Z),                         &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],        &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                          &
                                int(cells%number_of_columns, kind=c_size_t) ],                      &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                                &
            lMask=cells%active,                                                                     &
            rValues=cells%direct_soil_moisture,                                                     &
            rField=cells%nodata_fill_value )

    if ( OUTSPECS( NCDF_STORM_DRAIN_CAPTURE )%is_active ) &
    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_STORM_DRAIN_CAPTURE )%ncfile,         &
            iVarID=NC_OUT( NCDF_STORM_DRAIN_CAPTURE )%ncfile%iVarID(NC_Z),                          &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],        &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                          &
                                int(cells%number_of_columns, kind=c_size_t) ],                      &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                                &
            lMask=cells%active,                                                                     &
            rValues=cells%storm_drain_capture,                                                     &
            rField=cells%nodata_fill_value )

    RECHARGE_ARRAY = RECHARGE_ARRAY + cells%net_infiltration

  end subroutine write_output

end module output
