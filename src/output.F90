module output

  use constants_and_conversions
  use exceptions, only            : warn, assert, die
  use model_domain, only          : MODEL_DOMAIN_T
  use iso_c_binding, only         : c_short, c_int, c_float, c_double, c_size_t, c_ptrdiff_t
  use netcdf4_support
  use simulation_datetime, only   : SIM_DT
  implicit none

  private

  public :: initialize_output, set_output_directory, write_output, set_output_prefix, &
            set_output_latlon_option, RECHARGE_ARRAY

  real( kind=c_double ), allocatable      :: RECHARGE_ARRAY(:)

  type, public :: NETCDF_FILE_COLLECTION_T
    type (T_NETCDF4_FILE), pointer, public :: ncfile
  end type NETCDF_FILE_COLLECTION_T

  type (NETCDF_FILE_COLLECTION_T), allocatable, public :: NC_OUT(:)

  integer (kind=c_int), parameter   :: NCDF_NUM_OUTPUTS = 20

  type OUTPUT_SPECS_T
    character (len=27)          :: variable_name
    character (len=21)          :: variable_units
    real (kind=c_float)         :: valid_minimum
    real (kind=c_float)         :: valid_maximum
  end type OUTPUT_SPECS_T

  type (OUTPUT_SPECS_T)    :: OUTSPECS(NCDF_NUM_OUTPUTS) = [                                 &
    OUTPUT_SPECS_T( "gross_precipitation        ", "inches_per_day       ", 0.0, 60.0 ),        &
    OUTPUT_SPECS_T( "rainfall                   ", "inches_per_day       ", 0.0, 60.0 ),        &
    OUTPUT_SPECS_T( "snowfall                   ", "inches_per_day       ", 0.0, 60.0 ),        &
    OUTPUT_SPECS_T( "interception               ", "inches_per_day       ", 0.0, 60.0 ),        &
    OUTPUT_SPECS_T( "runon                      ", "inches_per_day       ", 0.0, 10000.0 ),     &
    OUTPUT_SPECS_T( "runoff                     ", "inches_per_day       ", 0.0, 10000.0 ),     &
    OUTPUT_SPECS_T( "snow_storage               ", "inches_per_day       ", 0.0, 2000.0 ),      &
    OUTPUT_SPECS_T( "soil_storage               ", "inches_per_day       ", 0.0, 2000.0 ),      &
    OUTPUT_SPECS_T( "reference_ET0              ", "inches_per_day       ", 0.0, 2000.0 ),      &
    OUTPUT_SPECS_T( "actual_et                  ", "inches_per_day       ", 0.0, 2000.0 ),      &
    OUTPUT_SPECS_T( "snowmelt                   ", "inches_per_day       ", 0.0, 2000.0 ),      &
    OUTPUT_SPECS_T( "tmin                       ", "degrees_fahrenheit   ", -100.0, 150.0 ),    &
    OUTPUT_SPECS_T( "tmax                       ", "degrees_fahrenheit   ", -100.0, 150.0 ),    &
    OUTPUT_SPECS_T( "potential_recharge         ", "inches_per_day       ", 0.0, 2000.0 ),      &
    OUTPUT_SPECS_T( "rejected_potential_recharge", "inches_per_day       ", 0.0, 5000.0 ),      &
    OUTPUT_SPECS_T( "infiltration               ", "inches_per_day       ", 0.0, 2000.0 ),      &
    OUTPUT_SPECS_T( "irrigation                 ", "inches_per_day       ", 0.0, 2000.0 ),      &
    OUTPUT_SPECS_T( "runoff_outside             ", "inches_per_day       ", 0.0, 10000.0 ),     &
    OUTPUT_SPECS_T( "crop_et                    ", "inches_per_day       ", 0.0, 10000.0 ),     &
    OUTPUT_SPECS_T( "gdd                        ", "degree_day_fahrenheit", 0.0, 10000.0 )   ]

  enum, bind(c)
    enumerator :: NCDF_GROSS_PRECIPITATION=1, NCDF_RAINFALL, NCDF_SNOWFALL,   &
                  NCDF_INTERCEPTION, NCDF_RUNON, NCDF_RUNOFF,                 &
                  NCDF_SNOW_STORAGE, NCDF_SOIL_STORAGE,                       &
                  NCDF_REFERENCE_ET0,                                         &
                  NCDF_ACTUAL_ET, NCDF_SNOWMELT, NCDF_TMIN, NCDF_TMAX,        &
                  NCDF_POTENTIAL_RECHARGE, NCDF_REJECTED_POTENTIAL_RECHARGE,  &
                  NCDF_INFILTRATION,                                          &
                  NCDF_IRRIGATION, NCDF_RUNOFF_OUTSIDE,                       &
                  NCDF_CROP_ET, NCDF_GDD
  end enum

  logical ( kind=c_bool ) :: OUTPUT_INCLUDES_LATLON = lTRUE

contains

  subroutine set_output_latlon_option( output_includes_latlon_ )

    logical (kind=c_bool) :: output_includes_latlon_

    OUTPUT_INCLUDES_LATLON = output_includes_latlon_

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

      enddo

    else

      do iIndex = 1, ubound(NC_OUT, 1)

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

      enddo

    endif

    cells%nodata_fill_value = NC_FILL_FLOAT

    allocate( RECHARGE_ARRAY( size( cells%potential_recharge, 1 ) ) )

  end subroutine initialize_output

!--------------------------------------------------------------------------------------------------

  subroutine write_output(cells)

    class (MODEL_DOMAIN_T), intent(inout)  :: cells

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    ! first put out the current time variable for all open NetCDF files
    do iIndex = 1, ubound( NC_OUT, 1 )


      call netcdf_put_variable_vector(NCFILE=NC_OUT(iIndex)%ncfile, &
         iVarID=NC_OUT(iIndex)%ncfile%iVarID(NC_TIME), &
         iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t)], &
         iCount=[1_c_size_t], &
         iStride=[1_c_ptrdiff_t], &
         dpValues=[real(SIM_DT%iNumDaysFromOrigin, kind=c_double)])

    enddo

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_GROSS_PRECIPITATION )%ncfile,     &
            iVarID=NC_OUT( NCDF_GROSS_PRECIPITATION )%ncfile%iVarID(NC_Z),                      &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%gross_precip,                                                          &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_RAINFALL )%ncfile,                &
            iVarID=NC_OUT( NCDF_RAINFALL )%ncfile%iVarID(NC_Z),                                 &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                      &
                                int(cells%number_of_columns, kind=c_size_t) ],                  &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                 &
            rValues=cells%rainfall,                                                             &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_INTERCEPTION )%ncfile,            &
            iVarID=NC_OUT( NCDF_INTERCEPTION )%ncfile%iVarID(NC_Z),                             &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%interception,                                                          &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_RUNOFF )%ncfile,                  &
            iVarID=NC_OUT( NCDF_RUNOFF )%ncfile%iVarID(NC_Z),                                   &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%runoff,                                                                &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_RUNOFF_OUTSIDE )%ncfile,          &
            iVarID=NC_OUT( NCDF_RUNOFF_OUTSIDE )%ncfile%iVarID(NC_Z),                           &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%runoff_outside,                                                        &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_RUNON )%ncfile,                   &
            iVarID=NC_OUT( NCDF_RUNON )%ncfile%iVarID(NC_Z),                                    &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%runon,                                                                 &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_INFILTRATION )%ncfile,            &
            iVarID=NC_OUT( NCDF_INFILTRATION )%ncfile%iVarID(NC_Z),                             &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%infiltration,                                                          &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_SNOWFALL )%ncfile,                &
            iVarID=NC_OUT( NCDF_SNOWFALL )%ncfile%iVarID(NC_Z),                                 &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%snowfall,                                                              &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_SNOWMELT )%ncfile,                &
            iVarID=NC_OUT( NCDF_SNOWMELT )%ncfile%iVarID(NC_Z),                                 &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%snowmelt,                                                              &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_SNOW_STORAGE )%ncfile,            &
            iVarID=NC_OUT( NCDF_SNOW_STORAGE )%ncfile%iVarID(NC_Z),                             &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%snow_storage,                                                          &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_SOIL_STORAGE )%ncfile,            &
            iVarID=NC_OUT( NCDF_SOIL_STORAGE )%ncfile%iVarID(NC_Z),                             &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%soil_storage,                                                          &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_POTENTIAL_RECHARGE )%ncfile,      &
            iVarID=NC_OUT( NCDF_POTENTIAL_RECHARGE )%ncfile%iVarID(NC_Z),                       &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%potential_recharge,                                                    &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_REFERENCE_ET0 )%ncfile,           &
            iVarID=NC_OUT( NCDF_REFERENCE_ET0 )%ncfile%iVarID(NC_Z),                            &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%reference_ET0,                                                         &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_ACTUAL_ET )%ncfile,               &
            iVarID=NC_OUT( NCDF_ACTUAL_ET )%ncfile%iVarID(NC_Z),                                &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%actual_et,                                                             &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_TMIN )%ncfile,                    &
            iVarID=NC_OUT( NCDF_TMIN )%ncfile%iVarID(NC_Z),                                     &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%tmin,                                                                  &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_TMAX )%ncfile,                    &
            iVarID=NC_OUT( NCDF_TMAX )%ncfile%iVarID(NC_Z),                                     &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%tmax,                                                                  &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_IRRIGATION )%ncfile,              &
            iVarID=NC_OUT( NCDF_IRRIGATION )%ncfile%iVarID(NC_Z),                               &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=cells%active,                                                                  &
            rValues=cells%irrigation,                                                            &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_GDD )%ncfile,                      &
            iVarID=NC_OUT( NCDF_GDD )%ncfile%iVarID(NC_Z),                                       &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],     &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                             &
            lMask=cells%active,                                                                  &
            rValues=cells%gdd,                                                                   &
            rField=cells%nodata_fill_value )

    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_REJECTED_POTENTIAL_RECHARGE )%ncfile, &
            iVarID=NC_OUT( NCDF_REJECTED_POTENTIAL_RECHARGE )%ncfile%iVarID(NC_Z),                  &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],        &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                          &
                                int(cells%number_of_columns, kind=c_size_t) ],                      &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                                &
            lMask=cells%active,                                                                     &
            rValues=cells%rejected_potential_recharge,                                              &
            rField=cells%nodata_fill_value )


    call netcdf_put_packed_variable_array(NCFILE=NC_OUT( NCDF_CROP_ET )%ncfile,                     &
            iVarID=NC_OUT( NCDF_CROP_ET )%ncfile%iVarID(NC_Z),                                      &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],        &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                          &
                                int(cells%number_of_columns, kind=c_size_t) ],                      &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                                &
            lMask=cells%active,                                                                     &
            rValues=cells%crop_etc,                                                                 &
            rField=cells%nodata_fill_value )

    RECHARGE_ARRAY = RECHARGE_ARRAY + cells%potential_recharge

  end subroutine write_output

end module output
