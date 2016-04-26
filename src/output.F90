module output

  use constants_and_conversions
  use exceptions, only            : warn, assert, die
  use model_domain, only          : MODEL_DOMAIN_T
  use iso_c_binding, only         : c_short, c_int, c_float, c_double, c_size_t, c_ptrdiff_t
  use netcdf4_support
  use simulation_datetime, only   : SIM_DT 
  implicit none

  private

  public :: initialize_output, set_output_directory, write_output
  public :: OUTPUT_DIRECTORY_NAME

  type, public :: NETCDF_FILE_COLLECTION_T
    type (T_NETCDF4_FILE), pointer, public :: ncfile
  end type NETCDF_FILE_COLLECTION_T

  type (NETCDF_FILE_COLLECTION_T), allocatable, public :: NC_OUT(:)

  character (len=64)   :: OUTPUT_DIRECTORY_NAME = ""

  integer (kind=c_int), parameter   :: NCDF_NUM_OUTPUTS = 17

  character (len=32), parameter :: NCDF_OUTPUT_NAMES( NCDF_NUM_OUTPUTS ) = [   &
              "gross_precipitation             ",                              &
              "rainfall                        ",                              &
              "snowfall                        ",                              &
              "interception                    ",                              &
              "runon                           ",                              &
              "runoff                          ",                              &              
              "snow_storage                    ",                              &
              "soil_storage                    ",                              &
              "reference_ET0                   ",                              &
              "actual_et                       ",                              &
              "snowmelt                        ",                              &
              "tmin                            ",                              &
              "tmax                            ",                              &
              "potential_recharge              ",                              &
              "infiltration                    ",                              &
              "irrigation                      ",                              &
              "runoff_outside                  "  ]

  enum, bind(c)
    enumerator :: NCDF_GROSS_PRECIPITATION=1, NCDF_RAINFALL, NCDF_SNOWFALL, &
                  NCDF_INTERCEPTION, NCDF_RUNON, NCDF_RUNOFF,               &
                  NCDF_SNOW_STORAGE, NCDF_SOIL_STORAGE,                     &
                  NCDF_REFERENCE_ET0,                                       &
                  NCDF_ACTUAL_ET, NCDF_SNOWMELT, NCDF_TMIN, NCDF_TMAX,      &
                  NCDF_POTENTIAL_RECHARGE, NCDF_INFILTRATION,               &
                  NCDF_IRRIGATION, NCDF_RUNOFF_OUTSIDE
  end enum 

contains

  subroutine set_output_directory( output_dir_name )

    character (len=*), intent(in)  :: output_dir_name

    OUTPUT_DIRECTORY_NAME = output_dir_name

  end subroutine set_output_directory  

!--------------------------------------------------------------------------------------------------

  subroutine initialize_output(cells)

    class (MODEL_DOMAIN_T), intent(inout)   :: cells

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iIndex

    allocate ( NC_OUT( NCDF_NUM_OUTPUTS ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    do iIndex = 1, ubound(NC_OUT, 1)
      allocate ( NC_OUT(iIndex)%ncfile )

      call netcdf_open_and_prepare_as_output(                                        &
            NCFILE=NC_OUT( iIndex )%ncfile,                                          &
            sVariableName=trim( NCDF_OUTPUT_NAMES( iIndex ) ),                       &
            sVariableUnits="inches_per_day",                                         &
            iNX=cells%number_of_columns,                                             &
            iNY=cells%number_of_rows,                                                &
            fX=cells%X,                                                              &
            fY=cells%Y,                                                              &
            StartDate=SIM_DT%start,                                                  &
            EndDate=SIM_DT%end,                                                      &
            PROJ4_string=cells%PROJ4_string,                                         &
            dpLat=cells%Y_lat,                                                       &
            dpLon=cells%X_lon,                                                       &
            fValidMin=0.0,                                                           &
            fValidMax=2000.0,                                                        &
            sDirName=OUTPUT_DIRECTORY_NAME )

    enddo  

      cells%nodata_fill_value = NC_FILL_FLOAT

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

    call cells%output_GDD()

  end subroutine write_output

end module output
