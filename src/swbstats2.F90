program swbstats2

  use iso_c_binding, only             : c_int, c_float, c_double, c_bool,      &
                                        c_size_t, c_ptrdiff_t, c_long
  use constants_and_conversions, only : TRUE, FALSE
  use data_catalog_entry, only        : DATA_CATALOG_ENTRY_T
  use datetime, only                  : DATETIME_T
  use grid
  use netcdf4_support
  use simulation_datetime, only       : SIM_DT
  use string_list, only               : STRING_LIST_T,                          &
                                        create_list
  use strings
  use version_control, only           : SWB_VERSION, GIT_COMMIT_HASH_STRING,    &
                                        GIT_BRANCH_STRING, COMPILE_DATE,        &
                                        COMPILE_TIME

  use iso_fortran_env, only           : OUTPUT_UNIT, COMPILER_OPTIONS,          &
                                        COMPILER_VERSION

  implicit none

  character (len=256)            :: temp_str, temp_str2, temp_str3
  character (len=:), allocatable :: netcdf_filename_str
  character (len=:), allocatable :: statistic_str
  character (len=:), allocatable :: slice_str
  character (len=:), allocatable :: start_date_str
  character (len=:), allocatable :: end_date_str
  integer (kind=c_int)           :: iNumArgs
  character (len=1024)           :: sCompilerFlags
  character (len=256)            :: sCompilerVersion
  character (len=256)            :: sVersionString
  character (len=256)            :: sGitHashString
  integer (kind=c_int)           :: iCount
  integer (kind=c_int)           :: iIndex
  integer (kind=c_int), allocatable           ::iIndex_array(:)
  integer (kind=c_int)           :: iLen
  real (kind=c_double)           :: start_date_dbl
  real (kind=c_double)           :: end_date_dbl
  integer (kind=c_int)           :: time_bnds_varid

  logical (kind=c_bool)          :: data_was_found

  type (DATETIME_T)              :: data_start_date
  type (DATETIME_T)              :: data_end_date
  type (DATETIME_T)              :: slice_start_date
  type (DATETIME_T)              :: slice_end_date

  type (STRING_LIST_T)           :: name_list
  type (STRING_LIST_T)           :: value_list

  type (T_NETCDF4_FILE), pointer          :: ncfile_in
  type (T_NETCDF4_FILE), pointer          :: ncfile_out

  type (DATA_CATALOG_ENTRY_T)    :: input_data_obj
  type (GENERAL_GRID_T), pointer :: pGrdNative
  type (GENERAL_GRID_T), pointer :: pGrdTemp
  integer (kind=c_int)           :: julian_day_number
  integer (kind=c_size_t)        :: iNX, iNY

  allocate( ncfile_in )
  allocate( ncfile_out )

  statistic_str = ""
  slice_str = ""
  start_date_str = ""
  end_date_str = ""
  netcdf_filename_str = ""

  iNumArgs = COMMAND_ARGUMENT_COUNT()

  sVersionString = "  SWBSTATS2 - Soil Water Balance Statistics Code version "//trim( SWB_VERSION )    &
      //" -- compiled on: "//trim(COMPILE_DATE)//" "//trim(COMPILE_TIME)

  sGitHashString = "    [ Git branch and commit hash: "//trim( GIT_BRANCH_STRING )    &
     //", "//trim( GIT_COMMIT_HASH_STRING )//" ]"

  iCount = max( len_trim( sVersionString ), len_trim( sGitHashString ) )

  write(unit=*, fmt="(/,a)") repeat("-",iCount + 2)
  write(UNIT=*,FMT="(a)") trim( sVersionString )
  write(UNIT=*,FMT="(a)") trim( sGitHashString )
  write(unit=*, fmt="(a,/)") repeat("-",iCount + 2)

  if(iNumArgs == 0 ) then

#ifdef __GFORTRAN__
    sCompilerFlags = COMPILER_OPTIONS()
    sCompilerVersion = COMPILER_VERSION()
    write(UNIT=*,FMT="(a,/)") "Compiled with: gfortran ("//TRIM(sCompilerVersion)//")"
    write(UNIT=*,FMT="(a)") "Compiler flags:"
    write(UNIT=*,FMT="(a)") "-------------------------------"
    write(UNIT=*,FMT="(a,/)") TRIM(sCompilerFlags)
#endif

#ifdef __INTEL_COMPILER
    write(UNIT=*,FMT="(a)") "Compiled with: Intel Fortran version " &
      //TRIM(int2char(__INTEL_COMPILER))
      write(UNIT=*,FMT="(a,/)") "Compiler build date:"//TRIM(int2char(__INTEL_COMPILER_BUILD_DATE))
#endif

#ifdef __G95__
    write(UNIT=*,FMT="(a,/)") "Compiled with: G95 minor version " &
      //TRIM(int2char(__G95_MINOR__))
#endif

    write(UNIT=*,FMT="(/,a,/,/,5(a,/))")  "Usage: swbstats2 netcdf_file_name ",                      &
             "[ --statistic= ]         summary statistic to calculate: sum, mean, both",             &
             "[ --slice= ]             dates over which statistics should be calculated:",           &
             "                            begin date,end date formatted as yyyy-mm-dd,yyyy-mm-dd"
    stop

  end if

  do iIndex=1, iNumArgs

    call GET_COMMAND_ARGUMENT( iIndex, temp_str )

    if ( temp_str .containssimilar. "statistic" ) then

      statistic_str = right(temp_str, substring="=")

    elseif ( temp_str .containssimilar. "slice" ) then

      slice_str       = right(temp_str, substring="=")
      start_date_str  = left(slice_str, substring=",")
      end_date_str    = right(slice_str, substring=",")

      call slice_start_date%setDateFormat("YYYY-MM-DD")
      call slice_start_date%parseDate(start_date_str)
      call slice_start_date%calcGregorianDate()

      call slice_end_date%setDateFormat("YYYY-MM-DD")
      call slice_end_date%parseDate(end_date_str)
      call slice_end_date%calcGregorianDate()

    else

      ! no match on the command-line argument flags; this must be the netCDF file
      netcdf_filename_str = trim(temp_str)
      call netcdf_open_and_prepare_for_merging( ncfile_in,                    &
                                                netcdf_filename_str,          &
                                                guess_z_var_name=TRUE )

      ! extract integer start and end Julian dates from netCDF file
      start_date_dbl = real( ncfile_in%iFirstDayJD, kind=c_double )
      end_date_dbl = real( ncfile_in%iLastDayJD, kind=c_double )

      ! populate datetime data structure
      call data_start_date%setJulianDate(start_date_dbl)
      print *, __LINE__
      call data_start_date%calcGregorianDate()
            print *, __LINE__
      call data_end_date%setJulianDate(end_date_dbl)
            print *, __LINE__
      call data_end_date%calcGregorianDate()
            print *, __LINE__

    endif

  enddo


  ! force slice dates to honor bounds of data dates
  if ( slice_start_date < data_start_date )  slice_start_date = data_start_date
  if ( slice_end_date > data_end_date ) slice_end_date = data_end_date

  call SIM_DT%initialize( slice_start_date, slice_end_date )


  print *, "filename:   ", netcdf_filename_str
  print *, "slice:      ", slice_str
  print *, "model_start_date: ", data_start_date%prettydate()
  print *, "model_end_date:   ", data_end_date%prettydate()
  print *, "slice_start_date: ", slice_start_date%prettydate()
  print *, "slice_end_date:   ", slice_end_date%prettydate()
  print *, "statistic:  ", statistic_str

  call netcdf_dump_cdl( ncfile_in, OUTPUT_UNIT )

  print *, ncfile_in%rX(NC_LEFT)
  print *, ncfile_in%rY(NC_BOTTOM)
  print *, ncfile_in%rGridCellSizeX, ncfile_in%rGridCellSizeY

  print *, ncfile_in%iFirstDayJD, ncfile_in%iLastDayJD


  call netcdf_get_attribute_list_for_variable( NCFILE=ncfile_in,              &
                                               variable_name="crs",  &
                                               attribute_name_list=name_list, &
                                               attribute_value_list=value_list )

  ! extract PROJ4 string from netCDF file
  iIndex_array = name_list%which("proj4_string")
  temp_str = value_list%get( iIndex_array(1) )

  temp_str2 = trim(ncfile_in%pNC_VAR(NC_Z)%sVariableName)

  ! need to set the variable name so that the netCDF routine can process
  ! the file correctly
  !ncfile_in%sVarName(NC_Z) = trim(temp_str2)
  !ncfile_in%iVarType(NC_Z) = NC_FLOAT

  call netcdf_get_attribute_list_for_variable( NCFILE=ncfile_in,                &
                                               variable_name=temp_str2,         &
                                               attribute_name_list=name_list,   &
                                               attribute_value_list=value_list )

  ! extract PROJ4 string from netCDF file
  iIndex_array = name_list%which("units")
  temp_str3 = value_list%get( iIndex_array(1) )


  pGrdNative => grid_Create ( iNX=ncfile_in%iNX, &
            iNY=ncfile_in%iNY, &
            rX0=ncfile_in%rX(NC_LEFT), &
            rY0=ncfile_in%rY(NC_BOTTOM), &
            rX1=ncfile_in%rX(NC_RIGHT), &
            rY1=ncfile_in%rY(NC_TOP), &
            iDataType=GRID_DATATYPE_REAL )

  pGrdTemp => grid_Create ( iNX=ncfile_in%iNX, &
            iNY=ncfile_in%iNY, &
            rX0=ncfile_in%rX(NC_LEFT), &
            rY0=ncfile_in%rY(NC_BOTTOM), &
            rX1=ncfile_in%rX(NC_RIGHT), &
            rY1=ncfile_in%rY(NC_TOP), &
            iDataType=GRID_DATATYPE_REAL )

  pGrdTemp%rData = 0.0_c_float

  call netcdf_open_and_prepare_as_output(                                     &
        NCFILE=ncfile_out,                                                    &
        sVariableName=trim(ncfile_in%pNC_VAR(NC_Z)%sVariableName),            &
        sVariableUnits=trim(temp_str3),                                       &
        iNX=ncfile_in%iNX,                                                    &
        iNY=ncfile_in%iNY,                                                    &
        fX=ncfile_in%rX_Coords,                                               &
        fY=ncfile_in%rY_Coords,                                               &
        PROJ4_string=trim(temp_str),                                          &
        StartDate=SIM_DT%start,                                               &
        EndDate=SIM_DT%end,                                                   &
        write_time_bounds=TRUE,                                               &
        filename_modifier=statistic_str )

  !       !        PROJ4_string=ncfile_in%PROJ4_string,                                  &
  !       !        dpLat=cells%Y_lat,                                                       &
  !       !        dpLon=cells%X_lon,                                                       &
  !       !        fValidMin=OUTSPECS( iIndex )%valid_minimum,                              &
  !       !        fValidMax=OUTSPECS( iIndex )%valid_maximum,                         &

  do

    print *, SIM_DT%curr%prettydate()

    call SIM_DT%addDay
    if ( SIM_DT%curr > slice_end_date )  exit
    julian_day_number = int( SIM_DT%curr%dJulianDate, kind=c_int)

    if ( netcdf_update_time_starting_index(ncfile_in, julian_day_number ) )   then
      call netcdf_get_variable_slice(NCFILE=ncfile_in, rValues=pGrdNative%rData )
    endif

    pGrdTemp%rData = pGrdTemp%rData + pGrdNative%rData

  end do

  call netcdf_put_variable_vector(NCFILE=ncfile_out,                          &
     iVarID=ncfile_out%iVarID(NC_TIME),                                       &
     iStart=[0_c_size_t],                                                     &
     iCount=[1_c_size_t],                                                     &
     iStride=[1_c_ptrdiff_t],                                                 &
     dpValues=[ 0.0_c_double ] )

  call netcdf_get_variable_id_for_variable( NCFILE=ncfile_out,                &
                                            variable_name="time_bnds",        &
                                            variable_id=time_bnds_varid      )

  call netcdf_put_variable_vector(NCFILE=ncfile_out,                          &
    iVarID=time_bnds_varid,                                                   &
    iStart=[0_c_size_t,0_c_size_t],                                           &
    iCount=[1_c_size_t,2_c_size_t],                                           &
    iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t],                                   &
    dpValues=[ 0.0_c_double, real( SIM_DT%iNumDaysFromOrigin, kind=c_double) ] )


  iNX = int( size(pGrdTemp%rData, 1), kind=c_size_t)
  iNY = int( size(pGrdTemp%rData, 2), kind=c_size_t)

  call netcdf_put_variable_array(NCFILE=ncfile_out,                           &
     iVarID=ncfile_out%iVarID(NC_Z),                                          &
     iStart=[0_c_size_t, 0_c_size_t, 0_c_size_t],                             &
     iCount=[ 1_c_size_t, iNY, iNX ],                                         &
     iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                   &
     rValues=pGrdTemp%rData )

  call netcdf_close_file(NCFILE=ncfile_out)

end program swbstats2
