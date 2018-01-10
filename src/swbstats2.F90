program swbstats2

  use iso_c_binding, only      : c_int, c_float, c_double
  use data_catalog_entry, only : DATA_CATALOG_ENTRY_T
  use datetime, only           : DATETIME_T
  use netcdf4_support, only    : T_NETCDF4_FILE,                              &
                                 netcdf_open_and_prepare_for_merging,         &
                                 netcdf_dump_cdl,                             &
                                 NC_LEFT, NC_BOTTOM
  use string_list, only        : STRING_LIST_T
  use strings
  use version_control, only    : SWB_VERSION, GIT_COMMIT_HASH_STRING, &
                                 GIT_BRANCH_STRING, COMPILE_DATE, COMPILE_TIME
  use iso_fortran_env, only    : OUTPUT_UNIT, COMPILER_OPTIONS, COMPILER_VERSION

  implicit none

  character (len=256)            :: temp_str
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
  integer (kind=c_int)           :: iLen
  real (kind=c_double)           :: start_date_dbl
  real (kind=c_double)           :: end_date_dbl

  type (DATETIME_T)              :: data_start_date
  type (DATETIME_T)              :: data_end_date
  type (DATETIME_T)              :: slice_start_date
  type (DATETIME_T)              :: slice_end_date

  type (T_NETCDF4_FILE)          :: ncfile_in
  type (T_NETCDF4_FILE)          :: ncfile_out

  type (DATA_CATALOG_ENTRY_T)    :: input_data_obj

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
      call netcdf_open_and_prepare_for_merging( ncfile_in, netcdf_filename_str )

      ! extract integer start and end Julian dates from netCDF file
      start_date_dbl = real( ncfile_in%iFirstDayJD, kind=c_double )
      end_date_dbl = real( ncfile_in%iLastDayJD, kind=c_double )

      ! populate datetime data structure
      call data_start_date%setJulianDate(start_date_dbl)
      call data_start_date%calcGregorianDate()
      call data_end_date%setJulianDate(end_date_dbl)
      call data_end_date%calcGregorianDate()

    endif

  enddo


  ! force slice dates to honor bounds of data dates
  if ( slice_start_date < data_start_date )  slice_start_date = data_start_date
  if ( slice_end_date > data_end_date ) slice_end_date = data_end_date


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


  ! call netcdf_open_and_prepare_as_output(                                     &
  !       NCFILE=ncfile_out,                                                    &
  !       sVariableName=trim( OUTSPECS( iIndex )%variable_name ),                  &
  !       sVariableUnits=trim( OUTSPECS( iIndex )%variable_units ),                &
  !       iNX=cells%number_of_columns,                                             &
  !       iNY=cells%number_of_rows,                                                &
  !       fX=ncfile_in%rX_Coords,                                               &
  !       fY=ncfile_in%rY_Coords                                                &
  !       StartDate=SIM_DT%start,                                                  &
  !       EndDate=SIM_DT%end,                                                      &
  !       PROJ4_string=cells%PROJ4_string,                                         &
  !       dpLat=cells%Y_lat,                                                       &
  !       dpLon=cells%X_lon,                                                       &
  !       fValidMin=OUTSPECS( iIndex )%valid_minimum,                              &
  !       fValidMax=OUTSPECS( iIndex )%valid_maximum )


end program swbstats2
