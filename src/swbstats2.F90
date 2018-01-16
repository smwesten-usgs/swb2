program swbstats2

  use iso_c_binding, only             : c_int, c_float, c_double, c_bool,      &
                                        c_size_t, c_ptrdiff_t, c_long
  use constants_and_conversions, only : TRUE, FALSE
  use file_operations, only           : ASCII_FILE_T
  use data_catalog_entry, only        : DATA_CATALOG_ENTRY_T
  use datetime, only                  : DATETIME_T, assignment(=), operator(>)
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

  integer (kind=c_int), parameter  :: SINGLE_SLICE = 1
  integer (kind=c_int), parameter  :: MANY_SLICES = 2

  integer (kind=c_int), parameter  :: STATS_SUM  = 1
  integer (kind=c_int), parameter  :: STATS_MEAN = 2
  integer (kind=c_int), parameter  :: STATS_BOTH = 3

  integer (kind=c_int) :: STATS_OPTION = STATS_SUM
  integer (kind=c_int) :: SLICE_OPTION = SINGLE_SLICE

  character (len=256)            :: temp_str, temp_str2, temp_str3
  character (len=:), allocatable :: netcdf_filename_str
  character (len=:), allocatable :: statistic_str
  character (len=:), allocatable :: stress_period_filename_str
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

  type (STRING_LIST_T)           :: start_date_list
  type (STRING_LIST_T)           :: end_date_list
  type (STRING_LIST_T)           :: stress_period_id_list

  type (T_NETCDF4_FILE), pointer          :: ncfile_in
  type (T_NETCDF4_FILE), pointer          :: ncfile_out

  type (DATA_CATALOG_ENTRY_T)    :: input_data_obj
  type (GENERAL_GRID_T), pointer :: pGrdNative
  type (GENERAL_GRID_T), pointer :: pGrdSum
  type (GENERAL_GRID_T), pointer :: pGrdMean
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

  call slice_start_date%setDateFormat("YYYY-MM-DD")
  call slice_end_date%setDateFormat("YYYY-MM-DD")

  do iIndex=1, iNumArgs

    call GET_COMMAND_ARGUMENT( iIndex, temp_str )

    if ( temp_str .containssimilar. "statistic" ) then

      statistic_str = right(temp_str, substring="=")

    elseif ( temp_str .containssimilar. "stress_period_file" ) then

      SLICE_OPTION = MANY_SLICES
      stress_period_filename_str = right(temp_str, substring="=")
      call read_stress_period_file( csv_filename=stress_period_filename_str,     &
                                    stress_period_id_list=stress_period_id_list, &
                                    start_date_list=start_date_list,             &
                                    end_date_list=end_date_list )

    elseif ( temp_str .containssimilar. "slice" ) then

      slice_str       = right(temp_str, substring="=")
      start_date_str  = left(slice_str, substring=",")
      end_date_str    = right(slice_str, substring=",")

      call slice_start_date%parseDate(start_date_str)
      call slice_start_date%calcGregorianDate()

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

      print *, "NETCDF START AND END JD:"
      print *, "      ",start_date_dbl, end_date_dbl

      ! populate datetime data structure
      call data_start_date%setJulianDate(start_date_dbl)
      call data_start_date%calcGregorianDate()
      call data_end_date%setJulianDate(end_date_dbl)
      call data_end_date%calcGregorianDate()

      print *, 'start: ',data_start_date%prettydate()
      print *, 'end: ',data_end_date%prettydate()

    endif

  enddo

  call netcdf_get_attribute_list_for_variable( NCFILE=ncfile_in,              &
                                               variable_name="crs",  &
                                               attribute_name_list=name_list, &
                                               attribute_value_list=value_list )

  ! extract PROJ4 string from netCDF file
  iIndex_array = name_list%which("proj4_string")
  temp_str = value_list%get( iIndex_array(1) )

  temp_str2 = trim(ncfile_in%pNC_VAR(NC_Z)%sVariableName)

  call netcdf_get_attribute_list_for_variable( NCFILE=ncfile_in,                &
                                               variable_name=temp_str2,         &
                                               attribute_name_list=name_list,   &
                                               attribute_value_list=value_list )

  ! extract PROJ4 string from netCDF file
  iIndex_array = name_list%which("units")
  temp_str3 = value_list%get( iIndex_array(1) )

  ! create some working grids to hold raw and summed values
  pGrdNative => grid_Create ( iNX=ncfile_in%iNX,           &
            iNY=ncfile_in%iNY,                             &
            rX0=ncfile_in%rX(NC_LEFT),                     &
            rY0=ncfile_in%rY(NC_BOTTOM),                   &
            rX1=ncfile_in%rX(NC_RIGHT),                    &
            rY1=ncfile_in%rY(NC_TOP),                      &
            iDataType=GRID_DATATYPE_REAL )

  pGrdSum => grid_Create ( iNX=ncfile_in%iNX,              &
            iNY=ncfile_in%iNY,                             &
            rX0=ncfile_in%rX(NC_LEFT),                     &
            rY0=ncfile_in%rY(NC_BOTTOM),                   &
            rX1=ncfile_in%rX(NC_RIGHT),                    &
            rY1=ncfile_in%rY(NC_TOP),                      &
            iDataType=GRID_DATATYPE_REAL )

  pGrdMean => grid_Create ( iNX=ncfile_in%iNX,             &
            iNY=ncfile_in%iNY,                             &
            rX0=ncfile_in%rX(NC_LEFT),                     &
            rY0=ncfile_in%rY(NC_BOTTOM),                   &
            rX1=ncfile_in%rX(NC_RIGHT),                    &
            rY1=ncfile_in%rY(NC_TOP),                      &
            iDataType=GRID_DATATYPE_REAL )

  iNX = int( size(pGrdMean%rData, 1), kind=c_size_t)
  iNY = int( size(pGrdMean%rData, 2), kind=c_size_t)

  if ( SLICE_OPTION == SINGLE_SLICE ) then
    ! force slice dates to honor bounds of data dates
    if ( slice_start_date < data_start_date )  slice_start_date = data_start_date
    if ( slice_end_date > data_end_date ) slice_end_date = data_end_date

    call SIM_DT%initialize( slice_start_date, slice_end_date )

  else
    call SIM_DT%initialize( data_start_date, data_end_date )
  endif

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

  call netcdf_get_variable_id_for_variable( NCFILE=ncfile_out,                &
                                            variable_name="time_bnds",        &
                                            variable_id=time_bnds_varid      )

  if ( SLICE_OPTION == SINGLE_SLICE ) then

    call iterate_over_slice(grid_sum=pGrdSum, grid_mean=pGrdMean,               &
                            start_date=slice_start_date,                        &
                            end_date=slice_end_date)


    call write_stats_to_files(grid_sum=pGrdSum, grid_mean=pGrdMean,             &
                              start_date=slice_start_date,                      &
                              end_date=slice_end_date)

  elseif ( SLICE_OPTION == MANY_SLICES ) then

    do iIndex=1, stress_period_id_list%count

      start_date_str = start_date_list%get( iIndex )
      end_date_str = end_date_list%get( iIndex )

      call slice_start_date%setDateFormat("YYYY-MM-DD")
      call slice_end_date%setDateFormat("YYYY-MM-DD")

      call slice_start_date%parseDate( start_date_str )
      call slice_end_date%parseDate( end_date_str )


      print *, ""
      print *, "Processing slice: ", slice_start_date%prettydate(), " to ", slice_end_date%prettydate()
      print *, repeat("-", 70)

      call iterate_over_slice(grid_sum=pGrdSum, grid_mean=pGrdMean,               &
                              start_date=slice_start_date,                        &
                              end_date=slice_end_date)

      call write_stats_to_files(grid_sum=pGrdSum, grid_mean=pGrdMean,             &
                                start_date=slice_start_date,                      &
                                end_date=slice_end_date)

    enddo

  endif

  call netcdf_close_file(NCFILE=ncfile_out)

contains

  subroutine write_stats_to_files( grid_sum, grid_mean, start_date, end_date )

    type (GENERAL_GRID_T), pointer          :: grid_sum
    type (GENERAL_GRID_T), pointer          :: grid_mean
    type (DATETIME_T), intent(inout)        :: start_date
    type (DATETIME_T), intent(inout)        :: end_date

    ! [ LOCALS ]
    real (kind=c_double) :: start_bnd
    real (kind=c_double) :: end_bnd

    start_bnd = SIM_DT%days_from_origin( start_date )
    end_bnd   = SIM_DT%days_from_origin( end_date )

    call netcdf_put_variable_vector(NCFILE=ncfile_out,                          &
       iVarID=ncfile_out%iVarID(NC_TIME),                                       &
       iStart=[0_c_size_t],                                                     &
       iCount=[1_c_size_t],                                                     &
       iStride=[1_c_ptrdiff_t],                                                 &
       dpValues=[ real( SIM_DT%iNumDaysFromOrigin, kind=c_double) ] )

    call netcdf_put_variable_vector(NCFILE=ncfile_out,                          &
      iVarID=time_bnds_varid,                                                   &
      iStart=[0_c_size_t,0_c_size_t],                                           &
      iCount=[1_c_size_t,2_c_size_t],                                           &
      iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t],                                   &
      dpValues=[ start_bnd, end_bnd ] )

    call netcdf_put_variable_array(NCFILE=ncfile_out,                         &
       iVarID=ncfile_out%iVarID(NC_Z),                                        &
       iStart=[0_c_size_t, 0_c_size_t, 0_c_size_t],                           &
       iCount=[ 1_c_size_t, iNY, iNX ],                                       &
       iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                 &
       rValues=grid_sum%rData )

  end subroutine write_stats_to_files

!------------------------------------------------------------------------------

  subroutine iterate_over_slices( grid_sum, grid_mean, stress_period_id_list, &
                                  start_date_list, end_date_list )

    type (GENERAL_GRID_T), pointer          :: grid_sum
    type (GENERAL_GRID_T), pointer          :: grid_mean
    type (STRING_LIST_T), intent(in)        :: stress_period_id_list
    type (STRING_LIST_T), intent(in)        :: start_date_list
    type (STRING_LIST_T), intent(in)        :: end_date_list





  end subroutine iterate_over_slices

  subroutine iterate_over_slice( grid_sum, grid_mean, start_date, end_date )

    type (GENERAL_GRID_T), pointer          :: grid_sum
    type (GENERAL_GRID_T), pointer          :: grid_mean
    type (DATETIME_T), intent(inout)        :: start_date
    type (DATETIME_T), intent(inout)        :: end_date

    ! [ LOCALS ]
    integer (kind=c_int) :: julian_day_number

    ! force slice dates to honor bounds of data dates
    if ( start_date < data_start_date )  start_date = data_start_date
    if ( end_date > data_end_date )      end_date = data_end_date

    call SIM_DT%initialize( start_date, end_date )

    grid_sum%rData = 0.0_c_float

    do

      call SIM_DT%addDay
      if ( SIM_DT%curr > end_date )  exit
      julian_day_number = int( SIM_DT%curr%dJulianDate, kind=c_int)

      if ( netcdf_update_time_starting_index(ncfile_in, julian_day_number ) )   then
        call netcdf_get_variable_slice(NCFILE=ncfile_in, rValues=pGrdNative%rData )
      endif

      where ( pGrdNative%rData > NC_FILL_FLOAT )
        grid_sum%rData = grid_sum%rData + pGrdNative%rData
      end where

    end do

    grid_mean%rData = grid_sum%rData / ( end_date - start_date + 1.0_c_double)

    where ( pGrdNative%rData <= NC_FILL_FLOAT )
      grid_mean%rData = NC_FILL_FLOAT
      grid_sum%rData = NC_FILL_FLOAT
    end where

  end subroutine iterate_over_slice

!------------------------------------------------------------------------------

  subroutine read_stress_period_file( csv_filename,                           &
                                      stress_period_id_list,                  &
                                      start_date_list,                        &
                                      end_date_list )

    character (len=*), intent(inout)   :: csv_filename
    type (STRING_LIST_T), intent(out)  :: stress_period_id_list
    type (STRING_LIST_T), intent(out)  :: start_date_list
    type (STRING_LIST_T), intent(out)  :: end_date_list

    ! [ LOCALS ]
    integer (kind=c_int)           :: iFileIndex, iColIndex
    integer (kind=c_int)           :: iStat
    type (ASCII_FILE_T)            :: DF
    character (len=256)            :: sRecord, sItem

   ! open the file associated with current file index value
    call DF%open(sFilename = csv_filename,                               &
                 sDelimiters=",",                                        &
                 sCommentChars = "%#!",                                  &
                 lHasHeader=TRUE         )

    ! read in and throw away header of file
    sRecord = DF%readLine()

    ! now read in the remainder of the file
    do while ( .not. DF%isEOF() )

      ! read in next line of file
      sRecord = DF%readLine()

      ! skip blank lines
      if ( len_trim(sRecord) == 0 ) cycle

      call chomp( sRecord, sItem, sDelimiters="," )
      call stress_period_id_list%append( sItem )

      call chomp( sRecord, sItem, sDelimiters="," )
      call start_date_list%append( sItem )
      call end_date_list%append( sRecord )

    enddo

    call DF%close()

  end subroutine read_stress_period_file

end program swbstats2
