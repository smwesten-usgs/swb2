program swbstats2

  use iso_c_binding, only             : c_int, c_float, c_double, c_bool,      &
                                        c_size_t, c_ptrdiff_t, c_long
  use iso_fortran_env, only           : OUTPUT_UNIT
  use constants_and_conversions, only : TRUE, FALSE, DATATYPE_INT,            &
                                        DATATYPE_FLOAT, BNDS, asFloat
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

  integer (kind=c_int), parameter  :: OUTPUT_TYPE_BOTH  = 1
  integer (kind=c_int), parameter  :: OUTPUT_TYPE_CSV_ONLY = 2

  integer (kind=c_size_t)          :: RECNUM = 0

  integer (kind=c_int) :: OUTPUT_TYPE_OPTION = OUTPUT_TYPE_BOTH
  integer (kind=c_int) :: SLICE_OPTION = SINGLE_SLICE
  logical (c_bool)     :: ZONE_STATS = FALSE
  logical (c_bool)     :: ANNUALIZE_MEANS = TRUE
  logical (c_bool)     :: COMPARISON_GRID = FALSE
  real (kind=c_float)  :: COMPARISON_GRID_CONVERSION_FACTOR = 1.0_c_float

  character (len=256)            :: temp_str, temp_str2, temp_str3
  character (len=:), allocatable :: target_proj4_str
  character (len=:), allocatable :: netcdf_filename_str
  character (len=:), allocatable :: zone_grid_filename_str
  character (len=:), allocatable :: comparison_grid_filename_str
  character (len=:), allocatable :: conversion_factor_str
  character (len=:), allocatable :: output_type_str
  character (len=:), allocatable :: stress_period_filename_str
  character (len=:), allocatable :: annualize_means_str
  character (len=:), allocatable :: slice_str
  character (len=:), allocatable :: start_date_str
  character (len=:), allocatable :: end_date_str
  character (len=:), allocatable :: stress_period_str
  character (len=:), allocatable :: nc_variable_name_str
  character (len=:), allocatable :: nx_str
  character (len=:), allocatable :: ny_str
  character (len=:), allocatable :: help_fmt_str

  type (DATA_CATALOG_ENTRY_T), pointer :: pZONE_GRID       => null()
  type (DATA_CATALOG_ENTRY_T), pointer :: pCOMPARISON_GRID => null()

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

  logical (kind=c_bool)          :: netcdf_active = FALSE

  type (DATETIME_T)              :: data_start_date
  type (DATETIME_T)              :: data_end_date
  type (DATETIME_T)              :: slice_start_date
  type (DATETIME_T)              :: slice_end_date

  type (STRING_LIST_T)           :: name_list
  type (STRING_LIST_T)           :: value_list

  type (STRING_LIST_T)           :: start_date_list
  type (STRING_LIST_T)           :: end_date_list
  type (STRING_LIST_T)           :: stress_period_id_list
  type (STRING_LIST_T)           :: unique_zone_list

  type (T_NETCDF4_FILE), pointer          :: ncfile_in
  type (T_NETCDF4_FILE), pointer          :: ncfile_out

  type (DATA_CATALOG_ENTRY_T)    :: input_data_obj
  type (GENERAL_GRID_T), pointer :: pGrdNative
  type (GENERAL_GRID_T), pointer :: pGrdSum
  type (GENERAL_GRID_T), pointer :: pGrdMean
  integer (kind=c_size_t)        :: nx, ny
  real (kind=c_float)             :: xll, yll, xur, yur, gridcell_size

  allocate( ncfile_in )
  allocate( ncfile_out )

  output_type_str = ""
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

    help_fmt_str = "(/,a,/,/,"            &     ! usage
      //"a,t36,a,/,"                      &     ! --output_type
      //"a,t36,a,/,t36,a,/,"              &     ! --slice
      //"a,t36,a,/,t36,a,/,4(t43,a,/),"   &     ! --stress_period_file
      //"3(a,t36,a,/))"                         ! --zone_grid,
                                                ! --comparison_conversion_factor,
                                                ! --comparison_grid

    write(UNIT=*,FMT=help_fmt_str)                                             &
      "usage: swbstats2 [options] netcdf_file_name ",                          &
     "[ --output_type= ]",                                                     &
       "form of output: CSV_ONLY, BOTH (default)",                             &
     "[ --slice= ]",                                                           &
       "dates over which statistics should be calculated, ",                   &
       "with start date and end date formatted as yyyy-mm-dd,yyyy-mm-dd",      &
     "[ --stress_period_file= ]",                                              &
       "comma-delimited file containing stress period start and ",             &
       "end date, with header labels and comments starting with'#':",          &
         "stress_period,start_date,end_date",                                  &
         "#2,1870-01-01,1898-12-31",                                           &
         "5,1920-01-01,1925-12-31",                                            &
         "6,1925-01-01,1930-12-31",                                            &
     "[ --zone_grid= ]",                                                       &
       "name of integer-valued grid for which zonal statistics are desired",   &
     "[ --comparison_conversion_factor=]",                                     &
     "value to multiply comparison grid by before calculating statistics",     &
     "[ --comparison_grid= ]",                                                 &
       "name of real-valued grid to compare SWB output against"

!             "[ --annualize_means ]     report mean values on an annual basis",                         &
    stop

  end if

  call slice_start_date%setDateFormat("YYYY-MM-DD")
  call slice_end_date%setDateFormat("YYYY-MM-DD")

  do iIndex=1, iNumArgs

    call GET_COMMAND_ARGUMENT( iIndex, temp_str )

    if ( temp_str .containssimilar. "output_type" ) then

      output_type_str = right(temp_str, substring="=")
      if (output_type_str .containssimilar. "CSV_ONLY") then

        OUTPUT_TYPE_OPTION = OUTPUT_TYPE_CSV_ONLY

      endif

    elseif ( temp_str .containssimilar. "comparison_conversion_factor" ) then

      conversion_factor_str = right(temp_str, substring="=")

      COMPARISON_GRID_CONVERSION_FACTOR = asFloat(conversion_factor_str)

    elseif ( temp_str .containssimilar. "annualize_means" ) then

      annualize_means_str = right(temp_str, substring="=")
      if (annualize_means_str .containssimilar. "TRUE") then

        ANNUALIZE_MEANS = TRUE

      elseif (annualize_means_str .containssimilar. "FALSE") then

        ANNUALIZE_MEANS = FALSE

      endif

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


    elseif ( temp_str .containssimilar. "zone_grid" ) then

      zone_grid_filename_str       = right(temp_str, substring="=")
      ZONE_STATS = TRUE

    elseif ( temp_str .containssimilar. "comparison_grid" ) then

      comparison_grid_filename_str       = right(temp_str, substring="=")
      COMPARISON_GRID = TRUE

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
      call data_start_date%calcGregorianDate()
      call data_end_date%setJulianDate(end_date_dbl)
      call data_end_date%calcGregorianDate()

      netcdf_active = TRUE

    endif

  enddo

  if (.not. netcdf_active)     &
    stop ("No netCDF file was specified or there was an error opening the file.")

  call netcdf_get_attribute_list_for_variable( NCFILE=ncfile_in,              &
                                               variable_name="crs",  &
                                               attribute_name_list=name_list, &
                                               attribute_value_list=value_list )

  ! extract PROJ4 string from netCDF file
  iIndex_array = name_list%which("proj4_string")
  temp_str = value_list%get( iIndex_array(1) )
  target_proj4_str = trim(temp_str)

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

  nx = pGrdNative%iNX
  ny = pGrdNative%iNY
  xll = pGrdNative%rX0
  yll = pGrdNative%rY0
  xur = pGrdNative%rX1
  yur = pGrdNative%rY1
  gridcell_size = pGrdNative%rGridCellSize

  call set_project_projection_params()


  if (ZONE_STATS) then
    call initialize_zone_grid(grid_filename=zone_grid_filename_str)
    call get_unique_int(pZONE_GRID%pGrdBase%iData, unique_zone_list)
  endif

  if (COMPARISON_GRID) then
    call initialize_comparison_grid(grid_filename=comparison_grid_filename_str)
  endif

  if ( SLICE_OPTION == SINGLE_SLICE ) then
    ! force slice dates to honor bounds of data dates
    if ( slice_start_date < data_start_date )  slice_start_date = data_start_date
    if ( slice_end_date > data_end_date ) slice_end_date = data_end_date

    call SIM_DT%initialize( slice_start_date, slice_end_date )

  else
    call SIM_DT%initialize( data_start_date, data_end_date )
  endif

  call open_output_netcdf_files()

  if ( SLICE_OPTION == SINGLE_SLICE ) then

    call iterate_over_slice(grid_sum=pGrdSum, grid_mean=pGrdMean,             &
                            start_date=slice_start_date,                      &
                            end_date=slice_end_date)

    if (OUTPUT_TYPE_OPTION /= OUTPUT_TYPE_CSV_ONLY) then
      call write_stats_to_files(grid_sum=pGrdSum, grid_mean=pGrdMean,           &
                                start_date=slice_start_date,                    &
                                end_date=slice_end_date)

      call write_stats_to_arcgrid(grid_sum=pGrdSum, grid_mean=pGrdMean,         &
                                  start_date=slice_start_date,                  &
                                  end_date=slice_end_date )

    endif

    if (ZONE_STATS) then
      if ( associated(pCOMPARISON_GRID) ) then

        call output_zonal_stats( start_date=slice_start_date,                 &
                                 end_date=slice_end_date,                     &
                                 values=pGrdMean%rData,                       &
                                 zone_ids=pZONE_GRID%pGrdBase%iData,          &
                                 unique_zone_list=unique_zone_list,           &
                                 comparison_values=pCOMPARISON_GRID%pGrdBase%rData)
      else

        call output_zonal_stats( start_date=slice_start_date,                 &
                                 end_date=slice_end_date,                     &
                                 values=pGrdMean%rData,                       &
                                 zone_ids=pZONE_GRID%pGrdBase%iData,          &
                                 unique_zone_list=unique_zone_list)
      endif
    endif

    call netcdf_close_file(NCFILE=ncfile_out)

  elseif ( SLICE_OPTION == MANY_SLICES ) then

    do iIndex=1, stress_period_id_list%count

      start_date_str = start_date_list%get( iIndex )
      end_date_str = end_date_list%get( iIndex )
      stress_period_str = stress_period_id_list%get( iIndex )

      call slice_start_date%setDateFormat("YYYY-MM-DD")
      call slice_end_date%setDateFormat("YYYY-MM-DD")

      call slice_start_date%parseDate( start_date_str )
      call slice_end_date%parseDate( end_date_str )


      print *, ""
      print *, "Processing slice: ", slice_start_date%prettydate(), " to ", slice_end_date%prettydate()
      print *, repeat("-", 70)

      call iterate_over_slice(grid_sum=pGrdSum, grid_mean=pGrdMean,           &
                              start_date=slice_start_date,                    &
                              end_date=slice_end_date)

      if (OUTPUT_TYPE_OPTION /= OUTPUT_TYPE_CSV_ONLY) then
        call write_stats_to_files(grid_sum=pGrdSum, grid_mean=pGrdMean,         &
                                  start_date=slice_start_date,                  &
                                  end_date=slice_end_date)

        call write_stats_to_arcgrid(grid_sum=pGrdSum, grid_mean=pGrdMean,       &
                                    start_date=slice_start_date,                &
                                    end_date=slice_end_date,                    &
                                    stress_period=stress_period_str)
      endif

      if (ZONE_STATS) then
        if ( associated(pCOMPARISON_GRID) ) then

          call output_zonal_stats( start_date=slice_start_date,                 &
                                   end_date=slice_end_date,                     &
                                   values=pGrdMean%rData,                       &
                                   zone_ids=pZONE_GRID%pGrdBase%iData,          &
                                   unique_zone_list=unique_zone_list,           &
                                   comparison_values=pCOMPARISON_GRID%pGrdBase%rData)
        else

          call output_zonal_stats( start_date=slice_start_date,                 &
                                   end_date=slice_end_date,                     &
                                   values=pGrdMean%rData,                       &
                                   zone_ids=pZONE_GRID%pGrdBase%iData,          &
                                   unique_zone_list=unique_zone_list)
        endif
      endif

      ! icky hack; need to advance the record number for the multiple slice calc
      RECNUM = RECNUM + 1

    enddo

    if (OUTPUT_TYPE_OPTION /= OUTPUT_TYPE_CSV_ONLY)  &
      call netcdf_close_file(NCFILE=ncfile_out)

  endif

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

    call netcdf_put_variable_vector(NCFILE=ncfile_out,                        &
       iVarID=ncfile_out%iVarID(NC_TIME),                                     &
       iStart=[ RECNUM ],                                                     &
       iCount=[1_c_size_t],                                                   &
       iStride=[1_c_ptrdiff_t],                                               &
       dpValues=[ ( start_bnd + end_bnd ) / 2.0_c_double ] )
!       dpValues=[ real( SIM_DT%iNumDaysFromOrigin, kind=c_double) ] )

    call netcdf_put_variable_vector(NCFILE=ncfile_out,                        &
      iVarID=time_bnds_varid,                                                 &
      iStart=[ RECNUM ,0_c_size_t],                                           &
      iCount=[1_c_size_t,2_c_size_t],                                         &
      iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t],                                 &
      dpValues=[ start_bnd, end_bnd ] )

    call netcdf_put_variable_array(NCFILE=ncfile_out,                         &
       iVarID=ncfile_out%iVarID(NC_Z),                                        &
       iStart=[ RECNUM , 0_c_size_t, 0_c_size_t],                             &
       iCount=[ 1_c_size_t, ny, nx ],                                         &
       iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                 &
       rValues=grid_mean%rData )

  end subroutine write_stats_to_files





  subroutine write_stats_to_arcgrid( grid_sum, grid_mean, start_date,         &
                                     end_date, stress_period )

    type (GENERAL_GRID_T), pointer          :: grid_sum
    type (GENERAL_GRID_T), pointer          :: grid_mean
    type (DATETIME_T), intent(inout)        :: start_date
    type (DATETIME_T), intent(inout)        :: end_date
    character (len=*), intent(in), optional :: stress_period

    ! [ LOCALS ]
    character (len=:), allocatable  :: filename

    if ( present(stress_period) ) then
      !@todo: change filename depending on the desired output statistic
      filename = trim(stress_period_str)//"__"//trim(nc_variable_name_str)      &
                 //"__"//start_date%prettydate()//"_to_"//end_date%prettydate() &
                 //"__"//asCharacter(nx)//"_by_"//asCharacter(ny)             &
                 //"__MEAN_ANNUAL.asc"
    else
      filename = trim(nc_variable_name_str)                                     &
                 //"__"//start_date%prettydate()//"_to_"//end_date%prettydate() &
                 //"__"//asCharacter(nx)//"_by_"//asCharacter(ny)             &
                 //"__MEAN_ANNUAL.asc"
    endif

    ! ugly hack to make the output match the default NODATA value
    where ( pGrdNative%rData <= NC_FILL_FLOAT )
      grid_mean%rData = -9999.
    end where

    call grid_WriteArcGrid( filename, grid_mean )

  end subroutine write_stats_to_arcgrid

!------------------------------------------------------------------------------
  !
  ! subroutine iterate_over_slices( grid_sum, grid_mean, stress_period_id_list, &
  !                                 start_date_list, end_date_list )
  !
  !   type (GENERAL_GRID_T), pointer          :: grid_sum
  !   type (GENERAL_GRID_T), pointer          :: grid_mean
  !   type (STRING_LIST_T), intent(in)        :: stress_period_id_list
  !   type (STRING_LIST_T), intent(in)        :: start_date_list
  !   type (STRING_LIST_T), intent(in)        :: end_date_list
  !
  !
  !
  !
  !
  ! end subroutine iterate_over_slices

  subroutine iterate_over_slice( grid_sum, grid_mean, start_date, end_date )

    type (GENERAL_GRID_T), pointer          :: grid_sum
    type (GENERAL_GRID_T), pointer          :: grid_mean
    type (DATETIME_T), intent(inout)        :: start_date
    type (DATETIME_T), intent(inout)        :: end_date

    ! [ LOCALS ]
    integer (kind=c_int)              :: julian_day_number
    real (kind=c_float), allocatable  :: tempvals(:)

    ! force slice dates to honor bounds of data dates
    if ( start_date < data_start_date )  start_date = data_start_date
    if ( end_date > data_end_date )      end_date = data_end_date

!    call SIM_DT%initialize( start_date, end_date )

    grid_sum%rData = 0.0_c_float
    grid_mean%rData = 0.0_c_float

    do

      call SIM_DT%addDay
      if ( SIM_DT%curr > end_date )  exit
      julian_day_number = int( SIM_DT%curr%dJulianDate, kind=c_int)

      if ( netcdf_update_time_starting_index(ncfile_in, julian_day_number ) )   then
        call netcdf_get_variable_slice(NCFILE=ncfile_in, rValues=pGrdNative%rData )
      endif

!      print *, "ncdat: ", minval(pGrdNative%rData,  mask=pGrdNative%rData > NC_FILL_FLOAT), &
!          maxval(pGrdNative%rData,  mask=pGrdNative%rData > NC_FILL_FLOAT)

      where ( pGrdNative%rData > NC_FILL_FLOAT )
        grid_sum%rData = grid_sum%rData + pGrdNative%rData
      end where

!      print *, "gridsum: ", minval(grid_sum%rData,  mask=pGrdNative%rData > NC_FILL_FLOAT), &
!          maxval(grid_sum%rData,  mask=pGrdNative%rData > NC_FILL_FLOAT)

    end do

    where ( pGrdNative%rData <= NC_FILL_FLOAT )
      grid_mean%rData = NC_FILL_FLOAT
      grid_sum%rData = NC_FILL_FLOAT
    endwhere

    if (ANNUALIZE_MEANS) then
      where ( pGrdNative%rData > NC_FILL_FLOAT )
        grid_mean%rData = grid_sum%rData / ( end_date - start_date + 1.0_c_double) * 365.25
      end where
    else
      where ( pGrdNative%rData > NC_FILL_FLOAT )
        grid_mean%rData = grid_sum%rData / ( end_date - start_date + 1.0_c_double)
      end where
    endif

!    print *, "gridsum: ", minval(grid_sum%rData,  mask=pGrdNative%rData > NC_FILL_FLOAT), &
!        maxval(grid_sum%rData,  mask=pGrdNative%rData > NC_FILL_FLOAT)

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

!------------------------------------------------------------------------------

  subroutine initialize_zone_grid( grid_filename )

    character (len=*), intent(inout)   :: grid_filename

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iIndex
    integer (kind=c_int)                 :: iStat

    pZONE_GRID => null()

    ! allocate memory for a generic data_catalog_entry
    allocate(pZONE_GRID, stat=iStat)

    call pZONE_GRID%set_target_PROJ4(target_proj4_str)

    call pZONE_GRID%initialize(          &
      sDescription="Zone Grid",          &
      sFileType="ARC_GRID",              &
      sFilename=trim(grid_filename),     &
      iDataType=DATATYPE_INT )

    call pZONE_GRID%getvalues()

    call grid_WriteArcGrid("Zone_grid__as_read_into_SWBSTATS2.asc", pZONE_GRID%pGrdBase )

  end subroutine initialize_zone_grid

!------------------------------------------------------------------------------

  subroutine initialize_comparison_grid( grid_filename )

    character (len=*), intent(inout)   :: grid_filename

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iIndex
    integer (kind=c_int)                 :: iStat
    character (len=:), allocatable       :: left_str
    character (len=:), allocatable       :: output_filename_str

    pCOMPARISON_GRID => null()

    left_str = left( grid_filename, substring=".")

    output_filename_str = "Comparison_grid__"//trim(left_str)//"__as_read_into_SWBSTATS2.asc"

    ! allocate memory for a generic data_catalog_entry
    if (.not. associated(pCOMPARISON_GRID))  allocate(pCOMPARISON_GRID, stat=iStat)

    call pCOMPARISON_GRID%set_target_PROJ4(target_proj4_str)

    call pCOMPARISON_GRID%initialize(          &
      sDescription="Comparison Grid",          &
      sFileType="ARC_GRID",                    &
      sFilename=trim(grid_filename),           &
      iDataType=DATATYPE_FLOAT )

    call pCOMPARISON_GRID%getvalues()

    pCOMPARISON_GRID%pGrdBase%rData = pCOMPARISON_GRID%pGrdBase%rData         &
                                      * COMPARISON_GRID_CONVERSION_FACTOR

    call grid_WriteArcGrid(output_filename_str, pCOMPARISON_GRID%pGrdBase )

  end subroutine initialize_comparison_grid

!------------------------------------------------------------------------------

  subroutine set_project_projection_params()

    ! BNDS is a module-level data structure that will be used in other modules to
    ! supply bounding box information for the SWB project area
    BNDS%iNumCols = nx
    BNDS%iNumRows = ny
    BNDS%fX_ll = xll
    BNDS%fY_ll = yll
    BNDS%fY_ur = yur
    BNDS%fX_ur = xur
    BNDS%fGridCellSize = gridcell_size
    BNDS%sPROJ4_string = trim(target_proj4_str)

  end subroutine set_project_projection_params

!------------------------------------------------------------------------------

  subroutine output_zonal_stats(start_date, end_date, values, zone_ids,       &
                                 unique_zone_list, funit, comparison_values)

    type (DATETIME_T), intent(in)            :: start_date
    type (DATETIME_T), intent(in)            :: end_date
    real (c_float), intent(inout)            :: values(:,:)
    integer (c_int), intent(inout)           :: zone_ids(:,:)
    type (STRING_LIST_T), intent(in)         :: unique_zone_list
    integer (c_int), intent(in), optional    :: funit
    real (c_float), intent(inout), optional  :: comparison_values(:,:)

    ! [ LOCALS ]
    integer (c_int)                :: n
    integer (c_int), allocatable   :: tempvals(:)
    integer (c_int)                :: number_of_matches
    real (c_float), allocatable    :: stats(:)
    integer (c_int)                :: funit_

    if (present(funit)) then
      funit_ = funit
    else
      funit_ = OUTPUT_UNIT
    endif

    tempvals = unique_zone_list%asInt()

    do n=minval(tempvals,1), maxval(tempvals,1)
      if ( any(tempvals==n) ) then
        if (present(comparison_values)) then
          call calc_zonal_stats(values, zone_ids, target_id=n, result_vector=stats, &
                comparison_values=comparison_values)
          write(unit=funit_, fmt="(2(a,', '),i8,', ',2(3(f12.3,', '),i8))")            &
            start_date%prettydate(),end_date%prettydate(), n, stats(1:3),int(stats(4)), &
            stats(5:7),int(stats(8))
        else
          call calc_zonal_stats(values, zone_ids, target_id=n, result_vector=stats)
          write(unit=funit_, fmt="(2(a,', '),i8,', ',3(f12.3,', '),i8)")         &
            start_date%prettydate(),end_date%prettydate(), n, stats(1:3),int(stats(4))
        endif
      endif
    enddo

  end subroutine output_zonal_stats

!------------------------------------------------------------------------------

  subroutine calc_zonal_stats(values, zone_ids, target_id, result_vector, comparison_values)

    real (c_float), intent(inout)                 :: values(:,:)
    integer (c_int), intent(inout)                :: zone_ids(:,:)
    integer (c_int), intent(inout)                :: target_id
    real (c_float), intent(inout), allocatable    :: result_vector(:)
    real (c_float), intent(inout), optional       :: comparison_values(:,:)

    ! [ LOCALS ]
    real (c_float)  :: min_val
    real (c_float)  :: max_val
    real (c_float)  :: mean_val
    real (c_double) :: sum_val
    real (c_float)  :: n_val

    real (c_float)  :: min_val_comp
    real (c_float)  :: max_val_comp
    real (c_float)  :: mean_val_comp
    real (c_double) :: sum_val_comp
    real (c_float)  :: n_val_comp

    mean_val = 0.0
    max_val = -9.9e-23
    min_val = 9.9e+23
    n_val = 0

    min_val = minval( values, zone_ids==target_id .and. values > NC_FILL_FLOAT )
    max_val = maxval( values, zone_ids==target_id .and. values > NC_FILL_FLOAT )
    sum_val = sum( values, zone_ids==target_id .and. values > NC_FILL_FLOAT )
    n_val = count( zone_ids==target_id .and. values > NC_FILL_FLOAT )

    if ( n_val > 0 ) mean_val = sum_val / n_val

    min_val_comp = minval( comparison_values, zone_ids==target_id .and. comparison_values > NC_FILL_FLOAT )
    max_val_comp = maxval( comparison_values, zone_ids==target_id .and. comparison_values > NC_FILL_FLOAT )
    sum_val_comp = sum( comparison_values, zone_ids==target_id .and. comparison_values > NC_FILL_FLOAT )
    n_val_comp = count( zone_ids==target_id .and. comparison_values > NC_FILL_FLOAT )

    if ( n_val_comp > 0 ) mean_val_comp = sum_val_comp / n_val_comp

    if ( allocated(result_vector) )  deallocate(result_vector)

    if ( present( comparison_values ) ) then
      allocate( result_vector(10) )
      result_vector(5) = min_val_comp
      result_vector(6) = max_val_comp
      result_vector(7) = mean_val_comp
      result_vector(8) = n_val_comp
    else
      allocate (result_vector(4))
    endif

    result_vector(1) = min_val
    result_vector(2) = max_val
    result_vector(3) = mean_val
    result_vector(4) = n_val

  end subroutine calc_zonal_stats

!------------------------------------------------------------------------------

  subroutine get_unique_int(grid_values, unique_val_list)

    integer (c_int), intent(in)           :: grid_values(:,:)
    type (STRING_LIST_T), intent(inout)   :: unique_val_list

    ! [ LOCALS ]
    integer (c_int)       :: ix, iy
    character (len=20)    :: sval

    do ix=1,nx
      do iy=1,ny

        if (grid_values(ix, iy) <= 0 )  cycle

        sval = asCharacter( grid_values(ix,iy) )

        if ( unique_val_list%count == 0) then
          call unique_val_list%append(sval)
        else
          if ( unique_val_list%countmatching( sval ) > 0 )  cycle
          call unique_val_list%append(sval)
        endif

      enddo
    enddo

  end subroutine get_unique_int

  subroutine open_output_netcdf_files()

    nc_variable_name_str = trim(ncfile_in%pNC_VAR(NC_Z)%sVariableName)

    if (OUTPUT_TYPE_OPTION /= OUTPUT_TYPE_CSV_ONLY) then

      call netcdf_open_and_prepare_as_output(                                     &
            NCFILE=ncfile_out,                                                    &
            sVariableName=trim(nc_variable_name_str),                             &
            sVariableUnits=trim(temp_str3),                                       &
            iNX=ncfile_in%iNX,                                                    &
            iNY=ncfile_in%iNY,                                                    &
            fX=ncfile_in%rX_Coords,                                               &
            fY=ncfile_in%rY_Coords,                                               &
            PROJ4_string=trim(temp_str),                                          &
            StartDate=SIM_DT%start,                                               &
            EndDate=SIM_DT%end,                                                   &
            write_time_bounds=TRUE,                                               &
            filename_modifier=output_type_str )

      call netcdf_get_variable_id_for_variable( NCFILE=ncfile_out,                &
                                                variable_name="time_bnds",        &
                                                variable_id=time_bnds_varid      )

    endif

  end subroutine open_output_netcdf_files

end program swbstats2
