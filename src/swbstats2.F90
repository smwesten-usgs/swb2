program swbstats2

  use iso_c_binding, only             : c_int, c_float, c_double, c_bool,      &
                                        c_size_t, c_ptrdiff_t, c_long
  use iso_fortran_env, only           : OUTPUT_UNIT
  use constants_and_conversions, only : TRUE, FALSE, DATATYPE_INT,             &
                                        DATATYPE_FLOAT, BNDS, asFloat,         &
                                        OS_NATIVE_PATH_DELIMITER
  use file_operations, only           : ASCII_FILE_T
  use data_catalog_entry, only        : DATA_CATALOG_ENTRY_T
  use datetime, only                  : DATETIME_T, assignment(=), operator(>)
  use grid
  use netcdf4_support
  use simulation_datetime, only       : SIM_DT, DATE_RANGE_T
  use string_list, only               : STRING_LIST_T,                         &
                                        create_list
  use strings
  use version_control, only           : SWB_VERSION, GIT_COMMIT_HASH_STRING,   &
                                        GIT_BRANCH_STRING, COMPILE_DATE,       &
                                        COMPILE_TIME, SYSTEM_NAME

  use iso_fortran_env, only           : OUTPUT_UNIT, COMPILER_OPTIONS,         &
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
  logical (c_bool)     :: ANNUAL_STATISTICS = FALSE
  logical (c_bool)     :: MULTIPLE_COMPARISON_GRIDS = FALSE
  logical (c_bool)     :: MULTIPLE_ZONE_GRIDS = FALSE
  real (kind=c_float)  :: COMPARISON_GRID_SCALE_FACTOR = 1.0_c_float

  character (len=256)            :: temp_string, sub_string

  character (len=:), allocatable :: help_fmt_str

  type (DATA_CATALOG_ENTRY_T), pointer :: pZONE_GRID       => null()
  type (DATA_CATALOG_ENTRY_T), pointer :: pCOMPARISON_GRID => null()

  integer (kind=c_int)           :: iNumArgs
  character (len=1024)           :: sCompilerFlags
  character (len=256)            :: sCompilerVersion
  character (len=256)            :: sVersionString
  character (len=256)            :: sGitHashString
  character (len=:), allocatable :: start_date_string
  character (len=:), allocatable :: end_date_string
  integer (kind=c_int)           :: iCount
  integer (kind=c_int)           :: iIndex
  integer (kind=c_int), allocatable           ::iIndex_array(:)
  integer (kind=c_int)           :: iLen
  real (kind=c_double)           :: start_date_dbl
  real (kind=c_double)           :: end_date_dbl
  integer (kind=c_int)           :: time_bnds_varid

  logical (kind=c_bool)          :: netcdf_active = FALSE

  type (STRING_LIST_T)           :: name_list
  type (STRING_LIST_T)           :: value_list

  type (ASCII_FILE_T)            :: csv_output_file

  type (T_NETCDF4_FILE), pointer          :: ncfile_in
  type (T_NETCDF4_FILE), pointer          :: ncfile_out

  type (DATA_CATALOG_ENTRY_T)    :: input_data_obj
  type (GENERAL_GRID_T), pointer :: pGrdNative
  type (GENERAL_GRID_T), pointer :: pGrdSum
  type (GENERAL_GRID_T), pointer :: pGrdMean
  integer (kind=c_size_t)        :: nx, ny
  real (kind=c_float)            :: xll, yll, xur, yur, gridcell_size

  integer (kind=c_int), parameter  :: OPT_NO_TIME_SLICE        = 1
  integer (kind=c_int), parameter  :: OPT_SINGLE_TIME_SLICE    = 2
  integer (kind=c_int), parameter  :: OPT_MULTIPLE_TIME_SLICES = 3

  type SWBSTATS_OPTIONS_T
    logical (kind=c_bool)          :: write_csv             = FALSE
    logical (kind=c_bool)          :: write_netcdf          = TRUE
    logical (kind=c_bool)          :: calc_annual_stats     = FALSE
    logical (kind=c_bool)          :: calc_zonal_stats      = FALSE
    logical (kind=c_bool)          :: compare_to_obs_values = FALSE
    logical (kind=c_bool)          :: annualize_mean_stats  = TRUE
    character (len=:), allocatable :: target_proj4_string
    type (DATETIME_T)              :: data_start_date
    type (DATETIME_T)              :: data_end_date
    type (DATETIME_T)              :: slice_start_date
    type (DATETIME_T)              :: slice_end_date
    character (len=:), allocatable :: date_range_string
    integer (kind=c_int)           :: slice_option      = OPT_NO_TIME_SLICE
    character (len=:), allocatable :: stress_period_filename
    character (len=:), allocatable :: comparison_period_filename
    character (len=:), allocatable :: zone_period_filename
    character (len=:), allocatable :: comparison_grid_filename
    character (len=:), allocatable :: zone_grid_filename
    character (len=:), allocatable :: netcdf_input_filename
    character (len=:), allocatable :: netcdf_variable_units_string
    character (len=:), allocatable :: netcdf_variable_name_string
    logical (kind=c_bool)          :: netcdf_input_file_is_open = FALSE
    logical (kind=c_bool)          :: multiple_zone_grids       = FALSE
    logical (kind=c_bool)          :: multiple_comparison_grids = FALSE
    real (kind=c_double)           :: comparison_grid_conversion_factor = 1.0_c_double
    type (STRING_LIST_T)           :: start_date_list
    type (STRING_LIST_T)           :: end_date_list
    type (STRING_LIST_T)           :: date_range_id_list
    type (STRING_LIST_T)           :: unique_zone_list
    type (STRING_LIST_T)           :: comparison_grid_file_list
    type (STRING_LIST_T)           :: zone_grid_file_list
  end type SWBSTATS_OPTIONS_T

  type (SWBSTATS_OPTIONS_T)   :: options

  allocate( ncfile_in )
  allocate( ncfile_out )

  iNumArgs = COMMAND_ARGUMENT_COUNT()

  sVersionString = "  SWBSTATS2 - Soil Water Balance Statistics Code, SWB2 version " &
      //trim( SWB_VERSION )                                                          &
      //" -- compiled on: "//trim(COMPILE_DATE)//" "//trim(COMPILE_TIME)             &
      //", system name="//trim(SYSTEM_NAME)

  sGitHashString = "    [ Git branch and commit hash: "//trim( GIT_BRANCH_STRING )    &
     //", "//trim( GIT_COMMIT_HASH_STRING )//" ]"

   if (     (SYSTEM_NAME .containssimilar. "Windows")                            &
       .or. (SYSTEM_NAME .containssimilar. "Mingw") ) then
     OS_NATIVE_PATH_DELIMITER = "\"
   else
     OS_NATIVE_PATH_DELIMITER = "/"
   endif


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
      //"a,t36,a,/,t36,a,/,4(t43,a,/),"   &     ! --comparison_period_file
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
         "id,start_date,end_date",                                             &
         "#2,1870-01-01,1898-12-31",                                           &
         "5,1920-01-01,1925-12-31",                                            &
         "6,1925-01-01,1930-12-31",                                            &
     "[ --comparison_period_file= ]",                                          &
       "comma-delimited file containing comparison period start and ",         &
       "end date, with header labels and comments starting with'#':",          &
         "id,start_date,end_date,comparison_grid_filename",                    &
         "#2,1870-01-01,1898-12-31,comparison_file_period_2.asc",              &
         "5,1920-01-01,1925-12-31,comparison_file_period_5.asc",               &
         "6,1925-01-01,1930-12-31,comparison_file_period_6.asc",               &
     "[ --zone_grid= ]",                                                       &
       "name of integer-valued grid for which zonal statistics are desired",   &
     "[ --comparison_scale_factor=]",                                          &
     "value to multiply comparison grid by before calculating statistics",     &
     "[ --comparison_grid= ]",                                                 &
       "name of real-valued grid to compare SWB output against"

!             "[ --annualize_means ]     report mean values on an annual basis",                         &
    stop

  end if

  call options%slice_start_date%setDateFormat("YYYY-MM-DD")
  call options%slice_end_date%setDateFormat("YYYY-MM-DD")

  do iIndex=1, iNumArgs

    call GET_COMMAND_ARGUMENT( iIndex, temp_string )

    if ( temp_string .containssimilar. "output_type" ) then

      sub_string = right(temp_string, substring="=")
      if (sub_string .containssimilar. "CSV_ONLY") then

        options%write_csv = TRUE
        options%write_netcdf = FALSE

      endif

    elseif ( temp_string .containssimilar. "comparison_scale_factor" ) then

      sub_string = right(temp_string, substring="=")

      options%comparison_grid_conversion_factor = asFloat(sub_string)

    elseif ( temp_string .containssimilar. "annual_statistics" ) then

      options%calc_annual_stats = TRUE
      options%slice_option      = OPT_MULTIPLE_TIME_SLICES

    elseif ( temp_string .containssimilar. "annualize_means" ) then

      sub_string = right(temp_string, substring="=")
      if (sub_string .containssimilar. "TRUE") then

        options%annualize_mean_stats = TRUE

      endif

    elseif ( temp_string .containssimilar. "stress_period_file" ) then

      options%slice_option = OPT_MULTIPLE_TIME_SLICES
      options%stress_period_filename = right(temp_string, substring="=")
      call read_date_range_file( csv_filename=options%stress_period_filename,       &
                                    date_range_id_list=options%date_range_id_list,  &
                                    start_date_list=options%start_date_list,        &
                                    end_date_list=options%end_date_list )

    elseif ( temp_string .containssimilar. "zone_period_file" ) then

      options%slice_option        = OPT_MULTIPLE_TIME_SLICES
      options%multiple_zone_grids = TRUE
      options%calc_zonal_stats    = TRUE

      options%zone_period_filename = right(temp_string, substring="=")
      call read_zone_period_file( csv_filename=options%zone_period_filename , &
                                    date_range_id_list=options%date_range_id_list,    &
                                    start_date_list=options%start_date_list,          &
                                    end_date_list=options%end_date_list,              &
                                    zonal_stats_grid_file_list=options%zone_grid_file_list )

    elseif ( temp_string .containssimilar. "comparison_period_file" ) then

      options%slice_option              = OPT_MULTIPLE_TIME_SLICES
      options%multiple_comparison_grids = TRUE
      options%compare_to_obs_values     = TRUE


      options%comparison_period_filename = right(temp_string, substring="=")
      call read_comparison_period_file( csv_filename=options%comparison_period_filename,         &
                                    date_range_id_list=options%date_range_id_list,               &
                                    start_date_list=options%start_date_list,                     &
                                    end_date_list=options%end_date_list,                         &
                                    comparison_grid_file_list=options%comparison_grid_file_list )

    elseif ( temp_string .containssimilar. "slice" ) then

      sub_string          = right(temp_string, substring="=")
      start_date_string   = left(sub_string, substring=",")
      end_date_string     = right(sub_string, substring=",")

      call options%slice_start_date%parseDate(start_date_string)
      call options%slice_start_date%calcGregorianDate()

      call options%slice_end_date%parseDate(end_date_string)
      call options%slice_end_date%calcGregorianDate()


    elseif ( temp_string .containssimilar. "zone_grid" ) then

      options%zone_grid_filename = right(temp_string, substring="=")
      options%calc_zonal_stats   = TRUE

    elseif ( temp_string .containssimilar. "comparison_grid" ) then

      options%comparison_grid_filename = right(temp_string, substring="=")
      options%compare_to_obs_values    = TRUE

    else

      ! no match on the command-line argument flags; this must be the netCDF file
      options%netcdf_input_filename = trim(temp_string)

      call netcdf_open_and_prepare_for_merging( ncfile_in,                     &
                                                options%netcdf_input_filename, &
                                                guess_z_var_name=TRUE )

      ! extract integer start and end Julian dates from netCDF file
      start_date_dbl = real( ncfile_in%iFirstDayJD, kind=c_double )
      end_date_dbl = real( ncfile_in%iLastDayJD, kind=c_double )

      ! populate datetime data structure
      call options%data_start_date%setJulianDate(start_date_dbl)
      call options%data_start_date%calcGregorianDate()
      call options%data_end_date%setJulianDate(end_date_dbl)
      call options%data_end_date%calcGregorianDate()

      options%netcdf_input_file_is_open = TRUE

    endif

  enddo

  if (.not. options%netcdf_input_file_is_open)                                   &
    stop ("No netCDF file was specified or there was an error opening the file.")

  call netcdf_get_attribute_list_for_variable( NCFILE=ncfile_in,              &
                                               variable_name="crs",  &
                                               attribute_name_list=name_list, &
                                               attribute_value_list=value_list )



  ! extract PROJ4 string from netCDF file
  iIndex_array = name_list%which("proj4_string")
  temp_string = value_list%get( iIndex_array(1) )
  options%target_proj4_string = trim(temp_string)

  options%netcdf_variable_name_string = trim(ncfile_in%pNC_VAR(NC_Z)%sVariableName)

  call netcdf_get_attribute_list_for_variable(                                &
                   NCFILE=ncfile_in,                                          &
                   variable_name=options%netcdf_variable_name_string,         &
                   attribute_name_list=name_list,                             &
                   attribute_value_list=value_list )

  ! extract PROJ4 string from netCDF file
  iIndex_array = name_list%which("units")
  options%netcdf_variable_units_string = value_list%get( iIndex_array(1) )

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

  ! extract basic information about the SWB computational grid
  BNDS%iNumCols = pGrdNative%iNX
  BNDS%iNumRows = pGrdNative%iNY
  BNDS%fX_ll = pGrdNative%rX0
  BNDS%fY_ll = pGrdNative%rY0
  BNDS%fX_ur = pGrdNative%rX1
  BNDS%fY_ur = pGrdNative%rY1
  BNDS%fGridCellSize = pGrdNative%rGridCellSize
  BNDS%sPROJ4_string = options%target_proj4_string

  if (options%calc_annual_stats) then

    call create_date_list_for_annual_statistics(options)

  endif

  if (options%compare_to_obs_values) then
    call initialize_comparison_grid(grid_filename=options%comparison_grid_filename)
  endif

  if (options%slice_option == OPT_SINGLE_TIME_SLICE) then
    ! force slice dates to honor bounds of data dates
    if ( options%slice_start_date < options%data_start_date )                  &
      options%slice_start_date = options%data_start_date
    if ( options%slice_end_date > options%data_end_date )                      &
      options%slice_end_date = options%data_end_date

    call SIM_DT%initialize( options%slice_start_date, options%slice_end_date )

  else
    call SIM_DT%initialize( options%data_start_date, options%data_end_date )
  endif

  if (options%calc_zonal_stats) then
    if (.not. options%multiple_zone_grids) then
      call initialize_zone_grid(grid_filename=options%zone_grid_filename)
      call get_unique_int(pZONE_GRID%pGrdBase%iData, options%unique_zone_list)
    endif
    temp_string = "zonal_stats_"//trim(options%netcdf_variable_name_string)//".csv"
    call open_output_csv_file(temp_string)
  endif

  call open_output_netcdf_files(options)

  if ( options%slice_option == OPT_SINGLE_TIME_SLICE ) then

    call iterate_over_slice(grid_sum=pGrdSum, grid_mean=pGrdMean,              &
                            start_date=options%slice_start_date,               &
                            end_date=options%slice_end_date)

    if (options%write_csv) then

      call write_stats_to_files(grid_sum=pGrdSum, grid_mean=pGrdMean,          &
                                start_date=options%slice_start_date,           &
                                end_date=options%slice_end_date)

      call write_stats_to_arcgrid(grid_sum=pGrdSum, grid_mean=pGrdMean,        &
                                  start_date=options%slice_start_date,         &
                                  end_date=options%slice_end_date )

    endif

    if (options%calc_zonal_stats) then
      if ( associated(pCOMPARISON_GRID) ) then

        call output_zonal_stats(                                            &
             start_date=options%slice_start_date,                           &
             end_date=options%slice_end_date,                               &
             values=pGrdMean%rData,                                         &
             zone_ids=pZONE_GRID%pGrdBase%iData,                            &
             unique_zone_list=options%unique_zone_list,                     &
             comparison_values=pCOMPARISON_GRID%pGrdBase%rData,             &
             funit=csv_output_file%unit() )

      else

        call output_zonal_stats(                                            &
             start_date=options%slice_start_date,                           &
             end_date=options%slice_end_date,                               &
             values=pGrdMean%rData,                                         &
             zone_ids=pZONE_GRID%pGrdBase%iData,                            &
             unique_zone_list=options%unique_zone_list,                     &
             funit=csv_output_file%unit() )

      endif
    endif

    call netcdf_close_file(NCFILE=ncfile_out)

  elseif (options%slice_option == OPT_MULTIPLE_TIME_SLICES) then

    do iIndex=1, options%date_range_id_list%count

      start_date_string = options%start_date_list%get( iIndex )
      end_date_string = options%end_date_list%get( iIndex )
!      options%date_range_string = options%date_range_id_list%get( iIndex )

      call options%slice_start_date%setDateFormat("YYYY-MM-DD")
      call options%slice_end_date%setDateFormat("YYYY-MM-DD")

      call options%slice_start_date%parseDate( start_date_string )
      call options%slice_end_date%parseDate( end_date_string )

      options%date_range_string = options%slice_start_date%prettydate()    &
         //"_to_"//options%slice_end_date%prettydate()

      print *, ""
      print *, "Processing slice: ", options%slice_start_date%prettydate()    &
         //" to "//options%slice_end_date%prettydate()
      print *, repeat("-", 70)

      call iterate_over_slice(grid_sum=pGrdSum, grid_mean=pGrdMean,           &
                              start_date=options%slice_start_date,            &
                              end_date=options%slice_end_date)

      if (options%multiple_comparison_grids) then
        options%comparison_grid_filename = options%comparison_grid_file_list%get( iIndex )
        call initialize_comparison_grid(grid_filename=options%comparison_grid_filename)
      endif

      if (options%multiple_zone_grids) then
        options%zone_grid_filename = options%zone_grid_file_list%get( iIndex )
        call initialize_zone_grid(grid_filename=options%zone_grid_filename)
        call get_unique_int(pZONE_GRID%pGrdBase%iData, options%unique_zone_list)
      endif

      if (options%write_csv) then
        call write_stats_to_files(grid_sum=pGrdSum, grid_mean=pGrdMean,         &
                                  start_date=options%slice_start_date,          &
                                  end_date=options%slice_end_date)

        call write_stats_to_arcgrid(grid_sum=pGrdSum, grid_mean=pGrdMean,       &
                                    start_date=options%slice_start_date,        &
                                    end_date=options%slice_end_date,            &
                                    date_range_string=options%date_range_string)
      endif

      if (options%calc_zonal_stats) then
        if ( associated(pCOMPARISON_GRID) ) then

          call output_zonal_stats(                                            &
               start_date=options%slice_start_date,                           &
               end_date=options%slice_end_date,                               &
               values=pGrdMean%rData,                                         &
               zone_ids=pZONE_GRID%pGrdBase%iData,                            &
               unique_zone_list=options%unique_zone_list,                     &
               comparison_values=pCOMPARISON_GRID%pGrdBase%rData,             &
               funit=csv_output_file%unit() )

        else
          call output_zonal_stats(                                            &
               start_date=options%slice_start_date,                           &
               end_date=options%slice_end_date,                               &
               values=pGrdMean%rData,                                         &
               zone_ids=pZONE_GRID%pGrdBase%iData,                            &
               unique_zone_list=options%unique_zone_list,                     &
               funit=csv_output_file%unit() )

        endif
      endif

      ! icky hack; need to advance the record number for the multiple slice calc
      RECNUM = RECNUM + 1

    enddo

    if (options%write_netcdf)  call netcdf_close_file(NCFILE=ncfile_out)

    if (options%write_csv)  call csv_output_file%close()

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
                                     end_date, date_range_string )

    type (GENERAL_GRID_T), pointer          :: grid_sum
    type (GENERAL_GRID_T), pointer          :: grid_mean
    type (DATETIME_T), intent(inout)        :: start_date
    type (DATETIME_T), intent(inout)        :: end_date
    character (len=*), intent(in), optional :: date_range_string

    ! [ LOCALS ]
    character (len=:), allocatable  :: filename
    integer (kind=c_int)            :: nx, ny

    nx = BNDS%iNumCols
    ny = BNDS%iNumRows

    if ( present(date_range_string) ) then
      !@todo: change filename depending on the desired output statistic
      filename = trim(date_range_string)//"__"                                 &
                 //trim(options%netcdf_variable_name_string)                   &
                 //"__"//start_date%prettydate()//"_to_"                       &
                 //end_date%prettydate()                                       &
                 //"__"//asCharacter(nx)//"_by_"//asCharacter(ny)              &
                 //"__MEAN_ANNUAL.asc"
    else
      filename = trim(options%netcdf_variable_name_string)                      &
                 //"__"//start_date%prettydate()//"_to_"//end_date%prettydate() &
                 //"__"//asCharacter(nx)//"_by_"//asCharacter(ny)               &
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
  ! subroutine iterate_over_slices( grid_sum, grid_mean, date_range_id_list, &
  !                                 start_date_list, end_date_list )
  !
  !   type (GENERAL_GRID_T), pointer          :: grid_sum
  !   type (GENERAL_GRID_T), pointer          :: grid_mean
  !   type (STRING_LIST_T), intent(in)        :: date_range_id_list
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
    if ( start_date < options%data_start_date )  start_date = options%data_start_date
    if ( end_date > options%data_end_date )      end_date = options%data_end_date

!    call SIM_DT%initialize( start_date, end_date )

    grid_sum%rData = 0.0_c_float
    grid_mean%rData = 0.0_c_float

    do

      if ( SIM_DT%curr < start_date ) then
        call SIM_DT%addDay()
        cycle
      endif

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

      call SIM_DT%addDay
      if ( SIM_DT%curr > end_date )  exit

    end do

    where ( pGrdNative%rData <= NC_FILL_FLOAT )
      grid_mean%rData = NC_FILL_FLOAT
      grid_sum%rData = NC_FILL_FLOAT
    endwhere

    if (options%annualize_mean_stats) then
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

  subroutine read_date_range_file( csv_filename,                           &
                                      date_range_id_list,                  &
                                      start_date_list,                        &
                                      end_date_list )

    character (len=*), intent(inout)   :: csv_filename
    type (STRING_LIST_T), intent(out)  :: date_range_id_list
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
      call options%date_range_id_list%append( sItem )

      call chomp( sRecord, sItem, sDelimiters="," )
      call options%start_date_list%append( sItem )
      call options%end_date_list%append( sRecord )

    enddo

    call DF%close()

  end subroutine read_date_range_file


!------------------------------------------------------------------------------

  subroutine read_comparison_period_file( csv_filename,                       &
                                          date_range_id_list,                 &
                                          start_date_list,                    &
                                          end_date_list,                      &
                                          comparison_grid_file_list)

    character (len=*), intent(inout)   :: csv_filename
    type (STRING_LIST_T), intent(out)  :: date_range_id_list
    type (STRING_LIST_T), intent(out)  :: start_date_list
    type (STRING_LIST_T), intent(out)  :: end_date_list
    type (STRING_LIST_T), intent(out)  :: comparison_grid_file_list

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
      call options%date_range_id_list%append( sItem )

      call chomp( sRecord, sItem, sDelimiters="," )
      call options%start_date_list%append( sItem )

      call chomp( sRecord, sItem, sDelimiters="," )
      call options%end_date_list%append( sItem )

      call options%comparison_grid_file_list%append( sRecord )

    enddo

    call DF%close()

  end subroutine read_comparison_period_file

!------------------------------------------------------------------------------

  subroutine read_zone_period_file( csv_filename,                             &
                                          date_range_id_list,                 &
                                          start_date_list,                    &
                                          end_date_list,                      &
                                          zonal_stats_grid_file_list)

    character (len=*), intent(inout)   :: csv_filename
    type (STRING_LIST_T), intent(out)  :: date_range_id_list
    type (STRING_LIST_T), intent(out)  :: start_date_list
    type (STRING_LIST_T), intent(out)  :: end_date_list
    type (STRING_LIST_T), intent(out)  :: zonal_stats_grid_file_list

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
      call options%date_range_id_list%append( sItem )

      call chomp( sRecord, sItem, sDelimiters="," )
      call options%start_date_list%append( sItem )

      call chomp( sRecord, sItem, sDelimiters="," )
      call options%end_date_list%append( sItem )

      call options%zone_grid_file_list%append( sRecord )

    enddo

    call DF%close()

  end subroutine read_zone_period_file

!------------------------------------------------------------------------------

  subroutine initialize_zone_grid( grid_filename )

    character (len=*), intent(inout)   :: grid_filename

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iIndex
    integer (kind=c_int)                 :: iStat
    character (len=:), allocatable       :: description_str
    character (len=:), allocatable       :: output_filename_str

    description_str = left( grid_filename, substring=".")
    if (description_str .contains. "/") description_str = right( description_str, substring="/")
    if (description_str .contains. "\") description_str = right( description_str, substring="\")

    output_filename_str = "Zone_grid__"//trim(description_str)//"__as_read_into_SWBSTATS2.asc"

    ! allocate memory for a generic data_catalog_entry
    if (associated(pZONE_GRID))  deallocate(pZONE_GRID)
    nullify(pZONE_GRID)
    allocate(pZONE_GRID)

    call pZONE_GRID%set_target_PROJ4(options%target_proj4_string)

    call pZONE_GRID%initialize(          &
      sDescription="Zone Grid",          &
      sFileType="ARC_GRID",              &
      sFilename=trim(grid_filename),     &
      iDataType=DATATYPE_INT )

    call pZONE_GRID%getvalues()

    where ( pGrdNative%rData <= NC_FILL_FLOAT )
      pZONE_GRID%pGrdBase%iData = NC_FILL_INT
    end where

    call grid_WriteArcGrid(output_filename_str, pZONE_GRID%pGrdBase )

  end subroutine initialize_zone_grid

!------------------------------------------------------------------------------

  subroutine initialize_comparison_grid( grid_filename )

    character (len=*), intent(inout)   :: grid_filename

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iIndex
    integer (kind=c_int)                 :: iStat
    character (len=:), allocatable       :: description_str
    character (len=:), allocatable       :: output_filename_str

    description_str = left( grid_filename, substring=".")
    if (description_str .contains. "/") description_str = right( description_str, substring="/")
    if (description_str .contains. "\") description_str = right( description_str, substring="\")

    output_filename_str = "Comparison_grid__"//trim(description_str)//"__as_read_into_SWBSTATS2.asc"

    ! allocate memory for a generic data_catalog_entry
    if (.not. associated(pCOMPARISON_GRID))  allocate(pCOMPARISON_GRID, stat=iStat)

    call pCOMPARISON_GRID%set_target_PROJ4(options%target_proj4_string)

    call pCOMPARISON_GRID%initialize(          &
      sDescription="Comparison Grid",          &
      sFileType="ARC_GRID",                    &
      sFilename=trim(grid_filename),           &
      iDataType=DATATYPE_FLOAT )

    call pCOMPARISON_GRID%getvalues()

    where ( pGrdNative%rData <= NC_FILL_FLOAT )
      pCOMPARISON_GRID%pGrdBase%rData = NC_FILL_FLOAT
    elsewhere
      pCOMPARISON_GRID%pGrdBase%rData = pCOMPARISON_GRID%pGrdBase%rData           &
                                      * options%comparison_grid_conversion_factor
    end where

    call grid_WriteArcGrid(output_filename_str, pCOMPARISON_GRID%pGrdBase )

  end subroutine initialize_comparison_grid

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
          write(unit=funit_, fmt="(2(a,', '),i8,', ',3(f12.3,', '),i8,', ',"  &
            //"3(f12.3,', '),i8)")                                            &
            start_date%prettydate(),end_date%prettydate(), n, stats(1:3),     &
            int(stats(4)), stats(5:7),int(stats(8))
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
    max_val = 0.0
    min_val = 0.0
    n_val = 0

    n_val = count( zone_ids==target_id .and. values > NC_FILL_FLOAT )

    if ( n_val > 0 ) then
      sum_val = sum( values, zone_ids==target_id .and. values > NC_FILL_FLOAT )
      mean_val = sum_val / n_val
      min_val = minval( values, zone_ids==target_id .and. values > NC_FILL_FLOAT )
      max_val = maxval( values, zone_ids==target_id .and. values > NC_FILL_FLOAT )
    endif

    if ( allocated(result_vector) )  deallocate(result_vector)

    if ( present( comparison_values ) ) then

      n_val_comp = count( zone_ids==target_id .and. comparison_values > NC_FILL_FLOAT )

      if ( n_val_comp > 0 ) then
        n_val_comp = count( zone_ids==target_id .and. comparison_values > NC_FILL_FLOAT )
        sum_val_comp = sum( comparison_values, zone_ids==target_id .and. comparison_values > NC_FILL_FLOAT )
        mean_val_comp = sum_val_comp / n_val_comp
        min_val_comp = minval( comparison_values, zone_ids==target_id .and. comparison_values > NC_FILL_FLOAT )
        max_val_comp = maxval( comparison_values, zone_ids==target_id .and. comparison_values > NC_FILL_FLOAT )
      endif

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

  subroutine open_output_netcdf_files(options)

    type (SWBSTATS_OPTIONS_T), intent(inout)     :: options

    if (options%write_netcdf) then

      call netcdf_open_and_prepare_as_output(                                     &
            NCFILE=ncfile_out,                                                    &
            sVariableName=trim(options%netcdf_variable_name_string),              &
            sVariableUnits=trim(options%netcdf_variable_units_string),            &
            iNX=ncfile_in%iNX,                                                    &
            iNY=ncfile_in%iNY,                                                    &
            fX=ncfile_in%rX_Coords,                                               &
            fY=ncfile_in%rY_Coords,                                               &
            PROJ4_string=trim(options%target_proj4_string),                       &
            StartDate=SIM_DT%start,                                               &
            EndDate=SIM_DT%end,                                                   &
            write_time_bounds=TRUE)
            !filename_modifier=output_type_str )

      call netcdf_get_variable_id_for_variable( NCFILE=ncfile_out,                &
                                                variable_name="time_bnds",        &
                                                variable_id=time_bnds_varid      )

    endif

  end subroutine open_output_netcdf_files

  subroutine open_output_csv_file(filename)

    character (len=*)   :: filename

    ! [ LOCALS ]
    character (len=256)    :: header_str

    call csv_output_file%open(filename)

    if (options%multiple_comparison_grids .or. options%compare_to_obs_values) then
      header_str = "start_date,end_date,zone_id,minimum_swb,maximum_swb,"     &
        //"mean_swb,count_swb,minimum_obs,maximum_obs,"                       &
        //"mean_obs,count_obs"
    else
      header_str = "start_date,end_date,zone_id,minimum_swb,maximum_swb,"     &
        //"mean_swb,count_swb"
    endif

    call csv_output_file%writeLine(trim(header_str))

  end subroutine open_output_csv_file

!------------------------------------------------------------------------------

  subroutine create_date_list_for_annual_statistics(options)

    type (SWBSTATS_OPTIONS_T), intent(inout)  :: options

    ! [ LOCALS ]
    integer (c_int)      :: indx
    type (DATE_RANGE_T)  :: ANN_DT

    indx = 0

    call ANN_DT%initialize( options%data_start_date, options%data_end_date )

    do
      indx = indx + 1
      call options%start_date_list%append( ANN_DT%curr%prettydate() )
      call ANN_DT%advance_to_last_doy()
      call options%date_range_id_list%append( asCharacter(indx) )
      if ( ANN_DT%curr < ANN_DT%end ) then
        call options%end_date_list%append( ANN_DT%curr%prettydate() )
        call ANN_DT%addDay()
      else
        call options%end_date_list%append( ANN_DT%end%prettydate() )
        exit
      endif
    enddo

  end subroutine create_date_list_for_annual_statistics

end program swbstats2
