program swbstats2

  use iso_c_binding
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

! supply apparently missing parameter values from Intel implementation of ISO_C_BINDING
#ifdef __INTEL_COMPILER
  integer, parameter :: c_ptrdiff_t = 8
  integer, parameter :: c_diff_t = 8
#endif

  integer (kind=c_size_t)          :: RECNUM = 0

  character (len=256)            :: temp_string, sub_string

  character (len=70), allocatable :: usage_string(:)

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
  type (GENERAL_GRID_T), pointer :: pGrdVar
  type (GENERAL_GRID_T), pointer :: pGrdDelta
  type (GENERAL_GRID_T), pointer :: pGrdDelta2

  integer (kind=c_int), parameter  :: OPT_NO_TIME_SLICE        = 1
  integer (kind=c_int), parameter  :: OPT_SINGLE_TIME_SLICE    = 2
  integer (kind=c_int), parameter  :: OPT_MULTIPLE_TIME_SLICES = 3

  type SWBSTATS_OPTIONS_T
    logical (kind=c_bool)          :: dump_options_to_screen = FALSE
    logical (kind=c_bool)          :: write_csv              = FALSE
    logical (kind=c_bool)          :: write_grids            = FALSE
    logical (kind=c_bool)          :: calc_annual_stats      = FALSE
    logical (kind=c_bool)          :: calc_monthly_stats     = FALSE
    logical (kind=c_bool)          :: calc_zonal_stats       = FALSE
    logical (kind=c_bool)          :: compare_to_obs_values  = FALSE
    logical (kind=c_bool)          :: annualize_stats        = FALSE
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
    character (len=:), allocatable :: filename_modifier_string
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
      //TRIM(asCharacter(__INTEL_COMPILER))
      write(UNIT=*,FMT="(a,/)") "Compiler build date:"//TRIM(asCharacter(__INTEL_COMPILER_BUILD_DATE))
#endif

#ifdef __G95__
    write(UNIT=*,FMT="(a,/)") "Compiled with: G95 minor version " &
      //TRIM(int2char(__G95_MINOR__))
#endif

    allocate(usage_string(36))

    usage_string = [                                                           &
     "usage: swbstats2 [options] netcdf_file_name                           ", &
     "                                                                      ", &
     "  options:                                                            ", &
     "                                                                      ", &
     "  [ --annual_statistics ]                                             ", &
     "    calculate statistics for every calendar year between start and end", &
     "  [ --monthly_statistics ]                                            ", &
     "    calculate statistics for every month between start and end        ", &
     "  [ --slice= ]                                                        ", &
     "    dates over which statistics should be calculated,                 ", &
     "    with start date and end date formatted as yyyy-mm-dd,yyyy-mm-dd   ", &
     "  [ --stress_period_file= ]                                           ", &
     "    comma-delimited file containing stress period start and           ", &
     "    end date, with header labels and comments starting with'#':       ", &
     "      id,start_date,end_date                                          ", &
     "      #2,1870-01-01,1898-12-31                                        ", &
     "      5,1920-01-01,1925-12-31                                         ", &
     "      6,1925-01-01,1930-12-31                                         ", &
     "  [ --comparison_scale_factor=]                                       ", &
     "    value to multiply comparison grid by before calculating statistics", &
     "  [ --comparison_grid= ]                                              ", &
     "    name of real-valued grid to compare SWB output against            ", &
     "  [ --comparison_period_file= ]                                       ", &
     "    comma-delimited file containing comparison period start and       ", &
     "    end date, with header labels and comments starting with'#':       ", &
     "      id,start_date,end_date,comparison_grid_filename                 ", &
     "      #2,1870-01-01,1898-12-31,comparison_file_period_2.asc           ", &
     "      5,1920-01-01,1925-12-31,comparison_file_period_5.asc            ", &
     "      6,1925-01-01,1930-12-31,comparison_file_period_6.asc            ", &
     "  [ --zone_grid= ]                                                    ", &
     "    name of integer-valued grid for which zonal statistics are desired", &
     "  [ --zone_period_file= ]                                             ", &
     "    comma-delimited file containing calculation period start and      ", &
     "    end date, with header labels and comments starting with'#':       ", &
     "      id,start_date,end_date,zone_grid_filename                       ", &
     "      #2,1870-01-01,1898-12-31,zone_grod_file_period_2.asc            ", &
     "      5,1920-01-01,1925-12-31,zone_grid_file_period_5.asc             ", &
     "      6,1925-01-01,1930-12-31,zone_grid_file_period_6.asc             "  &
     ]

     do iIndex=1,ubound(usage_string,1)
       write(unit=*, fmt="(a)") trim(usage_string(iIndex))
     enddo

    stop

  end if

  call options%slice_start_date%setDateFormat("YYYY-MM-DD")
  call options%slice_end_date%setDateFormat("YYYY-MM-DD")
  options%filename_modifier_string = "PERIOD_STATS"//"_"

  do iIndex=1, iNumArgs

    call GET_COMMAND_ARGUMENT( iIndex, temp_string )

    if ( temp_string .containssimilar. "comparison_scale_factor" ) then

      sub_string = right(temp_string, substring="=")

      options%comparison_grid_conversion_factor = asFloat(sub_string)

    elseif ( temp_string .containssimilar. "annual_statistics" ) then

      options%calc_annual_stats = TRUE
      options%slice_option      = OPT_MULTIPLE_TIME_SLICES
      options%write_grids       = TRUE
      options%filename_modifier_string = "YEARLY_STATS"//"_"

    elseif ( temp_string .containssimilar. "monthly_statistics" ) then

      options%calc_monthly_stats = TRUE
      options%slice_option       = OPT_MULTIPLE_TIME_SLICES
      options%write_grids        = TRUE
      options%filename_modifier_string = "MONTHLY_STATS"//"_"

    elseif ( temp_string .containssimilar. "dump_options" ) then

      options%dump_options_to_screen = TRUE

    elseif ( temp_string .containssimilar. "annualize_means" ) then

      sub_string = right(temp_string, substring="=")
      if (sub_string .containssimilar. "TRUE") then

        options%annualize_stats = TRUE

      endif

    elseif ( temp_string .containssimilar. "stress_period_file" ) then

      options%slice_option = OPT_MULTIPLE_TIME_SLICES
      options%write_grids = TRUE
      options%stress_period_filename = right(temp_string, substring="=")
      call read_date_range_file( csv_filename=options%stress_period_filename,       &
                                    date_range_id_list=options%date_range_id_list,  &
                                    start_date_list=options%start_date_list,        &
                                    end_date_list=options%end_date_list )
      options%filename_modifier_string = "STRESS_PERIOD_STATS"//"_"

    elseif ( temp_string .containssimilar. "zone_period_file" ) then

      options%slice_option        = OPT_MULTIPLE_TIME_SLICES
      options%multiple_zone_grids = TRUE
      options%calc_zonal_stats    = TRUE
      options%write_csv           = TRUE

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
      options%write_csv                 = TRUE
      options%filename_modifier_string = "COMPARISON_PERIOD_STATS"//"_"

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

      options%write_grids = TRUE
      options%slice_option = OPT_SINGLE_TIME_SLICE

      call options%slice_start_date%parseDate(start_date_string)
      call options%slice_start_date%calcGregorianDate()

      call options%slice_end_date%parseDate(end_date_string)
      call options%slice_end_date%calcGregorianDate()
      options%filename_modifier_string = "SLICE_STATS--"                     &
        //options%slice_start_date%prettydate()//"_to_"                        &
        //options%slice_end_date%prettydate()//"_"

    elseif ( temp_string .containssimilar. "zone_grid" ) then

      options%zone_grid_filename = right(temp_string, substring="=")
      options%calc_zonal_stats   = TRUE
      options%write_csv          = TRUE

    elseif ( temp_string .containssimilar. "comparison_grid" ) then

      options%comparison_grid_filename = right(temp_string, substring="=")
      options%compare_to_obs_values    = TRUE
      options%write_csv                = TRUE

    elseif ( temp_string .contains. "--" ) then

       ! ??? gfortran 5.4 doesn't support variables within the stop statement???
       print *, "Unknown swbstats2 option: "//sQuote(temp_string)//"."
      stop

    else

      ! no match on the command-line argument flags; options must be the netCDF file
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

  pGrdVar  => grid_Create ( iNX=ncfile_in%iNX,             &
            iNY=ncfile_in%iNY,                             &
            rX0=ncfile_in%rX(NC_LEFT),                     &
            rY0=ncfile_in%rY(NC_BOTTOM),                   &
            rX1=ncfile_in%rX(NC_RIGHT),                    &
            rY1=ncfile_in%rY(NC_TOP),                      &
            iDataType=GRID_DATATYPE_REAL )

  ! delta, delta2 are temporary work grids used in calculating variance
  pGrdDelta   =>  grid_Create ( iNX=ncfile_in%iNX,               &
                  iNY=ncfile_in%iNY,                             &
                  rX0=ncfile_in%rX(NC_LEFT),                     &
                  rY0=ncfile_in%rY(NC_BOTTOM),                   &
                  rX1=ncfile_in%rX(NC_RIGHT),                    &
                  rY1=ncfile_in%rY(NC_TOP),                      &
                  iDataType=GRID_DATATYPE_REAL )

  pGrdDelta2  =>  grid_Create ( iNX=ncfile_in%iNX,               &
                  iNY=ncfile_in%iNY,                             &
                  rX0=ncfile_in%rX(NC_LEFT),                     &
                  rY0=ncfile_in%rY(NC_BOTTOM),                   &
                  rX1=ncfile_in%rX(NC_RIGHT),                    &
                  rY1=ncfile_in%rY(NC_TOP),                      &
                  iDataType=GRID_DATATYPE_REAL )

  ! extract basic information about the SWB computational grid
  ! 'BNDS' is defined in module constants_and_conversions
  BNDS%iNumCols = ncfile_in%iNX
  BNDS%iNumRows = ncfile_in%iNY
  BNDS%fX_ll = pGrdNative%rX0
  BNDS%fY_ll = pGrdNative%rY0
  BNDS%fX_ur = pGrdNative%rX1
  BNDS%fY_ur = pGrdNative%rY1
  BNDS%fGridCellSize = pGrdNative%rGridCellSize
  BNDS%sPROJ4_string = options%target_proj4_string

  if (options%dump_options_to_screen)  call print_all_options(options)

  if (options%calc_annual_stats) then

    call create_date_list_for_annual_statistics(options)

  endif

  if (options%calc_monthly_stats) then

    call create_date_list_for_monthly_statistics(options)

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

  elseif (options%slice_option == OPT_NO_TIME_SLICE) then
    ! no slice dates were provided; default to a single slice with start/end date
    ! corresponding to the start/end date of the data bounds
    options%slice_option = OPT_SINGLE_TIME_SLICE
    call SIM_DT%initialize( options%data_start_date, options%data_end_date )
  else
    ! default time slice = all data in netcdf file
    call SIM_DT%initialize( options%data_start_date, options%data_end_date )
  endif

  if (options%calc_zonal_stats) then
    if (.not. options%multiple_zone_grids) then
      call initialize_zone_grid(grid_filename=options%zone_grid_filename)
      call get_unique_int(pZONE_GRID%pGrdBase%iData, options%unique_zone_list)
    endif
    temp_string = "zonal_stats_"//trim(options%netcdf_variable_name_string)//".csv"
    call open_output_csv_file(trim(temp_string))
  endif

  if (options%write_grids)  call open_output_netcdf_files(options)

  ! Done with preliminaries (opening files, setting options, etc.); begin calcs

  if ( options%slice_option == OPT_SINGLE_TIME_SLICE ) then

    call calculate_slice_statistics(grid_sum=pGrdSum, grid_mean=pGrdMean,           &
                            grid_variance=pGrdVar, grid_delta=pGrdDelta,    &
                            grid_delta2=pGrdDelta2,                         &
                            start_date=options%slice_start_date,            &
                            end_date=options%slice_end_date)

    if (options%write_grids) then

      call write_stats_to_netcdf(grid_sum=pGrdSum, grid_mean=pGrdMean,          &
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

    !call netcdf_close_file(NCFILE=ncfile_out)

  elseif (options%slice_option == OPT_MULTIPLE_TIME_SLICES) then

    call SIM_DT%initialize( options%data_start_date, options%data_end_date )

    do iIndex=1, options%date_range_id_list%count

      call options%slice_start_date%setDateFormat("YYYY-MM-DD")
      call options%slice_end_date%setDateFormat("YYYY-MM-DD")

      start_date_string = options%start_date_list%get( iIndex )
      end_date_string = options%end_date_list%get( iIndex )
!      options%date_range_string = options%date_range_id_list%get( iIndex )

      call options%slice_start_date%parseDate( start_date_string )
      call options%slice_end_date%parseDate( end_date_string )

      options%date_range_string = options%slice_start_date%prettydate()    &
         //"_to_"//options%slice_end_date%prettydate()

      write(*,fmt="(a)") ""
      write(*,fmt="(a)") "Processing slice: ", options%slice_start_date%prettydate()    &
         //" to "//options%slice_end_date%prettydate()
      write(*,fmt="(a)") repeat("-", 70)

      ! grid_sum = simple addition over stack of grids;
      ! grid_mean is the grid_sum divided by number of days = daily mean
      call calculate_slice_statistics(grid_sum=pGrdSum, grid_mean=pGrdMean,   &
                              grid_variance=pGrdVar, grid_delta=pGrdDelta,    &
                              grid_delta2=pGrdDelta2,                         &
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

      if (options%write_grids) then
        call write_stats_to_netcdf(grid_sum=pGrdSum, grid_mean=pGrdMean,         &
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
               values=pGrdSum%rData,                                          &
               zone_ids=pZONE_GRID%pGrdBase%iData,                            &
               unique_zone_list=options%unique_zone_list,                     &
               comparison_values=pCOMPARISON_GRID%pGrdBase%rData,             &
               funit=csv_output_file%unit() )

        else
          call output_zonal_stats(                                            &
               start_date=options%slice_start_date,                           &
               end_date=options%slice_end_date,                               &
               values=pGrdSum%rData,                                          &
               zone_ids=pZONE_GRID%pGrdBase%iData,                            &
               unique_zone_list=options%unique_zone_list,                     &
               funit=csv_output_file%unit() )

        endif
      endif

      ! icky hack; need to advance the record number for the multiple slice calc
      RECNUM = RECNUM + 1

    enddo

    if (options%write_grids)  call netcdf_close_file(NCFILE=ncfile_out)

    if (options%write_csv)  call csv_output_file%close()

  endif

  if (options%dump_options_to_screen)  call print_all_options(options)

contains

  subroutine print_all_options(options)

    type (SWBSTATS_OPTIONS_T) :: options

    ! [ LOCALS ]
    character (len=:), allocatable  :: fmt_string

    write(*,fmt="(/,/,a)") "  :: Dump of swbstats2 program options ::"
    write(*,fmt="(a,/)") repeat("-", 43)

    fmt_string = "(a,t30,': ',l)"
    write(*,fmt=fmt_string) "write_csv", options%write_csv
    write(*,fmt=fmt_string) "write_grids", options%write_grids
    write(*,fmt=fmt_string) "calc_annual_stats", options%calc_annual_stats
    write(*,fmt=fmt_string) "calc_zonal_stats", options%calc_zonal_stats
    write(*,fmt=fmt_string) "compare_to_obs_values", options%compare_to_obs_values
    write(*,fmt=fmt_string) "annualize_stats", options%annualize_stats

    fmt_string = "(a,t30,': ',a)"
    write(*,fmt=fmt_string) "target_proj4_string", options%target_proj4_string
    write(*,fmt=fmt_string) "data_start_date", options%data_start_date%prettydate()
    write(*,fmt=fmt_string) "data_end_date", options%data_end_date%prettydate()
    write(*,fmt=fmt_string) "slice_start_date", options%slice_start_date%prettydate()
    write(*,fmt=fmt_string) "slice_end_date", options%slice_end_date%prettydate()
    !date_range_string

    fmt_string = "(a,t30,': ',i0)"
    write(*,fmt=fmt_string) "input grid bounds, nx", BNDS%iNumCols
    write(*,fmt=fmt_string) "input grid bounds, ny", BNDS%iNumRows

    fmt_string = "(a,t35,': ',f16.5)"
    write(*,fmt=fmt_string) "input grid bounds, X_ll", BNDS%fX_ll
    write(*,fmt=fmt_string) "input grid bounds, Y_ll", BNDS%fY_ll
    write(*,fmt=fmt_string) "input grid bounds, X_ur", BNDS%fX_ur
    write(*,fmt=fmt_string) "input grid bounds, Y_ur", BNDS%fY_ur
    write(*,fmt=fmt_string) "input grid bounds, grid-cell size", BNDS%fGridCellSize

    fmt_string = "(a,t30,': ',i0)"
    write(*,fmt=fmt_string) "slice_option", options%slice_option

    fmt_string = "(a,t30,': ',a)"
    write(*,fmt=fmt_string) "stress_period_filename", options%stress_period_filename
    write(*,fmt=fmt_string) "comparison_period_filename", options%comparison_period_filename
    write(*,fmt=fmt_string) "zone_period_filename", options%zone_period_filename
    write(*,fmt=fmt_string) "comparison_grid_filename", options%comparison_grid_filename
    write(*,fmt=fmt_string) "zone_grid_filename", options%zone_grid_filename
    write(*,fmt=fmt_string) "netcdf_input_filename", options%netcdf_input_filename
    write(*,fmt=fmt_string) "netcdf_variable_name_string", options%netcdf_variable_name_string
    write(*,fmt=fmt_string) "netcdf_variable_units_string", options%netcdf_variable_units_string

    fmt_string = "(a,t30,': ',l)"
    write(*,fmt=fmt_string) "netcdf_input_file_is_open    : ", options%netcdf_input_file_is_open
    write(*,fmt=fmt_string) "multiple_zone_grids          : ", options%multiple_zone_grids
    write(*,fmt=fmt_string) "multiple_comparison_grids    : ", options%multiple_comparison_grids
    !comparison_grid_conversion_factor = 1.0_c_double
    fmt_string = "(/,a,/,'"//repeat("-",62)//"')"
    write(*,fmt=fmt_string) "Start date list"
    call options%start_date_list%print()
    write(*,fmt=fmt_string) "End date list"
    call options%end_date_list%print()
    write(*,fmt=fmt_string) "Date index list"
    call options%date_range_id_list%print()
    write(*,fmt=fmt_string) "Unique zone id list"
    call options%unique_zone_list%print()
    write(*,fmt=fmt_string) "Comparison grid file list"
    call options%comparison_grid_file_list%print()
    write(*,fmt=fmt_string) "Zone grid file list"
    call options%zone_grid_file_list%print()

  end subroutine print_all_options

  subroutine write_stats_to_netcdf( grid_sum, grid_mean, start_date, end_date )

    type (GENERAL_GRID_T), pointer          :: grid_sum
    type (GENERAL_GRID_T), pointer          :: grid_mean
    type (DATETIME_T), intent(inout)        :: start_date
    type (DATETIME_T), intent(inout)        :: end_date

    ! [ LOCALS ]
    real (kind=c_double) :: start_bnd
    real (kind=c_double) :: end_bnd
    integer (kind=c_size_t) :: nx, ny

    nx = BNDS%iNumCols
    ny = BNDS%iNumRows

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

  end subroutine write_stats_to_netcdf



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
  ! subroutine calculate_slice_statisticss( grid_sum, grid_mean, date_range_id_list, &
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
  ! end subroutine calculate_slice_statisticss

  subroutine calculate_slice_statistics( grid_sum, grid_mean, grid_variance,    &
                                 grid_delta, grid_delta2, start_date, end_date, &
                                 grid_mask )

    type (GENERAL_GRID_T), pointer           :: grid_sum
    type (GENERAL_GRID_T), pointer           :: grid_mean
    type (GENERAL_GRID_T), pointer           :: grid_variance
    type (GENERAL_GRID_T), pointer           :: grid_delta
    type (GENERAL_GRID_T), pointer           :: grid_delta2
    type (DATETIME_T), intent(inout)         :: start_date
    type (DATETIME_T), intent(inout)         :: end_date
    logical, optional                        :: grid_mask(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)               :: julian_day_number
    real (kind=c_float), allocatable   :: tempvals(:)
    integer (kind=c_int)               :: day_count
    logical (kind=c_bool), allocatable :: local_mask(:,:)

    ! force slice dates to honor bounds of data dates
    if ( start_date < options%data_start_date )  start_date = options%data_start_date
    if ( end_date > options%data_end_date )      end_date = options%data_end_date

!    call SIM_DT%initialize( start_date, end_date )

    associate( grd_sum => grid_sum%rData, grd_mean => grid_mean%rData,    &
               grd_var => grid_variance%rData, delta => grid_delta%rData, &
               delta2 => grid_delta2%rData, grd_new => pGrdNative%rData      )

      allocate(local_mask(ubound(grd_sum,1),ubound(grd_sum,2)))

      day_count = 0

      do

        if ( SIM_DT%curr < start_date ) then
          call SIM_DT%addDay()
          cycle
        endif

        julian_day_number = int( SIM_DT%curr%dJulianDate, kind=c_int)

        day_count = day_count + 1

        if ( netcdf_update_time_starting_index(ncfile_in, julian_day_number ) )   then
          call netcdf_get_variable_slice(NCFILE=ncfile_in, rValues=grd_new )
        endif

        if (day_count == 1) then

          if (present(grid_mask)) then
            local_mask = (grd_new > NC_FILL_FLOAT) .and. grid_mask
          else
            local_mask = grd_new > NC_FILL_FLOAT
          endif

          grd_sum = 0.0_c_float
          grd_mean = grd_new
          grd_var = 0.0_c_float

        else

          ! where ( local_mask )
          !   delta = grd_new - grd_mean
          !   grd_sum = grd_sum + grd_new
          !   grd_mean = grd_mean + delta / real( counter, kind=c_float )
          !   delta2 = grd_new - grd_mean
          !   grd_var = grd_var + delta * delta2
          ! end where

          where ( local_mask )
            grd_sum = grd_sum + grd_new
          end where

        endif

  !      write(*,fmt="(a)") "gridsum: ", minval(grid_sum%rData,  mask=pGrdNative%rData > NC_FILL_FLOAT), &
  !          maxval(grid_sum%rData,  mask=pGrdNative%rData > NC_FILL_FLOAT)

        call SIM_DT%addDay
        if ( SIM_DT%curr > end_date )  exit

      end do

      where ( .not. local_mask )
        grd_mean = NC_FILL_FLOAT
        grd_sum = NC_FILL_FLOAT
        grd_var = NC_FILL_FLOAT
      ! elsewhere
      !   grd_var = grd_var / (counter - 1)
      endwhere

      if (options%annualize_stats) then
        where ( local_mask )
          ! grd_mean = grd_mean * 365.25
          ! grd_var = grd_var * 365.25
          grd_mean = grd_sum / real( day_count, kind=c_float ) * 365.25
        end where
      else
        where ( local_mask )
          grd_mean = grd_sum / real( day_count, kind=c_float )
        end where
      endif

    end associate

!    write(*,fmt="(a)") "gridsum: ", minval(grid_sum%rData,  mask=pGrdNative%rData > NC_FILL_FLOAT), &
!        maxval(grid_sum%rData,  mask=pGrdNative%rData > NC_FILL_FLOAT)

  end subroutine calculate_slice_statistics

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

    output_filename_str = "zone_grid__"//trim(description_str)//"__as_read_into_SWBSTATS2.asc"

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
    integer (c_int)                :: indx
    integer (c_int), allocatable   :: tempvals(:)
    integer (c_int)                :: number_of_matches
    real (c_float), allocatable    :: stats(:)
    integer (c_int)                :: funit_

    if (present(funit)) then
      funit_ = funit
    else
      funit_ = OUTPUT_UNIT
    endif

    ! obtain list of unique integers present in the zone grid
    tempvals = unique_zone_list%asInt()

    do indx=1,ubound(tempvals,1)
      n = tempvals(indx)
      if (present(comparison_values)) then
        call calc_zonal_stats(values, zone_ids, target_id=n, result_vector=stats, &
              comparison_values=comparison_values)
        write(unit=funit_, fmt="(2(a,', '),i0,', ',4(f14.5,', '),i0,', ',"  &
          //"4(f14.5,', '),i0)")                                            &
          start_date%prettydate(),end_date%prettydate(), n, stats(1:4),     &
          int(stats(5)), stats(6:9),int(stats(10))
      else
        call calc_zonal_stats(values, zone_ids, target_id=n, result_vector=stats)
        write(unit=funit_, fmt="(2(a,', '),i0,', ',4(f14.5,', '),i0)")         &
          start_date%prettydate(),end_date%prettydate(), n, stats(1:4),int(stats(5))
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
    real (c_double)  :: min_val
    real (c_double)  :: max_val
    real (c_double)  :: mean_val
    real (c_double) :: sum_val
    real (c_double)  :: n_val

    real (c_double)  :: min_val_comp
    real (c_double)  :: max_val_comp
    real (c_double)  :: mean_val_comp
    real (c_double) :: sum_val_comp
    real (c_double)  :: n_val_comp

    mean_val = 0.0; mean_val_comp = 0.0
    max_val = 0.0; max_val_comp = 0.0
    min_val = 0.0; min_val_comp = 0.0
    sum_val = 0.0; sum_val = 0.0
    n_val = 0.0; n_val_comp = 0.0

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
      result_vector(6) = min_val_comp
      result_vector(7) = max_val_comp
      result_vector(8) = mean_val_comp
      result_vector(9) = sum_val_comp
      result_vector(10) = n_val_comp
    else
      allocate (result_vector(5))
    endif

    result_vector(1) = min_val
    result_vector(2) = max_val
    result_vector(3) = mean_val
    result_vector(4) = sum_val
    result_vector(5) = n_val

  end subroutine calc_zonal_stats

!------------------------------------------------------------------------------

  subroutine get_unique_int(grid_values, unique_val_list)

    integer (c_int), intent(in)           :: grid_values(:,:)
    type (STRING_LIST_T), intent(inout)   :: unique_val_list

    ! [ LOCALS ]
    integer (c_int)       :: ix, iy
    character (len=20)    :: sval

    do ix=1,ubound(grid_values,1)
      do iy=1,ubound(grid_values,2)

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

    if (options%write_grids) then

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
            write_time_bounds=TRUE,                                               &
            filename_modifier=options%filename_modifier_string )

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
        //"mean_swb,sum_swb,count_swb,minimum_obs,maximum_obs,"               &
        //"mean_obs,sum_obs,count_obs"
    else
      header_str = "start_date,end_date,zone_id,minimum_swb,maximum_swb,"     &
        //"mean_swb,sum_swb,count_swb"
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

  subroutine create_date_list_for_monthly_statistics(options)

    type (SWBSTATS_OPTIONS_T), intent(inout)  :: options

    ! [ LOCALS ]
    integer (c_int)      :: indx
    type (DATE_RANGE_T)  :: MON_DT

    indx = 0

    call MON_DT%initialize( options%data_start_date, options%data_end_date )

    do
      indx = indx + 1
      call options%start_date_list%append( MON_DT%curr%prettydate() )
      call MON_DT%advance_to_last_day_of_month()
      call options%date_range_id_list%append( asCharacter(indx) )
      if ( MON_DT%curr < MON_DT%end ) then
        call options%end_date_list%append( MON_DT%curr%prettydate() )
        call MON_DT%addDay()
      else
        call options%end_date_list%append( MON_DT%end%prettydate() )
        exit
      endif
    enddo

  end subroutine create_date_list_for_monthly_statistics

end program swbstats2
