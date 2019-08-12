program swbstats2

  use iso_c_binding
  use constants_and_conversions, only : TRUE, FALSE, DATATYPE_INT,             &
                                        DATATYPE_FLOAT, BNDS, asFloat,         &
                                        OS_NATIVE_PATH_DELIMITER
  use exceptions, only                : assert, die
  use datetime, only                  : DATETIME_T, assignment(=), operator(>)
  use grid
  use netcdf4_support
  use simulation_datetime, only       : SIM_DT, DATE_RANGE_T
  use fstring_list, only              : FSTRING_LIST_T,                         &
                                        create_list
  use fstring
  use version_control, only           : SWB_VERSION, GIT_COMMIT_HASH_STRING,   &
                                        GIT_BRANCH_STRING, COMPILE_DATE,       &
                                        COMPILE_TIME, SYSTEM_NAME

  use iso_fortran_env, only           : OUTPUT_UNIT, COMPILER_OPTIONS,         &
                                        COMPILER_VERSION

  use swbstats2_support
  implicit none

  character (len=256)             :: command_arg_str
  character (len=256)             :: temp_string, sub_string
  character (len=70), allocatable :: usage_string(:)

  integer (c_int)                :: iNumArgs
  character (len=1024)           :: sCompilerFlags
  character (len=256)            :: sCompilerVersion
  character (len=256)            :: sVersionString
  character (len=256)            :: sGitHashString
  character (len=256)            :: sProgramName
  character (len=256)            :: sCompilationDateString
  character (len=256)            :: sCompilationSystemString
  character (len=:), allocatable :: start_date_string
  character (len=:), allocatable :: end_date_string
  integer (c_int)                :: iCount
  integer (c_int)                :: iIndex
  integer (c_int), allocatable   ::iIndex_array(:)
  integer (c_int)                :: iLen
  real (c_double)                :: start_date_dbl
  real (c_double)                :: end_date_dbl
  integer (c_int)                :: TIME_BNDS_VARID

  logical (c_bool)               :: netcdf_active = FALSE

  type (FSTRING_LIST_T)           :: name_list
  type (FSTRING_LIST_T)           :: value_list

  type (SWBSTATS_T)             :: swbstats

  allocate( swbstats%ncfile_in )

  iNumArgs = COMMAND_ARGUMENT_COUNT()

  sProgramName = "  SWB Statistics Calculator (SWBSTATS2), a companion to"
  sVersionString = "  Soil Water Balance Code version "//trim( SWB_VERSION )
  sCompilationDateString   = "     compilation date           : "              &
                               //trim(COMPILE_DATE)//" "//trim(COMPILE_TIME)
  sCompilationSystemString = "     compiled on                : "//trim(SYSTEM_NAME)

  if (     (SYSTEM_NAME .containssimilar. "Windows")                           &
      .or. (SYSTEM_NAME .containssimilar. "Mingw") ) then
    OS_NATIVE_PATH_DELIMITER = "\"
  else
    OS_NATIVE_PATH_DELIMITER = "/"
  endif

  sGitHashString = "     Git branch and commit hash : "                        &
                         //trim( adjustl(GIT_BRANCH_STRING ) )                 &
                         //", "//trim( GIT_COMMIT_HASH_STRING )

  iCount = max( len_trim( sVersionString ), len_trim( sGitHashString ) )
  iCount = max( iCount, len_trim( sCompilationSystemString ) )
  iCount = max( iCount, len_trim( sProgramName) )

  write(unit=*, fmt="(/,a)") repeat("-",iCount + 4)
  write(unit=*, fmt="(a,/)") trim( sProgramName )
  write(UNIT=*,FMT="(a,/)") trim( sVersionString )
  write(UNIT=*,FMT="(a)") trim( sCompilationDateString )
  write(UNIT=*,FMT="(a)") trim( sCompilationSystemString )
  write(UNIT=*,FMT="(a)") trim( sGitHashString )
  write(unit=*, fmt="(a,/)") repeat("-",iCount + 4)

  if(iNumArgs == 0 ) then

#ifdef __GFORTRAN__l
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

#ifdef __G95__l
    write(UNIT=*,FMT="(a,/)") "Compiled with: G95 minor version " &
      //TRIM(int2char(__G95_MINOR__))
#endif

    allocate(usage_string(44))

    usage_string = [                                                           &
     "usage: swbstats2 [options] netcdf_file_name                           ", &
     "                                                                      ", &
     "  options:                                                            ", &
     "                                                                      ", &
     "  [ --annual_statistics ]                                             ", &
     "    calculate statistics for every calendar year between start and end", &
     "  [ --monthly_statistics ]                                            ", &
     "    calculate statistics for every month between start and end        ", &
     "  [ --report_as_volume ]                                              ", &
     "    express statistics as a volume (cubic meters)                     ", &
     "  [ --report_in_meters ]                                              ", &
     "    express output statistics in *meters*                             ", &
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
     "      6,1925-01-01,1930-12-31,zone_grid_file_period_6.asc             ", &
     "  [ --{no_}netcdf_output ]                                            ", &
     "    toggle whether netCDF file is target for gridded output           ", &
     "  [ --{no_}arcgrid_output ]                                           ", &
     "    toggle whether an ASCII Arc Grid is target for gridded output     "  &
     ]

     do iIndex=1,ubound(usage_string,1)
       write(unit=*, fmt="(a)") trim(usage_string(iIndex))
     enddo

    stop

  end if

  output_files(STATS_SUM)%output_active = TRUE
  output_files(STATS_MEAN)%output_active = TRUE

  call swbstats%slice_start_date%setDateFormat("YYYY-MM-DD")
  call swbstats%slice_end_date%setDateFormat("YYYY-MM-DD")
  swbstats%filename_modifier_string = ""

  do iIndex=1, iNumArgs

    call GET_COMMAND_ARGUMENT( iIndex, command_arg_str )

    if ( command_arg_str .containssimilar. "comparison_scale_factor" ) then

      sub_string = right(command_arg_str, substring="=")

      swbstats%comparison_grid_conversion_factor = asFloat(sub_string)

    elseif ( command_arg_str .containssimilar. "annual_statistics" ) then

      swbstats%calculation_time_period = CALC_PERIOD_ANNUAL
      swbstats%filename_modifier_string = "YEARLY"

    elseif ( command_arg_str .containssimilar. "monthly_statistics" ) then

      swbstats%calculation_time_period = CALC_PERIOD_MONTHLY
      swbstats%filename_modifier_string = "MONTHLY"

    elseif ( command_arg_str .containssimilar. "dump_options" ) then

      swbstats%dump_options_to_screen = TRUE

    elseif ( command_arg_str .containssimilar. "report_as_volume" ) then

      swbstats%report_as_volume = TRUE

    elseif ( command_arg_str .containssimilar. "report_in_meters" ) then

      swbstats%report_in_meters = TRUE

    elseif ( command_arg_str .containssimilar. "annualize_sums" ) then

      swbstats%annualize_stats = TRUE

    elseif ( command_arg_str .containssimilar. "stress_period_file" ) then

      swbstats%calculation_time_period = CALC_PERIOD_SLICE_MULTIPLE
      swbstats%stress_period_filename = right(command_arg_str, substring="=")
      call swbstats%read_date_range_file(                                            &
                                    csv_filename=swbstats%stress_period_filename,    &
                                    date_range_id_list=swbstats%date_range_id_list,  &
                                    start_date_list=swbstats%start_date_list,        &
                                    end_date_list=swbstats%end_date_list )
      swbstats%filename_modifier_string = "STRESS_PERIOD_STATS"

    elseif ( command_arg_str .containssimilar. "zone_period_file" ) then

      swbstats%calculation_time_period = CALC_PERIOD_SLICE_MULTIPLE
      swbstats%multiple_zone_grids = TRUE
      swbstats%calc_zonal_stats    = TRUE
      swbstats%write_csv           = TRUE

      swbstats%zone_period_filename = right(command_arg_str, substring="=")
      call swbstats%read_zone_period_file(                                                   &
                                  csv_filename=swbstats%zone_period_filename ,               &
                                  date_range_id_list=swbstats%date_range_id_list,            &
                                  start_date_list=swbstats%start_date_list,                  &
                                  end_date_list=swbstats%end_date_list,                      &
                                  zonal_stats_grid_file_list=swbstats%zone_grid_file_list )

    elseif ( command_arg_str .containssimilar. "comparison_period_file" ) then

      swbstats%calculation_time_period = CALC_PERIOD_SLICE_MULTIPLE
      swbstats%multiple_comparison_grids = TRUE
      swbstats%compare_to_obs_values     = TRUE
      swbstats%write_csv                 = TRUE
      swbstats%filename_modifier_string = "COMPARISON_PERIOD_STATS"

      swbstats%comparison_period_filename = right(command_arg_str, substring="=")
      call swbstats%read_comparison_period_file(                                                  &
                                    csv_filename=swbstats%comparison_period_filename,             &
                                    date_range_id_list=swbstats%date_range_id_list,               &
                                    start_date_list=swbstats%start_date_list,                     &
                                    end_date_list=swbstats%end_date_list,                         &
                                    comparison_grid_file_list=swbstats%comparison_grid_file_list )

    elseif ( command_arg_str .containssimilar. "slice" ) then

      sub_string          = right(command_arg_str, substring="=")
      start_date_string   = left(sub_string, substring=",")
      end_date_string     = right(sub_string, substring=",")

      swbstats%calculation_time_period = CALC_PERIOD_SLICE_SINGLE

      call swbstats%slice_start_date%parseDate(start_date_string)
      call swbstats%slice_start_date%calcGregorianDate()

      call swbstats%slice_end_date%parseDate(end_date_string)
      call swbstats%slice_end_date%calcGregorianDate()
      swbstats%filename_modifier_string = "SLICE_STATS--"                      &
        //swbstats%slice_start_date%prettydate()//"_to_"                       &
        //swbstats%slice_end_date%prettydate()

    elseif ( command_arg_str .containssimilar. "zone_grid=" ) then

      swbstats%zone_grid_filename = right(command_arg_str, substring="=")
      swbstats%calc_zonal_stats   = TRUE
      swbstats%write_csv          = TRUE

    elseif ( command_arg_str .containssimilar. "zone_grid2=" ) then

      swbstats%zone_grid2_filename = right(command_arg_str, substring="=")
      swbstats%calc_zonal_stats   = TRUE
      swbstats%write_csv          = TRUE

    elseif ( command_arg_str .containssimilar. "comparison_grid" ) then

      swbstats%comparison_grid_filename = right(command_arg_str, substring="=")
      swbstats%compare_to_obs_values    = TRUE
      swbstats%write_csv                = TRUE

    elseif ( command_arg_str .strapprox. "--netcdf_output") then

      output_files%write_netcdf             = TRUE

    elseif ( command_arg_str .strapprox. "--no_netcdf_output" ) then

      output_files%write_netcdf             = FALSE

    elseif ( command_arg_str .strapprox. "--arcgrid_output" ) then

      output_files%write_arcgrid            = TRUE

    elseif ( command_arg_str .strapprox. "--no_arcgrid_output" ) then

      output_files%write_arcgrid            = FALSE

    elseif ( command_arg_str .contains. "--" ) then

       ! ??? gfortran 5.4 does not support variables within the stop statement???
       print *, "Unknown swbstats2 option: "//sQuote(command_arg_str)//"."
      stop

    else

      ! no match on the command-line argument flags; must be the netCDF file
      swbstats%netcdf_input_filename = trim(command_arg_str)

      call netcdf_open_and_prepare_for_merging( swbstats%ncfile_in,             &
                                                swbstats%netcdf_input_filename, &
                                                guess_z_var_name=TRUE )

      ! extract integer start and end Julian dates from netCDF file
      start_date_dbl = real( swbstats%ncfile_in%iFirstDayJD, c_double )
      end_date_dbl = real( swbstats%ncfile_in%iLastDayJD, c_double )

      ! populate datetime data structure
      call swbstats%data_start_date%setJulianDate(start_date_dbl)
      call swbstats%data_start_date%calcGregorianDate()
      call swbstats%data_end_date%setJulianDate(end_date_dbl)
      call swbstats%data_end_date%calcGregorianDate()

      swbstats%netcdf_input_file_is_open = TRUE

    endif

  enddo

  if (.not. swbstats%netcdf_input_file_is_open)                                   &
    stop ("No netCDF file was specified or there was an error opening the file.")

!----------------------------------------------------------------------------
! end of argument processing; now take action based on the options selected
!----------------------------------------------------------------------------

  call netcdf_get_attribute_list_for_variable( NCFILE=swbstats%ncfile_in,              &
                                               variable_name="crs",  &
                                               attribute_name_list=name_list, &
                                               attribute_value_list=value_list )

  ! extract PROJ4 string from netCDF file
  iIndex_array = name_list%which("proj4_string")
  temp_string = value_list%get( iIndex_array(1) )
  swbstats%target_proj4_string = trim(temp_string)

  swbstats%netcdf_variable_name_string = trim(swbstats%ncfile_in%pNC_VAR(NC_Z)%sVariableName)

  call netcdf_get_attribute_list_for_variable(                                 &
                   NCFILE=swbstats%ncfile_in,                                  &
                   variable_name=swbstats%netcdf_variable_name_string,         &
                   attribute_name_list=name_list,                              &
                   attribute_value_list=value_list )

  ! extract units description string from netCDF file
  iIndex_array = name_list%which("units")
  swbstats%netcdf_variable_units_string = value_list%get( iIndex_array(1) )

  ! create working grids with dimensions extracted from netCDF file being munged
  call swbstats%create_working_grids()

  if (swbstats%calculation_time_period == CALC_PERIOD_ANNUAL) then

    call swbstats%create_date_list_for_annual_statistics()

  endif

  if (swbstats%calculation_time_period == CALC_PERIOD_MONTHLY) then

    call swbstats%create_date_list_for_monthly_statistics()

  endif

  if (swbstats%report_in_meters) then

    ! set unit conversions needed to obtain output units of "meters"
    if (swbstats%netcdf_variable_units_string .containssimilar. "inch") then
      swbstats%unit_conversion_factor = 0.0254_c_double
    elseif (swbstats%netcdf_variable_units_string .containssimilar. "cm") then
      swbstats%unit_conversion_factor = 0.01_c_double
    elseif (swbstats%netcdf_variable_units_string .containssimilar. "mm") then
      swbstats%unit_conversion_factor = 0.001_c_double
    else
      call die("Was attempting to determine the output unit conversion factor," &
      //" but the netCDF variable has units of "                                &
      //sQuote(swbstats%netcdf_variable_units_string)//", which cannot"         &
      //" be converted to units of 'meters'.")
    endif

    swbstats%filename_modifier_string = trim(swbstats%filename_modifier_string) &
                                       //"__meters"

  endif

  if (swbstats%report_as_volume) then

    if (swbstats%report_as_volume) then
      if (swbstats%target_proj4_string .containssimilar. "units=m") then
        swbstats%length_conversion_factor = 1.0_c_double
      elseif (swbstats%target_proj4_string .containssimilar. "units=us-ft") then
        swbstats%length_conversion_factor = 0.3048006096012192_c_double
      elseif (swbstats%target_proj4_string .containssimilar. "units=ft") then
        swbstats%length_conversion_factor = 0.3048_c_double
      else
        call die("Was attempting to define length conversion factor, but no units " &
          //"information found in input PROJ4 string.")
      endif
      swbstats%grid_cell_area_sq_meters = (swbstats%grd_native%rGridCellSize                  &
                                           * swbstats%length_conversion_factor)**2
      swbstats%output_conversion_factor = swbstats%grid_cell_area_sq_meters          &
                                           * swbstats%unit_conversion_factor
    else
      swbstats%output_conversion_factor = swbstats%unit_conversion_factor
    endif

    swbstats%filename_modifier_string = trim(swbstats%filename_modifier_string)   &
                                       //"__cubic_meters"

  endif

  call assert(.not. (swbstats%report_as_volume .and. swbstats%report_in_meters),  &
       "Cannot use '--report_as_volume' and '--report_in_meters' in the same"   &
       //" swbstats run")

  if (swbstats%compare_to_obs_values) then
    call swbstats%initialize_comparison_grid(grid_filename=swbstats%comparison_grid_filename)
  endif

  if ( swbstats%calculation_time_period == CALC_PERIOD_SLICE_SINGLE) then

    ! force slice dates to honor bounds of data dates
    if ( swbstats%slice_start_date < swbstats%data_start_date )                  &
      swbstats%slice_start_date = swbstats%data_start_date
    if ( swbstats%slice_end_date > swbstats%data_end_date )                      &
      swbstats%slice_end_date = swbstats%data_end_date

    call SIM_DT%initialize( swbstats%slice_start_date, swbstats%slice_end_date )
    call swbstats%create_date_list_for_period_statistics()

  elseif (swbstats%calculation_time_period == CALC_PERIOD_ALL) then
    ! no slice dates were provided; default to a single slice with start/end date
    ! corresponding to the start/end date of the data bounds
    swbstats%calculation_time_period = CALC_PERIOD_SLICE_SINGLE
    call SIM_DT%initialize( swbstats%data_start_date, swbstats%data_end_date )
    call swbstats%create_date_list_for_period_statistics()
  else
    ! default time slice = all data in netcdf file
    call SIM_DT%initialize( swbstats%data_start_date, swbstats%data_end_date )
  endif

  if (swbstats%calc_zonal_stats) then
    if (.not. swbstats%multiple_zone_grids) then
      call swbstats%initialize_zone_grid(grid_filename=swbstats%zone_grid_filename)
      call swbstats%get_unique_int(swbstats%grd_zone%pGrdBase%iData, swbstats%unique_zone_list)

      if (len_trim(swbstats%zone_grid2_filename) > 0) then
        call swbstats%initialize_secondary_zone_grid(grid_filename=swbstats%zone_grid2_filename)
        call swbstats%get_unique_int(swbstats%grd_zone2%pGrdBase%iData, swbstats%unique_zone2_list)
      endif

    endif
    temp_string = "zonal_stats_"//trim(swbstats%netcdf_variable_name_string)//".csv"
    call swbstats%open_zonal_stats_output_file(trim(temp_string))
  endif

  call swbstats%open_output_netcdf_files(output_files)

  ! Done with preliminaries (opening files, setting options, etc.); begin calcs

  if (swbstats%dump_options_to_screen)  call swbstats%print_all_options()

  ! depending on options selected, we will have a list of:
  !  a) arbitrary time periods based on user-supplied date ranges;
  !  b) time periods bracketing the start and end of all months within the
  !     netCDF start and end period; or
  !  c) time periods bracketing complete calendar years within the start and
  !     end dates contained within the netCDF file to be munged.

  do iIndex=1, swbstats%date_range_id_list%count

    call swbstats%slice_start_date%setDateFormat("YYYY-MM-DD")
    call swbstats%slice_end_date%setDateFormat("YYYY-MM-DD")

    start_date_string = swbstats%start_date_list%get( iIndex )
    end_date_string = swbstats%end_date_list%get( iIndex )
!      swbstats%date_range_string = swbstats%date_range_id_list%get( iIndex )

    call swbstats%slice_start_date%parseDate( start_date_string )
    call swbstats%slice_end_date%parseDate( end_date_string )

    swbstats%date_range_string = swbstats%slice_start_date%prettydate()    &
       //"_to_"//swbstats%slice_end_date%prettydate()

    write(*,fmt="(a)") ""
    write(*,fmt="(a)") "Processing slice: ", swbstats%slice_start_date%prettydate()    &
       //" to "//swbstats%slice_end_date%prettydate()
    write(*,fmt="(a)") repeat("-", 70)

    ! grid_sum = simple addition over stack of grids;
    ! grid_mean is the grid_sum divided by number of days = daily mean
    call swbstats%calculate_slice_statistics( grid_delta=swbstats%grd_delta,            &
                                     grid_delta2=swbstats%grd_delta2,                   &
                                     start_date=swbstats%slice_start_date,     &
                                     end_date=swbstats%slice_end_date)

    if (swbstats%multiple_comparison_grids) then
      swbstats%comparison_grid_filename = swbstats%comparison_grid_file_list%get( iIndex )
      call swbstats%initialize_comparison_grid(grid_filename=swbstats%comparison_grid_filename)
    endif

    if (swbstats%multiple_zone_grids) then
      swbstats%zone_grid_filename = swbstats%zone_grid_file_list%get( iIndex )
      call swbstats%initialize_zone_grid(grid_filename=swbstats%zone_grid_filename)
      call swbstats%get_unique_int(swbstats%grd_zone%pGrdBase%iData, swbstats%unique_zone_list)
    endif

      call swbstats%write_stats_to_netcdf(output_files=output_files,           &
                                start_date=swbstats%slice_start_date,          &
                                end_date=swbstats%slice_end_date)

      call swbstats%write_stats_to_arcgrid(output_files=output_files,          &
                                  start_date=swbstats%slice_start_date,        &
                                  end_date=swbstats%slice_end_date,            &
                                  date_range_string=swbstats%date_range_string)


    if (swbstats%calc_zonal_stats) then
      if ( len_trim(swbstats%zone_grid2_filename) > 0) then

        if ( associated(swbstats%grd_comparison) ) then

          call swbstats%output_zonal_stats(                                      &
               start_date=swbstats%slice_start_date,                             &
               end_date=swbstats%slice_end_date,                                 &
               values=output_files(STATS_SUM)%grid_ptr%dpData,                   &
               zone_ids=swbstats%grd_zone%pGrdBase%iData,                        &
               unique_zone_list=swbstats%unique_zone_list,                       &
               zone2_ids=swbstats%grd_zone2%pGrdBase%iData,                      &
               unique_zone2_list=swbstats%unique_zone2_list,                     &
               comparison_values=swbstats%grd_comparison%pGrdBase%dpData,        &
               funit=swbstats%zonal_stats_output_file%unit() )

        else
          call swbstats%output_zonal_stats(                                      &
               start_date=swbstats%slice_start_date,                             &
               end_date=swbstats%slice_end_date,                                 &
               values=output_files(STATS_SUM)%grid_ptr%dpData,                   &
               zone_ids=swbstats%grd_zone%pGrdBase%iData,                        &
               unique_zone_list=swbstats%unique_zone_list,                       &
               zone2_ids=swbstats%grd_zone2%pGrdBase%iData,                      &
               unique_zone2_list=swbstats%unique_zone2_list,                     &
               funit=swbstats%zonal_stats_output_file%unit() )

        endif

      else
        if ( associated(swbstats%grd_comparison) ) then
          call swbstats%output_zonal_stats(                                      &
               start_date=swbstats%slice_start_date,                             &
               end_date=swbstats%slice_end_date,                                 &
               values=output_files(STATS_SUM)%grid_ptr%dpData,                   &
               zone_ids=swbstats%grd_zone%pGrdBase%iData,                               &
               unique_zone_list=swbstats%unique_zone_list,                       &
               comparison_values=swbstats%grd_comparison%pGrdBase%dpData,               &
               funit=swbstats%zonal_stats_output_file%unit() )

        else
          call swbstats%output_zonal_stats(                                      &
               start_date=swbstats%slice_start_date,                             &
               end_date=swbstats%slice_end_date,                                 &
               values=output_files(STATS_SUM)%grid_ptr%dpData,                   &
               zone_ids=swbstats%grd_zone%pGrdBase%iData,                               &
               unique_zone_list=swbstats%unique_zone_list,                       &
               funit=swbstats%zonal_stats_output_file%unit() )

        endif
      endif
    endif

    ! icky hack; need to advance the record number for the multiple slice calc
    RECNUM = RECNUM + 1

  enddo

  !call netcdf_close_file(NCFILE=ncfile_out)
  call swbstats%close_output_netcdf_files(output_files=output_files)

  if (swbstats%write_csv)  call swbstats%zonal_stats_output_file%close()

! endif

  if (swbstats%dump_options_to_screen)  call swbstats%print_all_options()

end program swbstats2
