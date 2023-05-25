module swbstats2_support

  use constants_and_conversions, only : TRUE, FALSE, DATATYPE_INT,             &
                                        DATATYPE_FLOAT, BNDS, asFloat,         &
                                        OS_NATIVE_PATH_DELIMITER
  use iso_fortran_env, only           : OUTPUT_UNIT
  use data_catalog_entry, only        : DATA_CATALOG_ENTRY_T
  use datetime, only                  : DATETIME_T, assignment(=), operator(>), &
                                          MONTHS
  use exceptions, only                : assert, die
  use file_operations, only           : ASCII_FILE_T
  use grid, only                      : GENERAL_GRID_T,                         &
                                        GRID_DATATYPE_DOUBLE,                   &
                                        grid_Create,                            &
                                        grid_WriteArcGrid
  use netcdf4_support
  use simulation_datetime, only       : SIM_DT, DATE_RANGE_T
  use fstring
  use fstring_list, only              : FSTRING_LIST_T,                         &
                                        create_list

  use running_grid_stats, only        : RUNNING_STATS_T
  use iso_c_binding
  implicit none

  enum, bind(c)
    enumerator :: STATS_JAN_SUM=1, STATS_JAN_MEAN, STATS_JAN_VARIANCE, &
                  STATS_FEB_SUM, STATS_FEB_MEAN, STATS_FEB_VARIANCE,   &
                  STATS_MAR_SUM, STATS_MAR_MEAN, STATS_MAR_VARIANCE,   &
                  STATS_APR_SUM, STATS_APR_MEAN, STATS_APR_VARIANCE,   &
                  STATS_MAY_SUM, STATS_MAY_MEAN, STATS_MAY_VARIANCE,   &
                  STATS_JUN_SUM, STATS_JUN_MEAN, STATS_JUN_VARIANCE,   &
                  STATS_JUL_SUM, STATS_JUL_MEAN, STATS_JUL_VARIANCE,   &
                  STATS_AUG_SUM, STATS_AUG_MEAN, STATS_AUG_VARIANCE,   &
                  STATS_SEP_SUM, STATS_SEP_MEAN, STATS_SEP_VARIANCE,   &
                  STATS_OCT_SUM, STATS_OCT_MEAN, STATS_OCT_VARIANCE,   &
                  STATS_NOV_SUM, STATS_NOV_MEAN, STATS_NOV_VARIANCE,   &
                  STATS_DEC_SUM, STATS_DEC_MEAN, STATS_DEC_VARIANCE,   &
                  STATS_MEAN, STATS_SUM, STATS_VARIANCE
  end enum

  enum, bind(c)
    enumerator :: CALC_PERIOD_ALL=1, CALC_PERIOD_ANNUAL, CALC_PERIOD_MONTHLY,  &
                  CALC_PERIOD_SLICE_SINGLE, CALC_PERIOD_SLICE_MULTIPLE,        &
                  CALC_PERIOD_WATER_YEAR, CALC_PERIOD_DAILY
  end enum

  integer (c_size_t)               :: RECNUM = 0


  type LENGTH_UNIT_T
    character (len=24)      :: unit_name
    character (len=10)      :: unit_abbrev
    real (c_double)         :: conversion_factor_from_inches
  end type LENGTH_UNIT_T

  type VOLUME_UNIT_T
    character (len=24)      :: unit_name
    character (len=10)      :: unit_abbrev
    real (c_double)         :: conversion_factor_from_cubic_feet
  end type VOLUME_UNIT_T

  type FILE_COLLECTION_T
    type (T_NETCDF4_FILE), pointer        :: nc_ptr => null()
    type (GENERAL_GRID_T), pointer        :: grid_ptr => null()
    character (len=8)                     :: stats_description
    integer (c_int)                       :: n_count = 0
    logical (c_bool)                      :: write_netcdf     = FALSE
    logical (c_bool)                      :: write_arcgrid    = FALSE
    logical (c_bool)                      :: output_active    = FALSE
  end type FILE_COLLECTION_T

  type SWBSTATS_T
    logical (c_bool)                :: dump_options_to_screen = FALSE
    logical (c_bool)                :: write_csv              = FALSE
    logical (c_bool)                :: calc_zonal_stats       = FALSE
    logical (c_bool)                :: compare_to_obs_values  = FALSE
    logical (c_bool)                :: annualize_stats        = FALSE
    logical (c_bool)                :: report_as_volume       = FALSE
    logical (c_bool)                :: report_in_meters       = FALSE
    character (len=:), allocatable  :: output_file_prefix 
    character (len=:), allocatable  :: target_proj4_string
    type (DATETIME_T)               :: data_start_date
    type (DATETIME_T)               :: data_end_date
    type (DATETIME_T)               :: slice_start_date
    type (DATETIME_T)               :: slice_end_date
    character (len=:), allocatable  :: slice_range_string
    character (len=:), allocatable  :: date_range_string
    integer (c_int)                 :: calculation_time_period = CALC_PERIOD_ALL
    character (len=:), allocatable  :: stress_period_filename
    character (len=:), allocatable  :: comparison_period_filename
    character (len=:), allocatable  :: zone_period_filename
    character (len=:), allocatable  :: comparison_grid_filename
    character (len=:), allocatable  :: zone_grid_filename
    character (len=:), allocatable  :: zone_grid2_filename
    character (len=:), allocatable  :: netcdf_input_filename
    character (len=:), allocatable  :: netcdf_variable_units_string
    character (len=:), allocatable  :: netcdf_variable_name_string
    character (len=:), allocatable  :: filename_modifier_string
    logical (c_bool)                :: netcdf_input_file_is_open = FALSE
    logical (c_bool)                :: multiple_zone_grids       = FALSE
    logical (c_bool)                :: multiple_comparison_grids = FALSE
    real (c_double)                 :: comparison_grid_conversion_factor = 1.0_c_double
    type (FSTRING_LIST_T)           :: start_date_list
    type (FSTRING_LIST_T)           :: end_date_list
    type (FSTRING_LIST_T)           :: date_range_id_list
    type (FSTRING_LIST_T)           :: unique_zone_list
    type (FSTRING_LIST_T)           :: unique_zone2_list
    type (FSTRING_LIST_T)           :: comparison_grid_file_list
    type (FSTRING_LIST_T)           :: zone_grid_file_list
    real (c_double)                 :: output_conversion_factor = 1.0_c_double
    real (c_double)                 :: unit_conversion_factor = 1.0_c_double
    real (c_double)                 :: grid_cell_area_sq_meters = 1.0_c_double
    real (c_double)                 :: length_conversion_factor = 1.0_c_double
    type (T_NETCDF4_FILE), pointer  :: ncfile_in
    type (ASCII_FILE_T)             :: zonal_stats_output_file
    type (ASCII_FILE_T)             :: whole_grid_stats_output_file

    type (GENERAL_GRID_T), pointer       :: grd_native => null()
    type (GENERAL_GRID_T), pointer       :: grd_delta => null()
    type (GENERAL_GRID_T), pointer       :: grd_delta2 => null()    
    type (DATA_CATALOG_ENTRY_T), pointer :: grd_zone => null()
    type (DATA_CATALOG_ENTRY_T), pointer :: grd_zone2 => null()
    type (DATA_CATALOG_ENTRY_T), pointer :: grd_comparison => null()

  contains

    procedure :: set_conversion_factors_sub
    generic   :: set_conversion_factors => set_conversion_factors_sub

    procedure :: create_working_grids
    procedure :: create_monthly_working_grids

    procedure :: open_zonal_stats_output_file

    procedure :: create_date_list_for_annual_statistics
    procedure :: create_date_list_for_period_statistics
    procedure :: create_date_list_for_monthly_statistics
    procedure :: create_date_list_for_daily_statistics

    procedure :: read_date_range_file
    procedure :: read_zone_period_file
    procedure :: read_comparison_period_file

    procedure :: get_unique_int

    procedure :: initialize_zone_grid
    procedure :: initialize_secondary_zone_grid
    procedure :: initialize_comparison_grid

    procedure :: calculate_slice_statistics
    procedure :: calculate_monthly_statistics

    procedure :: open_output_netcdf_files
    procedure :: close_output_netcdf_files

    procedure :: write_stats_to_netcdf
    procedure :: write_stats_to_arcgrid
    procedure :: write_monthly_stats_to_arcgrid
    procedure :: output_zonal_stats

    procedure :: print_all_options

  end type SWBSTATS_T

  type (LENGTH_UNIT_T), parameter :: LENGTH_UNITS(3) = [                       &
    LENGTH_UNIT_T("meters     ", "m ", 0.254_c_double),                        &
    LENGTH_UNIT_T("feet       ", "ft", 1.0_c_double / 12.0_c_double),          &
    LENGTH_UNIT_T("millimeters", "mm", 25.4_c_double) ]

  type (VOLUME_UNIT_T), parameter :: VOLUME_UNITS(3) =  [                      &
    VOLUME_UNIT_T("billions of gallons", "BG   ", 7.4805194805E-9_c_double ),  &
    VOLUME_UNIT_T("millions of gallons", "MG   ", 7.4805194805E-6_c_double),   &
    VOLUME_UNIT_T("acre-feet          ", "ac-ft", 0.000022956840808_c_double) ]

  type (DATA_CATALOG_ENTRY_T) :: input_data_obj

  type (FILE_COLLECTION_T)    :: OUTPUT_FILES(39)

  type ( RUNNING_STATS_T)     :: RSTAT

  type (RUNNING_STATS_T)      :: MON_STAT(12)

contains

  subroutine create_working_grids(this)

    class (SWBSTATS_T), intent(inout)  :: this

    ! create some working grids to hold raw and summed values
    this%grd_native  => grid_Create ( iNX=this%ncfile_in%iNX,                &
              iNY=this%ncfile_in%iNY,                                        &
              rX0=this%ncfile_in%rX(NC_LEFT),                                &
              rY0=this%ncfile_in%rY(NC_BOTTOM),                              &
              rX1=this%ncfile_in%rX(NC_RIGHT),                               &
              rY1=this%ncfile_in%rY(NC_TOP),                                 &
              iDataType=GRID_DATATYPE_DOUBLE )

    OUTPUT_FILES(STATS_SUM)%grid_ptr => grid_Create (                        &
              iNX=this%ncfile_in%iNX,                                        &
              iNY=this%ncfile_in%iNY,                                        &
              rX0=this%ncfile_in%rX(NC_LEFT),                                &
              rY0=this%ncfile_in%rY(NC_BOTTOM),                              &
              rX1=this%ncfile_in%rX(NC_RIGHT),                               &
              rY1=this%ncfile_in%rY(NC_TOP),                                 &
              iDataType=GRID_DATATYPE_DOUBLE )
    OUTPUT_FILES(STATS_SUM)%stats_description = "SUM"

    OUTPUT_FILES(STATS_MEAN)%grid_ptr => grid_Create (                       &
              iNX=this%ncfile_in%iNX,                                        &
              iNY=this%ncfile_in%iNY,                                        &
              rX0=this%ncfile_in%rX(NC_LEFT),                                &
              rY0=this%ncfile_in%rY(NC_BOTTOM),                              &
              rX1=this%ncfile_in%rX(NC_RIGHT),                               &
              rY1=this%ncfile_in%rY(NC_TOP),                                 &
              iDataType=GRID_DATATYPE_DOUBLE )
    OUTPUT_FILES(STATS_MEAN)%stats_description = "MEAN"

    OUTPUT_FILES(STATS_VARIANCE)%grid_ptr => grid_Create (                   &
              iNX=this%ncfile_in%iNX,                                        &
              iNY=this%ncfile_in%iNY,                                        &
              rX0=this%ncfile_in%rX(NC_LEFT),                                &
              rY0=this%ncfile_in%rY(NC_BOTTOM),                              &
              rX1=this%ncfile_in%rX(NC_RIGHT),                               &
              rY1=this%ncfile_in%rY(NC_TOP),                                 &
              iDataType=GRID_DATATYPE_DOUBLE )
      OUTPUT_FILES(STATS_VARIANCE)%stats_description = "VARIANCE"

    ! delta, delta2 are temporary work grids used in calculating variance
    this%grd_delta   =>  grid_Create ( iNX=this%ncfile_in%iNX,               &
                    iNY=this%ncfile_in%iNY,                                  &
                    rX0=this%ncfile_in%rX(NC_LEFT),                          &
                    rY0=this%ncfile_in%rY(NC_BOTTOM),                        &
                    rX1=this%ncfile_in%rX(NC_RIGHT),                         &
                    rY1=this%ncfile_in%rY(NC_TOP),                           &
                    iDataType=GRID_DATATYPE_DOUBLE )

    this%grd_delta2 =>  grid_Create ( iNX=this%ncfile_in%iNX,                &
                    iNY=this%ncfile_in%iNY,                                  &
                    rX0=this%ncfile_in%rX(NC_LEFT),                          &
                    rY0=this%ncfile_in%rY(NC_BOTTOM),                        &
                    rX1=this%ncfile_in%rX(NC_RIGHT),                         &
                    rY1=this%ncfile_in%rY(NC_TOP),                           &
                    iDataType=GRID_DATATYPE_DOUBLE )

    ! extract basic information about the SWB computational grid
    ! 'BNDS' is defined in module constants_and_conversions
    BNDS%iNumCols = this%ncfile_in%iNX
    BNDS%iNumRows = this%ncfile_in%iNY
    BNDS%fX_ll = this%grd_native%rX0
    BNDS%fY_ll = this%grd_native%rY0
    BNDS%fX_ur = this%grd_native%rX1
    BNDS%fY_ur = this%grd_native%rY1
    BNDS%fGridCellSize = this%grd_native%rGridCellSize
    BNDS%sPROJ4_string = this%target_proj4_string

  end subroutine create_working_grids

!------------------------------------------------------------------------------

  subroutine create_monthly_working_grids(this)

    class (SWBSTATS_T), intent(inout)  :: this

    integer (c_int)   :: month_num
    integer (c_int)   :: sum_index_num
    integer (c_int)   :: mean_index_num
    integer (c_int)   :: variance_index_num
    character (len=3) :: month_name

    do month_num = 1,12

      ! extract month name from the datetime module MONTHS data structure
      month_name = MONTHS(month_num)%sName
      sum_index_num = ( month_num - 1 ) * 3 + 1
      mean_index_num = ( month_num - 1 ) * 3 + 2
      variance_index_num = ( month_num - 1 ) * 3 + 3

      OUTPUT_FILES(sum_index_num)%grid_ptr => grid_Create (          &
      iNX=this%ncfile_in%iNX,                                        &
      iNY=this%ncfile_in%iNY,                                        &
      rX0=this%ncfile_in%rX(NC_LEFT),                                &
      rY0=this%ncfile_in%rY(NC_BOTTOM),                              &
      rX1=this%ncfile_in%rX(NC_RIGHT),                               &
      rY1=this%ncfile_in%rY(NC_TOP),                                 &
      iDataType=GRID_DATATYPE_DOUBLE )
      OUTPUT_FILES(sum_index_num)%stats_description = "SUM"
      OUTPUT_FILES(sum_index_num)%write_arcgrid = TRUE

      OUTPUT_FILES(mean_index_num)%grid_ptr => grid_Create (         &
      iNX=this%ncfile_in%iNX,                                        &
      iNY=this%ncfile_in%iNY,                                        &
      rX0=this%ncfile_in%rX(NC_LEFT),                                &
      rY0=this%ncfile_in%rY(NC_BOTTOM),                              &
      rX1=this%ncfile_in%rX(NC_RIGHT),                               &
      rY1=this%ncfile_in%rY(NC_TOP),                                 &
      iDataType=GRID_DATATYPE_DOUBLE )
      OUTPUT_FILES(mean_index_num)%stats_description = "MEAN"
      OUTPUT_FILES(mean_index_num)%write_arcgrid = TRUE

      OUTPUT_FILES(variance_index_num)%grid_ptr => grid_Create (     &
      iNX=this%ncfile_in%iNX,                                        &
      iNY=this%ncfile_in%iNY,                                        &
      rX0=this%ncfile_in%rX(NC_LEFT),                                &
      rY0=this%ncfile_in%rY(NC_BOTTOM),                              &
      rX1=this%ncfile_in%rX(NC_RIGHT),                               &
      rY1=this%ncfile_in%rY(NC_TOP),                                 &
      iDataType=GRID_DATATYPE_DOUBLE )
      OUTPUT_FILES(variance_index_num)%stats_description = "VARIANCE"

    enddo
        
  end subroutine create_monthly_working_grids    

!------------------------------------------------------------------------------

  subroutine calc_zonal_stats(values, zone_ids, target_id, result_vector, comparison_values)

    real (c_double), intent(inout)                 :: values(:,:)
    integer (c_int), intent(inout)                 :: zone_ids(:,:)
    integer (c_int), intent(inout)                 :: target_id
    real (c_double), intent(inout), allocatable    :: result_vector(:)
    real (c_double), intent(inout), optional       :: comparison_values(:,:)

    ! [ LOCALS ]
    real (c_double)  :: min_val
    real (c_double)  :: max_val
    real (c_double)  :: mean_val
    real (c_double)  :: sum_val
    real (c_double)  :: n_val

    real (c_double)  :: min_val_comp
    real (c_double)  :: max_val_comp
    real (c_double)  :: mean_val_comp
    real (c_double)  :: sum_val_comp
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

  subroutine calc_multizonal_stats(values, zone_ids, zone2_ids,                &
      target_id, target2_id, result_vector, comparison_values)

    real (c_double), intent(inout)                 :: values(:,:)
    integer (c_int), intent(inout)                 :: zone_ids(:,:)
    integer (c_int), intent(inout)                 :: zone2_ids(:,:)
    integer (c_int), intent(inout)                 :: target_id
    integer (c_int), intent(inout)                 :: target2_id
    real (c_double), intent(inout), allocatable    :: result_vector(:)
    real (c_double), intent(inout), optional       :: comparison_values(:,:)

    ! [ LOCALS ]
    real (c_double)  :: min_val
    real (c_double)  :: max_val
    real (c_double)  :: mean_val
    real (c_double)  :: sum_val
    real (c_double)  :: n_val

    real (c_double)  :: min_val_comp
    real (c_double)  :: max_val_comp
    real (c_double)  :: mean_val_comp
    real (c_double)  :: sum_val_comp
    real (c_double)  :: n_val_comp

    mean_val = 0.0; mean_val_comp = 0.0
    max_val = 0.0; max_val_comp = 0.0
    min_val = 0.0; min_val_comp = 0.0
    sum_val = 0.0; sum_val = 0.0
    n_val = 0.0; n_val_comp = 0.0

    n_val = count( zone_ids==target_id .and. zone2_ids==target2_id             &
                  .and. values > NC_FILL_FLOAT )

    if ( n_val > 0 ) then
      sum_val = sum( values, zone_ids==target_id  .and. zone2_ids==target2_id     &
                            .and. values > NC_FILL_FLOAT )
      mean_val = sum_val / n_val
      min_val = minval( values, zone_ids==target_id  .and. zone2_ids==target2_id  &
                                .and. values > NC_FILL_FLOAT )
      max_val = maxval( values, zone_ids==target_id  .and. zone2_ids==target2_id  &
                                .and. values > NC_FILL_FLOAT )
    endif

    if ( allocated(result_vector) )  deallocate(result_vector)

    if ( present( comparison_values ) ) then

      n_val_comp = count( zone_ids==target_id  .and. zone2_ids==target2_id      &
                         .and. comparison_values > NC_FILL_FLOAT )

      if ( n_val_comp > 0 ) then
        n_val_comp = count( zone_ids==target_id  .and. zone2_ids==target2_id    &
                           .and. comparison_values > NC_FILL_FLOAT )
        sum_val_comp = sum( comparison_values, zone_ids==target_id  .and. zone2_ids==target2_id             &
                                              .and. comparison_values > NC_FILL_FLOAT )
        mean_val_comp = sum_val_comp / n_val_comp
        min_val_comp = minval( comparison_values, zone_ids==target_id  .and. zone2_ids==target2_id             &
                                                 .and. comparison_values > NC_FILL_FLOAT )
        max_val_comp = maxval( comparison_values, zone_ids==target_id  .and. zone2_ids==target2_id             &
                                                 .and. comparison_values > NC_FILL_FLOAT )
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

  end subroutine calc_multizonal_stats

!------------------------------------------------------------------------------

  subroutine get_unique_int(this, grid_values, unique_val_list)

    class (SWBSTATS_T), intent(inout)      :: this
    integer (c_int), intent(in)            :: grid_values(:,:)
    type (FSTRING_LIST_T), intent(inout)   :: unique_val_list

    ! [ LOCALS ]
    integer (c_int)       :: ix, iy
    character (len=20)    :: sval

    do ix=1,ubound(grid_values,1)
      do iy=1,ubound(grid_values,2)

        if (grid_values(ix, iy) < 0 )  cycle

        sval = asCharacter( grid_values(ix,iy) )

        if ( unique_val_list%count == 0) then
          call unique_val_list%append(sval)

        else
          if ( unique_val_list%count_matching( sval ) > 0 )  cycle
          call unique_val_list%append(sval)

        endif

      enddo
    enddo

  end subroutine get_unique_int

  !------------------------------------------------------------------------------

    subroutine create_date_list_for_annual_statistics(this)

      class (SWBSTATS_T), intent(inout)  :: this

      ! [ LOCALS ]
      integer (c_int)      :: indx
      type (DATE_RANGE_T)  :: ANN_DT

      indx = 0

      call ANN_DT%initialize( this%data_start_date, this%data_end_date )

      do
        indx = indx + 1
        call this%start_date_list%append( ANN_DT%curr%prettydate() )
        call ANN_DT%advance_to_last_doy()
        call this%date_range_id_list%append( asCharacter(indx) )
        if ( ANN_DT%curr < ANN_DT%end ) then
          call this%end_date_list%append( ANN_DT%curr%prettydate() )
          call ANN_DT%addDay()
        else
          call this%end_date_list%append( ANN_DT%end%prettydate() )
          exit
        endif
      enddo

    end subroutine create_date_list_for_annual_statistics

  !------------------------------------------------------------------------------

  subroutine create_date_list_for_monthly_statistics(this)

    class (SWBSTATS_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (c_int)      :: indx
    type (DATE_RANGE_T)  :: MON_DT

    indx = 0

    call MON_DT%initialize( this%data_start_date, this%data_end_date )

    do
      indx = indx + 1
      call this%start_date_list%append( MON_DT%curr%prettydate() )
      call MON_DT%advance_to_last_day_of_month()
      call this%date_range_id_list%append( asCharacter(indx) )
      if ( MON_DT%curr < MON_DT%end ) then
        call this%end_date_list%append( MON_DT%curr%prettydate() )
        call MON_DT%addDay()
      else
        call this%end_date_list%append( MON_DT%end%prettydate() )
        exit
      endif
    enddo

  end subroutine create_date_list_for_monthly_statistics

!------------------------------------------------------------------------------

  subroutine create_date_list_for_daily_statistics(this)

    class (SWBSTATS_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (c_int)      :: indx
    type (DATE_RANGE_T)  :: DAILY_DT

    indx = 0

    call DAILY_DT%initialize( this%data_start_date, this%data_end_date )

    do
      indx = indx + 1
      call this%start_date_list%append( DAILY_DT%curr%prettydate() )
      call this%end_date_list%append( DAILY_DT%curr%prettydate() )
      call this%date_range_id_list%append( asCharacter(indx) )

      if ( DAILY_DT%curr < DAILY_DT%end ) then
        call DAILY_DT%addDay()
      else
        exit
      endif
    enddo

  end subroutine create_date_list_for_daily_statistics

!------------------------------------------------------------------------------

  subroutine create_date_list_for_period_statistics(this, start_date, end_date)

    class (SWBSTATS_T), intent(inout)  :: this
    type (DATETIME_T), intent(in)      :: start_date
    type (DATETIME_T), intent(in)      :: end_date

    call this%start_date_list%append( start_date%prettydate() )
    call this%date_range_id_list%append( "1" )
    call this%end_date_list%append( end_date%prettydate() )

  end subroutine create_date_list_for_period_statistics

!------------------------------------------------------------------------------

  subroutine initialize_zone_grid(this, grid_filename)

    class (SWBSTATS_T), intent(inout)  :: this
    character (len=*), intent(inout)   :: grid_filename

    ! [ LOCALS ]
    integer (c_int)                 :: iIndex
    integer (c_int)                 :: iStat
    character (len=:), allocatable       :: description_str
    character (len=:), allocatable       :: OUTPUT_FILESname_str
    character (len=256), save            :: previous_zone_filename

    ! OK, if this is the same ARC ASCII grid file processed in a previous iteration,
    ! we leave the values as they are and skip reading in the grid again
    if ( .not. trim(previous_zone_filename)==trim(grid_filename) ) then

      ! remove file extension and path from zone filename
      description_str = left( grid_filename, substring=".")
      if (description_str .contains. "/") description_str = right( description_str, substring="/")
      if (description_str .contains. "\") description_str = right( description_str, substring="\")

      OUTPUT_FILESname_str = "zone_grid__"//trim(description_str)//"__as_read_into_SWBSTATS2.asc"

      ! allocate memory for a generic data_catalog_entry
      if (associated(this%grd_zone))  deallocate(this%grd_zone)
      nullify(this%grd_zone)
      allocate(this%grd_zone)

      call this%grd_zone%set_target_PROJ4(this%target_proj4_string)
      call this%grd_zone%initialize(          &
        sDescription="Zone Grid",             &
        sFileType="ARC_GRID",                 &
        sFilename=trim(grid_filename),        &
        iDataType=DATATYPE_INT )

      call this%grd_zone%getvalues()

!    where ( this%grd_native%dpData <= NC_FILL_FLOAT )
!      this%grd_zone%pGrdBase%iData = NC_FILL_INT
!    end where

      call grid_WriteArcGrid(OUTPUT_FILESname_str, this%grd_zone%pGrdBase )

      ! set value of previous filename to the name of the grid we just processed.
      previous_zone_filename = trim(grid_filename)

      call this%unique_zone_list%clear()
      call this%get_unique_int(this%grd_zone%pGrdBase%iData, this%unique_zone_list)

    endif  

  end subroutine initialize_zone_grid

!------------------------------------------------------------------------------

  subroutine initialize_secondary_zone_grid(this, grid_filename)

    class (SWBSTATS_T), intent(inout)  :: this
    character (len=*), intent(inout)   :: grid_filename

    ! [ LOCALS ]
    integer (c_int)                 :: iIndex
    integer (c_int)                 :: iStat
    character (len=:), allocatable       :: description_str
    character (len=:), allocatable       :: OUTPUT_FILESname_str
    character (len=256), save            :: previous_zone_filename

    ! OK, if this is the same ARC ASCII grid file processed in a previous iteration,
    ! we leave the values as they are and skip reading in the grid again
    if ( .not. trim(previous_zone_filename)==trim(grid_filename) ) then

      description_str = left( grid_filename, substring=".")
      if (description_str .contains. "/") description_str = right( description_str, substring="/")
      if (description_str .contains. "\") description_str = right( description_str, substring="\")

      OUTPUT_FILESname_str = "zone_grid2__"//trim(description_str)//"__as_read_into_SWBSTATS2.asc"

      ! allocate memory for a generic data_catalog_entry
      if (associated(this%grd_zone2))  deallocate(this%grd_zone2)
      nullify(this%grd_zone2)
      allocate(this%grd_zone2)

      call this%grd_zone2%set_target_PROJ4(this%target_proj4_string)
      call this%grd_zone2%initialize(          &
        sDescription="Zone Grid",             &
        sFileType="ARC_GRID",                 &
        sFilename=trim(grid_filename),        &
        iDataType=DATATYPE_INT )

      call this%grd_zone2%getvalues()

!    where ( this%grd_native%dpData <= NC_FILL_FLOAT )
!      this%grd_zone2%pGrdBase%iData = NC_FILL_INT
!    end where

      call grid_WriteArcGrid(OUTPUT_FILESname_str, this%grd_zone2%pGrdBase )

      previous_zone_filename = trim(grid_filename)

    endif

  end subroutine initialize_secondary_zone_grid

  !------------------------------------------------------------------------------

  subroutine initialize_comparison_grid( this, grid_filename )

    class (SWBSTATS_T), intent(inout)  :: this
    character (len=*), intent(inout)   :: grid_filename

    ! [ LOCALS ]
    integer (c_int)                 :: iIndex
    integer (c_int)                 :: iStat
    character (len=:), allocatable       :: description_str
    character (len=:), allocatable       :: OUTPUT_FILESname_str
    character (len=256), save            :: previous_comparison_filename

    ! OK, if this is the same ARC ASCII grid file processed in a previous iteration,
    ! we leave the values as they are and skip reading in the grid again
    if ( .not. trim(previous_comparison_filename)==trim(grid_filename) ) then

      description_str = left( grid_filename, substring=".")
      if (description_str .contains. "/") description_str = right( description_str, substring="/")
      if (description_str .contains. "\") description_str = right( description_str, substring="\")

      OUTPUT_FILESname_str = "Comparison_grid__"//trim(description_str)//"__as_read_into_SWBSTATS2.asc"

      ! allocate memory for a generic data_catalog_entry
      if (.not. associated(this%grd_comparison))  allocate(this%grd_comparison, stat=iStat)

      call this%grd_comparison%set_target_PROJ4(this%target_proj4_string)

      call this%grd_comparison%initialize(          &
        sDescription="Comparison Grid",          &
        sFileType="ARC_GRID",                    &
        sFilename=trim(grid_filename),           &
        iDataType=DATATYPE_FLOAT )

      call this%grd_comparison%getvalues()

!   where ( this%grd_native%dpData <= NC_FILL_FLOAT )
!     this%grd_comparison%pGrdBase%dpData = NC_FILL_FLOAT
!   elsewhere
        this%grd_comparison%pGrdBase%dpData = this%grd_comparison%pGrdBase%dpData           &
                                        * this%comparison_grid_conversion_factor
!    end where

      call grid_WriteArcGrid(OUTPUT_FILESname_str, this%grd_comparison%pGrdBase )

      previous_comparison_filename = trim(grid_filename)

    endif

  end subroutine initialize_comparison_grid

  subroutine set_conversion_factors_sub(this)

    class (SWBSTATS_T), intent(inout)  :: this

    if (this%report_as_volume) then
      if (this%target_proj4_string .containssimilar. "units=m") then
        this%length_conversion_factor = 1.0_c_double
      elseif (this%target_proj4_string .containssimilar. "units=us-ft") then
        this%length_conversion_factor = 0.3048006096012192_c_double
      elseif (this%target_proj4_string .containssimilar. "units=ft") then
        this%length_conversion_factor = 0.3048_c_double
      else
        call die("Was attempting to define length conversion factor, but no units " &
          //"information found in input PROJ4 string.")
      endif
      this%grid_cell_area_sq_meters = (this%grd_native%rGridCellSize                  &
                                           * this%length_conversion_factor)**2
      this%output_conversion_factor = this%grid_cell_area_sq_meters           &
                                           * this%unit_conversion_factor
    else
      this%output_conversion_factor = this%unit_conversion_factor
    endif

    this%filename_modifier_string = trim(this%filename_modifier_string)   &
                                       //"__cubic_meters"

  end subroutine set_conversion_factors_sub


  subroutine write_stats_to_netcdf( this,                                      &
                                    OUTPUT_FILES,                              &
                                    start_date,                                &
                                    end_date )

    class (SWBSTATS_T), intent(inout)       :: this
    type (FILE_COLLECTION_T), intent(inout) :: OUTPUT_FILES(:)
    type (DATETIME_T), intent(inout)        :: start_date
    type (DATETIME_T), intent(inout)        :: end_date

    ! [ LOCALS ]
    real (c_double) :: start_bnd
    real (c_double) :: end_bnd
    integer (c_size_t) :: nx, ny
    type (T_NETCDF4_FILE), pointer    :: ncfile_out
    type (GENERAL_GRID_T), pointer    :: grid_ptr
    integer (c_int)                   :: stat_indx

    nx = BNDS%iNumCols
    ny = BNDS%iNumRows

    start_bnd = SIM_DT%days_from_origin( start_date )
    end_bnd   = SIM_DT%days_from_origin( end_date )

    do stat_indx=STATS_MEAN, STATS_VARIANCE

      if ( .not. OUTPUT_FILES(stat_indx)%output_active )  cycle

      if ( OUTPUT_FILES(stat_indx)%write_netcdf ) then

        ncfile_out => OUTPUT_FILES(stat_indx)%nc_ptr
        grid_ptr => OUTPUT_FILES(stat_indx)%grid_ptr

        call netcdf_put_variable_vector(NCFILE=ncfile_out,                        &
           iVarID=ncfile_out%iVarID(NC_TIME),                                     &
           iStart=[ RECNUM ],                                                     &
           iCount=[1_c_size_t],                                                   &
           iStride=[1_c_size_t],                                                  &
           rValues=[ real(( start_bnd + end_bnd ) / 2.0_c_float, c_float) ] )
    !       dpValues=[ real( SIM_DT%iNumDaysFromOrigin, c_double) ] )

       ! call netcdf_put_variable_vector(NCFILE=ncfile_out,                        &
       !    iVarID=TIME_BNDS_VARID,                                                 &
       !    iStart=[ RECNUM ,0_c_size_t],                                           &
       !    iCount=[1_c_size_t,2_c_size_t],                                         &
       !    iStride=[1_c_size_t, 1_c_size_t],                                 &
       !    dpValues=[ start_bnd, end_bnd ] )

        call netcdf_put_variable_array(NCFILE=ncfile_out,                         &
           iVarID=ncfile_out%iVarID(NC_Z),                                        &
           iStart=[ RECNUM , 0_c_size_t, 0_c_size_t],                             &
           iCount=[ 1_c_size_t, ny, nx ],                                         &
           iStride=[1_c_size_t, 1_c_size_t, 1_c_size_t],                          &
           rValues=real(grid_ptr%dpData, c_float) )

      endif

    enddo

  end subroutine write_stats_to_netcdf

!------------------------------------------------------------------------------

  subroutine write_monthly_stats_to_arcgrid( this, OUTPUT_FILES,    &
                                             date_range_string,     &
                                             output_file_prefix )

    class (SWBSTATS_T), intent(inout)                    :: this
    type (FILE_COLLECTION_T), intent(inout)              :: OUTPUT_FILES(:)
    character (len=*), intent(in), optional              :: date_range_string
    character (len=*), intent(in), optional              :: output_file_prefix

    ! [ LOCALS ]
    character (len=:), allocatable    :: filename
    integer (c_int)                   :: nx, ny
    type (GENERAL_GRID_T), pointer    :: grid_ptr
    character (len=8)                 :: stats_description
    character (len=:), allocatable    :: units_string
    character (len=:), allocatable    :: filename_modifier
    character (len=:), allocatable    :: file_prefix
    
    integer (c_int)   :: sum_index_num
    integer (c_int)   :: mean_index_num
    integer (c_int)   :: variance_index_num
    character (len=3) :: month_name
    integer (c_int)   :: month_num

    nx = BNDS%iNumCols
    ny = BNDS%iNumRows

    do month_num=1,12

      ! extract month name from the datetime module MONTHS data structure
      month_name = MONTHS(month_num)%sName
      sum_index_num = ( month_num - 1 ) * 3 + 1
      mean_index_num = ( month_num - 1 ) * 3 + 2
      variance_index_num = ( month_num - 1 ) * 3 + 3

      if ( OUTPUT_FILES(mean_index_num)%write_arcgrid) then

        if (this%report_as_volume) then
          units_string = "cubic meters"
        elseif (this%report_in_meters) then
          units_string = "meters"
        else
          units_string = trim(this%netcdf_variable_units_string)
        endif

        call replace(units_string, ' ', '_' )

        ! remove commas from filename modifier
        filename_modifier = clean(units_string, ",")

        ! ** preliminaries done; now begin to write out to Arc Grid files


        ! write out MEAN values grid
        grid_ptr => OUTPUT_FILES(mean_index_num)%grid_ptr
        call assert(associated(grid_ptr), "Unallocated array", __FILE__, __LINE__)
        stats_description = OUTPUT_FILES(mean_index_num)%stats_description

        if ( present(output_file_prefix) ) then
          file_prefix = output_file_prefix // "__" // "monthly_mean_over_all"
        else
          file_prefix = "monthly_mean_over_all"
        endif  
    
        filename = trim(file_prefix)                                             &
                     //"_"//trim(month_name)                                     &
                     //"__"//trim(this%netcdf_variable_name_string)              &
                     //"__"//trim(this%date_range_string)                        &
                     //"__"//asCharacter(nx)//"_by_"//asCharacter(ny)            &
                     //"__"//trim(filename_modifier)//".asc"

        call grid_WriteArcGrid( filename, grid_ptr )

        !!! Now output the VARIANCE grid 
        grid_ptr => OUTPUT_FILES(variance_index_num)%grid_ptr
        call assert(associated(grid_ptr), "Unallocated array", __FILE__, __LINE__)
        stats_description = OUTPUT_FILES(variance_index_num)%stats_description

        if ( present(output_file_prefix) ) then
          file_prefix = output_file_prefix // "__" // "monthly_variance_over_all"
        else
          file_prefix = "monthly_variance_over_all"
        endif  
    
        filename = trim(file_prefix)                                             &
                     //"_"//trim(month_name)                                     &
                     //"__"//trim(this%netcdf_variable_name_string)              &
                     //"__"//trim(this%date_range_string)                        &
                     //"__"//asCharacter(nx)//"_by_"//asCharacter(ny)            &
                     //"__"//trim(filename_modifier)//".asc"
           
        call grid_WriteArcGrid( filename, grid_ptr )


      endif

    enddo

  end subroutine write_monthly_stats_to_arcgrid


!------------------------------------------------------------------------------

  subroutine write_stats_to_arcgrid( this, OUTPUT_FILES, start_date,         &
                                     end_date, date_range_string,            &
                                     output_file_prefix )

    class (SWBSTATS_T), intent(inout)                    :: this
    type (FILE_COLLECTION_T), intent(inout)              :: OUTPUT_FILES(:)
    type (DATETIME_T), intent(inout)                     :: start_date
    type (DATETIME_T), intent(inout)                     :: end_date
    character (len=*), intent(in), optional              :: date_range_string
    character (len=:), allocatable, intent(in), optional :: output_file_prefix

    ! [ LOCALS ]
    character (len=:), allocatable    :: filename
    integer (c_int)              :: nx, ny
    type (GENERAL_GRID_T), pointer    :: grid_ptr
    integer (c_int)              :: stat_indx
    character (len=8)                 :: stats_description
    character (len=:), allocatable    :: units_string
    character (len=:), allocatable    :: filename_modifier
    character (len=:), allocatable    :: file_prefix

    if ( present(output_file_prefix) ) then
      if (allocated(output_file_prefix))  file_prefix = output_file_prefix // "__"
    else
      file_prefix = ""
    endif  

    nx = BNDS%iNumCols
    ny = BNDS%iNumRows

    do stat_indx=STATS_MEAN, STATS_VARIANCE

      if ( .not. OUTPUT_FILES(stat_indx)%output_active )  cycle

      if ( OUTPUT_FILES(stat_indx)%write_arcgrid) then

        if ( this%annualize_stats .and. stat_indx == STATS_SUM) then
          units_string = trim(this%netcdf_variable_units_string)//", per year"
        elseif (this%report_as_volume) then
          units_string = "cubic meters"
        elseif (this%report_in_meters) then
          units_string = "meters"
        else
          units_string = trim(this%netcdf_variable_units_string)
        endif

        call replace(units_string, ' ', '_' )

        ! remove commas from filename modifier
        filename_modifier = clean(units_string, ",")

        grid_ptr => OUTPUT_FILES(stat_indx)%grid_ptr
        stats_description = OUTPUT_FILES(stat_indx)%stats_description

        if ( present(date_range_string) ) then
          !@todo: change filename depending on the desired output statistic
          filename = output_file_prefix                                            &
                     //trim(this%netcdf_variable_name_string)                      &
                     //"__"//trim(date_range_string)                               &
                     //"__"//asCharacter(nx)//"_by_"//asCharacter(ny)              &
                     //"__"//trim(stats_description)                               &
                     //"__"//trim(filename_modifier)//".asc"
        else
          filename = output_file_prefix                                             &
                     //trim(this%netcdf_variable_name_string)                       &
                     //"__"//start_date%prettydate()//"_to_"//end_date%prettydate() &
                     //"__"//asCharacter(nx)//"_by_"//asCharacter(ny)               &
                     //"__"//trim(filename_modifier)//".asc"
        endif

        ! ugly hack to make the output match the default NODATA value
        where ( this%grd_native%dpData <= NC_FILL_FLOAT )
          grid_ptr%dpData = -9999.
        end where

        call grid_WriteArcGrid( filename, grid_ptr )

      endif

    enddo

  end subroutine write_stats_to_arcgrid

!------------------------------------------------------------------------------

!   subroutine calculate_slice_statistics( this, grid_delta, grid_delta2, start_date, end_date, grid_mask )

!     class (SWBSTATS_T), intent(inout)        :: this
!     type (GENERAL_GRID_T), pointer           :: grid_delta
!     type (GENERAL_GRID_T), pointer           :: grid_delta2
!     type (DATETIME_T), intent(inout)         :: start_date
!     type (DATETIME_T), intent(inout)         :: end_date
!     logical, optional                        :: grid_mask(:,:)

!     ! [ LOCALS ]
!     integer (c_int)               :: julian_day_number
!     real (c_float), allocatable   :: tempvals(:)
!     integer (c_int)               :: day_count
!     logical (c_bool), allocatable :: local_mask(:,:)
!     real (c_float), dimension(size(grid_delta%dpData,1), size(grid_delta%dpData,2)) :: rTemp

!     ! force slice dates to honor bounds of data dates
!     if ( start_date < this%data_start_date )  start_date = this%data_start_date
!     if ( end_date > this%data_end_date )      end_date = this%data_end_date

! !    call SIM_DT%initialize( start_date, end_date )

!     associate( grd_sum => OUTPUT_FILES(STATS_SUM)%grid_ptr%dpData,                &
!                grd_mean => OUTPUT_FILES(STATS_MEAN)%grid_ptr%dpData,              &
!                grd_var => OUTPUT_FILES(STATS_VARIANCE)%grid_ptr%dpData,           &
!                delta =>  grid_delta%dpData,                                      &
!                delta2 => grid_delta2%dpData,                                     &
!                grd_new => this%grd_native%dpData      )

!       allocate(local_mask(ubound(grd_sum,1),ubound(grd_sum,2)))

!       day_count = 0

!       ! SIM_DT is being used here simply because it provides a simple way to march through
!       ! time on a day-by-day basis; need to ensure that the "current" day aligns with the 
!       ! desired start and end dates of the slice. 
!       if ( SIM_DT%curr > start_date ) then
!         call SIM_DT%set_current_date(start_date)
!       endif

!       do

!         if ( SIM_DT%curr < start_date ) then
!           call SIM_DT%addDay()
!           cycle
!         endif

!         julian_day_number = int( SIM_DT%curr%dJulianDate, c_int)

!         ! keep track of number of days that are summed together so we can calculated a mean grid value later
!         day_count = day_count + 1

!         if ( netcdf_update_time_starting_index(this%ncfile_in, julian_day_number ) )   then
!           call netcdf_get_variable_slice(NCFILE=this%ncfile_in, rValues=rTemp )
!         endif

!         ! volumetric_conversion_factor is 1.0 *unless* user has specifically
!         ! asked for volumetric reporting or other unit conversions; in this case,
!         ! the output_conversion_factor amounts to the cell area times the
!         ! unit length, appropriately converted to cubic meters, or if output
!         ! is requested in meters, the conversion factor is just 0.
!         grd_new = real(rTemp, c_double) * this%output_conversion_factor

!         if (day_count == 1) then

!           if (present(grid_mask)) then
!             local_mask = (grd_new > NC_FILL_FLOAT) .and. grid_mask
!           else
!             local_mask = grd_new > NC_FILL_FLOAT
!           endif

!           grd_sum = 0.0_c_double
!           grd_mean = 0.0_c_double
!           grd_var = 0.0_c_double

!         endif

!         where ( local_mask )
!           delta = grd_new - grd_mean
!           grd_sum = grd_sum + grd_new
!           grd_mean = grd_mean + delta / real( day_count, c_double )
!           delta2 = grd_new - grd_mean
!           grd_var = grd_var + delta * delta2
!         end where

!          ! write(*,fmt="(a,f14.3,f14.3)") "gridsum: ", minval(grd_sum,  mask=grd_new > NC_FILL_FLOAT), &
!          !     maxval(grd_sum,  mask=grd_new > NC_FILL_FLOAT)
!          !
!          ! write(*,fmt="(a,f14.3,f14.3)") "gridnew: ", minval(grd_new,  mask=grd_new > NC_FILL_FLOAT), &
!          !     maxval(grd_new,  mask=grd_new > NC_FILL_FLOAT)
!          !
!          ! write(*,fmt="(a,f14.3,f14.3)") "gridnew: ", minval(this%grd_native%dpData,  mask=this%grd_native%dpData > NC_FILL_FLOAT), &
!          !     maxval(this%grd_native%dpData,  mask=this%grd_native%dpData > NC_FILL_FLOAT)

!         call SIM_DT%addDay
!         if ( SIM_DT%curr > end_date )  exit

!       end do

!       where ( .not. local_mask )
!         grd_mean = NC_FILL_FLOAT
!         grd_sum = NC_FILL_FLOAT
!         grd_var = NC_FILL_FLOAT
!       ! elsewhere
!       !   grd_var = grd_var / (counter - 1)
!       endwhere

!       if (this%annualize_stats) then
!         where ( local_mask )
! !          grd_mean = grd_mean/ real( day_count, c_float ) * 365.25
! !          grd_var = grd_var/ real( day_count, c_float ) * 365.25
!           grd_sum = grd_sum / real( day_count, c_double ) * 365.25
!         end where
!       ! else
!       !   where ( local_mask )
!       !     grd_mean = grd_sum / real( day_count, c_float )
!       !   end where
!       endif

!       deallocate(local_mask)

!     end associate

! !    write(*,fmt="(a)") "gridsum: ", minval(grid_sum%rData,  mask=this%grd_native%rData > NC_FILL_FLOAT), &
! !        maxval(grid_sum%rData,  mask=this%grd_native%rData > NC_FILL_FLOAT)

!   end subroutine calculate_slice_statistics

  subroutine calculate_slice_statistics( this, start_date, end_date, grd_sum, grd_mean, grd_var )

    class (SWBSTATS_T), intent(inout)        :: this
    type (DATETIME_T), intent(inout)         :: start_date
    type (DATETIME_T), intent(inout)         :: end_date
    real (c_double), intent(inout)           :: grd_sum(:,:)
    real (c_double), intent(inout)           :: grd_mean(:,:)
    real (c_double), intent(inout)           :: grd_var(:,:)

    ! [ LOCALS ]
    integer (c_int)               :: julian_day_number
    integer (c_int)               :: day_count
    logical (c_bool), allocatable :: local_mask(:,:)
    real (c_float), allocatable   :: rTemp(:,:)
    real (c_double), allocatable  :: grd_new(:,:)

    ! force slice dates to honor bounds of data dates
    if ( start_date < this%data_start_date )  start_date = this%data_start_date
    if ( end_date > this%data_end_date )      end_date = this%data_end_date

    allocate(local_mask(this%ncfile_in%iNX,this%ncfile_in%iNY))
    allocate(rTemp(this%ncfile_in%iNX,this%ncfile_in%iNY))
    allocate(grd_new(this%ncfile_in%iNX,this%ncfile_in%iNY))

    day_count = 0

    ! call RSTAT%initialize(NX=this%ncfile_in%iNX,                  &
    !                       NY=this%ncfile_in%iNY,                  &
    !                       X0=this%ncfile_in%rX(NC_LEFT),          &
    !                       Y0=this%ncfile_in%rY(NC_BOTTOM),        &
    !                       X1=this%ncfile_in%rX(NC_RIGHT),         &
    !                       Y1=this%ncfile_in%rY(NC_TOP),           &
    !                       nodata_value=-9999._c_double)

    call RSTAT%clear()                          

    ! SIM_DT is being used here simply because it provides a simple way to march through
    ! time on a day-by-day basis; need to ensure that the "current" day aligns with the 
    ! desired start and end dates of the slice. 
    if ( SIM_DT%curr > start_date ) then
      call SIM_DT%set_current_date(start_date)
    endif

    do

      if ( SIM_DT%curr < start_date ) then
        call SIM_DT%addDay()
        cycle
      endif

      julian_day_number = int( SIM_DT%curr%dJulianDate, c_int)

      ! keep track of number of days that are summed together so we can calculated a mean grid value later
      day_count = day_count + 1

      if ( netcdf_update_time_starting_index(this%ncfile_in, julian_day_number ) )   then
        call netcdf_get_variable_slice(NCFILE=this%ncfile_in, rValues=rTemp )
      endif

      ! volumetric_conversion_factor is 1.0 *unless* user has specifically
      ! asked for volumetric reporting or other unit conversions; in this case,
      ! the output_conversion_factor amounts to the cell area times the
      ! unit length, appropriately converted to cubic meters, or if output
      ! is requested in meters, the conversion factor is just 0.
      grd_new = real(rTemp, c_double) * this%output_conversion_factor

      if (day_count == 1) then
        local_mask = grd_new > NC_FILL_FLOAT
      endif

      call RSTAT%push(grd_data=grd_new, mask=local_mask)


         ! write(*,fmt="(a,f14.3,f14.3)") "gridsum: ", minval(grd_sum,  mask=grd_new > NC_FILL_FLOAT), &
         !     maxval(grd_sum,  mask=grd_new > NC_FILL_FLOAT)
         !
         ! write(*,fmt="(a,f14.3,f14.3)") "gridnew: ", minval(grd_new,  mask=grd_new > NC_FILL_FLOAT), &
         !     maxval(grd_new,  mask=grd_new > NC_FILL_FLOAT)
         !
         ! write(*,fmt="(a,f14.3,f14.3)") "gridnew: ", minval(this%grd_native%dpData,  mask=this%grd_native%dpData > NC_FILL_FLOAT), &
         !     maxval(this%grd_native%dpData,  mask=this%grd_native%dpData > NC_FILL_FLOAT)

      call SIM_DT%addDay
      if ( SIM_DT%curr > end_date )  exit

    end do

    call RSTAT%mean(grd_mean)
    call RSTAT%sum(grd_sum)
    call RSTAT%variance(grd_var)

    where ( .not. local_mask )
      grd_mean = NC_FILL_FLOAT
      grd_sum = NC_FILL_FLOAT
      grd_var = NC_FILL_FLOAT
      ! elsewhere
      !   grd_var = grd_var / (counter - 1)
    end where

    if (this%annualize_stats) then
      where ( local_mask )
!        grd_mean = grd_mean/ real( day_count, c_float ) * 365.25
!        grd_var = grd_var/ real( day_count, c_float ) * 365.25
        grd_sum = grd_sum / real( day_count, c_double ) * 365.25
      end where
      ! else
      !   where ( local_mask )
      !     grd_mean = grd_sum / real( day_count, c_float )
      !   end where
    endif

    deallocate(local_mask)
    deallocate(grd_new)
    deallocate(rTemp)

  end subroutine calculate_slice_statistics

!------------------------------------------------------------------------------
 
  ! ASSUMPTION: this routine will be called *AFTER* the slice statistics are calculated and
  !             ONLY when the monthly statistics flag has been set from command line
  subroutine calculate_monthly_statistics(this, month_num, finalize)

    class (SWBSTATS_T), intent(inout)        :: this
    integer (c_short), intent(in)            :: month_num
    logical (c_bool), intent(in)             :: finalize 

    integer (c_int)   :: sum_index_num
    integer (c_int)   :: mean_index_num
    integer (c_int)   :: variance_index_num
    character (len=3) :: month_name
    integer (c_int)   :: month_index

    if (.not. finalize) then

      call MON_STAT(month_num)%push(grd_data=OUTPUT_FILES(STATS_SUM)%grid_ptr%dpData,                &
                                     mask=logical(OUTPUT_FILES(STATS_SUM)%grid_ptr%dpData > -9999._c_double, c_bool))
      
    else  !finalize monthly outputs

      do month_index=1,12

        month_name = MONTHS(month_index)%sName
        sum_index_num = ( month_index - 1 ) * 3 + 1
        mean_index_num = ( month_index- 1 ) * 3 + 2
        variance_index_num = ( month_index- 1 ) * 3 + 3
    
        call MON_STAT(month_index)%mean(OUTPUT_FILES(mean_index_num)%grid_ptr%dpData)
        call MON_STAT(month_index)%sum(OUTPUT_FILES(sum_index_num)%grid_ptr%dpData)
        call MON_STAT(month_index)%variance(OUTPUT_FILES(variance_index_num)%grid_ptr%dpData)

        ! if ( OUTPUT_FILES(sum_index_num)%n_count > 0 ) then

        !   where ( OUTPUT_FILES(sum_index_num)%grid_ptr%dpData > -9999. )
        !     OUTPUT_FILES(mean_index_num)%grid_ptr%dpData = OUTPUT_FILES(sum_index_num)%grid_ptr%dpData   &
        !                                                / real(OUTPUT_FILES(sum_index_num)%n_count, kind=c_double)
        !   else where
          
        !     OUTPUT_FILES(mean_index_num)%grid_ptr%dpData = -9999.
          
        !   end where

        ! else

        !   OUTPUT_FILES(mean_index_num)%grid_ptr%dpData = -9999.

        ! endif  

        ! write(*,fmt='(a,i0,1x,6(f0.4, 2x))') 'FINALIZING: ', month_index,    &
        ! minval(OUTPUT_FILES(sum_index_num)%grid_ptr%dpData),                 &
        ! maxval(OUTPUT_FILES(sum_index_num)%grid_ptr%dpData),                 &
        ! minval(OUTPUT_FILES(mean_index_num)%grid_ptr%dpData),                &
        ! maxval(OUTPUT_FILES(mean_index_num)%grid_ptr%dpData),                &
        ! minval(OUTPUT_FILES(variance_index_num)%grid_ptr%dpData),            &
        ! maxval(OUTPUT_FILES(variance_index_num)%grid_ptr%dpData)

      enddo  

    endif
    
  end subroutine calculate_monthly_statistics
!------------------------------------------------------------------------------

  subroutine read_date_range_file( this,                                       &
                                   csv_filename,                               &
                                   date_range_id_list,                         &
                                   start_date_list,                            &
                                   end_date_list )

    class (SWBSTATS_T), intent(inout)   :: this
    character (len=*), intent(inout)    :: csv_filename
    type (FSTRING_LIST_T), intent(out)  :: date_range_id_list
    type (FSTRING_LIST_T), intent(out)  :: start_date_list
    type (FSTRING_LIST_T), intent(out)  :: end_date_list

    ! [ LOCALS ]
    integer (c_int)           :: iFileIndex, iColIndex
    integer (c_int)           :: iStat
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

      call chomp( sRecord, sItem, delimiter_chr="," )
      call this%date_range_id_list%append( sItem )

      call chomp( sRecord, sItem, delimiter_chr="," )
      call this%start_date_list%append( sItem )
      call this%end_date_list%append( sRecord )

    enddo

    call DF%close()

  end subroutine read_date_range_file


!------------------------------------------------------------------------------

  subroutine read_comparison_period_file( this,                                &
                                          csv_filename,                        &
                                          date_range_id_list,                  &
                                          start_date_list,                     &
                                          end_date_list,                       &
                                          comparison_grid_file_list)

    class (SWBSTATS_T), intent(inout)   :: this
    character (len=*), intent(inout)    :: csv_filename
    type (FSTRING_LIST_T), intent(out)  :: date_range_id_list
    type (FSTRING_LIST_T), intent(out)  :: start_date_list
    type (FSTRING_LIST_T), intent(out)  :: end_date_list
    type (FSTRING_LIST_T), intent(out)  :: comparison_grid_file_list

    ! [ LOCALS ]
    integer (c_int)           :: iFileIndex, iColIndex
    integer (c_int)           :: iStat
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

      call chomp( sRecord, sItem, delimiter_chr="," )
      call this%date_range_id_list%append( sItem )

      call chomp( sRecord, sItem, delimiter_chr="," )
      call this%start_date_list%append( sItem )

      call chomp( sRecord, sItem, delimiter_chr="," )
      call this%end_date_list%append( sItem )

      call this%comparison_grid_file_list%append( sRecord )

    enddo

    call DF%close()

  end subroutine read_comparison_period_file

!------------------------------------------------------------------------------

  subroutine read_zone_period_file( this,                                      &
                                    csv_filename,                              &
                                    date_range_id_list,                        &
                                    start_date_list,                           &
                                    end_date_list,                             &
                                    zonal_stats_grid_file_list)

    class (SWBSTATS_T), intent(inout)   :: this
    character (len=*), intent(inout)    :: csv_filename
    type (FSTRING_LIST_T), intent(out)  :: date_range_id_list
    type (FSTRING_LIST_T), intent(out)  :: start_date_list
    type (FSTRING_LIST_T), intent(out)  :: end_date_list
    type (FSTRING_LIST_T), intent(out)  :: zonal_stats_grid_file_list

    ! [ LOCALS ]
    integer (c_int)           :: iFileIndex, iColIndex
    integer (c_int)           :: iStat
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

      call chomp( sRecord, sItem, delimiter_chr="," )
      call this%date_range_id_list%append( sItem )

      call chomp( sRecord, sItem, delimiter_chr="," )
      call this%start_date_list%append( sItem )

      call chomp( sRecord, sItem, delimiter_chr="," )
      call this%end_date_list%append( sItem )

      call this%zone_grid_file_list%append( sRecord )

    enddo

    call DF%close()

  end subroutine read_zone_period_file

!------------------------------------------------------------------------------

  subroutine output_zonal_stats(this,                                          &
                                start_date,                                    &
                                end_date,                                      &
                                values,                                        &
                                zone_ids,                                      &
                                unique_zone_list,                              &
                                delimiter,                                     &
                                funit,                                         &
                                zone2_ids,                                     &
                                unique_zone2_list,                             &
                                comparison_values)

    class (SWBSTATS_T), intent(inout)               :: this
    type (DATETIME_T), intent(in)                   :: start_date
    type (DATETIME_T), intent(in)                   :: end_date
    real (c_double), intent(inout)                  :: values(:,:)
    integer (c_int), intent(inout)                  :: zone_ids(:,:)
    type (FSTRING_LIST_T), intent(inout)            :: unique_zone_list
    character (len=*), intent(in), optional         :: delimiter
    integer (c_int), intent(in), optional           :: funit
    integer (c_int), intent(inout), optional        :: zone2_ids(:,:)
    type (FSTRING_LIST_T), intent(inout), optional  :: unique_zone2_list
    real (c_double), intent(inout), optional        :: comparison_values(:,:)

    ! [ LOCALS ]
    integer (c_int)                :: n, m
    integer (c_int)                :: i, j
    integer (c_int), allocatable   :: zone_values(:)
    integer (c_int), allocatable   :: zone2_values(:)
    integer (c_int)                :: number_of_matches
    real (c_double), allocatable   :: stats(:)
    integer (c_int)                :: funit_l
    character (len=:), allocatable :: delimiter_
    character (len=6), parameter   :: NUM_FMT = "g25.16"

    if (present(funit)) then
      funit_l = funit
    else
      funit_l = OUTPUT_UNIT
    endif

    if (present(delimiter) ) then
      delimiter_ = trim(delimiter)
    else
      delimiter_ = ","
    endif

    if (present(zone2_ids) .and. present(unique_zone2_list)) then

      ! sort the values before use
      call unique_zone_list%sort_integer()
      call unique_zone2_list%sort_integer()

      ! obtain list of unique integers present in the zone grid
      zone_values = unique_zone_list%get_integer()
      zone2_values = unique_zone2_list%get_integer()

      if (present(comparison_values)) then

        do i=1,ubound(zone_values,1)
          do j=1,ubound(zone2_values,1)
            n = zone_values(i)
            m = zone2_values(j)
            call calc_multizonal_stats(values, zone_ids, target_id=n, target2_id=m,    &
                                  result_vector=stats,                                 &
                                  zone2_ids=zone2_ids,                                 &
                                  comparison_values=comparison_values)
  
            write(unit=funit_l, fmt="(2(a,a),2(i0,a),4("//NUM_FMT//",a),i0,a,4("//NUM_FMT//",a),i0)") &
              start_date%prettydate(),delimiter_,                                                     &
              end_date%prettydate(), delimiter_,                                                      &
              n, delimiter_, m, delimiter_,                                                           &
              stats(1), delimiter_, stats(2), delimiter_, stats(3), delimiter_,                       &
              stats(4), delimiter_, int(stats(5)), delimiter_,                                        &
              stats(6), delimiter_, stats(7), delimiter_, stats(8), delimiter_,                       &
              stats(9), delimiter_, int(stats(10))

          enddo
        enddo

      else 

        do i=1,ubound(zone_values,1)
          do j=1,ubound(zone2_values,1)
            n = zone_values(i)
            m = zone2_values(j)

            call calc_multizonal_stats(values, zone_ids, target_id=n, target2_id=m,    &
              result_vector=stats, zone2_ids=zone2_ids)

            write(unit=funit_l, fmt="(2(a,a),2(i0,a),4("//NUM_FMT//",a),i0)")          &
              start_date%prettydate(), delimiter_,                                     &
              end_date%prettydate(), delimiter_,                                       &
              n, delimiter_, m, delimiter_,                                            &
              stats(1), delimiter_, stats(2), delimiter_, stats(3), delimiter_,        &
              stats(4), delimiter_, int(stats(5))
          enddo
        enddo

      endif  

    else
      ! obtain list of unique integers present in the zone grid
      zone_values = unique_zone_list%get_integer()

      if (present(comparison_values)) then

        do i=1,ubound(zone_values,1)
          n = zone_values(i)
   
          call calc_zonal_stats(values, zone_ids, target_id=n, result_vector=stats,   &
            comparison_values=comparison_values)
          write(unit=funit_l, fmt="(2(a,a),i0,a,4("//NUM_FMT//",a),i0,a,4("//NUM_FMT//",a),i0)")    &
            start_date%prettydate(), delimiter_,                                                    &
            end_date%prettydate(), delimiter_,                                                      &
            n, delimiter_,                                                                          &
            stats(1), delimiter_, stats(2), delimiter_, stats(3), delimiter_,                       &
            stats(4), delimiter_, int(stats(5)), delimiter_,                                        &
            stats(6), delimiter_, stats(7), delimiter_, stats(8), delimiter_,                       &
            stats(9), delimiter_, int(stats(10))

        enddo
      
      else

        do i=1,ubound(zone_values,1)
          n = zone_values(i)

          call calc_zonal_stats(values, zone_ids, target_id=n, result_vector=stats)
          write(unit=funit_l, fmt="(2(a,a),i0,a,4("//NUM_FMT//",a),i0)")                &
            start_date%prettydate(), delimiter_,                                        &
            end_date%prettydate(), delimiter_,                                          &
            n, delimiter_,                                                              &
            stats(1), delimiter_, stats(2), delimiter_, stats(3), delimiter_,           &
            stats(4), delimiter_, int(stats(5))
        enddo

      endif

    endif

  end subroutine output_zonal_stats

!------------------------------------------------------------------------------

  subroutine open_output_netcdf_files(this,                                    &
                                      OUTPUT_FILES)

    class (SWBSTATS_T), intent(inout)           :: this
    type (FILE_COLLECTION_T), intent(inout)     :: OUTPUT_FILES(:)

    ! [ LOCALS ]
    integer (c_int)                   :: stat_indx
    integer (c_int)                   :: status
    type (T_NETCDF4_FILE), pointer    :: ncfile_out
    character (len=:), allocatable    :: units_string
    character (len=:), allocatable    :: filename_prefix

    if (allocated( this%output_file_prefix) ) then
      filename_prefix = this%output_file_prefix
    else
      filename_prefix = ""
    endif

    do stat_indx=STATS_MEAN, STATS_VARIANCE

      if ( .not. OUTPUT_FILES(stat_indx)%output_active )  cycle

      if (OUTPUT_FILES(stat_indx)%write_netcdf) then

        if ( this%annualize_stats .and. stat_indx == STATS_SUM) then
          units_string = trim(this%netcdf_variable_units_string)//", per year"
        elseif (this%report_as_volume) then
          units_string = "cubic meters"
        elseif (this%report_in_meters) then
          units_string = "meters"
        else
          units_string = trim(this%netcdf_variable_units_string)
        endif

        allocate( OUTPUT_FILES(stat_indx)%nc_ptr, stat=status)
        call assert( status==0, "Problem allocating netcdf file data structure.", &
          __FILE__, __LINE__)
        ncfile_out => OUTPUT_FILES(stat_indx)%nc_ptr

        call netcdf_open_and_prepare_as_output(                                     &
              NCFILE=ncfile_out,                                                    &
              sVariableName=trim(this%netcdf_variable_name_string),                 &
              sVariableUnits=units_string,                                          &
              iNX=this%ncfile_in%iNX,                                               &
              iNY=this%ncfile_in%iNY,                                               &
              fX=this%ncfile_in%rX_Coords,                                          &
              fY=this%ncfile_in%rY_Coords,                                          &
              PROJ4_string=trim(this%target_proj4_string),                          &
              StartDate=SIM_DT%start,                                               &
              EndDate=SIM_DT%end,                                                   &
              executable_name="SWBSTATS2",                                          &
              filename_prefix=filename_prefix,                                      &
              filename_modifier=this%filename_modifier_string                       &
                                //"_"//OUTPUT_FILES(stat_indx)%stats_description )

      endif

    enddo

  end subroutine open_output_netcdf_files

!------------------------------------------------------------------------------

  subroutine close_output_netcdf_files(this, OUTPUT_FILES)

    class (SWBSTATS_T), intent(inout)         :: this
    type (FILE_COLLECTION_T), intent(inout)   :: OUTPUT_FILES(:)

    ! [ LOCALS ]
    integer (c_int)              :: stat_indx
    integer (c_int)              :: status
    type (T_NETCDF4_FILE), pointer    :: ncfile_out

    do stat_indx=STATS_MEAN, STATS_VARIANCE

      if ( .not. OUTPUT_FILES(stat_indx)%output_active )  cycle

      if (OUTPUT_FILES(stat_indx)%write_netcdf) then

        ncfile_out => OUTPUT_FILES(stat_indx)%nc_ptr
        call netcdf_close_file(NCFILE=ncfile_out)

      endif

    enddo

  end subroutine close_output_netcdf_files

!------------------------------------------------------------------------------

  subroutine open_zonal_stats_output_file(this, filename, delimiter)

    class (SWBSTATS_T), intent(inout)       :: this
    character (len=*)                       :: filename
    character (len=*), intent(in), optional :: delimiter 

    ! [ LOCALS ]
    character (len=256)            :: header_str
    character (len=:), allocatable :: delimiter_

    if (present(delimiter)) then
      delimiter_ = delimiter
    else
      delimiter_ = ","
    endif  

    call this%zonal_stats_output_file%open(filename)

    if (len_trim(this%zone_grid2_filename) > 0) then

      if (this%multiple_comparison_grids .or. this%compare_to_obs_values) then
        header_str = "start_date"//delimiter_//"end_date"//delimiter_//"zone_id"   &
          //delimiter_//"zone2_id"//delimiter_//"minimum_swb"//delimiter_           &
          //"maximum_swb"//delimiter_//"mean_swb"//delimiter_//"sum_swb"           &
          //delimiter_//"count_swb"//delimiter_//"minimum_obs"//delimiter_          &
          //"maximum_obs"//delimiter_//"mean_obs"//delimiter_//"sum_obs"           &
          //delimiter_//"count_obs"
      else
        header_str = "start_date"//delimiter_//"end_date"//delimiter_//"zone_id"   &
        //delimiter_//"zone2_id"//delimiter_//"minimum_swb"//delimiter_             &
          //"maximum_swb"//delimiter_//"mean_swb"//delimiter_//"sum_swb"           &
          //delimiter_//"count_swb"
      endif

    else

      if (this%multiple_comparison_grids .or. this%compare_to_obs_values) then

        header_str = "start_date"//delimiter_//"end_date"//delimiter_//"zone_id"   &
          //delimiter_//"minimum_swb"//delimiter_                                  &
          //"maximum_swb"//delimiter_//"mean_swb"//delimiter_//"sum_swb"           &
          //delimiter_//"count_swb"//delimiter_//"minimum_obs"//delimiter_          &
          //"maximum_obs"//delimiter_//"mean_obs"//delimiter_//"sum_obs"           &
          //delimiter_//"count_obs"

      else

        header_str = "start_date"//delimiter_//"end_date"//delimiter_//"zone_id"   &
          //delimiter_//"minimum_swb"//delimiter_                                  &
          //"maximum_swb"//delimiter_//"mean_swb"//delimiter_//"sum_swb"           &
          //delimiter_//"count_swb"

      endif

    endif

    call this%zonal_stats_output_file%writeLine(trim(header_str))

  end subroutine open_zonal_stats_output_file

!------------------------------------------------------------------------------

  subroutine print_all_options(this)

    class (SWBSTATS_T) :: this

    ! [ LOCALS ]
    character (len=:), allocatable  :: fmt_string

    write(*,fmt="(/,/,a)") "  :: Dump of swbstats2 program this ::"
    write(*,fmt="(a,/)") repeat("-", 43)

    fmt_string = "(a,t30,': ',l)"
    write(*,fmt=fmt_string) "write_csv", this%write_csv
    write(*,fmt=fmt_string) "write_netcdf (SUM)", OUTPUT_FILES(STATS_SUM)%write_netcdf
    write(*,fmt=fmt_string) "write_netcdf (MEAN)", OUTPUT_FILES(STATS_MEAN)%write_netcdf
    write(*,fmt=fmt_string) "write_netcdf (VARIANCE)", OUTPUT_FILES(STATS_VARIANCE)%write_netcdf
    write(*,fmt=fmt_string) "write_arcgrid (SUM)", OUTPUT_FILES(STATS_SUM)%write_arcgrid
    write(*,fmt=fmt_string) "write_arcgrid (MEAN)", OUTPUT_FILES(STATS_MEAN)%write_arcgrid
    write(*,fmt=fmt_string) "write_arcgrid (VARIANCE)", OUTPUT_FILES(STATS_VARIANCE)%write_arcgrid
    write(*,fmt=fmt_string) "output_active (SUM)", OUTPUT_FILES(STATS_SUM)%output_active
    write(*,fmt=fmt_string) "output_active (MEAN)", OUTPUT_FILES(STATS_MEAN)%output_active
    write(*,fmt=fmt_string) "output_active (VARIANCE)", OUTPUT_FILES(STATS_VARIANCE)%output_active

    fmt_string = "(a,t40,': ',l)"
    write(*,fmt=fmt_string) "grid pointer allocated? (SUM)", associated( OUTPUT_FILES(STATS_SUM)%grid_ptr)
    write(*,fmt=fmt_string) "grid pointer allocated? (MEAN)", associated( OUTPUT_FILES(STATS_MEAN)%grid_ptr)
    write(*,fmt=fmt_string) "grid pointer allocated? (VARIANCE)", associated( OUTPUT_FILES(STATS_VARIANCE)%grid_ptr)

    fmt_string = "(a,t30,': ',l)"
    write(*,fmt=fmt_string) "calc_zonal_stats", this%calc_zonal_stats
    write(*,fmt=fmt_string) "compare_to_obs_values", this%compare_to_obs_values
    write(*,fmt=fmt_string) "annualize_stats", this%annualize_stats

    fmt_string = "(a,t30,': ',a)"
    write(*,fmt=fmt_string) "target_proj4_string", this%target_proj4_string
    write(*,fmt=fmt_string) "data_start_date", this%data_start_date%prettydate()
    write(*,fmt=fmt_string) "data_end_date", this%data_end_date%prettydate()
    write(*,fmt=fmt_string) "slice_start_date", this%slice_start_date%prettydate()
    write(*,fmt=fmt_string) "slice_end_date", this%slice_end_date%prettydate()
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
    write(*,fmt=fmt_string) "calculation_time_period", this%calculation_time_period

    fmt_string = "(a,t30,': ',a)"
    write(*,fmt=fmt_string) "stress_period_filename", this%stress_period_filename
    write(*,fmt=fmt_string) "comparison_period_filename", this%comparison_period_filename
    write(*,fmt=fmt_string) "zone_period_filename", this%zone_period_filename
    write(*,fmt=fmt_string) "comparison_grid_filename", this%comparison_grid_filename
    write(*,fmt=fmt_string) "zone_grid_filename", this%zone_grid_filename
    write(*,fmt=fmt_string) "netcdf_input_filename", this%netcdf_input_filename
    write(*,fmt=fmt_string) "netcdf_variable_name_string", this%netcdf_variable_name_string
    write(*,fmt=fmt_string) "netcdf_variable_units_string", this%netcdf_variable_units_string

    fmt_string = "(a,t30,': ',l)"
    write(*,fmt=fmt_string) "netcdf_input_file_is_open    : ", this%netcdf_input_file_is_open
    write(*,fmt=fmt_string) "multiple_zone_grids          : ", this%multiple_zone_grids
    write(*,fmt=fmt_string) "multiple_comparison_grids    : ", this%multiple_comparison_grids
    !comparison_grid_conversion_factor = 1.0_c_double
    fmt_string = "(/,a,/,'"//repeat("-",62)//"')"
    write(*,fmt=fmt_string) "Start date list"
    call this%start_date_list%print()
    write(*,fmt=fmt_string) "End date list"
    call this%end_date_list%print()
    write(*,fmt=fmt_string) "Date index list"
    call this%date_range_id_list%print()
    write(*,fmt=fmt_string) "Unique zone id list"
    call this%unique_zone_list%print()
    write(*,fmt=fmt_string) "Comparison grid file list"
    call this%comparison_grid_file_list%print()
    write(*,fmt=fmt_string) "Zone grid file list"
    call this%zone_grid_file_list%print()

  end subroutine print_all_options

end module swbstats2_support

































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































