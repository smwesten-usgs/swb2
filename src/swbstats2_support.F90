module swbstats2_support

  use constants_and_conversions, only : TRUE, FALSE, DATATYPE_INT,             &
                                        DATATYPE_FLOAT, BNDS, asFloat,         &
                                        OS_NATIVE_PATH_DELIMITER
  use iso_fortran_env, only           : OUTPUT_UNIT
  use data_catalog_entry, only        : DATA_CATALOG_ENTRY_T
  use datetime, only                  : DATETIME_T, assignment(=), operator(>)
  use exceptions, only                : assert, die
  use file_operations, only           : ASCII_FILE_T
  use grid
  use netcdf4_support
  use simulation_datetime, only       : SIM_DT, DATE_RANGE_T
  use fstring
  use fstring_list, only              : FSTRING_LIST_T,                         &
                                        create_list
  use iso_c_binding
  implicit none

  enum, bind(c)
    enumerator :: STATS_MEAN=1, STATS_SUM, STATS_VARIANCE
  end enum

  enum, bind(c)
    enumerator :: CALC_PERIOD_ALL=1, CALC_PERIOD_ANNUAL, CALC_PERIOD_MONTHLY,  &
                  CALC_PERIOD_SLICE_SINGLE, CALC_PERIOD_SLICE_MULTIPLE,        &
                  CALC_PERIOD_WATER_YEAR
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
    logical (c_bool)                      :: write_netcdf     = TRUE
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

    procedure :: open_zonal_stats_output_file

    procedure :: create_date_list_for_annual_statistics
    procedure :: create_date_list_for_period_statistics
    procedure :: create_date_list_for_monthly_statistics

    procedure :: read_date_range_file
    procedure :: read_zone_period_file
    procedure :: read_comparison_period_file

    procedure :: get_unique_int

    procedure :: initialize_zone_grid
    procedure :: initialize_secondary_zone_grid
    procedure :: initialize_comparison_grid

    procedure :: calculate_slice_statistics

    procedure :: open_output_netcdf_files
    procedure :: close_output_netcdf_files

    procedure :: write_stats_to_netcdf
    procedure :: write_stats_to_arcgrid
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

  type (DATA_CATALOG_ENTRY_T)    :: input_data_obj

  type (FILE_COLLECTION_T)       :: output_files(3)

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

    output_files(STATS_SUM)%grid_ptr => grid_Create (                        &
              iNX=this%ncfile_in%iNX,                                        &
              iNY=this%ncfile_in%iNY,                                        &
              rX0=this%ncfile_in%rX(NC_LEFT),                                &
              rY0=this%ncfile_in%rY(NC_BOTTOM),                              &
              rX1=this%ncfile_in%rX(NC_RIGHT),                               &
              rY1=this%ncfile_in%rY(NC_TOP),                                 &
              iDataType=GRID_DATATYPE_DOUBLE )
    output_files(STATS_SUM)%stats_description = "SUM"

    output_files(STATS_MEAN)%grid_ptr => grid_Create (                       &
              iNX=this%ncfile_in%iNX,                                        &
              iNY=this%ncfile_in%iNY,                                        &
              rX0=this%ncfile_in%rX(NC_LEFT),                                &
              rY0=this%ncfile_in%rY(NC_BOTTOM),                              &
              rX1=this%ncfile_in%rX(NC_RIGHT),                               &
              rY1=this%ncfile_in%rY(NC_TOP),                                 &
              iDataType=GRID_DATATYPE_DOUBLE )
    output_files(STATS_MEAN)%stats_description = "MEAN"

    output_files(STATS_VARIANCE)%grid_ptr => grid_Create (                   &
              iNX=this%ncfile_in%iNX,                                        &
              iNY=this%ncfile_in%iNY,                                        &
              rX0=this%ncfile_in%rX(NC_LEFT),                                &
              rY0=this%ncfile_in%rY(NC_BOTTOM),                              &
              rX1=this%ncfile_in%rX(NC_RIGHT),                               &
              rY1=this%ncfile_in%rY(NC_TOP),                                 &
              iDataType=GRID_DATATYPE_DOUBLE )
      output_files(STATS_VARIANCE)%stats_description = "VARIANCE"

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

        if (grid_values(ix, iy) <= 0 )  cycle

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

  subroutine create_date_list_for_period_statistics(this)

    class (SWBSTATS_T), intent(inout)  :: this

    call this%start_date_list%append( this%data_start_date%prettydate() )
    call this%date_range_id_list%append( "1" )
    call this%end_date_list%append( this%data_end_date%prettydate() )

  end subroutine create_date_list_for_period_statistics

!------------------------------------------------------------------------------

  subroutine initialize_zone_grid(this, grid_filename)

    class (SWBSTATS_T), intent(inout)  :: this
    character (len=*), intent(inout)   :: grid_filename

    ! [ LOCALS ]
    integer (c_int)                 :: iIndex
    integer (c_int)                 :: iStat
    character (len=:), allocatable       :: description_str
    character (len=:), allocatable       :: output_filesname_str

    description_str = left( grid_filename, substring=".")
    if (description_str .contains. "/") description_str = right( description_str, substring="/")
    if (description_str .contains. "\") description_str = right( description_str, substring="\")

    output_filesname_str = "zone_grid__"//trim(description_str)//"__as_read_into_SWBSTATS2.asc"

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

    where ( this%grd_native%dpData <= NC_FILL_FLOAT )
      this%grd_zone%pGrdBase%iData = NC_FILL_INT
    end where

    call grid_WriteArcGrid(output_filesname_str, this%grd_zone%pGrdBase )

  end subroutine initialize_zone_grid

!------------------------------------------------------------------------------

  subroutine initialize_secondary_zone_grid(this, grid_filename)

    class (SWBSTATS_T), intent(inout)  :: this
    character (len=*), intent(inout)   :: grid_filename

    ! [ LOCALS ]
    integer (c_int)                 :: iIndex
    integer (c_int)                 :: iStat
    character (len=:), allocatable       :: description_str
    character (len=:), allocatable       :: output_filesname_str

    description_str = left( grid_filename, substring=".")
    if (description_str .contains. "/") description_str = right( description_str, substring="/")
    if (description_str .contains. "\") description_str = right( description_str, substring="\")

    output_filesname_str = "zone_grid2__"//trim(description_str)//"__as_read_into_SWBSTATS2.asc"

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

    where ( this%grd_native%dpData <= NC_FILL_FLOAT )
      this%grd_zone2%pGrdBase%iData = NC_FILL_INT
    end where

    call grid_WriteArcGrid(output_filesname_str, this%grd_zone2%pGrdBase )

  end subroutine initialize_secondary_zone_grid

  !------------------------------------------------------------------------------

  subroutine initialize_comparison_grid( this, grid_filename )

    class (SWBSTATS_T), intent(inout)  :: this
    character (len=*), intent(inout)   :: grid_filename

    ! [ LOCALS ]
    integer (c_int)                 :: iIndex
    integer (c_int)                 :: iStat
    character (len=:), allocatable       :: description_str
    character (len=:), allocatable       :: output_filesname_str

    description_str = left( grid_filename, substring=".")
    if (description_str .contains. "/") description_str = right( description_str, substring="/")
    if (description_str .contains. "\") description_str = right( description_str, substring="\")

    output_filesname_str = "Comparison_grid__"//trim(description_str)//"__as_read_into_SWBSTATS2.asc"

    ! allocate memory for a generic data_catalog_entry
    if (.not. associated(this%grd_comparison))  allocate(this%grd_comparison, stat=iStat)

    call this%grd_comparison%set_target_PROJ4(this%target_proj4_string)

    call this%grd_comparison%initialize(          &
      sDescription="Comparison Grid",          &
      sFileType="ARC_GRID",                    &
      sFilename=trim(grid_filename),           &
      iDataType=DATATYPE_FLOAT )

    call this%grd_comparison%getvalues()

    where ( this%grd_native%dpData <= NC_FILL_FLOAT )
      this%grd_comparison%pGrdBase%dpData = NC_FILL_FLOAT
    elsewhere
      this%grd_comparison%pGrdBase%dpData = this%grd_comparison%pGrdBase%dpData           &
                                      * this%comparison_grid_conversion_factor
    end where

    call grid_WriteArcGrid(output_filesname_str, this%grd_comparison%pGrdBase )

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
                                    output_files,                              &
                                    start_date,                                &
                                    end_date )

    class (SWBSTATS_T), intent(inout)       :: this
    type (FILE_COLLECTION_T), intent(inout) :: output_files(:)
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

      if ( .not. output_files(stat_indx)%output_active )  cycle

      if ( output_files(stat_indx)%write_netcdf ) then

        ncfile_out => output_files(stat_indx)%nc_ptr
        grid_ptr => output_files(stat_indx)%grid_ptr

        call netcdf_put_variable_vector(NCFILE=ncfile_out,                        &
           iVarID=ncfile_out%iVarID(NC_TIME),                                     &
           iStart=[ RECNUM ],                                                     &
           iCount=[1_c_size_t],                                                   &
           iStride=[1_c_size_t],                                               &
           dpValues=[ ( start_bnd + end_bnd ) / 2.0_c_double ] )
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
           iStride=[1_c_size_t, 1_c_size_t, 1_c_size_t],                 &
           rValues=real(grid_ptr%dpData, c_float) )

      endif

    enddo

  end subroutine write_stats_to_netcdf

!------------------------------------------------------------------------------

  subroutine write_stats_to_arcgrid( this, output_files, start_date,         &
                                     end_date, date_range_string )

    class (SWBSTATS_T), intent(inout)       :: this
    type (FILE_COLLECTION_T), intent(inout) :: output_files(:)
    type (DATETIME_T), intent(inout)        :: start_date
    type (DATETIME_T), intent(inout)        :: end_date
    character (len=*), intent(in), optional :: date_range_string

    ! [ LOCALS ]
    character (len=:), allocatable    :: filename
    integer (c_int)              :: nx, ny
    type (GENERAL_GRID_T), pointer    :: grid_ptr
    integer (c_int)              :: stat_indx
    character (len=8)                 :: stats_description
    character (len=:), allocatable    :: units_string

    nx = BNDS%iNumCols
    ny = BNDS%iNumRows

    do stat_indx=STATS_MEAN, STATS_VARIANCE

      if ( .not. output_files(stat_indx)%output_active )  cycle

      if ( output_files(stat_indx)%write_arcgrid) then

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

        grid_ptr => output_files(stat_indx)%grid_ptr
        stats_description = output_files(stat_indx)%stats_description

        if ( present(date_range_string) ) then
          !@todo: change filename depending on the desired output statistic
          filename = trim(this%netcdf_variable_name_string)                     &
                     //"__"//trim(date_range_string)                               &
                     //"__"//asCharacter(nx)//"_by_"//asCharacter(ny)              &
                     //"__"//trim(stats_description)                               &
                     //"__"//trim(units_string)//".asc"
        else
          filename = trim(this%netcdf_variable_name_string)                      &
                     //"__"//start_date%prettydate()//"_to_"//end_date%prettydate() &
                     //"__"//asCharacter(nx)//"_by_"//asCharacter(ny)               &
                     //"__"//trim(units_string)//".asc"
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
  !
  ! subroutine calculate_slice_statisticss( grid_sum, grid_mean, date_range_id_list, &
  !                                 start_date_list, end_date_list )
  !
  !   type (GENERAL_GRID_T), pointer          :: grid_sum
  !   type (GENERAL_GRID_T), pointer          :: grid_mean
  !   type (FSTRING_LIST_T), intent(in)        :: date_range_id_list
  !   type (FSTRING_LIST_T), intent(in)        :: start_date_list
  !   type (FSTRING_LIST_T), intent(in)        :: end_date_list
  !
  !
  !
  !
  !
  ! end subroutine calculate_slice_statisticss

  subroutine calculate_slice_statistics( this, grid_delta, grid_delta2, start_date, end_date, grid_mask )

    class (SWBSTATS_T), intent(inout)        :: this
    type (GENERAL_GRID_T), pointer           :: grid_delta
    type (GENERAL_GRID_T), pointer           :: grid_delta2
    type (DATETIME_T), intent(inout)         :: start_date
    type (DATETIME_T), intent(inout)         :: end_date
    logical, optional                        :: grid_mask(:,:)

    ! [ LOCALS ]
    integer (c_int)               :: julian_day_number
    real (c_float), allocatable   :: tempvals(:)
    integer (c_int)               :: day_count
    logical (c_bool), allocatable :: local_mask(:,:)
    real (c_float), dimension(size(grid_delta%dpData,1), size(grid_delta%dpData,2)) :: rTemp

    ! force slice dates to honor bounds of data dates
    if ( start_date < this%data_start_date )  start_date = this%data_start_date
    if ( end_date > this%data_end_date )      end_date = this%data_end_date

!    call SIM_DT%initialize( start_date, end_date )

    associate( grd_sum => output_files(STATS_SUM)%grid_ptr%dpData,                &
               grd_mean => output_files(STATS_MEAN)%grid_ptr%dpData,              &
               grd_var => output_files(STATS_VARIANCE)%grid_ptr%dpData,           &
               delta =>  grid_delta%dpData,                                      &
               delta2 => grid_delta2%dpData,                                     &
               grd_new => this%grd_native%dpData      )

      allocate(local_mask(ubound(grd_sum,1),ubound(grd_sum,2)))

      day_count = 0

      do

        if ( SIM_DT%curr < start_date ) then
          call SIM_DT%addDay()
          cycle
        endif

        julian_day_number = int( SIM_DT%curr%dJulianDate, c_int)

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

          if (present(grid_mask)) then
            local_mask = (grd_new > NC_FILL_FLOAT) .and. grid_mask
          else
            local_mask = grd_new > NC_FILL_FLOAT
          endif

          grd_sum = 0.0_c_double
          grd_mean = 0.0_c_double
          grd_var = 0.0_c_double

        endif

        where ( local_mask )
          delta = grd_new - grd_mean
          grd_sum = grd_sum + grd_new
          grd_mean = grd_mean + delta / real( day_count, c_double )
          delta2 = grd_new - grd_mean
          grd_var = grd_var + delta * delta2
        end where

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

      where ( .not. local_mask )
        grd_mean = NC_FILL_FLOAT
        grd_sum = NC_FILL_FLOAT
        grd_var = NC_FILL_FLOAT
      ! elsewhere
      !   grd_var = grd_var / (counter - 1)
      endwhere

      if (this%annualize_stats) then
        where ( local_mask )
!          grd_mean = grd_mean/ real( day_count, c_float ) * 365.25
!          grd_var = grd_var/ real( day_count, c_float ) * 365.25
          grd_sum = grd_sum / real( day_count, c_double ) * 365.25
        end where
      ! else
      !   where ( local_mask )
      !     grd_mean = grd_sum / real( day_count, c_float )
      !   end where
      endif

    end associate

!    write(*,fmt="(a)") "gridsum: ", minval(grid_sum%rData,  mask=this%grd_native%rData > NC_FILL_FLOAT), &
!        maxval(grid_sum%rData,  mask=this%grd_native%rData > NC_FILL_FLOAT)

  end subroutine calculate_slice_statistics

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

    if (present(funit)) then
      funit_l = funit
    else
      funit_l = OUTPUT_UNIT
    endif

    if (present(zone2_ids) .and. present(unique_zone2_list)) then

      ! sort the values before use
      call unique_zone_list%sort_integer()
      call unique_zone2_list%sort_integer()

      ! obtain list of unique integers present in the zone grid
      zone_values = unique_zone_list%get_integer()
      zone2_values = unique_zone2_list%get_integer()

      do i=1,ubound(zone_values,1)
        do j=1,ubound(zone2_values,1)
          n = zone_values(i)
          m = zone2_values(j)
          if (present(comparison_values)) then
            call calc_multizonal_stats(values, zone_ids, target_id=n, target2_id=m,   &
                                  result_vector=stats,                                &
                                  zone2_ids=zone2_ids,                                &
                                  comparison_values=comparison_values)

            write(unit=funit_l, fmt="(2(a,', '),2(i0,', '),4(f14.5,', '),i0,', ',"    &
              //"4(f14.5,', '),i0)")                                                  &
              start_date%prettydate(),end_date%prettydate(), n, m, stats(1:4),        &
              int(stats(5)), stats(6:9),int(stats(10))
          else
            call calc_multizonal_stats(values, zone_ids, target_id=n, target2_id=m,   &
                                  result_vector=stats,                                &
                                  zone2_ids=zone2_ids)
            write(unit=funit_l, fmt="(2(a,', '),2(i0,', '),4(f14.5,', '),i0)")           &
              start_date%prettydate(),end_date%prettydate(), n, m, stats(1:4),int(stats(5))
          endif
        enddo
      enddo

    else
      ! obtain list of unique integers present in the zone grid
      zone_values = unique_zone_list%get_integer()

      do i=1,ubound(zone_values,1)
        n = zone_values(i)
        if (present(comparison_values)) then
          call calc_zonal_stats(values, zone_ids, target_id=n, result_vector=stats, &
                comparison_values=comparison_values)
          write(unit=funit_l, fmt="(2(a,', '),i0,', ',4(f14.5,', '),i0,', ',"  &
            //"4(f14.5,', '),i0)")                                            &
            start_date%prettydate(),end_date%prettydate(), n, stats(1:4),     &
            int(stats(5)), stats(6:9),int(stats(10))
        else
          call calc_zonal_stats(values, zone_ids, target_id=n, result_vector=stats)
          write(unit=funit_l, fmt="(2(a,', '),i0,', ',4(f14.5,', '),i0)")         &
            start_date%prettydate(),end_date%prettydate(), n, stats(1:4),int(stats(5))
        endif
      enddo

    endif

  end subroutine output_zonal_stats

!------------------------------------------------------------------------------

  subroutine open_output_netcdf_files(this,                                    &
                                      output_files)

    class (SWBSTATS_T), intent(inout)           :: this
    type (FILE_COLLECTION_T), intent(inout)     :: output_files(:)

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

      if ( .not. output_files(stat_indx)%output_active )  cycle

      if (output_files(stat_indx)%write_netcdf) then

        if ( this%annualize_stats .and. stat_indx == STATS_SUM) then
          units_string = trim(this%netcdf_variable_units_string)//", per year"
        elseif (this%report_as_volume) then
          units_string = "cubic meters"
        elseif (this%report_in_meters) then
          units_string = "meters"
        else
          units_string = trim(this%netcdf_variable_units_string)
        endif

        allocate( output_files(stat_indx)%nc_ptr, stat=status)
        call assert( status==0, "Problem allocating netcdf file data structure.", &
          __SRCNAME__, __LINE__)
        ncfile_out => output_files(stat_indx)%nc_ptr

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
                                //"_"//output_files(stat_indx)%stats_description )

      endif

    enddo

  end subroutine open_output_netcdf_files

!------------------------------------------------------------------------------

  subroutine close_output_netcdf_files(this, output_files)

    class (SWBSTATS_T), intent(inout)         :: this
    type (FILE_COLLECTION_T), intent(inout)   :: output_files(:)

    ! [ LOCALS ]
    integer (c_int)              :: stat_indx
    integer (c_int)              :: status
    type (T_NETCDF4_FILE), pointer    :: ncfile_out

    do stat_indx=STATS_MEAN, STATS_VARIANCE

      if ( .not. output_files(stat_indx)%output_active )  cycle

      if (output_files(stat_indx)%write_netcdf) then

        ncfile_out => output_files(stat_indx)%nc_ptr
        call netcdf_close_file(NCFILE=ncfile_out)

      endif

    enddo

  end subroutine close_output_netcdf_files

!------------------------------------------------------------------------------

  subroutine open_zonal_stats_output_file(this, filename)

    class (SWBSTATS_T), intent(inout)   :: this
    character (len=*)                   :: filename

    ! [ LOCALS ]
    character (len=256)    :: header_str

    call this%zonal_stats_output_file%open(filename)

    if (len_trim(this%zone_grid2_filename) > 0) then

      if (this%multiple_comparison_grids .or. this%compare_to_obs_values) then
        header_str = "start_date,end_date,zone_id,zone2_id,minimum_swb,"        &
          //"maximum_swb,mean_swb,sum_swb,count_swb,minimum_obs,maximum_obs,"   &
          //"mean_obs,sum_obs,count_obs"
      else
        header_str = "start_date,end_date,zone_id,zone2_id,minimum_swb,"        &
          //"maximum_swb,mean_swb,sum_swb,count_swb"
      endif

    else

      if (this%multiple_comparison_grids .or. this%compare_to_obs_values) then
        header_str = "start_date,end_date,zone_id,minimum_swb,maximum_swb,"     &
          //"mean_swb,sum_swb,count_swb,minimum_obs,maximum_obs,"               &
          //"mean_obs,sum_obs,count_obs"
      else
        header_str = "start_date,end_date,zone_id,minimum_swb,maximum_swb,"     &
          //"mean_swb,sum_swb,count_swb"
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
    write(*,fmt=fmt_string) "write_netcdf (SUM)", output_files(STATS_SUM)%write_netcdf
    write(*,fmt=fmt_string) "write_netcdf (MEAN)", output_files(STATS_MEAN)%write_netcdf
    write(*,fmt=fmt_string) "write_netcdf (VARIANCE)", output_files(STATS_VARIANCE)%write_netcdf
    write(*,fmt=fmt_string) "write_arcgrid (SUM)", output_files(STATS_SUM)%write_arcgrid
    write(*,fmt=fmt_string) "write_arcgrid (MEAN)", output_files(STATS_MEAN)%write_arcgrid
    write(*,fmt=fmt_string) "write_arcgrid (VARIANCE)", output_files(STATS_VARIANCE)%write_arcgrid
    write(*,fmt=fmt_string) "output_active (SUM)", output_files(STATS_SUM)%output_active
    write(*,fmt=fmt_string) "output_active (MEAN)", output_files(STATS_MEAN)%output_active
    write(*,fmt=fmt_string) "output_active (VARIANCE)", output_files(STATS_VARIANCE)%output_active

    fmt_string = "(a,t40,': ',l)"
    write(*,fmt=fmt_string) "grid pointer allocated? (SUM)", associated( output_files(STATS_SUM)%grid_ptr)
    write(*,fmt=fmt_string) "grid pointer allocated? (MEAN)", associated( output_files(STATS_MEAN)%grid_ptr)
    write(*,fmt=fmt_string) "grid pointer allocated? (VARIANCE)", associated( output_files(STATS_VARIANCE)%grid_ptr)

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
