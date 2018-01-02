module model_domain

  use iso_c_binding
  use constants_and_conversions
  use continuous_frozen_ground_index
  use data_catalog
  use data_catalog_entry
  use exceptions
  use logfiles
  use simulation_datetime
  use snowfall__original
  use string_list, only      : STRING_LIST_T
  use grid
  use parameters, only       : PARAMS, PARAMS_DICT
  use netcdf4_support, only  : NC_FILL_FLOAT
  use strings
  implicit none

  private

  ! concept: the only state variables that should appear in this module should be
  !          those that are required regardless of what other program options are active
  !
  !
  ! additionally: concept is to have a set of more or less standard procedure pointers that are common to
  !               a soil moisture balance; the pointer may be set to a different procedure thus changing
  !               the behavior and mechanisms within the model

  type, public :: MODEL_DOMAIN_T

    character (len=:), allocatable     :: output_directory_name
    character (len=:), allocatable     :: PROJ4_string
    integer (kind=c_int)               :: number_of_columns
    integer (kind=c_int)               :: number_of_rows
    real (kind=c_double)               :: X_ll, Y_ll
    real (kind=c_double)               :: X_ur, Y_ur
    real (kind=c_double), allocatable  :: X(:), Y(:)
    real (kind=c_double), allocatable  :: X_lon(:,:), Y_lat(:,:)
    real (kind=c_double)               :: gridcellsize

    logical (kind=c_bool), allocatable     :: active(:,:)
    real (kind=c_float), allocatable       :: nodata_fill_value(:,:)
    real (kind=c_float), allocatable       :: array_output(:,:)

    integer (kind=c_int), allocatable      :: landuse_code(:)
    integer (kind=c_int), allocatable      :: landuse_index(:)
    integer (kind=c_int), allocatable      :: soil_code(:)
    integer (kind=c_int), allocatable      :: soil_group(:)
    integer (kind=c_int), allocatable      :: num_upslope_connections(:)
    integer (kind=c_int), allocatable      :: sum_upslope_cells(:)

    integer (kind=c_int), allocatable      :: col_num_2D(:,:)
    integer (kind=c_int), allocatable      :: row_num_2D(:,:)

    integer (kind=c_int), allocatable      :: col_num_1D(:)
    integer (kind=c_int), allocatable      :: row_num_1D(:)

    real (kind=c_float), allocatable       :: pervious_fraction(:)
    real (kind=c_float), allocatable       :: canopy_cover_fraction(:)
    real (kind=c_float), allocatable       :: awc(:)
    real (kind=c_float), allocatable       :: curve_num_adj(:)
    real (kind=c_float), allocatable       :: gdd(:)
    real (kind=c_float), allocatable       :: crop_coefficient_kcb(:)
    real (kind=c_float), allocatable       :: surf_evap_coef_ke(:)
    real (kind=c_float), allocatable       :: plant_stress_coef_ks(:)
    real (kind=c_float), allocatable       :: evap_reduction_coef_kr(:)
    real (kind=c_float), allocatable       :: total_available_water_taw(:)
    real (kind=c_float), allocatable       :: readily_available_water_raw(:)

    real (kind=c_float), allocatable       :: continuous_frozen_ground_index(:)

    real (kind=c_float), allocatable       :: rooting_depth_max(:)

    integer (kind=c_int), allocatable      :: polygon_id(:)
    real (kind=c_float), allocatable       :: latitude(:)
    real (kind=c_float), allocatable       :: reference_et0(:)
    real (kind=c_float), allocatable       :: crop_etc(:)

    real (kind=c_float), allocatable       :: actual_et_interception(:)
    real (kind=c_float), allocatable       :: actual_et_impervious(:)
    real (kind=c_float), allocatable       :: actual_et_soil(:)
    real (kind=c_float), allocatable       :: actual_et(:)
    real (kind=c_float), allocatable       :: bare_soil_evap(:)

    real (kind=c_float), allocatable       :: inflow(:)
    real (kind=c_float), allocatable       :: runon(:)
    real (kind=c_float), allocatable       :: runoff(:)
    real (kind=c_float), allocatable       :: monthly_runoff(:)
    real (kind=c_float), allocatable       :: runoff_outside(:)
    real (kind=c_float), allocatable       :: outflow(:)
    real (kind=c_float), allocatable       :: infiltration(:)
    real (kind=c_float), allocatable       :: potential_snowmelt(:)
    real (kind=c_float), allocatable       :: snowmelt(:)
    type (GENERAL_GRID_T), pointer         :: pGrdOut
    real (kind=c_float), allocatable       :: interception(:)
    real (kind=c_float), allocatable       :: interception_storage(:)

    real (kind=c_float), allocatable       :: snow_storage(:)
    real (kind=c_float), allocatable       :: surface_storage(:)
    real (kind=c_float), allocatable       :: surface_storage_max(:)
    real (kind=c_float), allocatable       :: surface_storage_excess(:)
    real (kind=c_float), allocatable       :: storm_drain_capture(:)
    real (kind=c_float), allocatable       :: delta_soil_storage(:)
    real (kind=c_float), allocatable       :: soil_storage(:)
    real (kind=c_float), allocatable       :: soil_storage_max(:)
    real (kind=c_float), allocatable       :: net_infiltration(:)
    real (kind=c_float), allocatable       :: rejected_net_infiltration(:)
    real (kind=c_float), allocatable       :: direct_net_infiltration(:)
    real (kind=c_float), allocatable       :: direct_soil_moisture(:)
    real (kind=c_float), allocatable       :: current_rooting_depth(:)
    integer (kind=c_int), allocatable      :: number_of_days_since_planting(:)
    logical (kind=c_bool), allocatable     :: it_is_growing_season(:)

    real (kind=c_float), allocatable       :: gross_precip(:)
    real (kind=c_float), allocatable       :: monthly_gross_precip(:)
    real (kind=c_float), allocatable       :: fog(:)
    real (kind=c_float), allocatable       :: rainfall(:)
    real (kind=c_float), allocatable       :: snowfall(:)
    real (kind=c_float), allocatable       :: irrigation(:)

    real (kind=c_float), allocatable       :: tmin(:)
    real (kind=c_float), allocatable       :: tmax(:)
    real (kind=c_float), allocatable       :: tmean(:)

    real (kind=c_float), allocatable       :: routing_fraction(:)

    integer (kind=c_int), allocatable      :: sort_order(:)

    real (kind=c_float), allocatable       :: adjusted_depletion_fraction_p(:)
    real (kind=c_float), allocatable       :: fraction_exposed_and_wetted_soil(:)

    ! member variables that are only allocated if particular optional methods are invoked

    real (kind=c_float), allocatable       :: irrigation_mask(:)

    !> declare and initialize procedure pointers such that the default methods are in place
    procedure ( array_method ), pointer  :: init_interception                                                &
                                                => model_initialize_interception_bucket
    procedure ( array_method ), pointer  :: init_runoff                                                      &
                                                => model_initialize_runoff_curve_number
    procedure ( array_method ), pointer  :: init_reference_et                                                &
                                                => model_initialize_et_hargreaves
    procedure ( array_method ), pointer  :: init_actual_et                                                   &
                                                => model_initialize_actual_et_thornthwaite_mather_eqns
    procedure ( array_method ), pointer  :: init_routing                                                     &
                                                => model_initialize_routing_D8
    procedure ( array_method ), pointer  :: init_soil_storage_max                                            &
                                                => model_initialize_soil_storage_max_internally_calculated
    procedure ( array_method ), pointer  :: init_snowfall                                                    &
                                                => model_initialize_snowfall_original
    procedure ( array_method ), pointer  :: init_snowmelt                                                    &
                                                => model_initialize_snowmelt_original
    procedure ( array_method ), pointer  :: init_precipitation_data                                          &
                                                => model_initialize_precip_normal
    procedure ( array_method ), pointer  :: init_fog                                                         &
                                                => model_initialize_fog_none
    procedure ( array_method ), pointer  :: init_irrigation                                                  &
                                                => model_initialize_irrigation_none
    procedure ( array_method ), pointer  :: init_direct_net_infiltration                                             &
                                                => model_initialize_direct_net_infiltration_gridded
    procedure ( array_method ), pointer  :: init_direct_soil_moisture                                        &
                                                => model_initialize_direct_soil_moisture_none
    procedure ( array_method ), pointer  :: update_landuse_codes                                             &
                                                => model_update_landuse_codes_static
    procedure ( array_method ), pointer  :: init_GDD                                                         &
                                                => model_initialize_GDD
    procedure ( array_method ), pointer  :: init_AWC                                                         &
                                                => model_initialize_available_water_content_gridded
    procedure ( array_method ), pointer  :: init_crop_coefficient                                            &
                                                => model_initialize_crop_coefficient_none
    procedure ( array_method ), pointer  :: calc_interception                                                &
                                                => model_calculate_interception_bucket
    procedure ( array_method ), pointer  :: update_crop_coefficient                                          &
                                                => model_update_crop_coefficient_none

    procedure ( array_method ), pointer  :: update_rooting_depth                                          &
                                                => model_update_rooting_depth_none

    procedure ( array_method ), pointer  :: init_continuous_frozen_ground_index                              &
                                                => model_initialize_continuous_frozen_ground_index
    procedure ( array_method ), pointer  :: calc_continuous_frozen_ground_index                              &
                                                => model_calculate_continuous_frozen_ground_index

    procedure ( array_method ), pointer  :: init_maximum_net_infiltration                                  &
                                                => model_initialize_maximum_net_infiltration_gridded
    procedure ( index_method ), pointer  :: calc_maximum_net_infiltration                                  &
                                                => model_calculate_maximum_net_infiltration_gridded

    procedure ( index_method ), pointer  :: calc_runoff                                                      &
                                                => model_calculate_runoff_curve_number

    procedure ( array_method ), pointer  :: calc_reference_et                                                &
                                                => model_calculate_et_hargreaves
    procedure ( index_method ), pointer  :: calc_routing                                                     &
                                                => model_calculate_routing_D8

    procedure ( index_method ), pointer  :: calc_actual_et                                                   &
                                                => model_calculate_actual_et_thornthwaite_mather_eqns
    procedure ( array_method ), pointer  :: calc_snowfall                                                    &
                                                => model_calculate_snowfall_original
    procedure ( array_method ), pointer  :: calc_snowmelt                                                    &
                                                => model_calculate_snowmelt_original
    procedure ( array_method ), pointer  :: calc_fog                                                         &
                                                => model_calculate_fog_none
    procedure ( array_method ), pointer  :: calc_irrigation                                                  &
                                                => model_calculate_irrigation_none
    procedure ( array_method ), pointer  :: calc_GDD                                                         &
                                                => model_calculate_GDD
    procedure ( index_method ), pointer  :: calc_direct_net_infiltration                                             &
                                                => model_calculate_direct_net_infiltration_none
    procedure ( index_method ), pointer  :: calc_direct_soil_moisture                                        &
                                                => model_calculate_direct_soil_moisture_none

    procedure (array_method), pointer    :: output_irrigation                                                &
                                                => model_output_irrigation_none
    procedure (array_method), pointer    :: dump_variables                                                   &
                                                => model_dump_variables_none

    procedure ( array_method ), pointer  :: read_awc_data                                                    &
                                                => model_read_available_water_content_gridded
    procedure ( array_method ), pointer  :: get_precipitation_data                                           &
                                                => model_get_precip_normal
    procedure ( array_method ), pointer  :: get_minimum_air_temperature_data                                 &
                                                => model_get_minimum_air_temperature_normal
    procedure ( array_method ), pointer  :: get_maximum_air_temperature_data                                 &
                                                => model_get_maximum_air_temperature_normal
    procedure ( array_method ), pointer  :: calculate_mean_air_temperature                                   &
                                                => model_calculate_mean_air_temperature

  contains

    procedure :: model_update_rooting_depth_table_sub
    procedure :: update_rooting_depth_table => model_update_rooting_depth_table_sub

    procedure :: initialize_arrays_sub
    generic   :: initialize_arrays => initialize_arrays_sub

    procedure :: initialize_grid_sub
    generic   :: initialize_grid => initialize_grid_sub

    procedure :: set_method_pointers_sub
    generic   :: set_method_pointers => set_method_pointers_sub

    procedure :: set_inactive_cells_sub
    generic   :: set_inactive_cells => set_inactive_cells_sub

    procedure :: preflight_check_method_pointers

    procedure :: get_climate_data

    procedure :: set_output_directory => set_output_directory_sub

    procedure :: initialize_methods_sub
    generic   :: initialize_methods => initialize_methods_sub

    procedure :: summarize_state_variables_sub
    generic   :: summarize => summarize_state_variables_sub

    procedure :: initialize_row_column_indices_sub
    generic   :: initialize_row_column_indices => initialize_row_column_indices_sub

    procedure :: row_column_to_index_fn
    generic   :: row_column_to_index => row_column_to_index_fn

    procedure :: model_initialize_growing_season
    generic   :: initialize_growing_season => model_initialize_growing_season

    procedure :: model_update_growing_season
    generic   :: update_growing_season => model_update_growing_season

  end type MODEL_DOMAIN_T

  type :: CELL_COL_ROW_T
    integer (kind=c_int) :: unitnum
    integer (kind=c_int) :: col
    integer (kind=c_int) :: row
    integer (kind=c_int) :: indx_start
    integer (kind=c_int) :: indx_end
  end type CELL_COL_ROW_T

  type ( CELL_COL_ROW_T ) :: DUMP(5)

  ! array method: designed to be called using whole-array notation
  abstract interface
    subroutine array_method( this )
      import :: MODEL_DOMAIN_T
      class ( MODEL_DOMAIN_T ), intent(inout)  :: this
    end subroutine array_method
  end interface

  ! indexed method: designed to be called sequentially with explicit
  ! index values provided
  abstract interface
    subroutine index_method( this, index )
      import :: MODEL_DOMAIN_T, c_int
      class ( MODEL_DOMAIN_T ), intent(inout)       :: this
      integer (kind=c_int), intent(in)              :: index
    end subroutine index_method
  end interface

  interface minmaxmean
    procedure :: minmaxmean_float
    procedure :: minmaxmean_int
  end interface minmaxmean

  ! creating several module-level globals
  type (MODEL_DOMAIN_T), public             :: MODEL

  real (kind=c_float), allocatable, public  :: ROOTING_DEPTH_MAX(:,:)

  type (GENERAL_GRID_T), pointer            :: pROOTING_DEPTH

  public :: minmaxmean, initialize_landuse_codes, read_landuse_codes

contains

  !
  ! current concept:
  !
  ! The only 2-D array is the array that contains the mask of active cells.
  !
  ! All remaining state variables and ancillary variables are kept in 1-D vectors
  ! that are PACK-ed and UNPACK-ed as needed by i/o routines. This is cumbersome for fully
  ! active grids, but should amount to significant memory and processing savings when running
  ! SWB for, say, an island domain.
  !

  subroutine set_output_directory_sub( this, output_dir_name )

    class (MODEL_DOMAIN_T), intent(inout)        :: this
    character (len=*), intent(in)                :: output_dir_name

    this%output_directory_name = output_dir_name

  end subroutine set_output_directory_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_grid_sub(this, iNumCols, iNumRows, dX_ll, dY_ll, dGridCellSize )

    class (MODEL_DOMAIN_T), intent(inout)        :: this
    integer (kind=c_int), intent(in)             :: iNumCols
    integer (kind=c_int), intent(in)             :: iNumRows
    real (kind=c_double), intent(in)             :: dX_ll
    real (kind=c_double), intent(in)             :: dY_ll
    real (kind=c_double), intent(in)             :: dGridcellSize

    ! [ LOCALS ]
    integer (kind=c_int)  :: iStat

    this%number_of_columns = iNumCols
    this%number_of_rows = iNumRows
    this%X_ll = dX_ll
    this%Y_ll = dY_ll
    this%gridcellsize = dGridcellSize

    allocate(this%active(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __SRCNAME__, __LINE__)

    allocate(this%nodata_fill_value(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __SRCNAME__, __LINE__)

    allocate(this%array_output(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __SRCNAME__, __LINE__)

    allocate(this%col_num_2D(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __SRCNAME__, __LINE__)

    allocate(this%row_num_2D(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __SRCNAME__, __LINE__)

    this%pGrdOut => grid_CreateSimple( iNX=iNumCols, iNY=iNumRows,        &
                      rX0=dX_ll, rY0=dY_ll, rGridCellSize=dGridCellSize,  &
                      iDataType=GRID_DATATYPE_REAL )

    this%nodata_fill_value = NC_FILL_FLOAT

  end subroutine initialize_grid_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_arrays_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)        :: this

    ! [ LOCALS ]
    integer (kind=c_int)  :: iCount
    integer (kind=c_int)  :: iIndex
    integer (kind=c_int)  :: indx
    integer (kind=c_int)  :: iStat(61)

    iCount = count( this%active )
    iStat = 0

    allocate( this%landuse_code(iCount), stat=iStat(1) )
    allocate( this%landuse_index(iCount), stat=iStat(2) )
    allocate( this%soil_group(iCount), stat=iStat(3) )
    allocate( this%num_upslope_connections(iCount), stat=iStat(4) )
    allocate( this%sum_upslope_cells(iCount), stat=iStat(5) )
    allocate( this%awc(iCount), stat=iStat(6) )
    allocate( this%latitude(iCount), stat=iStat(7) )
    allocate( this%reference_ET0(iCount), stat=iStat(8) )
    allocate( this%actual_ET(iCount), stat=iStat(9) )
    allocate( this%inflow(iCount), stat=iStat(10))
    allocate( this%runon(iCount), stat=iStat(11) )
    allocate( this%runoff(iCount), stat=iStat(12) )
    allocate( this%outflow(iCount), stat=iStat(13) )
    allocate( this%infiltration(iCount), stat=iStat(14) )
    allocate( this%snowfall(iCount), stat=iStat(15) )
    allocate( this%snowmelt(iCount), stat=iStat(16) )
    allocate( this%interception(iCount), stat=iStat(17) )
    allocate( this%rainfall(iCount), stat=iStat(18) )
    allocate( this%interception_storage(iCount), stat=iStat(19) )
    allocate( this%snow_storage(iCount), stat=iStat(20) )
    allocate( this%soil_storage(iCount), stat=iStat(21) )
    allocate( this%soil_storage_max(iCount), stat=iStat(22) )
    allocate( this%net_infiltration(iCount), stat=iStat(23) )
    allocate( this%fog(iCount), stat=iStat(24) )
    allocate( this%irrigation(iCount), stat=iStat(25) )
    allocate( this%sort_order(iCount), stat=iStat(26) )
    allocate( this%runoff_outside( iCount ), stat=iStat(28) )
    allocate( this%pervious_fraction( iCount ), stat=iStat(29) )
    allocate( this%surface_storage( iCount ), stat=iStat(30) )
    allocate( this%surface_storage_excess( iCount ), stat=iStat(31) )
    allocate( this%surface_storage_max( iCount ), stat=iStat(32) )
    allocate( this%storm_drain_capture( iCount ), stat=iStat(33) )
    allocate( this%canopy_cover_fraction( iCount ), stat=iStat(34) )
    allocate( this%crop_coefficient_kcb( iCount ), stat=iStat(35) )
    allocate( this%potential_snowmelt( iCount ), stat=iStat(36) )
    allocate( this%continuous_frozen_ground_index( iCount ), stat=iStat(37) )
    allocate( this%rooting_depth_max( iCount ), stat=iStat(38) )
    allocate( this%current_rooting_depth( iCount ), stat=iStat(39) )
    allocate( this%polygon_id( iCount ), stat=iStat(40) )
    allocate( this%actual_et_soil( iCount ), stat=iStat(41) )
    allocate( this%actual_et_impervious( iCount ), stat=iStat(42) )
    allocate( this%actual_et_interception( iCount ), stat=iStat(43) )
    allocate( this%adjusted_depletion_fraction_p( iCount ), stat=iStat(44) )
    allocate( this%crop_etc( iCount ), stat=iStat(45) )
    allocate( this%direct_net_infiltration( iCount ), stat=iStat(46) )
    allocate( this%direct_soil_moisture( iCount ), stat=iStat(47) )
    allocate( this%number_of_days_since_planting( iCount ), stat=iStat(48) )
    allocate( this%col_num_1D( iCount ), stat=iStat(49) )
    allocate( this%row_num_1D( iCount ), stat=iStat(50) )
    allocate( this%it_is_growing_season( iCount ), stat=iStat(51) )
    allocate( this%curve_num_adj( iCount ), stat=iStat(52) )
    allocate( this%rejected_net_infiltration( iCount ), stat=iStat(53) )
    allocate( this%evap_reduction_coef_kr( iCount ), stat=iStat(54) )
    allocate( this%surf_evap_coef_ke( iCount ), stat=iStat(55) )
    allocate( this%plant_stress_coef_ks( iCount ), stat=iStat(56) )
    allocate( this%total_available_water_taw( iCount ), stat=iStat(57) )
    allocate( this%readily_available_water_raw( iCount ), stat=iStat(58) )
    allocate( this%bare_soil_evap( iCount ), stat=iStat(59) )
    allocate( this%fraction_exposed_and_wetted_soil( iCount ), stat=iStat(60) )
    allocate( this%delta_soil_storage( iCount ), stat=iStat(60) )

    do iIndex = 1, ubound( iStat, 1)
      if ( iStat( iIndex ) /= 0 )   call warn("INTERNAL PROGRAMMING ERROR"                    &
                                              //"--Problem allocating memory; iIndex="        &
                                              //asCharacter(iIndex), __SRCNAME__, __LINE__ )
    enddo

    if (any( iStat /= 0) ) call die ( "Unable to allocate memory for one or more arrays.",    &
                                      __SRCNAME__, __LINE__ )

    this%landuse_code                        = 0_c_int
    this%landuse_index                       = 0_c_int
    this%soil_group                          = 0_c_int
    this%num_upslope_connections             = 0_c_int
    this%sum_upslope_cells                   = 0_c_int
    this%awc                                 = 0.0_c_float
    this%latitude                            = 0.0_c_float
    this%reference_ET0                       = 0.0_c_float
    this%actual_ET                           = 0.0_c_float
    this%bare_soil_evap                      = 0.0_c_float
    this%inflow                              = 0.0_c_float
    this%runon                               = 0.0_c_float
    this%runoff                              = 0.0_c_float
    this%outflow                             = 0.0_c_float
    this%infiltration                        = 0.0_c_float
    this%snowfall                            = 0.0_c_float
    this%snowmelt                            = 0.0_c_float

    this%interception                        = 0.0_c_float
    this%rainfall                            = 0.0_c_float
    this%interception_storage                = 0.0_c_float
    this%snow_storage                        = 0.0_c_float
    this%soil_storage                        = 0.0_c_float
    this%soil_storage_max                    = 0.0_c_float
    this%delta_soil_storage                  = 0.0_c_float

    this%net_infiltration                  = 0.0_c_float
    this%rejected_net_infiltration         = 0.0_c_float
    this%fog                                 = 0.0_c_float
    this%irrigation                          = 0.0_c_float
    this%curve_num_adj                       = 0.0_c_float
    this%runoff_outside                      = 0.0_c_float
    this%pervious_fraction                   = 1.0_c_float     ! note: default is 100% pervious area
    this%surface_storage                     = 0.0_c_float
    this%surface_storage_excess              = 0.0_c_float
    this%surface_storage_max                 = 0.0_c_float
    this%storm_drain_capture                 = 0.0_c_float
    this%canopy_cover_fraction               = 0.0_c_float
    this%crop_coefficient_kcb                = 0.0_c_float
    this%potential_snowmelt                  = 0.0_c_float
    this%continuous_frozen_ground_index      = 0.0_c_float
    this%rooting_depth_max                   = 0.0_c_float
    this%current_rooting_depth               = 0.0_c_float
    this%polygon_id                          = 0_c_int
    this%actual_et_soil                      = 0.0_c_float
    this%actual_et_impervious                = 0.0_c_float
    this%actual_et_interception              = 0.0_c_float
    this%adjusted_depletion_fraction_p       = 0.0_c_float
    this%crop_etc                            = 0.0_c_float
    this%direct_net_infiltration                     = 0.0_c_float
    this%direct_soil_moisture                = 0.0_c_float
    this%number_of_days_since_planting       = 0_c_int
    this%evap_reduction_coef_kr              = 0.0_c_float
    this%surf_evap_coef_ke                   = 0.0_c_float
    this%plant_stress_coef_ks                = 0.0_c_float
    this%total_available_water_taw           = 0.0_c_float
    this%readily_available_water_raw         = 0.0_c_float
    this%fraction_exposed_and_wetted_soil    = 0.0_c_float
    this%it_is_growing_season                = lFALSE

    do iIndex=1, iCount
      this%sort_order( iIndex ) = iIndex
    enddo

  end subroutine initialize_arrays_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_row_column_indices_sub( this )

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: row_num, col_num
    integer (kind=c_int) :: status

    do row_num=lbound( this%row_num_2D, 2), ubound( this%row_num_2D, 2)
      this%row_num_2D( :, row_num ) = row_num
    enddo

    do col_num=lbound( this%col_num_2D, 1), ubound( this%col_num_2D, 1 )
      this%col_num_2D( col_num, : ) = col_num
    enddo

    this%col_num_1D = pack( this%col_num_2D, this%active )
    this%row_num_1D = pack( this%row_num_2D, this%active )

    deallocate( this%col_num_2D, stat=status )
    deallocate( this%row_num_2D, stat=status )

  end subroutine initialize_row_column_indices_sub

!--------------------------------------------------------------------------------------------------

  function row_column_to_index_fn( this, col_num, row_num )   result( indexval )

    class (MODEL_DOMAIN_T), intent(inout)   :: this
    integer (kind=c_int), intent(in)        :: col_num
    integer (kind=c_int), intent(in)        :: row_num
    integer (kind=c_int)                    :: indexval

    ! [ LOCALS ]
    logical (kind=c_bool)   :: found_match

    found_match = FALSE

    do indexval=lbound( this%col_num_1D, 1 ), ubound( this%col_num_1D, 1 )

      if (     ( this%col_num_1D( indexval ) == col_num )            &
         .and. ( this%row_num_1D( indexval ) == row_num ) ) then
        found_match = TRUE
        exit
      endif

    enddo

    if ( .not. found_match )  indexval = -9999

  end function row_column_to_index_fn

!--------------------------------------------------------------------------------------------------

  subroutine initialize_methods_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    call this%init_interception

    call this%init_snowfall

    call this%init_snowmelt

    call this%init_fog

    call this%init_precipitation_data

    call this%init_runoff

    call this%init_soil_storage_max

    call this%init_routing

    call this%init_actual_et

    call this%init_reference_et

    call this%init_GDD

    call this%init_irrigation

    call this%init_direct_net_infiltration

    call this%init_maximum_net_infiltration

    call this%init_crop_coefficient

  end subroutine initialize_methods_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_inactive_cells_sub(this)

    use awc__depth_integrated, only  : AVAILABLE_WATER_CONTENT

    class (MODEL_DOMAIN_T), intent(inout)   :: this
    type (DATA_CATALOG_ENTRY_T), pointer :: pHSG
    type (DATA_CATALOG_ENTRY_T), pointer :: pLULC
    type (DATA_CATALOG_ENTRY_T), pointer :: pAWC
    type (DATA_CATALOG_ENTRY_T), pointer :: pSoil_Storage_Max
    type (DATA_CATALOG_ENTRY_T), pointer :: pD8_FLOWDIR

    pLULC             => DAT%find("LAND_USE")
    pHSG              => DAT%find("HYDROLOGIC_SOILS_GROUP")
    pAWC              => DAT%find("AVAILABLE_WATER_CONTENT")
    pSoil_Storage_Max => DAT%find("SOIL_STORAGE_MAX")
    pD8_FLOWDIR => DAT%find("FLOW_DIRECTION")

    if( .not. associated( pAWC ) )  pAWC => DAT%find("AVAILABLE_WATER_CONTENT")

    if ( .not. associated(pHSG) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)

    if ( .not. associated(pHSG%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)

    if ( .not. allocated(pHSG%pGrdBase%iData) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of UNALLOCATED variable", __SRCNAME__, __LINE__)

!     if ( .not. associated(pAWC) ) &
!       call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)

!     if ( .not. associated(pAWC%pGrdBase) ) &
!       call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)

!     if ( .not. allocated(pAWC%pGrdBase%rData) ) &
!       call die("INTERNAL PROGRAMMING ERROR: attempted use of UNALLOCATED variable", __SRCNAME__, __LINE__)

    if ( .not. associated(pLULC) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)

    if ( .not. associated(pLULC%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)

    if ( .not. allocated(pLULC%pGrdBase%iData) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of UNALLOCATED variable", __SRCNAME__, __LINE__)

    this%active = .true._c_bool

    if ( associated( pD8_FLOWDIR ) ) then

      call pD8_FLOWDIR%getvalues()
      where ( pD8_FLOWDIR%pGrdBase%iData < 0 )   this%active = .false._c_bool

    endif

    if ( associated( pAWC) ) then

      where (       ( pHSG%pGrdBase%iData  < 1 )                  &
              .or.  ( pLULC%pGrdBase%iData < 0 )                  &
              .or.  ( pAWC%pGrdBase%rData < 0.0 ) )

        this%active = .false._c_bool

      end where

    elseif ( allocated( AVAILABLE_WATER_CONTENT ) ) then

      where (       ( pHSG%pGrdBase%iData  < 1 )                  &
              .or.  ( pLULC%pGrdBase%iData < 0 )                  &
              .or.  ( AVAILABLE_WATER_CONTENT < 0.0 ) )

        this%active = .false._c_bool

      end where

    elseif ( associated( pSoil_Storage_Max ) ) then

      where (       ( pHSG%pGrdBase%iData  < 1 )                  &
              .or.  ( pLULC%pGrdBase%iData < 0 )                  &
              .or.  ( pSoil_Storage_Max%pGrdBase%rData < 0.0 ) )

        this%active = .false._c_bool

      end where

    else

      call die( "Failed to find gridded or tabular data to use in initializing " &
               //"available water capacity or soil storage.", &
        __SRCNAME__, __LINE__ )

    endif

    call LOGS%write(asCharacter(count(this%active))//" cells are currently active out of a total of " &
      //asCharacter(size(this%active)), iLinesBefore=1, iLinesAfter=1, iLogLevel=LOG_ALL)

  end subroutine set_inactive_cells_sub

!--------------------------------------------------------------------------------------------------

  subroutine read_landuse_codes

    ! [ LOCALS ]
    type (DATA_CATALOG_ENTRY_T), pointer :: pLULC
    character (len=10)                   :: date_str

    pLULC => DAT%find("LAND_USE")

    if ( associated(pLULC) ) then

      if (pLULC%iSourceDataForm == DYNAMIC_GRID) then

        call pLULC%getvalues(iMonth=int(SIM_DT%curr%iMonth, kind=c_int),      &
                             iDay=int(SIM_DT%curr%iDay, kind=c_int),          &
                             iYear=SIM_DT%curr%iYear )

        if ( pLULC%lGridHasChanged ) then
          date_str = SIM_DT%curr%prettydate()
          call grid_WriteArcGrid("Landuse_land_cover__as_read_into_SWB__"     &
                                //trim(date_str)//".asc", pLULC%pGrdBase )
        endif

      else

        call pLULC%getvalues()
        call grid_WriteArcGrid("Landuse_land_cover__as_read_into_SWB.asc", pLULC%pGrdBase )

      endif

    else

      call warn(sMessage="LAND_USE dataset is flawed or missing.", lFatal=lTRUE,         &
        iLogLevel = LOG_ALL, sHints="Check to see that a valid path and filename have"   &
        //" been ~included in the control file for the LAND_USE dataset.",               &
        lEcho = lTRUE )

    endif

  end subroutine read_landuse_codes

!--------------------------------------------------------------------------------------------------

  !> Match landuse codes from table with those contained in the gridded landuse.
  !!
  !! This routine loops through all known

  subroutine initialize_landuse_codes()

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iIndex
    integer (kind=c_int), allocatable    :: iLandUseCodes(:)
    type (DATA_CATALOG_ENTRY_T), pointer :: pLULC
    integer (kind=c_int)                 :: iIndex2
    integer (kind=c_int)                 :: iCount
    integer (kind=c_int)                 :: iStat
    logical (kind=c_bool)                :: lMatch
    type (STRING_LIST_T)                 :: slList

    call slList%append("LU_Code")
    call slList%append("LU_code")
    call slList%append("Landuse_Code")
    call slList%append("LULC_Code")

    !> Determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=slList, iValues=iLanduseCodes, lFatal=TRUE )

    ! obtain a pointer to the LAND_USE grid
    pLULC => DAT%find("LAND_USE")

    if ( associated(pLULC) ) then

      if (associated( pLULC%pGrdBase) ) then
        MODEL%landuse_code = pack( pLULC%pGrdBase%iData, MODEL%active )
      else
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)
      endif
    else
      call die("Attempted use of NULL pointer. Failed to find LAND_USE data element.", &
        __SRCNAME__, __LINE__)
    endif

    ! setting this to a value that is likely valid; if this is set to a negative value, a landuse
    ! code that is present in the grid but not in the lookup table will trigger a fatal error, however,
    ! processing will continue until a bounds error is triggered somewhere else in the code,
    MODEL%landuse_index = -9999
    iCount = 0

    ! only run through matching process if we've found a LU_Code entry in the
    ! parameter dictionary
    if ( all( iLandUseCodes >= 0 ) ) then

      do iIndex = 1, ubound(MODEL%landuse_code,1)

        lMatch = lFALSE

        do iIndex2=1, ubound(iLandUseCodes, 1)

          if (MODEL%landuse_code(iIndex) == iLandUseCodes(iIndex2) ) then
            MODEL%landuse_index(iIndex) = iIndex2
            iCount = iCount + 1
            lMatch = lTRUE
            exit
          endif

        enddo

        if ( .not. lMatch ) then
          call warn(sMessage="Failed to match landuse code "//asCharacter(MODEL%landuse_code(iIndex) ) &
            //" with a corresponding landuse code from lookup tables.",                                &
            sHints="Make sure your lookup table(s) have landuse codes corresponding to all values in " &
            //"the land-use grid.", lFatal=TRUE, iLogLevel=LOG_ALL, lEcho=TRUE)
          ! we're setting this value to a valid value. this shouldn't cause problems with any
          ! calculations because we've already thrown a fatal error
          MODEL%landuse_index(iIndex) = 1
        endif
      enddo

      call LOGS%write("Matches were found between landuse grid value and table value for " &
        //asCharacter(iCount)//" cells out of a total of "//asCharacter(ubound(MODEL%landuse_code,1))//" active cells.", &
        iLinesBefore=1, iLinesAfter=1, iLogLevel=LOG_ALL)

      call slList%clear()

    endif

    ! if we have more than one cell for which an index value could not be assigned, trigger fatal error
    if ( count(MODEL%landuse_index < 0) > 0 ) then
      call warn(asCharacter(count(MODEL%landuse_index < 0))//" landuse codes could not be "        &
      //" assigned a landuse index value.", lFatal=lTRUE, sHints="Make sure that you have an "        &
      //"entry in the landuse lookup table for each unique code contained in your landuse grid." )
    endif


  end subroutine initialize_landuse_codes

!--------------------------------------------------------------------------------------------------

!   subroutine read_in_available_water_content_sub( this )

!     class (MODEL_DOMAIN_T), intent(inout)     :: this

!     ! [ LOCALS ]
!     integer (kind=c_int)                 :: iStat
!     integer (kind=c_int)                 :: iIndex
!     type (DATA_CATALOG_ENTRY_T), pointer :: pAWC

!     pAWC => DAT%find("AVAILABLE_WATER_CONTENT")

!     if ( associated(pAWC) ) then

!       call pAWC%getvalues()
!       call grid_WriteArcGrid("Available_Water_Content__as_read_in_by_SWB.asc", pAWC%pGrdBase )

!     else

!       call warn(sMessage="AVAILABLE_WATER_CONTENT dataset is flawed or missing.", lFatal=lFALSE,    &
!         iLogLevel = LOG_ALL, sHints="Check to see that a valid path and filename have"              &
!         //" been ~included in the control file for the AVAILABLE_WATER_CONTENT dataset.",           &
!         lEcho = lTRUE )

!     endif

!   end subroutine read_in_available_water_content_sub

!--------------------------------------------------------------------------------------------------

  subroutine get_climate_data(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iJulianDay
    integer (kind=c_int) ::iMonth
    integer (kind=c_int) ::iDay
    integer (kind=c_int) ::iYear

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear


      ! the following statements process the raw data in order to get it into the
      ! right units or properly pack the data

      call this%get_precipitation_data()
      call this%get_minimum_air_temperature_data()
      call this%get_maximum_air_temperature_data()

      call this%calculate_mean_air_temperature()

    end associate

  end subroutine get_climate_data

!--------------------------------------------------------------------------------------------------

  subroutine preflight_check_method_pointers(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    if (.not. associated( this%init_interception) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )
    if (.not. associated( this%calc_interception) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )

    if (.not. associated( this%init_irrigation) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )
    if (.not. associated( this%calc_irrigation) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )

    if (.not. associated( this%init_runoff) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )
    if (.not. associated( this%calc_runoff) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )

    if (.not. associated( this%init_reference_et) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )
    if (.not. associated( this%calc_reference_et) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )

    if (.not. associated( this%init_snowmelt) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )
    if (.not. associated( this%calc_snowmelt) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )

    if (.not. associated( this%init_snowfall) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )
    if (.not. associated( this%calc_snowfall) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )

    if (.not. associated( this%init_GDD) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )
    if (.not. associated( this%calc_GDD) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )

    if (.not. associated( this%init_routing) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )
    if (.not. associated( this%calc_routing) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )

    if (.not. associated( this%init_direct_net_infiltration) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )
    if (.not. associated( this%calc_direct_net_infiltration) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )

    if (.not. associated( this%init_soil_storage_max) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )

    if (.not. associated( this%get_precipitation_data ) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )

    if (.not. associated( this%get_minimum_air_temperature_data ) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )

    if (.not. associated( this%get_maximum_air_temperature_data ) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __SRCNAME__, __LINE__ )

  end subroutine preflight_check_method_pointers

  !------------------------------------------------------------------------------------------------

  subroutine set_method_pointers_sub(this, sCmdText, argv_list )

    class (MODEL_DOMAIN_T), intent(inout)   :: this
    character (len=*), intent(in)           :: sCmdText
    type (STRING_LIST_T), intent(in)        :: argv_list

    ! [ LOCALS ]
    integer (kind=c_int)              :: indx
    integer (kind=c_int)              :: iostat
    integer (kind=c_int)              :: unitnum
    character (len=256)               :: filename
    character (len=:), allocatable    :: Method_Name
    integer (kind=c_int)              :: col, row
    integer (kind=c_int)              :: indx_start, indx_end
    real (kind=c_double)              :: xcoord, ycoord

    Method_Name = argv_list%get(1)

    if ( sCmdText .containssimilar. "INTERCEPTION" ) then

      if ( Method_Name .strapprox. "BUCKET" ) then

        this%init_interception => model_initialize_interception_bucket
        this%calc_interception => model_calculate_interception_bucket

        call LOGS%WRITE( "==> BUCKET INTERCEPTION submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( Method_Name .strapprox. "GASH" ) then

        this%init_interception => model_initialize_interception_gash
        this%calc_interception => model_calculate_interception_gash

        call LOGS%WRITE( "==> GASH INTERCEPTION submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported INTERCEPTION method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .containssimilar. "RUNOFF" ) then

      if ( ( Method_Name .strapprox. "C-N" ) .or. ( Method_Name .strapprox. "CURVE_NUMBER" ) ) then

        this%init_runoff => model_initialize_runoff_curve_number
        this%calc_runoff => model_calculate_runoff_curve_number

        call LOGS%WRITE( "==> CURVE NUMBER RUNOFF submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( Method_Name .strapprox. "RUNOFF_RATIO" ) .or. ( Method_Name .strapprox. "MONTHLY_GRID" ) ) then

        this%init_runoff => model_initialize_runoff_gridded_values
        this%calc_runoff => model_calculate_runoff_gridded_values

        call LOGS%WRITE( "==> RUNOFF RATIO submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported RUNOFF method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .containssimilar. "ROOTING" ) then

      if ( ( Method_Name .strapprox. "DYNAMIC" ) .or. ( Method_Name .strapprox. "FAO_56" )   &
          .or. ( Method_Name .strapprox. "FAO-56" ) )                                          then

        this%update_rooting_depth => model_update_rooting_depth_FAO56
        call LOGS%WRITE( "==> DYNAMIC rooting depth submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        this%update_rooting_depth => model_update_rooting_depth_none
        call LOGS%WRITE( "==> STATIC rooting depth submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      endif

    elseif ( sCmdText .containssimilar. "DYNAMIC_LANDUSE" ) then

        this%update_landuse_codes => model_update_landuse_codes_dynamic

    elseif ( sCmdText .containssimilar. "STATIC_LANDUSE" ) then

        this%update_landuse_codes => model_update_landuse_codes_static

    elseif ( sCmdText .containssimilar. "SNOWFALL" ) then

      if ( ( Method_Name .strapprox. "ORIGINAL" ) .or. ( Method_Name .strapprox. "ORIGINAL_SWB_METHOD" ) ) then

        this%init_snowfall => model_initialize_snowfall_original
        this%calc_snowfall => model_calculate_snowfall_original

        call LOGS%WRITE( "==> ORIGINAL SNOWFALL submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( Method_Name .strapprox. "PRMS" ) then

        this%init_snowfall => model_initialize_snowfall_prms
        this%calc_snowfall => model_calculate_snowfall_prms

        call LOGS%WRITE( "==> PRMS SNOWFALL submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported SNOWFALL method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( ( sCmdText .containssimilar. "AVAILABLE_WATER_CONTENT" )           &
        .or. ( sCmdText .containssimilar. "AVAILABLE_WATER_CAPACITY") ) then

      if ( ( Method_Name .strapprox. "TABLE" ) ) then

        this%init_awc => model_initialize_available_water_content_depth_integrated
        this%read_awc_data => model_read_available_water_content_depth_integrated

        call LOGS%WRITE( "==> TABLE method for populating AVAILABLE_WATER_CONTENT/AVAILABLE_WATER_CAPACITY selected.",     &
                         iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( Method_Name .strapprox. "GRID" ) .or. ( Method_Name .strapprox. "GRIDDED" ) ) then

        this%init_awc => model_initialize_available_water_content_gridded
        this%read_awc_data => model_read_available_water_content_gridded

        call LOGS%WRITE( "==> GRIDDED VALUES method for populating AVAILABLE_WATER_CONTENT/AVAILABLE_WATER_CAPACITY selected.",   &
                         iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported AVAILABLE_WATER_CONTENT/"    &
                  //"AVAILABLE_WATER_CAPACITY method.",                                               &
                  lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .containssimilar. "FLOW_ROUTING" ) then

      if ( ( Method_Name .strapprox. "D8" )                       &
          .or. ( Method_Name .strapprox. "DOWNHILL" ) ) then

        this%init_routing => model_initialize_routing_D8
        this%calc_routing => model_calculate_routing_D8

        call LOGS%WRITE( "==> D8 FLOW ROUTING submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        this%init_routing => model_initialize_routing_none
        this%calc_routing => model_calculate_routing_none

        call LOGS%WRITE( "==> NULL FLOW ROUTING submodel selected -- NO routing will be performed.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      endif

    elseif ( ( sCmdText .containssimilar. "CROP_COEFFICIENT" )    &
        .or. ( sCmdText .containssimilar. "CROP_COEF" ) )  then

      if ( ( Method_Name .strapprox. "FAO56" )                   &
             .or. ( Method_Name .strapprox. "FAO-56" )           &
             .or. ( Method_Name .strapprox. "FAO_56" ) ) then

        this%init_crop_coefficient => model_initialize_crop_coefficient_FAO56
        this%update_crop_coefficient => model_update_crop_coefficient_FAO56

        call LOGS%WRITE( "==> FAO-56 crop coefficient calculation method selected.", iLogLevel = LOG_ALL, &
           lEcho = lFALSE )

      else

        this%init_crop_coefficient => model_initialize_crop_coefficient_none
        this%update_crop_coefficient => model_update_crop_coefficient_none

        call LOGS%WRITE( "==> NO crop coefficient calculation method selected. Kcb defaults to 1.0.",     &
          iLogLevel = LOG_ALL, lEcho = lFALSE )

      endif

    elseif ( sCmdText .containssimilar. "FOG" ) then

      if ( Method_Name .strapprox. "MONTHLY_GRID" ) then

        this%init_fog => model_initialize_fog_monthly_grid
        this%calc_fog => model_calculate_fog_monthly_grid

        call LOGS%WRITE( "==> MONTHLY_GRID FOG submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        this%init_fog => model_initialize_fog_none
        this%calc_fog => model_calculate_fog_none

        call LOGS%WRITE( "==> NULL FOG submodel selected (i.e. no fog term).", iLogLevel = LOG_ALL, lEcho = lFALSE )

      endif

    elseif ( sCmdText .containssimilar. "GROWING_DEGREE_DAY" ) then

      if ( ( Method_Name .strapprox. "BASKERVILLE_EMIN" )       &
             .or. ( Method_Name .strapprox. "BE" )              &
             .or. ( Method_Name .strapprox. "SINUSOIDAL" )  )        then

        this%init_GDD => model_initialize_GDD_be
        this%calc_GDD => model_calculate_GDD_be
        call LOGS%WRITE( "==> Growing degree-day (GDD) will be calculated as described "  &
          //"in Baskerville and Emin (1969)", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        this%init_GDD => model_initialize_GDD
        this%calc_GDD => model_calculate_GDD
        call LOGS%WRITE( "==> Growing degree-day (GDD) will be calculated using "  &
          //"simple averaging of TMAX and TMIN.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      endif

    elseif ( sCmdText .containssimilar. "IRRIGATION" ) then

      if ( ( Method_Name .strapprox. "FAO56" )                   &
             .or. ( Method_Name .strapprox. "FAO-56" )           &
             .or. ( Method_Name .strapprox. "FAO_56" ) ) then

        this%init_irrigation => model_initialize_irrigation
        this%calc_irrigation => model_calculate_irrigation

        call LOGS%WRITE( "==> IRRIGATION will be calculated and applied as needed.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        this%init_irrigation => model_initialize_irrigation_none
        this%calc_irrigation => model_calculate_irrigation_none

        call LOGS%WRITE( "==> IRRIGATION will *NOT* be active.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      endif


    elseif ( ( sCmdText .containssimilar. "SOIL_STORAGE_MAX" )            &
        .or. ( sCmdText .containssimilar. "PLANT_AVAILABLE_WATER" ) ) then

      if ( Method_Name .strapprox. "GRIDDED" ) then

        this%init_soil_storage_max => model_initialize_soil_storage_max_gridded

        call LOGS%WRITE( "==> SOIL_STORAGE_MAX will be read from a grid. Rooting depths will be recalculated" &
          //" as ~SOIL_STORAGE_MAX / AWC.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        this%init_soil_storage_max => model_initialize_soil_storage_max_internally_calculated

        call LOGS%WRITE( "==> SOIL_STORAGE_MAX will be internally calculated from the given AWC and rooting depth values.",   &
          iLogLevel = LOG_ALL, lEcho = lFALSE )

      endif


    elseif ( sCmdText .containssimilar. "EVAPOTRANSPIRATION" ) then

      if (   ( Method_Name .strapprox. "HARGREAVES" )                 &
        .or. ( Method_Name .strapprox. "HARGREAVES-SAMANI" )          &
        .or. ( Method_Name .strapprox. "HARGREAVES_SAMANI" ) ) then

        this%init_reference_et => model_initialize_et_hargreaves
        this%calc_reference_et => model_calculate_et_hargreaves

        call LOGS%WRITE( "==> HARGREAVES-SAMANI EVAPOTRANSPIRATION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( Method_Name .strapprox. "JENSEN-HAISE" )             &
           .or. ( Method_Name .strapprox. "JENSEN_HAISE" )            &
           .or. ( Method_Name .strapprox. "JH" ) ) then

        this%init_reference_et => model_initialize_et_jensen_haise
        this%calc_reference_et => model_calculate_et_jensen_haise

        call LOGS%WRITE( "==> JENSEN_HAISE EVAPOTRANSPIRATION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( Method_Name .strapprox. "MONTHLY_GRID" ) &
           .or. ( Method_Name .strapprox. "MONTHLY_GRID" ) ) then

        this%init_reference_et => model_initialize_et_monthly_grid
        this%calc_reference_et => model_calculate_et_monthly_grid

        call LOGS%WRITE( "==> MONTHLY_GRID EVAPOTRANSPIRATION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( Method_Name .strapprox. "DAILY_GRID" ) &
           .or. ( Method_Name .strapprox. "DAILY_GRID" ) ) then

        this%init_reference_et => model_initialize_et_daily_grid
        this%calc_reference_et => model_calculate_et_daily_grid

        call LOGS%WRITE( "==> DAILY_GRID EVAPOTRANSPIRATION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported EVAPOTRANSPIRATION method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .containssimilar. "PRECIPITATION" ) then

      if ( ( Method_Name .strapprox. "NORMAL" )                &
           .or. ( Method_Name .strapprox. "GRIDDED" )            &
           .or. ( Method_Name .strapprox. "STANDARD" ) ) then

        this%init_precipitation_data => model_initialize_precip_normal
        this%get_precipitation_data => model_get_precip_normal

        call LOGS%WRITE( "==> STANDARD PRECIPITATION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( Method_Name .strapprox. "METHOD_OF_FRAGMENTS" ) &
           .or. ( Method_Name .strapprox. "FRAGMENTS" ) ) then

        this%init_precipitation_data => model_initialize_precip_method_of_fragments
        this%get_precipitation_data => model_get_precip_method_of_fragments

        call LOGS%WRITE( "==> METHOD OF FRAGMENTS PRECIPITATION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported PRECIPITATION method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( (sCmdText .containssimilar. "DIRECT_NET_INFILTRATION" )       &
           .or. ( sCmdText .containssimilar. "DIRECT_NET_INFILTRATION" ) )        then

      this%init_direct_net_infiltration => model_initialize_direct_net_infiltration_gridded
      this%calc_direct_net_infiltration => model_calculate_direct_net_infiltration_gridded

      call LOGS%WRITE( "==> GRIDDED or TABULAR values for water main leakage and other direct " &
        //"recharge will be used.", iLogLevel = LOG_ALL, lEcho = lFALSE )

    elseif ( sCmdText .containssimilar. "DIRECT_SOIL_MOISTURE" ) then

      this%init_direct_soil_moisture => model_initialize_direct_soil_moisture_gridded
      this%calc_direct_soil_moisture => model_calculate_direct_soil_moisture_gridded

      call LOGS%WRITE( "==> GRIDDED or TABULAR values for septic drainage and other direct " &
        //"inputs to the root zone will be used.", iLogLevel = LOG_ALL, lEcho = lFALSE )

    elseif ( sCmdText .containssimilar. "SOIL_MOISTURE" ) then

      if ( ( Method_Name .strapprox. "T-M" ) .or. ( Method_Name .strapprox. "THORNTHWAITE-MATHER" )      &
             .or. ( Method_Name .strapprox. "THORNTHWAITE") ) then

        this%init_actual_et => model_initialize_actual_et_thornthwaite_mather
        this%calc_actual_et => model_calculate_actual_et_thornthwaite_mather

        call LOGS%WRITE( "==> THORNTHWAITE-MATHER SOIL MOISTURE RETENTION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( Method_Name .strapprox. "T-M_EQUATIONS" )                           &
             .or. ( Method_Name .strapprox. "THORNTHWAITE-MATHER_EQUATIONS" )        &
             .or. ( Method_Name .strapprox. "THORNTHWAITE_MATHER_EQUATIONS") ) then

        this%init_actual_et => model_initialize_actual_et_thornthwaite_mather_eqns
        this%calc_actual_et => model_calculate_actual_et_thornthwaite_mather_eqns

        call LOGS%WRITE( "==> THORNTHWAITE-MATHER SOIL MOISTURE RETENTION (SWB 1.0 equations) submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( Method_Name .strapprox. "FAO56_TWO_STAGE" ) .or. ( Method_Name .strapprox. "FAO-56_TWO_STAGE" ) ) then

        this%init_actual_et => model_initialize_actual_et_fao56__two_stage
        this%calc_actual_et => model_calculate_actual_et_fao56__two_stage

        call LOGS%WRITE( "==> **TWO-STAGE** FAO-56 SOIL MOISTURE RETENTION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( Method_Name .strapprox. "FAO56" ) .or. ( Method_Name .strapprox. "FAO-56" ) ) then

        this%init_actual_et => model_initialize_actual_et_fao56
        this%calc_actual_et => model_calculate_actual_et_fao56

        call LOGS%WRITE( "==> FAO-56 SOIL MOISTURE RETENTION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported SOIL_MOISTURE method.", &
          lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .containssimilar. "DUMP_VARIABLES" ) then

        row = 0; col = 0; indx_start = 0; indx_end = 0

        this%dump_variables => model_dump_variables_by_cell

        if ( ( Method_Name .containssimilar. "INDEX_RANGE") .and. ( argv_list%count == 3 ) ) then

          indx_start = asInt( argv_list%get(2) )
          indx_end   = asInt( argv_list%get(3) )

        elseif ( ( Method_Name .containssimilar. "COORD") .and. ( argv_list%count == 3 ) ) then

          xcoord = asFloat( argv_list%get(2) )
          ycoord = asFloat( argv_list%get(3) )
          row = grid_GetGridRowNum( this%pGrdOut, ycoord )
          col = grid_GetGridColNum( this%pGrdOut, xcoord )

        elseif ( argv_list%count == 2 ) then

          col = asInt( argv_list%get(1) )
          row = asInt( argv_list%get(2) )

        else

          call warn("Unknown option and/or arguments supplied to DUMP_VARIABLES method.",          &
            sHints="The only known option keywords are 'COORD' and 'INDEX_RANGE'.",                &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

        endif

        if ( ( ( col > 0 ) .and. ( row > 0 ) )                                                  &
            .or. ( ( indx_start > 0 ) .and. ( indx_end > 0 ) ) ) then

          do indx=1, ubound( DUMP, 1)
            if (DUMP( indx )%col /= 0 .or. DUMP( indx )%indx_start /= 0 )  cycle

            DUMP( indx )%col = col
            DUMP( indx )%row = row
            DUMP( indx )%indx_start = indx_start
            DUMP( indx )%indx_end   = indx_end

            if ( ( col > 0 ) .and. ( row > 0 ) ) then
              call LOGS%WRITE( "==> SWB will dump variables for cell ("//asCharacter(col)//","     &
                //asCharacter(row)//").", iLogLevel = LOG_ALL, lEcho = lFALSE )
                filename = "SWB2_variable_values__col_"//asCharacter( col )//"__row_"              &
                           //asCharacter( row )//".csv"

            else
              call LOGS%WRITE( "==> SWB will dump variables for cell indices ranging from "        &
                //asCharacter(indx_start)//" to "//asCharacter(indx_end)//").",                    &
                iLogLevel = LOG_ALL, lEcho = lFALSE )
                filename = "SWB2_variable_values__start_index_"//asCharacter( indx_start )         &
                           //"__end_index_"//asCharacter( indx_end )//".csv"
            endif

            open( newunit=unitnum, file=trim(filename), iostat=iostat, action="write", status="replace" )

            call assert( iostat == 0, "Could not open variable dump file "//sQuote(filename)     &
              //" for writing. iostat = "//asCharacter( iostat ) )
            DUMP( indx )%unitnum = unitnum

            write( unit=DUMP( indx )%unitnum, fmt="(a)")                                                           &
              "month, day, year,landuse_code, landuse_index, soil_group, num_upslope_connections, "                &
              //"sum_upslope_cells, solution_order, cell_index, target_index, awc, latitude, reference_ET0, "      &
              //"actual_ET, curve_num_adj, inflow, runon, "                                                        &
              //"runoff, outflow, infiltration, snowfall, potential_snowmelt, snowmelt, interception, "            &
              //"rainfall, interception_storage, tmax, tmin, tmean, snow_storage, "                                &
              //"soil_storage, soil_storage_max, delta_soil_storage, surface_storage, "                            &
              //"surface_storage_excess, surface_storage_max, net_infiltration, "                                  &
              //"rejected_net_infiltration, fog, irrigation, gdd, runoff_outside, "                                &
              //"pervious_fraction, storm_drain_capture, canopy_cover_fraction, crop_coefficient_kcb, "            &
              //"cfgi, rooting_depth_max, current_rooting_depth, actual_et_soil, "                                 &
              //"readily_available_water, total_available_water, plant_stress_coef_ks, "                           &
              //"evap_reduction_coef_kr, surf_evap_coef_ke, fraction_exposed_and_wetted_soil, "                    &
              //"actual_et_impervious, actual_et_interception, adjusted_depletion_fraction_p, crop_etc, "          &
              //" bare_soil_evap, direct_net_infiltration, "                                                       &
              //"direct_soil_moisture, inflowbuf1, inflowbuf2, inflowbuf3, inflowbuf4, inflowbuf5, inflowbuf_sum"
            exit

          enddo

        endif

    else

      call warn("Your control file references an unknown or unsupported method: "//dquote(sCmdText), &
          lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

    endif

  end subroutine set_method_pointers_sub

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_interception_bucket(this)

    use interception__bucket

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call interception_bucket_initialize( this%active )

  end subroutine model_initialize_interception_bucket

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_interception_bucket(this)

    use interception__bucket, only  : interception_bucket_calculate

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    this%interception = interception_bucket_calculate( this%landuse_index, this%gross_precip, this%fog,   &
                                                         this%canopy_cover_fraction, this%it_is_growing_season )

  end subroutine model_calculate_interception_bucket

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_interception_gash(this)

    use interception__gash

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call interception_gash_initialize( this%active, this%canopy_cover_fraction, this%landuse_index )

  end subroutine model_initialize_interception_gash

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_interception_gash(this)

    use interception__gash

    class (MODEL_DOMAIN_T), intent(inout)      :: this

    ! [ LOCALS ]
    real (kind=c_float), allocatable :: fTrunk_Storage_Capacity(:)
    real (kind=c_float), allocatable :: fStemflow_Fraction(:)

    fTrunk_Storage_Capacity = TRUNK_STORAGE_CAPACITY_TABLE_VALUES( this%landuse_index )
    fStemflow_Fraction = STEMFLOW_FRACTION_TABLE_VALUES( this%landuse_index )

    call interception_gash_calculate( this%rainfall,                                      &
                               this%fog,                                                  &
                               this%canopy_cover_fraction,                                &
                               !TRUNK_STORAGE_CAPACITY_TABLE_VALUES( this%landuse_index ), &
                               fTrunk_Storage_Capacity, &
                               !STEMFLOW_FRACTION_TABLE_VALUES( this%landuse_index ),      &
                               fStemflow_Fraction, &
                               EVAPORATION_TO_RAINFALL_RATIO,                             &
                               P_SAT,                                                     &
                               this%interception )

  end subroutine model_calculate_interception_gash

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_routing_none(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    this%runon = 0.0_c_float

  end subroutine model_initialize_routing_none

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_routing_none(this, indx)

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    integer (kind=c_int), intent(in)       :: indx

    this%runoff_outside( indx ) =                                        &
      this%runoff( indx )                                                &
      + this%rejected_net_infiltration( indx )

  end subroutine model_calculate_routing_none

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_routing_D8(this)

    use routing__D8

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call routing_D8_initialize( this%active, this%sort_order )
    this%num_upslope_connections = pack( NUMBER_OF_UPSLOPE_CONNECTIONS, this%active )
    this%sum_upslope_cells = pack( SUM_OF_UPSLOPE_CELLS, this%active )

  end subroutine model_initialize_routing_D8

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_routing_D8( this, indx )

    use routing__D8, only   : get_target_index, get_cell_index

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    integer (kind=c_int), intent(in)       :: indx

    ! [ LOCALS ]
    integer (kind=c_int) :: target_index
    integer (kind=c_int) :: cell_index
    real (kind=c_float)  :: msb

    integer (kind=c_int) :: cell_row, cell_col, targ_row, targ_col

    cell_index    = get_cell_index( indx )
    target_index  = get_target_index( indx )

    cell_row=this%row_num_1D(cell_index)
    cell_col=this%col_num_1D(cell_index)
    targ_row=-9999
    targ_col=-9999

    ! if the target cell is within valid bounds, move the water downslope
    if ( (    target_index >= lbound( this%runon, 1) )                            &
      .and. ( target_index <= ubound( this%runon, 1) ) ) then

        targ_row=this%row_num_1D(target_index)
        targ_col=this%col_num_1D(target_index)

        this%runon( target_index ) =                                              &
              this%runon( target_index )                                          &
            + this%runoff( cell_index )                                           &
            + this%rejected_net_infiltration( cell_index )

        ! msb = this%rainfall( cell_index ) + this%snowmelt( cell_index ) + this%runon( cell_index )   &
        !       - this%runoff( cell_index ) + this%delta_soil_storage( cell_index )     &
        !       - this%net_infiltration( cell_index ) - this%actual_et( cell_index )   &
        !       - this%rejected_net_infiltration( cell_index )
        !
        ! print *, "moving water from "//asCharacter(cell_index)//" to "//asCharacter(target_index)//"."
        ! print *, "                      ("//asCharacter( cell_col )//","//asCharacter( cell_row )//")"  &
        !        //" to ("//asCharacter( targ_col )//","//asCharacter( targ_row )//")"
        ! print *, "   cell runon              = ", this%runon( cell_index )
        ! print *, "   cell rainfall           = ", this%rainfall( cell_index )
        ! print *, "   cell snowmelt           = ", this%snowmelt( cell_index )
        ! print *, "   cell fog                = ", this%fog( cell_index )
        ! print *, "   cell irrigation         = ", this%irrigation( cell_index )
        ! print *, "   surface storage excess  = ", this%surface_storage_excess( cell_index )
        ! print *, "   direct_soil_moisture    = ", this%direct_soil_moisture( cell_index )
        ! print *, "   cell infiltration       =", this%infiltration( cell_index )
        ! print *, "   cell runoff             = ", this%runoff( cell_index )
        ! print *, "   cell delta_soil_storage = ", this%delta_soil_storage( cell_index )
        ! print *, "   cell act_et             = ", this%actual_et( cell_index )
        ! print *, "   cell net_infiltration           =", this%net_infiltration( cell_index )
        ! print *, "   cell rejected_net_infiltration  =", this%rejected_net_infiltration( cell_index )
        ! print *, "   target runon            = ", this%runon( target_index )
        ! print *, "   cell msb                = ", msb

    else

      ! move the water out of grid
      this%runoff_outside( cell_index ) =                                         &
        this%runoff_outside( cell_index )                                         &
        + this%runoff( cell_index )                                               &
        + this%rejected_net_infiltration( cell_index )

    endif

  end subroutine model_calculate_routing_D8

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_continuous_frozen_ground_index(this)

    use continuous_frozen_ground_index, only   : initialize_continuous_frozen_ground_index

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call initialize_continuous_frozen_ground_index( this%continuous_frozen_ground_index, this%active )

  end subroutine model_initialize_continuous_frozen_ground_index

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_continuous_frozen_ground_index(this)

    use continuous_frozen_ground_index, only   : update_continuous_frozen_ground_index

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call update_continuous_frozen_ground_index( this%continuous_frozen_ground_index, this%tmin,    &
                                                this%tmax, this%snow_storage )

  end subroutine model_calculate_continuous_frozen_ground_index

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_snowfall_original(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_snowfall_original

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_snowfall_original(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call snowfall_original_calculate( this%snowfall, this%rainfall, this%tmin, this%tmax, this%gross_precip )

  end subroutine model_calculate_snowfall_original

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_snowfall_prms(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_snowfall_prms

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_snowfall_prms(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_calculate_snowfall_prms

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_snowmelt_original(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_snowmelt_original

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_snowmelt_original(this)

    use snowmelt__original

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call snowmelt_original_calculate( potential_snowmelt=this%potential_snowmelt, tmin=this%tmin,   &
                                      tmax=this%tmax, imperial_units=lTRUE )

  end subroutine model_calculate_snowmelt_original

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_snowmelt_prms(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_snowmelt_prms

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_snowmelt_prms(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_calculate_snowmelt_prms

  !--------------------------------------------------------------------------------------------------

  subroutine model_initialize_et_hargreaves(this)

    use et__hargreaves_samani

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_et_hargreaves

  !--------------------------------------------------------------------------------------------------

  subroutine model_calculate_et_hargreaves(this)

    use et__hargreaves_samani

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    this%reference_ET0 = et_hargreaves_calculate( iDayOfYear=SIM_DT%iDOY, iNumDaysInYear=SIM_DT%iDaysInYear,    &
         fLatitude=this%latitude, fTMin=this%Tmin, fTMax=this%Tmax )

  end subroutine model_calculate_et_hargreaves

  !--------------------------------------------------------------------------------------------------

  subroutine model_initialize_et_daily_grid(this)

    use et__gridded_values

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call et_gridded_values_initialize( this%active )

  end subroutine model_initialize_et_daily_grid

  !--------------------------------------------------------------------------------------------------

  subroutine model_calculate_et_daily_grid(this)

    use et__gridded_values

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call et_gridded_values_calculate( )

    this%reference_ET0 = pack( pET_GRID%pGrdBase%rData, this%active )

  end subroutine model_calculate_et_daily_grid

  !--------------------------------------------------------------------------------------------------

  subroutine model_initialize_et_monthly_grid(this)

    use et__gridded_values

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call et_gridded_values_initialize( this%active )

  end subroutine model_initialize_et_monthly_grid

  !--------------------------------------------------------------------------------------------------

  subroutine model_calculate_et_monthly_grid(this)

    use et__gridded_values

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call et_gridded_values_calculate( )

    this%reference_ET0 = pack( pET_GRID%pGrdBase%rData, this%active ) &
                                      / real( SIM_DT%iDaysInMonth, kind=c_float)

  end subroutine model_calculate_et_monthly_grid

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_et_jensen_haise(this)

    use et__jensen_haise

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_et_jensen_haise

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_et_jensen_haise(this)

    use et__jensen_haise

    class (MODEL_DOMAIN_T), intent(inout)  :: this

     this%reference_ET0 = et_jh_calculate( iDayOfYear=SIM_DT%iDOY, iNumDaysInYear=SIM_DT%iDaysInYear, &
       fLatitude=this%latitude, fTMin=this%Tmin, fTMax=this%Tmax )

  end subroutine model_calculate_et_jensen_haise

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_runoff_curve_number(this)

    use runoff__curve_number

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call runoff_curve_number_initialize( this%active )

  end subroutine model_initialize_runoff_curve_number

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_runoff_curve_number(this, cell_index )

    use ieee_arithmetic,      only  : ieee_is_nan, ieee_is_finite
    use runoff__curve_number, only  : runoff_curve_number_calculate,     &
                                      update_previous_5_day_rainfall

    class (MODEL_DOMAIN_T), intent(inout)          :: this
    integer (kind=c_int), intent(in)               :: cell_index

    integer (kind=c_int)  :: indx

    !> @TODO: Should interception term be part of this? Initial abstraction should include
    !!        some of this interception...

!    call update_previous_5_day_rainfall( this%inflow( cell_index ), cell_index )

    call runoff_curve_number_calculate(runoff=this%runoff( cell_index ),                               &
                                       curve_num_adj=this%curve_num_adj( cell_index ),                 &
                                       landuse_index=this%landuse_index( cell_index ),                 &
                                       cell_index=cell_index,                                          &
                                       soil_group=this%soil_group( cell_index ),                       &
                                       soil_storage_max=this%soil_storage_max( cell_index ),           &
                                       it_is_growing_season=this%it_is_growing_season( cell_index ),   &
                                       inflow=this%inflow( cell_index ),                               &
                                       continuous_frozen_ground_index=                           &
                                           this%continuous_frozen_ground_index( cell_index ) )

    call update_previous_5_day_rainfall( this%inflow( cell_index ), cell_index )

    ! if ( ieee_is_nan( this%runoff( cell_index ) )                               &
    !   .or. ( .not. ieee_is_finite( this%runoff( cell_index ) ) ) ) then
    !
    !   print *, "*** NaN or infinite runoff value detected ***"
    !   print *, "            LU: ", this%landuse_code( cell_index )
    !   print *, "          soil: ", this%soil_group( cell_index )
    !   print *, "           col: ", this%col_num_1D( cell_index )
    !   print *, "           row: ", this%row_num_1D( cell_index )
    !   print *, "         runon: ", this%runon( cell_index )
    !   print *, "        inflow: ", this%inflow( cell_index )
    !   print *, " soil_stor_max: ", this%soil_storage_max( cell_index )
    !   print *, "     soil_stor: ", this%soil_storage( cell_index )
    !   print *, " curve_num_adj: ", this%curve_num_adj( cell_index )
    !
    ! endif

  end subroutine model_calculate_runoff_curve_number

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_runoff_gridded_values(this)

    use runoff__gridded_values

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: status

    allocate( this%monthly_runoff( count( this%active ) ), stat=status)
    call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    call runoff_gridded_values_initialize( this%active )
    call runoff_gridded_values_update_ratios( )

  end subroutine model_initialize_runoff_gridded_values

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_runoff_gridded_values(this, indx )

    use runoff__gridded_values
    use datetime, only           : DATETIME_T

    class (MODEL_DOMAIN_T), intent(inout)       :: this
    integer (kind=c_int), intent(in)            :: indx

    ! [ LOCALS ]
    type (DATETIME_T), save     :: date_of_last_grid_update
    real (kind=c_float)         :: interim_inflow

    if ( .not. ( date_of_last_grid_update == SIM_DT%curr ) ) then
      call runoff_gridded_values_update_ratios( )
      date_of_last_grid_update = SIM_DT%curr
    endif

    interim_inflow = this%rainfall( indx ) + this%snowmelt( indx )

    this%runoff( indx ) = interim_inflow * RUNOFF_RATIOS( indx )

    if ( allocated( this%monthly_gross_precip ) )                                                &
      this%monthly_runoff( indx ) = this%monthly_gross_precip( indx ) * RUNOFF_RATIOS( indx )

  end subroutine model_calculate_runoff_gridded_values







  subroutine model_update_rooting_depth_table_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)       :: this

    ! [ LOCALS ]
    integer (kind=c_int)              :: iNumActiveCells
    integer (kind=c_int)              :: iStat
    integer (kind=c_int)              :: iNumberOfLanduses
    integer (kind=c_int)              :: iNumberOfSoilGroups
    integer (kind=c_int)              :: iSoilsIndex
    integer (kind=c_int)              :: iLUIndex
    integer (kind=c_int), allocatable :: iLanduseCodes(:)
    type (STRING_LIST_T)              :: slList
    type (STRING_LIST_T)              :: slRZ
    integer (kind=c_int), allocatable :: iRZ_SeqNums(:)
    real (kind=c_float), allocatable  :: RZ(:)
    character (len=:), allocatable    :: sText
    real (kind=c_float), allocatable  :: water_capacity(:)
    integer (kind=c_int)              :: iIndex
    type (GENERAL_GRID_T), pointer    :: pRooting_Depth
    real (kind=c_float), allocatable  :: fMax_Rooting_Depth(:,:)
    character (len=10)                :: date_str

    type (DATA_CATALOG_ENTRY_T), pointer :: pHSG
    type (DATA_CATALOG_ENTRY_T), pointer :: pLULC

    pLULC => DAT%find("LAND_USE")
    pHSG => DAT%find("HYDROLOGIC_SOILS_GROUP")

    date_str = SIM_DT%curr%prettydate()

    call assert( associated( pLULC), "Possible INTERNAL PROGRAMMING ERROR -- Null pointer detected for pLULC", &
      __SRCNAME__, __LINE__ )

    call assert( associated( pLULC%pGrdBase ),   &
      "Possible INTERNAL PROGRAMMING ERROR -- Null pointer detected for pLULC%pGrdBase", __SRCNAME__, __LINE__ )

    call assert( allocated( pLULC%pGrdBase%iData ),   &
      "Possible INTERNAL PROGRAMMING ERROR -- Unallocated array detected for pLULC%pGrdBase%iData", __SRCNAME__, __LINE__ )

    call assert( associated( pHSG), "Possible INTERNAL PROGRAMMING ERROR -- Null pointer detected for pHSG", &
      __SRCNAME__, __LINE__ )

    call assert( associated( pHSG%pGrdBase ),      &
      "Possible INTERNAL PROGRAMMING ERROR -- Null pointer detected for pHSG%pGrdBase", __SRCNAME__, __LINE__ )

    call assert( allocated( pHSG%pGrdBase%iData ),      &
      "Possible INTERNAL PROGRAMMING ERROR -- Unallocated array detected for pHSG%pGrdBase%iData", __SRCNAME__, __LINE__ )


    pRooting_Depth => grid_Create( iNX=this%number_of_columns, iNY=this%number_of_rows, &
        rX0=this%X_ll, rY0=this%Y_ll, &
        rGridCellSize=this%gridcellsize, iDataType=GRID_DATATYPE_REAL )

    iNumActiveCells = ubound(this%soil_storage_max,1)

    call slList%append("LU_Code")
    call slList%append("Landuse_Code")
    call slList%append("Landuse_Lookup_Code")

    !> Determine how many soil groups are present

    ! retrieve a string list of all keys associated with root zone depth (i.e. RZ_1, RZ_2, RZ_3, etc.)
    slRZ = PARAMS%grep_name("RZ", lFatal=TRUE )
    ! Convert the string list to an vector of integers; MODEL call strips off the "RZ_" part of label
    iRZ_SeqNums = slRZ%asInt()
    ! count how many items are present in the vector; MODEL should equal the number of soils groups
    iNumberOfSoilGroups = count( iRZ_SeqNums > 0 )

    !> Determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=slList, iValues=iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes >= 0 )

    allocate( fMax_Rooting_Depth(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for maximum rooting depth table", &
      __SRCNAME__, __LINE__)

    ! we should have the max rooting depth table fully filled out following MODEL block
    do iSoilsIndex = 1, iNumberOfSoilGroups
      sText = "RZ_"//asCharacter(iSoilsIndex)
      call PARAMS%get_parameters( sKey=sText, fValues=RZ )
      fMax_Rooting_Depth(:, iSoilsIndex) = RZ
    enddo

    call LOGS%WRITE( "Landuse Code |  Soils Code  | Number of Matches | Rooting Depth (ft)",   &
      iLogLevel = LOG_DEBUG, lEcho = lFALSE )
    call LOGS%WRITE( "-------------|--------------|-------------------|------------------ ",   &
      iLogLevel = LOG_DEBUG, lEcho = lFALSE )

    do iSoilsIndex = 1, iNumberOfSoilGroups
      do iLUIndex = 1, iNumberOfLanduses

        call LOGS%WRITE( asCharacter(iLanduseCodes( iLUIndex) )//" | "//asCharacter(iSoilsIndex)//" | "//    &
            asCharacter(count( pLULC%pGrdBase%iData == iLanduseCodes( iLUIndex)               &
                                 .and. pHSG%pGrdBase%iData == iSoilsIndex ) )//" | "          &
                                 //asCharacter( fMax_Rooting_Depth( iLUIndex, iSoilsIndex) ), &
                                 iLogLevel = LOG_DEBUG, lEcho = lFALSE )


         where ( pLULC%pGrdBase%iData == iLanduseCodes( iLUIndex) .and. pHSG%pGrdBase%iData == iSoilsIndex )

           pRooting_Depth%rData = fMax_Rooting_Depth( iLUIndex, iSoilsIndex )

         endwhere

      enddo

    enddo

    call slList%clear()

    date_str = SIM_DT%curr%prettydate()

    call grid_WriteArcGrid("Maximum_rooting_depth__as_assembled_from_table__"   &
      //trim(date_str)//".asc", pRooting_Depth )

    ROOTING_DEPTH_MAX = pRooting_Depth%rData

    call grid_Destroy( pRooting_Depth )

  end subroutine model_update_rooting_depth_table_sub

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_soil_storage_max_internally_calculated(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    type ( GENERAL_GRID_T ), pointer :: pTempGrd
    character (len=10)               :: date_str

    !> @todo this should be in its own routine...
    this%current_rooting_depth = pack( ROOTING_DEPTH_MAX, MODEL%active )
    this%rooting_depth_max = pack( ROOTING_DEPTH_MAX, MODEL%active )

    this%soil_storage_max = this%rooting_depth_max * this%awc

    pTempGrd => grid_Create( iNX=this%number_of_columns, iNY=this%number_of_rows, &
        rX0=this%X_ll, rY0=this%Y_ll, &
        rGridCellSize=this%gridcellsize, iDataType=GRID_DATATYPE_REAL )

    pTempGrd%rData = unpack( this%soil_storage_max, this%active, this%nodata_fill_value )

    date_str = SIM_DT%curr%prettydate()

    call grid_WriteArcGrid( sFilename="Soil_Storage_Maximum__as_calculated_inches__" &
      //trim(date_str)//".asc", pGrd=pTempGrd )

    call grid_Destroy( pTempGrd )

  end subroutine model_initialize_soil_storage_max_internally_calculated

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_soil_storage_max_gridded(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    type ( GENERAL_GRID_T ), pointer :: pTempGrd
    integer (kind=c_int)             :: iStat
    type (DATA_CATALOG_ENTRY_T), pointer :: pSOIL_STORAGE_MAX_GRID

    pSOIL_STORAGE_MAX_GRID => null()

    pTempGrd => grid_Create( iNX=this%number_of_columns, iNY=this%number_of_rows, &
    rX0=this%X_ll, rY0=this%Y_ll, &
    rGridCellSize=this%gridcellsize, iDataType=GRID_DATATYPE_REAL )

    ! locate the data structure associated with the gridded rainfall zone entries
    pSOIL_STORAGE_MAX_GRID => DAT%find("SOIL_STORAGE_MAX")
    if ( .not. associated(pSOIL_STORAGE_MAX_GRID) ) &
      pSOIL_STORAGE_MAX_GRID => DAT%find("PLANT_AVAILABLE_WATER")

    if ( .not. associated(pSOIL_STORAGE_MAX_GRID) ) &
        call die("A SOIL_STORAGE_MAX or PLANT_AVAILABLE_WATER grid must be supplied in order"    &
        //" to make use of this option.", __SRCNAME__, __LINE__ )

    call pSOIL_STORAGE_MAX_GRID%getvalues( )

    this%soil_storage_max = pack( pSOIL_STORAGE_MAX_GRID%pGrdBase%rData, this%active )

    pTempGrd%rData = unpack( this%soil_storage_max, this%active, this%nodata_fill_value )

    ! back-calculate awc to make it consistent with rooting_depth_max and given
    ! soil_storage_max gridded values

    where ( .not. ( this%rooting_depth_max .approxequal. 0.0_c_float ) )

      this%awc = this%soil_storage_max / this%rooting_depth_max

    else where

      this%awc = 0.0_c_float

    end where

    this%current_rooting_depth = this%rooting_depth_max

    call grid_WriteArcGrid( sFilename="Maximum_Soil_Storage__as_read_in_inches.asc", pGrd=pTempGrd )

    pTempGrd%rData = unpack( this%rooting_depth_max, this%active, this%nodata_fill_value )

    call grid_WriteArcGrid( sFilename="Available_water_content__as_RECALCULATED_in_inches_per_foot.asc", pGrd=pTempGrd )

    call grid_Destroy( pTempGrd )

  end subroutine model_initialize_soil_storage_max_gridded

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_irrigation_none( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    this%irrigation = 0.0_c_float

  end subroutine model_initialize_irrigation_none

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_irrigation_none( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    !> Nothing here to see.

  end subroutine model_calculate_irrigation_none

!--------------------------------------------------------------------------------------------------

  subroutine model_output_irrigation_none( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    !> Nothing here to see.

  end subroutine model_output_irrigation_none

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_irrigation( this )

    use irrigation

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: status

    allocate( this%irrigation_mask( count( this%active ) ), stat=status )
    call assert( status==0, "Problem allocating memory.", &
      __SRCNAME__, __LINE__ )

    call irrigation__initialize( this%irrigation_mask, this%active )

  end subroutine model_initialize_irrigation

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_irrigation( this )

  use irrigation

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int)               :: index
    real (kind=c_float), allocatable   :: deficit(:)

    if ( allocated( this%monthly_gross_precip ) .and. allocated( this%monthly_runoff ) ) then

      call irrigation__calculate( irrigation_amount=this%irrigation,                             &
                                  landuse_index=this%landuse_index,                              &
                                  soil_storage=this%soil_storage,                                &
                                  soil_storage_max=this%soil_storage_max,                        &
                                  total_available_water=this%total_available_water_taw,          &
                                  rainfall=this%rainfall,                                        &
                                  runoff=this%runoff,                                            &
                                  crop_etc=this%crop_etc,                                        &
                                  irrigation_mask=this%irrigation_mask,                          &
                                  num_days_since_planting=this%number_of_days_since_planting,    &
                                  monthly_rainfall=this%monthly_gross_precip,                    &
                                  monthly_runoff=this%monthly_runoff )

!      call minmaxmean( this%monthly_runoff, "MONTHLY_RUNOFF")
!      call minmaxmean( this%monthly_gross_precip, "MONTHLY_RAINFALL")

    else


      call irrigation__calculate( irrigation_amount=this%irrigation,                             &
                                  landuse_index=this%landuse_index,                              &
                                  soil_storage=this%soil_storage,                                &
                                  soil_storage_max=this%soil_storage_max,                        &
                                  total_available_water=this%total_available_water_taw,          &
                                  rainfall=this%rainfall,                                        &
                                  runoff=this%runoff,                                            &
                                  crop_etc=this%crop_etc,                                        &
                                  irrigation_mask=this%irrigation_mask,                          &
                                  num_days_since_planting=this%number_of_days_since_planting )

    endif

  end subroutine model_calculate_irrigation

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_growing_season( this )

    use growing_season

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call growing_season_initialize( )

  end subroutine model_initialize_growing_season

!--------------------------------------------------------------------------------------------------

  subroutine model_update_growing_season( this )

    use growing_season

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call growing_season_update( landuse_index=this%landuse_index,                                &
                                              GDD=this%gdd,                                      &
                                              mean_air_temp=this%tmean,                          &
                                              it_is_growing_season=this%it_is_growing_season )

  end subroutine model_update_growing_season

!--------------------------------------------------------------------------------------------------

  subroutine model_read_available_water_content_gridded( this )

    use awc__gridded_values

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call awc_gridded_values_read( )

  end subroutine model_read_available_water_content_gridded

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_available_water_content_gridded( this )

    use awc__gridded_values

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    type (GENERAL_GRID_T), pointer  :: pTempGrd

    pTempGrd => grid_Create( iNX=this%number_of_columns, iNY=this%number_of_rows, &
      rX0=this%X_ll, rY0=this%Y_ll, &
      rGridCellSize=this%gridcellsize, iDataType=GRID_DATATYPE_REAL )


    call awc_gridded_values_initialize( lActive=this%active,  &
                                        fAWC=this%awc )

    pTempGrd%rData = unpack( this%awc, this%active, this%nodata_fill_value )

    call grid_WriteArcGrid( sFilename="Available_water_content__as_read_in_inches_per_foot.asc", pGrd=pTempGrd )

    call grid_Destroy( pTempGrd )


  end subroutine model_initialize_available_water_content_gridded

!--------------------------------------------------------------------------------------------------

  subroutine model_read_available_water_content_depth_integrated( this )

    use awc__depth_integrated

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call awc_depth_integrated_read( fRooting_Depth=ROOTING_DEPTH_MAX )

  end subroutine model_read_available_water_content_depth_integrated

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_available_water_content_depth_integrated( this )

    use awc__depth_integrated

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int)            :: iStat
    type (GENERAL_GRID_T), pointer  :: pTempGrd

    pTempGrd => grid_Create( iNX=this%number_of_columns, iNY=this%number_of_rows, &
      rX0=this%X_ll, rY0=this%Y_ll, &
      rGridCellSize=this%gridcellsize, iDataType=GRID_DATATYPE_REAL )


    allocate ( this%soil_code (count( this%active ) ), stat=iStat )

    call awc_depth_integrated_initialize( lActive=this%active,                        &
                                      fAWC=this%awc,                              &
                                      iSoils_Code=this%soil_code )

    pTempGrd%rData = unpack( this%awc, this%active, this%nodata_fill_value )

    call grid_WriteArcGrid( sFilename="Available_water_content__as_calculated_inches_per_foot.asc", pGrd=pTempGrd )

    call grid_Destroy( pTempGrd )

  end subroutine model_initialize_available_water_content_depth_integrated

  !--------------------------------------------------------------------------------------------------

    subroutine model_update_landuse_codes_static ( this )

      class (MODEL_DOMAIN_T), intent(inout)  :: this
      !> Nothing here to see.

    end subroutine model_update_landuse_codes_static

!--------------------------------------------------------------------------------------------------

  subroutine model_update_landuse_codes_dynamic ( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    ! [ LOCALS ]
    type (DATA_CATALOG_ENTRY_T), pointer :: pLULC

    pLULC => DAT%find("LAND_USE")

    if ( associated(pLULC) ) then

      call read_landuse_codes()

      if ( pLULC%lGridHasChanged ) then
        call initialize_landuse_codes()

        ! update the rooting depths by landuse and hydrologic soil group
        call this%update_rooting_depth_table()
        call this%init_soil_storage_max()

      endif

    endif

  end subroutine model_update_landuse_codes_dynamic

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_GDD_none( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    !> Nothing here to see.

  end subroutine model_initialize_GDD_none

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_GDD_none( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    !> Nothing here to see.

  end subroutine model_calculate_GDD_none

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_GDD( this )

    use growing_degree_day, only           : growing_degree_day_initialize

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int)  :: status

    allocate( this%gdd( count( this%active ) ), stat=status )
    call assert( status==0, "Problem allocating memory.", __SRCNAME__, __LINE__ )

    this%gdd = 0.0_c_float

    call growing_degree_day_initialize( is_cell_active=this%active,                           &
                                        landuse_index=this%landuse_index )

  end subroutine model_initialize_GDD

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_GDD( this )

    use growing_degree_day, only           : growing_degree_day_calculate

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call growing_degree_day_calculate( gdd=this%gdd,                                        &
                                       tmean=this%tmean,                                    &
                                       order=this%sort_order )

  end subroutine model_calculate_GDD

  !--------------------------------------------------------------------------------------------------

    subroutine model_initialize_GDD_be( this )

      use growing_degree_day_baskerville_emin, only : growing_degree_day_be_initialize

      class (MODEL_DOMAIN_T), intent(inout)  :: this

      ! [ LOCALS ]
      integer (kind=c_int)  :: status

      allocate( this%gdd( count( this%active ) ), stat=status )
      call assert( status==0, "Problem allocating memory.", __SRCNAME__, __LINE__ )

      this%gdd = 0.0_c_float

      call growing_degree_day_be_initialize( is_cell_active=this%active,                           &
                                             landuse_index=this%landuse_index )

    end subroutine model_initialize_GDD_be

  !--------------------------------------------------------------------------------------------------

    subroutine model_calculate_GDD_be( this )

      use growing_degree_day_baskerville_emin, only    : growing_degree_day_be_calculate

      class (MODEL_DOMAIN_T), intent(inout)  :: this

      call growing_degree_day_be_calculate( gdd=this%gdd,                                          &
                                                 tmean=this%tmean,                                      &
                                                 tmin=this%tmin,                                        &
                                                 tmax=this%tmax,                                        &
                                                 order=this%sort_order )

    end subroutine model_calculate_GDD_be

!--------------------------------------------------------------------------------------------------

  subroutine model_dump_variables_none( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    !> Nothing here to see.

  end subroutine model_dump_variables_none

!--------------------------------------------------------------------------------------------------

  subroutine model_dump_variables_by_cell( this )

    use routing__D8, only  : get_sort_order

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int)   :: jndx, indx_start, indx_end

    ! iterating over the list of cells or cell ranges to dump
    do jndx=lbound( DUMP, 1), ubound( DUMP, 1)

      indx_start = DUMP( jndx )%indx_start
      indx_end   = DUMP( jndx )%indx_end

      if ( (indx_start >= lbound( this%landuse_code, 1) )                                        &
         .and. ( indx_start <= ubound( this%landuse_code, 1) )                                   &
         .and. (indx_end >= lbound( this%landuse_code, 1) )                                    &
         .and. (indx_end <= ubound( this%landuse_code, 1) ) ) then

         call model_dump_variables( this=this, unitnum=DUMP( jndx )%unitnum,                     &
                                    indx_start=indx_start, indx_end=indx_end )

      else

        ! this call returns the cell_index value
        indx_start = this%row_column_to_index( col_num=DUMP( jndx )%col, row_num=DUMP( jndx )%row)

        ! the 'dump' subroutine expects to be gived the sort_order, not the actual
        ! cell_index
        indx_start = get_sort_order( indx_start )

        if ( (indx_start >= lbound( this%landuse_code, 1) )                                        &
           .and. ( indx_start <= ubound( this%landuse_code, 1) ) )                                 &

          call model_dump_variables( this=this, unitnum=DUMP( jndx )%unitnum, indx_start=indx_start )

      endif

    enddo

  end subroutine model_dump_variables_by_cell

!--------------------------------------------------------------------------------------------------

!   subroutine model_dump_variables_by_row( this )

!     class (MODEL_DOMAIN_T), intent(inout)  :: this

!     ! [ LOCALS ]
!     integer (kind=c_int)   :: indx, jndx

!     do jndx=lbound( DUMP, 1), ubound( DUMP, 1)

!       indx = this%row_column_to_index( col_num=DUMP( jndx )%col, row_num=DUMP( jndx )%row)

!       if ( (indx > lbound( this%landuse_code, 1) ) .and. ( indx <= ubound( this%landuse_code, 1) ) ) then

!         call model_dump_variables( this=this, unitnum=DUMP( jndx )%unitnum, indx=indx )

!       endif

!     enddo

!   end subroutine model_dump_variables_by_cell

!--------------------------------------------------------------------------------------------------

  subroutine model_dump_variables( this, unitnum, indx_start, indx_end )

    use runoff__curve_number, only   : PREV_5_DAYS_RAIN
    use routing__D8, only            : get_cell_index, get_target_index

    class (MODEL_DOMAIN_T), intent(inout)       :: this
    integer (kind=c_int), intent(in)            :: unitnum
    integer (kind=c_int), intent(in)            :: indx_start
    integer (kind=c_int), intent(in), optional  :: indx_end

    ! [ LOCALS ]
    integer (kind=c_int) :: kndx
    integer (kind=c_int) :: target_indx
    integer (kind=c_int) :: cell_indx
    integer (kind=c_int) :: indx_end_
    integer (kind=c_int) :: indx

    if ( present( indx_end ) ) then
      indx_end_ = indx_end
    else
      indx_end_ = indx_start
    endif

    do indx=indx_start, indx_end_

      target_indx = get_target_index( indx )
      cell_indx   = get_cell_index( indx )

      write( unit=unitnum, fmt="(i2,',',i2,',',i4,',',8(i6,','),58(g14.7,','),g14.7)")                  &
        SIM_DT%curr%iMonth, SIM_DT%curr%iDay, SIM_DT%curr%iYear,                                        &
        this%landuse_code( cell_indx ), this%landuse_index( cell_indx ),                                          &
        this%soil_group( cell_indx ), this%num_upslope_connections( cell_indx ), this%sum_upslope_cells( cell_indx ),  &
        indx, cell_indx, target_indx,                                                                  &
        this%awc( cell_indx ), this%latitude( cell_indx ), this%reference_ET0( cell_indx ), this%actual_ET( cell_indx ),    &
        this%curve_num_adj( cell_indx ), this%inflow( cell_indx ), this%runon( cell_indx ), this%runoff( cell_indx ),       &
        this%outflow( cell_indx ),                                                                           &
        this%infiltration( cell_indx ), this%snowfall( cell_indx ), this%potential_snowmelt( cell_indx ),              &
        this%snowmelt( cell_indx ), this%interception( cell_indx ), this%rainfall( cell_indx ),                        &
        this%interception_storage( cell_indx ), this%tmax( cell_indx ), this%tmin( cell_indx ), this%tmean( cell_indx ),    &
        this%snow_storage( cell_indx ), this%soil_storage( cell_indx ), this%soil_storage_max( cell_indx ),            &
        this%delta_soil_storage( cell_indx ),                                                                                   &
        this%surface_storage( cell_indx ), this%surface_storage_excess( cell_indx ),                              &
        this%surface_storage_max( cell_indx ), this%net_infiltration( cell_indx ),                                &
        this%rejected_net_infiltration( cell_indx ), this%fog( cell_indx ),                                       &
        this%irrigation( cell_indx ), this%gdd( cell_indx ), this%runoff_outside( cell_indx ),                         &
        this%pervious_fraction( cell_indx ), this%storm_drain_capture( cell_indx ),                               &
        this%canopy_cover_fraction( cell_indx ), this%crop_coefficient_kcb( cell_indx ),                          &
        this%continuous_frozen_ground_index( cell_indx ), this%rooting_depth_max( cell_indx ),                    &
        this%current_rooting_depth( cell_indx ), this%actual_et_soil( cell_indx ),                                &
        this%readily_available_water_raw( cell_indx ), this%total_available_water_taw( cell_indx ),               &
        this%plant_stress_coef_ks( cell_indx ), this%evap_reduction_coef_kr( cell_indx ),                         &
        this%surf_evap_coef_ke( cell_indx ), this%fraction_exposed_and_wetted_soil( cell_indx ),                  &
        this%actual_et_impervious( cell_indx ), this%actual_et_interception( cell_indx ),                         &
        this%adjusted_depletion_fraction_p( cell_indx ), this%crop_etc( cell_indx ), this%bare_soil_evap( cell_indx ), &
        this%direct_net_infiltration( cell_indx ),                                                                   &
        this%direct_soil_moisture( cell_indx ), (PREV_5_DAYS_RAIN( cell_indx, kndx), kndx=1,6)

    enddo

  end subroutine model_dump_variables

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_actual_et_thornthwaite_mather_eqns(this)

    use actual_et__thornthwaite_mather_eqns, only : initialize_actual_et_thornthwaite_mather_eqns

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call initialize_actual_et_thornthwaite_mather_eqns( soil_moisture=this%soil_storage,    &
                                                        max_soil_moisture=this%soil_storage_max )

  end subroutine model_initialize_actual_et_thornthwaite_mather_eqns

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_actual_et_thornthwaite_mather_eqns( this, indx )

    use actual_et__thornthwaite_mather_eqns, only : calculate_actual_et_thornthwaite_mather_eqns

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    integer (kind=c_int), intent(in)       :: indx

    call calculate_actual_et_thornthwaite_mather_eqns(                                               &
                                                  actual_et=this%actual_et( indx ),                  &
                                                  soil_moisture=this%soil_storage( indx ),           &
                                                  max_soil_moisture=this%soil_storage_max( indx ),   &
                                                  precipitation=this%infiltration( indx ),           &
                                                  crop_etc=this%crop_etc( indx ),                    &
                                                  indx=indx )

  end subroutine model_calculate_actual_et_thornthwaite_mather_eqns

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_actual_et_thornthwaite_mather(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_actual_et_thornthwaite_mather

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_actual_et_thornthwaite_mather( this, indx )

    use actual_et__thornthwaite_mather

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    integer (kind=c_int), intent(in)       :: indx

    call calculate_actual_et_thornthwaite_mather( actual_et=this%actual_et_soil( indx ),            &
                                                  soil_storage=this%soil_storage( indx ),           &
                                                  soil_storage_max=this%soil_storage_max( indx ),   &
                                                  precipitation=this%infiltration( indx ),          &
                                                  crop_etc=this%crop_etc( indx ) )

  end subroutine model_calculate_actual_et_thornthwaite_mather

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_actual_et_fao56(this)

    use actual_et__fao56

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call initialize_actual_et_fao56()

  end subroutine model_initialize_actual_et_fao56

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_actual_et_fao56( this, indx )

    use actual_et__fao56

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    integer (kind=c_int), intent(in)       :: indx

    ! [ LOCALS ]
    integer (kind=c_int) :: landuse_index

    landuse_index = this%landuse_index( indx )

    call calculate_actual_et_fao56( actual_et=this%actual_et_soil( indx ),                                    &
                                    adjusted_depletion_fraction_p=this%adjusted_depletion_fraction_p( indx ), &
                                    soil_storage=this%soil_storage( indx ),                                   &
                                    soil_storage_max=this%soil_storage_max( indx ),                           &
                                    infiltration=this%infiltration( indx ),                                   &
                                    crop_etc=this%crop_etc( indx ),                                           &
                                    depletion_fraction_p=DEPLETION_FRACTION( landuse_index ) )

  end subroutine model_calculate_actual_et_fao56

  !--------------------------------------------------------------------------------------------------

  subroutine model_initialize_actual_et_fao56__two_stage(this)

    use actual_et__fao56__two_stage

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call actual_et_FAO56_two_stage_initialize( )

  end subroutine model_initialize_actual_et_fao56__two_stage

  !--------------------------------------------------------------------------------------------------

  subroutine model_calculate_actual_et_fao56__two_stage( this, indx )

    use actual_et__fao56__two_stage

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    integer (kind=c_int), intent(in)       :: indx

    ! [ LOCALS ]
    integer (kind=c_int) :: landuse_index

    landuse_index = this%landuse_index( indx )

    call calculate_actual_et_fao56_two_stage(                                                   &
              actual_et=this%actual_et_soil( indx ),                                                &
              crop_etc=this%crop_etc( indx ),                                                   &
              bare_soil_evap=this%bare_soil_evap( indx ),                                       &
              taw=this%total_available_water_taw( indx ),                                       &
              raw=this%readily_available_water_raw( indx ),                                     &
              fraction_exposed_and_wetted_soil=this%fraction_exposed_and_wetted_soil( indx ),   &
              Kr=this%evap_reduction_coef_kr( indx ),                                           &
              Ke=this%surf_evap_coef_ke( indx ),                                                &
              Ks=this%plant_stress_coef_ks( indx ),                                             &
              adjusted_depletion_fraction_p=this%adjusted_depletion_fraction_p( indx ),         &
              Kcb=this%crop_coefficient_kcb( indx ),                                            &
              landuse_index=this%landuse_index( indx ),                                         &
              soil_group=this%soil_group( indx ),                                               &
              awc=this%awc( indx ),                                                             &
              current_rooting_depth=this%current_rooting_depth( indx ),                         &
              soil_storage=this%soil_storage( indx ),                                           &
              soil_storage_max=this%soil_storage_max( indx ),                                   &
              reference_et0=this%reference_et0( indx ) )

  end subroutine model_calculate_actual_et_fao56__two_stage

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_direct_net_infiltration_none ( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_direct_net_infiltration_none

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_direct_net_infiltration_none ( this, indx )

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    integer ( kind=c_int ), intent(in)     :: indx

  end subroutine model_calculate_direct_net_infiltration_none

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_direct_net_infiltration_gridded ( this )

    use direct_net_infiltration__gridded_data

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call direct_net_infiltration_initialize( is_cell_active=this%active,                  &
                                     landuse_index=this%landuse_index )

  end subroutine model_initialize_direct_net_infiltration_gridded

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_direct_net_infiltration_gridded ( this, indx )

    use direct_net_infiltration__gridded_data

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    integer ( kind=c_int ), intent(in)     :: indx

    call direct_net_infiltration_calculate( direct_net_infiltration = this%direct_net_infiltration( indx ),      &
                                    indx=indx,                                           &
                                    is_cell_active=this%active,                          &
                                    nodata_fill_value=this%nodata_fill_value )

  end subroutine model_calculate_direct_net_infiltration_gridded

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_direct_soil_moisture_none ( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_direct_soil_moisture_none

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_direct_soil_moisture_none ( this, indx )

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    integer ( kind=c_int ), intent(in)     :: indx

  end subroutine model_calculate_direct_soil_moisture_none

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_direct_soil_moisture_gridded ( this )

    use direct_soil_moisture__gridded_data

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call direct_soil_moisture_initialize( is_cell_active=this%active,                 &
                                          landuse_index=this%landuse_index )

  end subroutine model_initialize_direct_soil_moisture_gridded

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_direct_soil_moisture_gridded ( this, indx )

    use direct_soil_moisture__gridded_data

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    integer ( kind=c_int ), intent(in)     :: indx

    call direct_soil_moisture_calculate( direct_soil_moisture = this%direct_soil_moisture( indx ),   &
                                         indx=indx,                                                  &
                                         is_cell_active=this%active  )

  end subroutine model_calculate_direct_soil_moisture_gridded

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_maximum_net_infiltration_none ( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_maximum_net_infiltration_none

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_maximum_net_infiltration_none ( this, indx )

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    integer ( kind=c_int ), intent(in)     :: indx

  end subroutine model_calculate_maximum_net_infiltration_none

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_maximum_net_infiltration_gridded ( this )

    use maximum_net_infiltration__gridded_data

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call maximum_net_infiltration_initialize( is_cell_active=this%active,                 &
                                                landuse_index=this%landuse_index )

  end subroutine model_initialize_maximum_net_infiltration_gridded

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_maximum_net_infiltration_gridded ( this, indx )

    use maximum_net_infiltration__gridded_data

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    integer ( kind=c_int ), intent(in)     :: indx

    call maximum_net_infiltration_calculate( net_infiltration = this%net_infiltration( indx ),                    &
                                               rejected_net_infiltration = this%rejected_net_infiltration( indx ),  &
                                               indx=indx )

  end subroutine model_calculate_maximum_net_infiltration_gridded

  !--------------------------------------------------------------------------------------------------

  subroutine model_update_rooting_depth_none(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_update_rooting_depth_none

  !--------------------------------------------------------------------------------------------------

  subroutine model_update_rooting_depth_FAO56(this)

    use rooting_depth__FAO56, only : update_rooting_depth

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call update_rooting_depth(        &
                       Zr_i=this%current_rooting_depth,       &
                       Zr_max=this%rooting_depth_max,         &
                       landuse_index=this%landuse_index,      &
                       Kcb=this%crop_coefficient_kcb )

  end subroutine model_update_rooting_depth_FAO56

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_crop_coefficient_none(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    this%crop_coefficient_kcb = 1.0_c_float

  end subroutine model_initialize_crop_coefficient_none

!--------------------------------------------------------------------------------------------------

  subroutine model_update_crop_coefficient_none(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_update_crop_coefficient_none

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_crop_coefficient_FAO56(this)

    use crop_coefficients__FAO56, only : crop_coefficients_FAO56_initialize

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call crop_coefficients_FAO56_initialize( fSoilStorage=this%soil_storage,      &
                                             iLanduseIndex=this%landuse_index,    &
                                             iSoilGroup=this%soil_group,          &
                                             fAvailable_Water_Content=this%awc,   &
                                             lActive=this%active )

  end subroutine model_initialize_crop_coefficient_FAO56

!--------------------------------------------------------------------------------------------------

  subroutine model_update_crop_coefficient_FAO56(this)

    use crop_coefficients__FAO56, only : crop_coefficients_FAO56_calculate,                 &
                                         crop_coefficients_FAO56_update_growth_stage_dates, &
                                         GROWTH_STAGE_DATE, PLANTING_DATE

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call crop_coefficients_FAO56_update_growth_stage_dates( )

    this%number_of_days_since_planting = SIM_DT%curr                            &
              - GROWTH_STAGE_DATE( PLANTING_DATE, this%landuse_index )

    call crop_coefficients_FAO56_calculate( Kcb=this%crop_coefficient_kcb,             &
                                            GDD=this%gdd,                              &
                                            landuse_index=this%landuse_index )

  end subroutine model_update_crop_coefficient_FAO56

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_fog_none(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    !> Nothing here to see. Initialization not really needed for the "normal" method.
    this%fog = 0.0_c_float

  end subroutine model_initialize_fog_none

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_fog_monthly_grid(this)

    use fog__monthly_grid

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call fog_monthly_grid_initialize( lActive=this%active, PROJ4_string=this%PROJ4_string,  &
      dX=this%X, dY=this%Y, dX_lon=this%X_lon , dY_lat=this%Y_lat )

  end subroutine model_initialize_fog_monthly_grid

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_precip_normal(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    !> Nothing here to see. Initialization not really needed for the "normal" method.

  end subroutine model_initialize_precip_normal

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_fog_none(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_calculate_fog_none

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_fog_monthly_grid(this)

    use fog__monthly_grid

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call fog_monthly_grid_calculate( fRainfall=this%rainfall, fFog=this%fog,          &
      iLanduse_Index=this%landuse_index, lActive=this%active,                         &
      nodata_fill_value=this%nodata_fill_value )

  end subroutine model_calculate_fog_monthly_grid

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_mean_air_temperature(this)

     class ( MODEL_DOMAIN_T ), intent(inout)   :: this

     this%tmean = ( this%tmin + this%tmax ) / 2.0_c_float

  end subroutine model_calculate_mean_air_temperature

!--------------------------------------------------------------------------------------------------

  subroutine model_get_maximum_air_temperature_normal(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iJulianDay
    integer (kind=c_int) ::iMonth
    integer (kind=c_int) ::iDay
    integer (kind=c_int) ::iYear

    type (DATA_CATALOG_ENTRY_T), pointer :: pTMAX

    pTMAX => DAT%find("TMAX")
    if ( .not. associated(pTMAX) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear

      call pTMAX%getvalues( iMonth, iDay, iYear, iJulianDay )

    end associate

    if (.not. associated(pTMAX%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: Call to NULL pointer.", __SRCNAME__, __LINE__)

    this%tmax = pack( pTMAX%pGrdBase%rData, this%active )

  end subroutine model_get_maximum_air_temperature_normal

!--------------------------------------------------------------------------------------------------

  subroutine model_get_minimum_air_temperature_normal(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iJulianDay
    integer (kind=c_int) ::iMonth
    integer (kind=c_int) ::iDay
    integer (kind=c_int) ::iYear

    type (DATA_CATALOG_ENTRY_T), pointer :: pTMIN

    pTMIN => DAT%find("TMIN")
    if ( .not. associated(pTMIN) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear

      call pTMIN%getvalues( iMonth, iDay, iYear, iJulianDay )

    end associate

    if (.not. associated(pTMIN%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: Call to NULL pointer.", __SRCNAME__, __LINE__)

    this%tmin = pack( pTMIN%pGrdBase%rData, this%active )

  end subroutine model_get_minimum_air_temperature_normal

!--------------------------------------------------------------------------------------------------

  subroutine model_get_precip_normal(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    type (DATA_CATALOG_ENTRY_T), pointer :: pPRCP
    integer (kind=c_int) :: iJulianDay
    integer (kind=c_int) ::iMonth
    integer (kind=c_int) ::iDay
    integer (kind=c_int) ::iYear

    pPRCP => DAT%find("PRECIPITATION")
    if ( .not. associated(pPRCP) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear

      ! next three statements retrieve the data from the raw or native form
      call pPRCP%getvalues( iMonth, iDay, iYear, iJulianDay )

    end associate

    if (.not. associated(pPRCP%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: Call to NULL pointer.", __SRCNAME__, __LINE__)

    this%gross_precip = pack( pPRCP%pGrdBase%rData, this%active )

  end subroutine model_get_precip_normal

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_precip_method_of_fragments(this)

    use precipitation__method_of_fragments

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: status

    allocate( this%monthly_gross_precip( count( this%active ) ), stat=status)
    call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    call precipitation_method_of_fragments_initialize( this%active )
    call this%get_precipitation_data()

  end subroutine model_initialize_precip_method_of_fragments

!--------------------------------------------------------------------------------------------------

  subroutine model_get_precip_method_of_fragments(this)

    use precipitation__method_of_fragments

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    type (DATA_CATALOG_ENTRY_T), pointer :: pPRCP
    integer (kind=c_int) :: iJulianDay
    integer (kind=c_int) ::iMonth
    integer (kind=c_int) ::iDay
    integer (kind=c_int) ::iYear
    integer (kind=c_int) :: targetindex
    integer (kind=c_int) :: indexval

    ! in this usage, it is assumed that the precipitation grids that are being read in represent
    ! MONTHLY sum of precipitation
    pPRCP => DAT%find("PRECIPITATION")
    if ( .not. associated(pPRCP) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear

      ! get current grid value for precip; in the design case, this is a grid of MONTHLY
      ! mean precipitation totals in inches
      call pPRCP%getvalues( iMonth, iDay, iYear, iJulianDay )

    end associate

    if (.not. associated(pPRCP%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer.", __SRCNAME__, __LINE__)

    call precipitation_method_of_fragments_calculate( this%active )

    this%gross_precip = pack( pPRCP%pGrdBase%rData, this%active ) * FRAGMENT_VALUE * RAINFALL_ADJUST_FACTOR
    this%monthly_gross_precip = pack( pPRCP%pGrdBase%rData, this%active ) * RAINFALL_ADJUST_FACTOR

    ! print *, "*****************************************************************"
    ! do indexval=5,7

    !   targetindex = this%row_column_to_index_fn( indexval, 151 )

    !   if ( targetindex > 0 ) then

    !     print *, " row 151, col ", indexval
    !     print *, "----------------"
    !     print *, "active?:        ", this%active( indexval,151 )
    !     print *, "gross_precip:   ", this%gross_precip( targetindex )
    !     print *, "FRAGMENT_VALUE: ", FRAGMENT_VALUE( targetindex )
    !     print *, "RAIN_ADJST_FAC: ", RAINFALL_ADJUST_FACTOR( targetindex )
    !     print *, "Raw PRCP grid:  ", pPRCP%pGrdBase%rData( indexval, 151 )
    !     print *, "Rain Gage ID:   ", RAIN_GAGE_ID( targetindex )
    !     print *, " "
    !   endif

    ! enddo

    ! print *, "*****************************************************************"

  end subroutine model_get_precip_method_of_fragments

!--------------------------------------------------------------------------------------------------

  subroutine minmaxmean_float( variable , varname, active_cells )

    real (kind=c_float), dimension(:)  :: variable
    character (len=*), intent(in)      :: varname
    logical, dimension(:), optional    :: active_cells

    ! [ LOCALS ]
    integer (kind=c_int) :: iCount
    character (len=20)   :: sVarname
    character (len=14)   :: sMin
    character (len=14)   :: sMax
    character (len=14)   :: sMean
    character (len=10)   :: sCount

    write (sVarname, fmt="(a20)") adjustl(varname)

    if ( present( active_cells ) ) then

      if (count( active_cells ) > 0 ) then
        write (sMin, fmt="(g14.3)")   minval(variable, active_cells )
        write (sMax, fmt="(g14.3)")   maxval(variable, active_cells )
        write (sMean, fmt="(g14.3)")  sum(variable, active_cells ) / count( active_cells )
        write (sCount, fmt="(i10)") count( active_cells )
      else
        write (sMin, fmt="(g14.3)")   -9999.
        write (sMax, fmt="(g14.3)")   -9999.
        write (sMean, fmt="(g14.3)")  -9999.
        write (sCount, fmt="(i10)")       0
      endif

    else

      if (size( variable, 1) > 0 ) then
        write (sMin, fmt="(g14.3)")   minval(variable)
        write (sMax, fmt="(g14.3)")   maxval(variable)
        write (sMean, fmt="(g14.3)")  sum(variable) / size(variable,1)
        write (sCount, fmt="(i10)") size(variable,1)
      else
        write (sMin, fmt="(g14.3)")   -9999.
        write (sMax, fmt="(g14.3)")   -9999.
        write (sMean, fmt="(g14.3)")  -9999.
        write (sCount, fmt="(i10)")       0
      endif

    endif

    call LOGS%write( adjustl(sVarname)//" | "//adjustl(sMin)//" | "//adjustl(sMax) &
       //" | "//adjustl(sMean)//" | "//adjustl(sCount), iLogLevel=LOG_ALL, lEcho=lTRUE )


  end subroutine minmaxmean_float


  subroutine minmaxmean_int( variable , varname, active_cells )

    integer (kind=c_int), dimension(:)  :: variable
    character (len=*), intent(in)       :: varname
    logical, dimension(:), optional     :: active_cells

    ! [ LOCALS ]
    integer (kind=c_int) :: iCount
    character (len=20)   :: sVarname
    character (len=14)   :: sMin
    character (len=14)   :: sMax
    character (len=14)   :: sMean
    character (len=10)   :: sCount

    write (sVarname, fmt="(a20)") adjustl(varname)

    if ( present( active_cells ) ) then

      if (count( active_cells ) > 0 ) then
        write (sMin, fmt="(i14)")   minval(variable, active_cells )
        write (sMax, fmt="(i14)")   maxval(variable, active_cells )
        write (sMean, fmt="(i14)")  sum(variable, active_cells ) / count( active_cells )
        write (sCount, fmt="(i10)") count( active_cells )
      else
        write (sMin, fmt="(i14)")   -9999
        write (sMax, fmt="(i14)")   -9999
        write (sMean, fmt="(i14)")  -9999
        write (sCount, fmt="(i10)")  0
      endif

    else

      if (size( variable, 1) > 0 ) then
        write (sMin, fmt="(i14)")   minval(variable)
        write (sMax, fmt="(i14)")   maxval(variable)
        write (sMean, fmt="(i14)")  sum(variable) / size(variable,1)
        write (sCount, fmt="(i10)") size(variable,1)
      else
        write (sMin, fmt="(i14)")   -9999
        write (sMax, fmt="(i14)")   -9999
        write (sMean, fmt="(i14)")  -9999
        write (sCount, fmt="(i10)")  0
      endif

    endif

    call LOGS%write( adjustl(sVarname)//" | "//adjustl(sMin)//" | "//adjustl(sMax) &
       //" | "//adjustl(sMean)//" | "//adjustl(sCount), iLogLevel=LOG_ALL, lEcho=lTRUE )

  end subroutine minmaxmean_int

  subroutine summarize_state_variables_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    call LOGS%write( "variable name        | min            | max            | mean           | count         ")
    call LOGS%write( "-------------------- | -------------- | -------------- | -------------- | --------------")

    call minmaxmean( this%landuse_code , "LULC")
    call minmaxmean( this%landuse_index, "LULC_index")
    call minmaxmean( this%soil_group, "HSG")
    call minmaxmean( this%num_upslope_connections, "upslope")
    call minmaxmean( this%sum_upslope_cells, "sum_upslope")

    call minmaxmean( this%awc, "AWC")

    call minmaxmean( this%latitude, "Lat")
    call minmaxmean( this%reference_ET0, "ET0")
    call minmaxmean( this%actual_ET, "actET")
    call minmaxmean( this%inflow, "inflow")
    call minmaxmean( this%runon, "runon")
    call minmaxmean( this%runoff, "runoff")
    call minmaxmean( this%outflow, "outflow")
    call minmaxmean( this%infiltration, "infilt")
    call minmaxmean( this%snowfall, "snowfall")
    call minmaxmean( this%snowmelt, "snowmelt")
    call minmaxmean( this%interception, "intercept")
    call minmaxmean( this%rainfall, "rainfall")

    call minmaxmean( this%interception_storage, "intcp_stor")
    call minmaxmean( this%snow_storage, "snow_stor")
    call minmaxmean( this%soil_storage, "soil_stor")
    call minmaxmean( this%soil_storage_max, "soil_stor_max")
    call minmaxmean( this%net_infiltration, "net_infiltration")

  end subroutine summarize_state_variables_sub

!--------------------------------------------------------------------------------------------------

end module model_domain
