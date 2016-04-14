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
    real (kind=c_float), allocatable       :: gdd(:)
    real (kind=c_float), allocatable       :: crop_coefficient_kcb(:)
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
    real (kind=c_float), allocatable       :: soil_storage(:)
    real (kind=c_float), allocatable       :: soil_storage_max(:)
    real (kind=c_float), allocatable       :: potential_recharge(:)
    real (kind=c_float), allocatable       :: direct_recharge(:)    
    real (kind=c_float), allocatable       :: current_rooting_depth(:)
    integer (kind=c_int), allocatable      :: number_of_days_since_planting(:)
         
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

    integer (kind=c_int), allocatable      :: index_order(:)

    real (kind=c_float), allocatable       :: adjusted_depletion_fraction_p(:)

    ! member variables that are only allocated if particular optional methods are invoked

    real (kind=c_float), allocatable       :: irrigation_mask(:)

    !> declare and initialize procedure pointers such that the default methods are in place
    procedure ( simple_method ), pointer         :: init_interception       => model_initialize_interception_bucket
    procedure ( simple_method ), pointer         :: init_runoff             => model_initialize_runoff_curve_number
    procedure ( simple_method ), pointer         :: init_reference_et       => model_initialize_et_hargreaves
    procedure ( simple_method ), pointer         :: init_actual_et          => model_initialize_actual_et_thornthwaite_mather
    procedure ( simple_method ), pointer         :: init_routing            => model_initialize_routing_D8
    procedure ( simple_method ), pointer         :: init_soil_storage_max   => model_initialize_soil_storage_max_internally_calculated
    procedure ( simple_method ), pointer         :: init_snowfall           => model_initialize_snowfall_original
    procedure ( simple_method ), pointer         :: init_snowmelt           => model_initialize_snowmelt_original
    procedure ( simple_method ), pointer         :: init_precipitation_data => model_initialize_precip_normal
    procedure ( simple_method ), pointer         :: init_fog                => model_initialize_fog_none
    procedure ( simple_method ), pointer         :: init_irrigation         => model_initialize_irrigation_none
    procedure ( simple_method ), pointer         :: init_direct_recharge    => model_initialize_direct_recharge_none
    procedure ( simple_method ), pointer         :: init_GDD                => model_initialize_GDD_none
    procedure ( simple_method ), pointer         :: init_AWC                => model_initialize_available_water_content_gridded
    procedure ( simple_method ), pointer         :: init_crop_coefficient   => model_initialize_crop_coefficient_none
    procedure ( simple_method ), pointer         :: calc_interception       => model_calculate_interception_bucket
    procedure ( simple_method ), pointer         :: update_crop_coefficient => model_update_crop_coefficient_none    

    procedure ( simple_method_w_optional ), pointer   :: calc_runoff       => model_calculate_runoff_curve_number
    
    procedure ( simple_method ), pointer         :: calc_reference_et      => model_calculate_et_hargreaves
    procedure ( simple_method ), pointer         :: calc_routing           => model_calculate_routing_D8

    procedure ( simple_method ), pointer         :: calc_actual_et         => model_calculate_actual_et_thornthwaite_mather
    procedure ( simple_method ), pointer         :: calc_snowfall          => model_calculate_snowfall_original
    procedure ( simple_method ), pointer         :: calc_snowmelt          => model_calculate_snowmelt_original  
    procedure ( simple_method ), pointer         :: calc_fog               => model_calculate_fog_none
    procedure ( simple_method ), pointer         :: calc_irrigation        => model_calculate_irrigation_none
    procedure ( simple_method ), pointer         :: calc_GDD               => model_calculate_GDD_none
    procedure ( simple_method ), pointer         :: calc_direct_recharge => model_calculate_direct_recharge_none    

    procedure (simple_method), pointer           :: output_GDD             => model_output_GDD_none
    procedure (simple_method), pointer           :: output_soil_moisture   => model_output_irrigation_none
    procedure (simple_method), pointer           :: output_irrigation      => model_output_irrigation_none

    procedure ( simple_method ), pointer         :: read_awc_data           => model_read_available_water_content_gridded
    procedure ( simple_method ), pointer         :: get_precipitation_data => model_get_precip_normal
    procedure ( simple_method ), pointer         :: get_minimum_air_temperature_data                                       &     
                                                                           => model_get_minimum_air_temperature_normal
    procedure ( simple_method ), pointer         :: get_maximum_air_temperature_data                                       &     
                                                                           => model_get_maximum_air_temperature_normal
    procedure ( simple_method ), pointer         :: calculate_mean_air_temperature                                       &     
                                                                           => model_calculate_mean_air_temperature

  contains

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

    procedure :: dump_model_values_by_cell_sub
    generic   :: dump_model_values_by_cell => dump_model_values_by_cell_sub
  
  end type MODEL_DOMAIN_T


  abstract interface
    subroutine simple_method( this )
      import :: MODEL_DOMAIN_T
      class ( MODEL_DOMAIN_T ), intent(inout)  :: this
    end subroutine simple_method
  end interface  


  abstract interface
    subroutine simple_method_w_optional( this, index )
      import :: MODEL_DOMAIN_T, c_int
      class ( MODEL_DOMAIN_T ), intent(inout)       :: this
      integer (kind=c_int), intent(in), optional    :: index
    end subroutine simple_method_w_optional
  end interface  


  interface minmaxmean
    procedure :: minmaxmean_float
    procedure :: minmaxmean_int
  end interface minmaxmean

  ! creating several module-level globals
  type (MODEL_DOMAIN_T), public             :: MODEL

  real (kind=c_float), allocatable, public  :: ROOTING_DEPTH_MAX(:,:)

  type (GENERAL_GRID_T), pointer            :: pROOTING_DEPTH

  public :: minmaxmean

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
    call assert (iStat == 0, "Problem allocating memory", __FILE__, __LINE__)

    allocate(this%nodata_fill_value(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __FILE__, __LINE__)

    allocate(this%array_output(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __FILE__, __LINE__)

    allocate(this%col_num_2D(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __FILE__, __LINE__)

    allocate(this%row_num_2D(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __FILE__, __LINE__)

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
    integer (kind=c_int)  :: iStat(49)

    iCount = count( this%active )

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
    allocate( this%potential_recharge(iCount), stat=iStat(23) )
    allocate( this%fog(iCount), stat=iStat(24) )
    allocate( this%irrigation(iCount), stat=iStat(25) )
    allocate( this%index_order(iCount), stat=iStat(26) )
    allocate( this%gdd(iCount), stat=iStat(27) )
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
    allocate( this%direct_recharge( iCount ), stat=iStat(46) )     
    allocate( this%number_of_days_since_planting( iCount ), stat=iStat(47) )   
    allocate( this%col_num_1D( iCount ), stat=iStat(48) )
    allocate( this%row_num_1D( iCount ), stat=iStat(49) )    

    do iIndex = 1, ubound( iStat, 1)
      if ( iStat( iIndex ) /= 0 )   call warn("INTERNAL PROGRAMMING ERROR--Problem allocating memory; iIndex="  &
        //asCharacter(iIndex), __FILE__, __LINE__ )
    enddo
    
    if (any( iStat /= 0) ) call die ( "Unable to allocate memory for one or more arrays.", __FILE__, __LINE__ )  

!$OMP PARALLEL WORKSHARE

    this%landuse_code                        = 0_c_int
    this%landuse_index                       = 0_c_int
    this%soil_group                          = 0_c_int
    this%num_upslope_connections             = 0_c_int
    this%sum_upslope_cells                   = 0_c_int
    this%awc                                 = 0.0_c_float
    this%latitude                            = 0.0_c_float
    this%reference_ET0                       = 0.0_c_float
    this%actual_ET                           = 0.0_c_float
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

    this%potential_recharge                  = 0.0_c_float
    this%fog                                 = 0.0_c_float
    this%irrigation                          = 0.0_c_float
    this%index_order                         = 0_c_int
    this%gdd                                 = 0.0_c_float
    this%runoff_outside                      = 0.0_c_float
    this%pervious_fraction                   = 0.0_c_float
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
    this%direct_recharge                     = 0.0_c_float
    this%number_of_days_since_planting       = 0_c_int
    
!$OMP END PARALLEL WORKSHARE

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

    call this%init_direct_recharge

    call this%init_crop_coefficient

  end subroutine initialize_methods_sub

!--------------------------------------------------------------------------------------------------
  
  subroutine set_inactive_cells_sub(this)

    use awc__table_values, only  : AVAILABLE_WATER_CONTENT

    class (MODEL_DOMAIN_T), intent(inout)   :: this
    type (DATA_CATALOG_ENTRY_T), pointer :: pHSG
    type (DATA_CATALOG_ENTRY_T), pointer :: pLULC      
    type (DATA_CATALOG_ENTRY_T), pointer :: pAWC      


    pLULC => DAT%find("LAND_USE")
    pHSG => DAT%find("HYDROLOGIC_SOILS_GROUP")
    pAWC => DAT%find("AVAILABLE_WATER_CONTENT")
    
    if ( .not. associated(pHSG) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    if ( .not. associated(pHSG%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    if ( .not. allocated(pHSG%pGrdBase%iData) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of UNALLOCATED variable", __FILE__, __LINE__)

!     if ( .not. associated(pAWC) ) &
!       call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

!     if ( .not. associated(pAWC%pGrdBase) ) &
!       call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

!     if ( .not. allocated(pAWC%pGrdBase%rData) ) &
!       call die("INTERNAL PROGRAMMING ERROR: attempted use of UNALLOCATED variable", __FILE__, __LINE__)

    if ( .not. associated(pLULC) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    if ( .not. associated(pLULC%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    if ( .not. allocated(pLULC%pGrdBase%iData) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of UNALLOCATED variable", __FILE__, __LINE__)

    this%active = .true._c_bool

    if ( associated( pAWC) ) then

      where (       ( pHSG%pGrdBase%iData  < 1 )      &
              .or.  ( pLULC%pGrdBase%iData < 0 )      &
              .or.  ( pAWC%pGrdBase%rData < 0.0 ) )

        this%active = .false._c_bool

      end where

    elseif ( allocated( AVAILABLE_WATER_CONTENT ) ) then

      where (       ( pHSG%pGrdBase%iData  < 1 )      &
              .or.  ( pLULC%pGrdBase%iData < 0 )      &
              .or.  ( AVAILABLE_WATER_CONTENT < 0.0 ) )

        this%active = .false._c_bool

      end where
      
    else

      call die( "Found neither gridded nor tabular data to use in initializing available water capacity.", &
        __FILE__, __LINE__ )

    endif

    call LOGS%write(asCharacter(count(this%active))//" cells are currently active out of a total of " &
      //asCharacter(size(this%active)), iLinesBefore=1, iLinesAfter=1, iLogLevel=LOG_ALL)

  end subroutine set_inactive_cells_sub

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
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )
    if (.not. associated( this%calc_interception) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ ) 

    if (.not. associated( this%init_irrigation) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )
    if (.not. associated( this%calc_irrigation) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_runoff) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )
    if (.not. associated( this%calc_runoff) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_reference_et) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )
    if (.not. associated( this%calc_reference_et) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_snowmelt) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )
    if (.not. associated( this%calc_snowmelt) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_snowfall) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )
    if (.not. associated( this%calc_snowfall) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_GDD) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )
    if (.not. associated( this%calc_GDD) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_routing) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )    
    if (.not. associated( this%calc_routing) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_direct_recharge) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )
    if (.not. associated( this%calc_direct_recharge) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_soil_storage_max) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%get_precipitation_data ) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%get_minimum_air_temperature_data ) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%get_maximum_air_temperature_data ) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

  end subroutine preflight_check_method_pointers

  !------------------------------------------------------------------------------------------------

  subroutine set_method_pointers_sub(this, sCmdText, sMethodName)

    class (MODEL_DOMAIN_T), intent(inout)   :: this
    character (len=*), intent(in)           :: sCmdText
    character (len=*), intent(in)           :: sMethodName


    if ( sCmdText .contains. "INTERCEPTION" ) then

      if ( sMethodName .strequal. "BUCKET" ) then

        this%init_interception => model_initialize_interception_bucket
        this%calc_interception => model_calculate_interception_bucket

        ! when the "bucket" model is used, it requires some means of determining 
        ! "growing" vs. "non-growing" season
        this%init_GDD => model_initialize_GDD
        this%calc_GDD => model_calculate_GDD

        call LOGS%WRITE( "==> BUCKET INTERCEPTION submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( sMethodName .strequal. "GASH" ) then

        this%init_interception => model_initialize_interception_gash
        this%calc_interception => model_calculate_interception_gash

        call LOGS%WRITE( "==> GASH INTERCEPTION submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported INTERCEPTION method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .contains. "RUNOFF" ) then

      if ( ( sMethodName .strequal. "C-N" ) .or. ( sMethodName .strequal. "CURVE_NUMBER" ) ) then

        this%init_runoff => model_initialize_runoff_curve_number
        this%calc_runoff => model_calculate_runoff_curve_number

        call LOGS%WRITE( "==> CURVE NUMBER RUNOFF submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( sMethodName .strequal. "RUNOFF_RATIO" ) .or. ( sMethodName .strequal. "MONTHLY_GRID" ) ) then

        this%init_runoff => model_initialize_runoff_gridded_values
        this%calc_runoff => model_calculate_runoff_gridded_values

        call LOGS%WRITE( "==> RUNOFF RATIO submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported RUNOFF method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .contains. "SNOWFALL" ) then

      if ( ( sMethodName .strequal. "ORIGINAL" ) .or. ( sMethodName .strequal. "ORIGINAL_SWB_METHOD" ) ) then

        this%init_snowfall => model_initialize_snowfall_original
        this%calc_snowfall => model_calculate_snowfall_original

        call LOGS%WRITE( "==> ORIGINAL SNOWFALL submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( sMethodName .strequal. "PRMS" ) then

        this%init_snowfall => model_initialize_snowfall_prms
        this%calc_snowfall => model_calculate_snowfall_prms

        call LOGS%WRITE( "==> PRMS SNOWFALL submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported SNOWFALL method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .contains. "AVAILABLE_WATER_CONTENT" ) then

      if ( ( sMethodName .strequal. "TABLE" ) ) then

        this%init_awc => model_initialize_available_water_content_table
        this%read_awc_data => model_read_available_water_content_table

        call LOGS%WRITE( "==> TABLE method for populating AVAILABLE_WATER_CONTENT selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( sMethodName .strequal. "GRID" ) .or. ( sMethodName .strequal. "GRIDDED" ) ) then

        this%init_awc => model_initialize_available_water_content_gridded
        this%read_awc_data => model_read_available_water_content_gridded

        call LOGS%WRITE( "==> GRIDDED VALUES method for populating AVAILABLE_WATER_CONTENT selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported AVAILABLE_WATER_CONTENT method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .contains. "FLOW_ROUTING" ) then

      if ( sMethodName .strequal. "D8" ) then

        this%init_routing => model_initialize_routing_D8
        this%calc_routing => model_calculate_routing_D8

        call LOGS%WRITE( "==> D8 FLOW ROUTING submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        this%init_routing => model_initialize_routing_none
        this%calc_routing => model_calculate_routing_none

        call LOGS%WRITE( "==> NULL FLOW ROUTING submodel selected -- NO routing will be performed.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      endif

    elseif ( ( sCmdText .contains. "CROP_COEFFICIENT" )    &
        .or. ( sCmdText .contains. "CROP_COEF" ) )  then

      if ( ( sMethodName .strequal. "FAO56" )  &
             .or. ( sMethodName .strequal. "FAO-56" ) ) then

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

    elseif ( sCmdText .contains. "FOG" ) then

      if ( sMethodName .strequal. "MONTHLY_GRID" ) then

        this%init_fog => model_initialize_fog_monthly_grid
        this%calc_fog => model_calculate_fog_monthly_grid

        call LOGS%WRITE( "==> MONTHLY_GRID FOG submodel selected.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        this%init_fog => model_initialize_fog_none
        this%calc_fog => model_calculate_fog_none

        call LOGS%WRITE( "==> NULL FOG submodel selected (i.e. no fog term).", iLogLevel = LOG_ALL, lEcho = lFALSE )

      endif

    elseif ( sCmdText .contains. "IRRIGATION" ) then

      if ( ( sMethodName .strequal. "FAO56" )  &
             .or. ( sMethodName .strequal. "FAO-56" ) ) then

        this%init_irrigation => model_initialize_irrigation
        this%calc_irrigation => model_calculate_irrigation

        ! when irrigation is invoked, we need to track GDD as a possible means
        ! by which to update and model the seasonal change in crop coefficient (Kcb)
        this%init_GDD => model_initialize_GDD
        this%calc_GDD => model_calculate_GDD

        call LOGS%WRITE( "==> IRRIGATION will be calculated and applied as needed.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        this%init_irrigation => model_initialize_irrigation_none
        this%calc_irrigation => model_calculate_irrigation_none

        call LOGS%WRITE( "==> IRRIGATION will *NOT* be active.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      endif


    elseif ( sCmdText .contains. "SOIL_STORAGE_MAX" ) then

      if ( sMethodName .strequal. "GRIDDED" ) then

        this%init_soil_storage_max => model_initialize_soil_storage_max_gridded

        call LOGS%WRITE( "==> SOIL_STORAGE_MAX will be read from a grid. Rooting depths will be recalculated" &
          //" as ~SOIL_STORAGE_MAX / AWC.", iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        this%init_soil_storage_max => model_initialize_soil_storage_max_internally_calculated

        call LOGS%WRITE( "==> SOIL_STORAGE_MAX will be internally calculated from the given AWC and rooting depth values.",   &
          iLogLevel = LOG_ALL, lEcho = lFALSE )

      endif


    elseif ( sCmdText .contains. "EVAPOTRANSPIRATION" ) then

      if ( ( sMethodName .strequal. "HARGREAVES" ) &
           .or. ( sMethodName .strequal. "HARGREAVES-SAMANI" ) ) then

        this%init_reference_et => model_initialize_et_hargreaves
        this%calc_reference_et => model_calculate_et_hargreaves

        call LOGS%WRITE( "==> HARGREAVES-SAMANI EVAPOTRANSPIRATION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( sMethodName .strequal. "JENSEN-HAISE" ) &
           .or. ( sMethodName .strequal. "JH" ) ) then

        this%init_reference_et => model_initialize_et_jensen_haise
        this%calc_reference_et => model_calculate_et_jensen_haise

        call LOGS%WRITE( "==> JENSEN_HAISE EVAPOTRANSPIRATION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( sMethodName .strequal. "MONTHLY_GRID" ) &
           .or. ( sMethodName .strequal. "MONTHLY_GRIDDED" ) ) then

        this%init_reference_et => model_initialize_et_monthly_grid
        this%calc_reference_et => model_calculate_et_monthly_grid

        call LOGS%WRITE( "==> MONTHLY_GRID EVAPOTRANSPIRATION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( sMethodName .strequal. "DAILY_GRID" ) &
           .or. ( sMethodName .strequal. "DAILY_GRIDDED" ) ) then

        this%init_reference_et => model_initialize_et_daily_grid
        this%calc_reference_et => model_calculate_et_daily_grid

        call LOGS%WRITE( "==> DAILY_GRID EVAPOTRANSPIRATION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported EVAPOTRANSPIRATION method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .contains. "PRECIPITATION" ) then

      if ( ( sMethodName .strequal. "NORMAL" )                &
           .or. ( sMethodName .strequal. "GRIDDED" )            &
           .or. ( sMethodName .strequal. "STANDARD" ) ) then

        this%init_precipitation_data => model_initialize_precip_normal
        this%get_precipitation_data => model_get_precip_normal

        call LOGS%WRITE( "==> STANDARD PRECIPITATION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( sMethodName .strequal. "METHOD_OF_FRAGMENTS" ) &
           .or. ( sMethodName .strequal. "FRAGMENTS" ) ) then

        this%init_precipitation_data => model_initialize_precip_method_of_fragments
        this%get_precipitation_data => model_get_precip_method_of_fragments

        call LOGS%WRITE( "==> METHOD OF FRAGMENTS PRECIPITATION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported PRECIPITATION method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .contains. "DIRECT_RECHARGE" ) then

      if ( ( sMethodName .strequal. "EXTERNAL" )                                               &
           .or. ( sMethodName .strequal. "GRIDDED" ) ) then

        this%init_direct_recharge => model_initialize_direct_recharge_gridded
        this%calc_direct_recharge => model_calculate_direct_recharge_gridded

        call LOGS%WRITE( "==> GRIDDED values for water main leakage (etc.) will be used.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( sMethodName .strequal. "NONE") then

        ! no action needed; defaults to null method

      else

        call warn("Your control file specifies an unknown or unsupported DIRECT_RECHARGE method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .contains. "SOIL_MOISTURE" ) then

      if ( ( sMethodName .strequal. "T-M" ) .or. ( sMethodName .strequal. "THORNTHWAITE-MATHER" )      &
             .or. ( sMethodName .strequal. "THORNTHWAITE") ) then

        this%init_actual_et => model_initialize_actual_et_thornthwaite_mather
        this%calc_actual_et => model_calculate_actual_et_thornthwaite_mather

        call LOGS%WRITE( "==> THORNTHWAITE-MATHER SOIL MOISTURE RETENTION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      elseif ( ( sMethodName .strequal. "FAO56" ) .or. ( sMethodName .strequal. "FAO-56" ) ) then

        this%init_actual_et => model_initialize_actual_et_fao56
        this%calc_actual_et => model_calculate_actual_et_fao56

        call LOGS%WRITE( "==> FAO-56 SOIL MOISTURE RETENTION submodel selected.", &
            iLogLevel = LOG_ALL, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported SOIL_MOISTURE method.", &
          lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

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

    use interception__bucket, only  : interception_bucket_calculate, IS_GROWING_SEASON, &
                                      interception_bucket_update_growing_season

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call interception_bucket_update_growing_season( landuse_index=this%landuse_index,        &
                                                    GDD=this%GDD,                            &
                                                    mean_air_temp=this%tmean,                &
                                                    it_is_growing_season=IS_GROWING_SEASON )

    this%interception = interception_bucket_calculate( this%landuse_index, this%rainfall, this%fog, &
                                                         this%canopy_cover_fraction, IS_GROWING_SEASON )

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

  end subroutine model_initialize_routing_none  

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_routing_none(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! no routing to be done; just call the runoff calculation routine and move on
    this%runon = 0.0_c_float
    call this%calc_runoff()

  end subroutine model_calculate_routing_none  

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_routing_D8(this)

    use routing__D8

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call routing_D8_initialize( this%active )

  end subroutine model_initialize_routing_D8  

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_routing_D8(this)

    use routing__D8, only   : TARGET_INDEX, ORDER_INDEX

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: index

    this%runon=0.0_c_float

    do index=lbound( ORDER_INDEX, 1 ), ubound( ORDER_INDEX, 1 )

      call this%calc_runoff( index )

      if ( (    TARGET_INDEX( index ) >= lbound( ORDER_INDEX, 1) ) &
        .and. ( TARGET_INDEX( index ) <= ubound( ORDER_INDEX, 1) ) ) then

        this%runon( TARGET_INDEX( index ) ) = this%runoff( ORDER_INDEX( index ) )

      endif  
      
    enddo

  end subroutine model_calculate_routing_D8  

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

    call runoff_curve_number_initialize()

  end subroutine model_initialize_runoff_curve_number

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_runoff_curve_number(this, index )

    use runoff__curve_number, only  : runoff_curve_number_calculate

    class (MODEL_DOMAIN_T), intent(inout)       :: this
    integer (kind=c_int), intent(in), optional  :: index

    real (kind=c_float) :: interim_inflow

    if ( present(index) ) then

      interim_inflow = this%runon( index ) + this%rainfall( index )   &
                      + this%snowmelt( index ) - this%interception( index )

      call runoff_curve_number_calculate(runoff=this%runoff( index ),                                    &
                                                    landuse_index=this%landuse_index( index ),           &
                                                    soil_group=this%soil_group( index ),                 &
                                                    soil_storage=this%soil_storage( index ),             &
                                                    soil_storage_max=this%soil_storage_max( index ),     & 
                                                    inflow=interim_inflow,                               &
                                                    continuous_frozen_ground_index=                      &
                                                        this%continuous_frozen_ground_index( index ) ) 

    else

      call runoff_curve_number_calculate(runoff=this%runoff,                                             &
                                                    landuse_index=this%landuse_index,                    &
                                                    soil_group=this%soil_group,                          &
                                                    soil_storage=this%soil_storage,                      &
                                                    soil_storage_max=this%soil_storage_max,              & 
                                                    inflow=this%runon+this%rainfall                      &
                                                           +this%snowmelt-this%interception,             &
                                                    continuous_frozen_ground_index=                      &
                                                        this%continuous_frozen_ground_index ) 

    endif

  end subroutine model_calculate_runoff_curve_number

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_runoff_gridded_values(this)

    use runoff__gridded_values

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: status

    allocate( this%monthly_runoff( count( this%active ) ), stat=status)
    call assert( status==0, "Problem allicating memory", __FILE__, __LINE__ )

    call runoff_gridded_values_initialize( this%active )
    call runoff_gridded_values_update_ratios( )

    print *, __FILE__, ": ", __LINE__
    call minmaxmean( RUNOFF_RATIOS, "RUNOFF_RATIOS" ) 
    call minmaxmean( this%monthly_runoff, "MONTHLY_RUNOFF")
    call minmaxmean( this%monthly_gross_precip, "MONTHLY_RAINFALL")


  end subroutine model_initialize_runoff_gridded_values

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_runoff_gridded_values(this, index )

    use runoff__gridded_values
    use datetime, only           : DATETIME_T

    class (MODEL_DOMAIN_T), intent(inout)       :: this
    integer (kind=c_int), intent(in), optional  :: index

    ! [ LOCALS ]
    type (DATETIME_T), save     :: date_of_last_grid_update
    real (kind=c_float)         :: interim_inflow

    if ( .not. ( date_of_last_grid_update == SIM_DT%curr ) ) then
      call runoff_gridded_values_update_ratios( )
      date_of_last_grid_update = SIM_DT%curr
    endif

    if ( present( index ) ) then
  
      interim_inflow = this%rainfall( index ) + this%snowmelt( index ) 

      this%runoff( index ) = interim_inflow * RUNOFF_RATIOS( index )

    else

      this%runoff = ( this%rainfall + this%snowmelt ) * RUNOFF_RATIOS

    endif

    if ( allocated( this%monthly_gross_precip ) )  &
      this%monthly_runoff = this%monthly_gross_precip * RUNOFF_RATIOS

  end subroutine model_calculate_runoff_gridded_values

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_soil_storage_max_internally_calculated(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    type ( GENERAL_GRID_T ), pointer :: pTempGrd

    !> @todo this should be in its own routine...
    this%current_rooting_depth = pack( ROOTING_DEPTH_MAX, MODEL%active )
    this%rooting_depth_max = pack( ROOTING_DEPTH_MAX, MODEL%active )

    this%soil_storage_max = this%rooting_depth_max * this%awc

    pTempGrd => grid_Create( iNX=this%number_of_columns, iNY=this%number_of_rows, &
        rX0=this%X_ll, rY0=this%Y_ll, &
        rGridCellSize=this%gridcellsize, iDataType=GRID_DATATYPE_REAL )  

    pTempGrd%rData = unpack( this%soil_storage_max, this%active, this%nodata_fill_value )

    call grid_WriteArcGrid( sFilename="Soil_Storage_Maximum__as_calculated_inches.asc", pGrd=pTempGrd )

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
        call die("A SOIL_STORAGE_MAX grid must be supplied in order to make use of this option.", &
           __FILE__, __LINE__)

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
      __FILE__, __LINE__ )

    call irrigation__initialize( this%irrigation_mask, this%active )

  end subroutine model_initialize_irrigation

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_irrigation( this )

  use irrigation

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: index

    if ( allocated( this%monthly_gross_precip ) .and. allocated( this%monthly_runoff ) ) then

      call irrigation__calculate( irrigation_amount=this%irrigation,                             &
                                  landuse_index=this%landuse_index,                              &
                                  soil_storage=this%soil_storage,                                & 
                                  soil_storage_max=this%soil_storage_max,                        &
                                  rainfall=this%rainfall,                                        &
                                  runoff=this%runoff,                                            &
                                  crop_etc=this%crop_etc,                                        &
                                  irrigation_mask=this%irrigation_mask,                          &
                                  num_days_since_planting=this%number_of_days_since_planting,    &
                                  monthly_rainfall=this%monthly_gross_precip,                    &
                                  monthly_runoff=this%monthly_runoff )

      call minmaxmean( this%monthly_runoff, "MONTHLY_RUNOFF")
      call minmaxmean( this%monthly_gross_precip, "MONTHLY_RAINFALL")

    else


      call irrigation__calculate( irrigation_amount=this%irrigation,                             &
                                  landuse_index=this%landuse_index,                              &
                                  soil_storage=this%soil_storage,                                & 
                                  soil_storage_max=this%soil_storage_max,                        &
                                  rainfall=this%rainfall,                                        &
                                  runoff=this%runoff,                                            &
                                  crop_etc=this%crop_etc,                                        &
                                  irrigation_mask=this%irrigation_mask,                          &
                                  num_days_since_planting=this%number_of_days_since_planting )
    endif  

  end subroutine model_calculate_irrigation

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

  subroutine model_read_available_water_content_table( this )

    use awc__table_values

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call awc_table_values_read( fRooting_Depth=ROOTING_DEPTH_MAX )

  end subroutine model_read_available_water_content_table  

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_available_water_content_table( this )

    use awc__table_values

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int)            :: iStat
    type (GENERAL_GRID_T), pointer  :: pTempGrd

    pTempGrd => grid_Create( iNX=this%number_of_columns, iNY=this%number_of_rows, &
      rX0=this%X_ll, rY0=this%Y_ll, &
      rGridCellSize=this%gridcellsize, iDataType=GRID_DATATYPE_REAL )  


    allocate ( this%soil_code (count( this%active ) ), stat=iStat )

    call awc_table_values_initialize( lActive=this%active,                        &
                                      fAWC=this%awc,                              &
                                      iSoils_Code=this%soil_code )

    pTempGrd%rData = unpack( this%awc, this%active, this%nodata_fill_value )

    call grid_WriteArcGrid( sFilename="Available_water_content__as_calculated_inches_per_foot.asc", pGrd=pTempGrd )

    call grid_Destroy( pTempGrd )

  end subroutine model_initialize_available_water_content_table  

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_GDD_none( this )

    use growing_degree_day

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    !> Nothing here to see. 

  end subroutine model_initialize_GDD_none

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_GDD_none( this )

    use growing_degree_day, only : GDD, GDD_BASE, GDD_MAX, growing_degree_day_calculate

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    !> Nothing here to see. 

  end subroutine model_calculate_GDD_none

!--------------------------------------------------------------------------------------------------

  subroutine model_output_GDD_none( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    !> Nothing here to see. 

  end subroutine model_output_GDD_none

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_GDD( this )

    use growing_degree_day

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call growing_degree_day_initialize( lActive=this%active,                               &
                                        iLanduseIndex=this%landuse_index,                  &
                                        dX=this%X,                                         & 
                                        dY=this%Y,                                         &
                                        dX_lon=this%X_lon,                                 &
                                        dY_lat=this%Y_lat,                                 &
                                        output_directory_name=this%output_directory_name )

  end subroutine model_initialize_GDD

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_GDD( this )

    use growing_degree_day, only : GDD, GDD_BASE, GDD_MAX, GDD_RESET_DATE, growing_degree_day_calculate

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call growing_degree_day_calculate( fGDD=GDD,                                            &
                                       iGDD_Reset_DOY=GDD_RESET_DATE( this%landuse_index ), &
                                       fTMean=this%tmean,                                   &
                                       fT_GDD_Base=GDD_BASE,                                &
                                       fT_GDD_Max=GDD_MAX      )

  end subroutine model_calculate_GDD

!--------------------------------------------------------------------------------------------------

  subroutine model_output_GDD( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    !> Nothing here to see. 

  end subroutine model_output_GDD

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_actual_et_thornthwaite_mather(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this


  end subroutine model_initialize_actual_et_thornthwaite_mather    

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_actual_et_thornthwaite_mather( this )

    use actual_et__thornthwaite_mather

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    real (kind=c_float)   :: actual_et_2( ubound( this%actual_et_soil,1 ) )
    real (kind=c_float)   :: actual_et_3( ubound( this%actual_et_soil,1 ) )    
    real (kind=c_float)   :: diff
    integer (kind=c_int)  :: indx

    call calculate_actual_et_thornthwaite_mather( actual_et=this%actual_et_soil,                    &
                                                  actual_et_2=actual_et_2,  &  
                                                  actual_et_3=actual_et_3,  &
                                                  soil_storage=this%soil_storage,                   &
                                                  soil_storage_max=this%soil_storage_max,           &
                                                  precipitation=this%infiltration,                  &
                                                  crop_etc=this%crop_etc )

!     do indx=1,ubound( this%actual_et_soil,1) 
!       if ( actual_et_2( indx ) > 0.0 ) then  
!         diff = this%actual_et_soil(indx) - actual_et_2(indx) 
!         if ( abs(diff) > 1.0e-3 )                                                 &                                        
!         print *, indx, this%actual_et_soil( indx ), actual_et_2( indx ),          &
!           actual_et_3(indx),                                                      &
!           diff,                                                                   &
!           this%soil_storage( indx ), this%soil_storage_max( indx ),               &
!           this%infiltration( indx ), this%crop_etc( indx ), this%landuse_code( indx )
!       endif    
!     enddo  


  end subroutine model_calculate_actual_et_thornthwaite_mather

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_actual_et_fao56(this)

    use actual_et__fao56

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call initialize_actual_et_fao56()

  end subroutine model_initialize_actual_et_fao56    

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_actual_et_fao56( this )

    use actual_et__fao56

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call calculate_actual_et_fao56( actual_et=this%actual_et_soil,                                    &
                                    adjusted_depletion_fraction_p=this%adjusted_depletion_fraction_p, &
                                    soil_storage=this%soil_storage,                                   &
                                    soil_storage_max=this%soil_storage_max,                           &
                                    infiltration=this%infiltration,                                   &
                                    crop_etc=this%crop_etc,                                           &
                                    depletion_fraction_p=DEPLETION_FRACTION( this%landuse_index ) )

  end subroutine model_calculate_actual_et_fao56

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_direct_recharge_none ( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_direct_recharge_none

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_direct_recharge_none ( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_calculate_direct_recharge_none

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_direct_recharge_gridded ( this )

    use direct_recharge__gridded_data

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call direct_recharge_initialize( lActive=this%active, iLandUseIndex=this%landuse_index,   &
                                     dX=this%X, dY=this%Y,                                    &
                                     dX_lon=this%X_lon , dY_lat=this%Y_lat )

  end subroutine model_initialize_direct_recharge_gridded

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_direct_recharge_gridded ( this )

    use direct_recharge__gridded_data

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call direct_recharge_calculate( direct_recharge = this%direct_recharge,      &
                                    iLanduse_Index=this%landuse_index,           &
                                    lActive=this%active,                         &
                                    nodata_fill_value=this%nodata_fill_value )

  end subroutine model_calculate_direct_recharge_gridded


!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_crop_coefficient_none(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_crop_coefficient_none

!--------------------------------------------------------------------------------------------------

  subroutine model_update_crop_coefficient_none(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    this%crop_coefficient_kcb = 1.0_c_float

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

    call crop_coefficients_FAO56_calculate( Kcb=this%crop_coefficient_kcb,      &
                                            GDD=this%gdd,                       &
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

    call fog_monthly_grid_initialize( lActive=this%active, dX=this%X, dY=this%Y,                &
      dX_lon=this%X_lon , dY_lat=this%Y_lat, output_directory_name=this%output_directory_name )

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
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear
  
      call pTMAX%getvalues( iMonth, iDay, iYear, iJulianDay )

    end associate

    if (.not. associated(pTMAX%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: Call to NULL pointer.", __FILE__, __LINE__)

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
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear
  
      call pTMIN%getvalues( iMonth, iDay, iYear, iJulianDay )

    end associate

    if (.not. associated(pTMIN%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: Call to NULL pointer.", __FILE__, __LINE__)

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
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear
  
      ! next three statements retrieve the data from the raw or native form
      call pPRCP%getvalues( iMonth, iDay, iYear, iJulianDay )

    end associate

    if (.not. associated(pPRCP%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: Call to NULL pointer.", __FILE__, __LINE__)

    this%gross_precip = pack( pPRCP%pGrdBase%rData, this%active )

  end subroutine model_get_precip_normal  

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_precip_method_of_fragments(this)

    use precipitation__method_of_fragments

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: status

    allocate( this%monthly_gross_precip( count( this%active ) ), stat=status)
    call assert( status==0, "Problem allocating memory", __FILE__, __LINE__ )

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
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

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
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer.", __FILE__, __LINE__)

    call precipitation_method_of_fragments_calculate( this%active )

    this%gross_precip = pack( pPRCP%pGrdBase%rData, this%active ) * FRAGMENT_VALUE * RAINFALL_ADJUST_FACTOR
    this%monthly_gross_precip = pack( pPRCP%pGrdBase%rData, this%active ) * RAINFALL_ADJUST_FACTOR

      print *, __FILE__, ": ", __LINE__
      call minmaxmean( this%monthly_gross_precip, "MONTHLY_RAINFALL")


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
    call minmaxmean( this%potential_recharge, "potential_recharge")

  end subroutine summarize_state_variables_sub

!--------------------------------------------------------------------------------------------------

  subroutine dump_model_values_by_cell_sub( this, colnum, rownum )

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    integer (kind=c_int), intent(in)       :: colnum
    integer (kind=c_int), intent(in)       :: rownum    

    ! [ LOCALS ]
    integer (kind=c_int)   :: indx

    indx = this%row_column_to_index( col_num=colnum, row_num=rownum)

    if ( (indx > lbound( this%landuse_code, 1) ) .and. ( indx <= ubound( this%landuse_code, 1) ) ) then

      write(*,fmt="('Dump of variable values for cell: ',i8,'  row: ',i8,'   col: ',i8)") indx, rownum, colnum
      write(*,fmt="(a)") repeat('-',80)
      write(*,fmt="('LU code:',t25,i8)") this%landuse_code( indx )                     
      write(*,fmt="('LU index:',t25,i8)")this%landuse_index( indx )
      write(*,fmt="('index order:',t25,i8)") this%index_order( indx )               
      write(*,fmt="('soil group:',t25,i8)")this%soil_group( indx )            
      write(*,fmt="('# upslope connections:',t25,i8)")this%num_upslope_connections( indx )
      write(*,fmt="('# upslope cells:',t25,i8)")this%sum_upslope_cells( indx )  

      write(*,fmt="('avail. water. capy.:',t25,f14.3)")this%awc( indx )        
      write(*,fmt="('Latitude:',t25,f14.3)")this%latitude( indx )                      
      write(*,fmt="('Reference ET0:',t25,f14.3)")this%reference_ET0( indx )                 
      write(*,fmt="('Actual ET:',t25,f14.3)")this%actual_ET( indx )            
      write(*,fmt="('inflow:',t25,f14.3)")this%inflow( indx )                
      write(*,fmt="('runon:',t25,f14.3)")this%runon( indx )                   
      write(*,fmt="('runoff:',t25,f14.3)")this%runoff( indx )                    
      write(*,fmt="('outflow:',t25,f14.3)") this%outflow( indx )                   
      write(*,fmt="('infiltration:',t25,f14.3)") this%infiltration( indx )                  
      write(*,fmt="('snowfall:',t25,f14.3)") this%snowfall( indx )             
      write(*,fmt="('potential snowmelt:',t25,f14.3)") this%potential_snowmelt( indx )     
      write(*,fmt="('snowmelt:',t25,f14.3)") this%snowmelt( indx )                 

      write(*,fmt="('interception:',t25,f14.3)") this%interception( indx )                 
      write(*,fmt="('rainfall:',t25,f14.3)") this%rainfall( indx )             
      write(*,fmt="('intercept. stor.:',t25,f14.3)") this%interception_storage( indx )
      write(*,fmt="('tmax:',t25,f14.3)") this%tmax( indx )
      write(*,fmt="('tmin:',t25,f14.3)") this%tmin( indx )
      write(*,fmt="('tmean:',t25,f14.3)") this%tmean( indx )            
      write(*,fmt="('snow storage:',t25,f14.3)") this%snow_storage( indx )     
      write(*,fmt="('soil storage:',t25,f14.3)") this%soil_storage( indx )             
      write(*,fmt="('soil storage max:',t25,f14.3)") this%soil_storage_max( indx )             
      write(*,fmt="('surface storage:',t25,f14.3)") this%surface_storage( indx )        
      write(*,fmt="('surface storage excess:',t25,f14.3)") this%surface_storage_excess( indx )          
      write(*,fmt="('surface storage max:',t25,f14.3)") this%surface_storage_max( indx )   

      write(*,fmt="('potential recharge:',t25,f14.3)") this%potential_recharge( indx )         
      write(*,fmt="('fog:',t25,f14.3)") this%fog( indx )       
      write(*,fmt="('irrigation:',t25,f14.3)") this%irrigation( indx )                      
      write(*,fmt="('GDD:',t25,f14.3)") this%gdd( indx )              
      write(*,fmt="('runoff outside:',t25,f14.3)") this%runoff_outside( indx )
      write(*,fmt="('pervious fraction:',t25,f14.3)") this%pervious_fraction( indx )           
      write(*,fmt="('storm drain capture:',t25,f14.3)") this%storm_drain_capture( indx )      
      write(*,fmt="('canopy cover fraction:',t25,f14.3)") this%canopy_cover_fraction( indx )      
      write(*,fmt="('crop coefficient Kcb:',t25,f14.3)") this%crop_coefficient_kcb( indx )    
      write(*,fmt="('CFGI:',t25,f14.3)") this%continuous_frozen_ground_index( indx )
      write(*,fmt="('rooting depth max:',t25,f14.3)") this%rooting_depth_max( indx )
      write(*,fmt="('current rooting depth',t25,f14.3)") this%current_rooting_depth( indx )        
      write(*,fmt="('polygon id:',t25,i8)") this%polygon_id( indx )    
      write(*,fmt="('actual_et_soil:',t25,f14.3)") this%actual_et_soil( indx )               
      write(*,fmt="('actual_et_impervious:',t25,f14.3)") this%actual_et_impervious( indx )           
      write(*,fmt="('actual_et_interception:',t25,f14.3)") this%actual_et_interception( indx )     
      write(*,fmt="('adjusted depletion frac p:',t25,f14.3)") this%adjusted_depletion_fraction_p( indx )   
      write(*,fmt="('crop Etc:',t25,f14.3)") this%crop_etc( indx )    
      write(*,fmt="('direct recharge:',t25,f14.3)") this%direct_recharge( indx )                 
      write(*,fmt="('days since planting:',t25,i8)") this%number_of_days_since_planting( indx )
      
    endif  

  end subroutine dump_model_values_by_cell_sub

!--------------------------------------------------------------------------------------------------

end module model_domain