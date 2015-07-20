module model_domain

  use iso_c_binding
  use continuous_frozen_ground_index
  use data_catalog
  use data_catalog_entry
  use exceptions
  use simulation_datetime
  use snowfall__original
  use string_list, only      : STRING_LIST_T
  use parameters, only       : PARAMS, PARAMS_DICT 
  use netcdf4_support, only  : NC_FILL_FLOAT
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

    character (len=:), allocatable     :: PROJ4_string
    integer (kind=c_int)               :: number_of_columns
    integer (kind=c_int)               :: number_of_rows
    real (kind=c_double)               :: X_ll, Y_ll
    real (kind=c_double)               :: X_ur, Y_ur
    real (kind=c_double), allocatable  :: X(:), Y(:)
    real (kind=c_double)               :: gridcellsize

    !> @todo: why do we need a BNDS data structure that duplicates the project boundary
    !!        information present in the MODEL_DOMAIN_T data structure??

    logical (kind=c_bool), allocatable     :: active(:,:)
    real (kind=c_float), allocatable       :: dont_care(:,:)
    real (kind=c_float), allocatable       :: array_output(:,:)

    integer (kind=c_int), allocatable      :: landuse_code(:)
    integer (kind=c_int), allocatable      :: landuse_index(:)
    integer (kind=c_int), allocatable      :: soil_group(:)
    integer (kind=c_int), allocatable      :: num_upslope_connections(:)
    integer (kind=c_int), allocatable      :: sum_upslope_cells(:)

    real (kind=c_float), allocatable       :: awc(:)
    real (kind=c_float), allocatable       :: gdd(:)

    real (kind=c_float), allocatable       :: rooting_depth(:)
     
    real (kind=c_float), allocatable       :: latitude(:)
    real (kind=c_float), allocatable       :: reference_ET0(:)
    real (kind=c_float), allocatable       :: reference_ET0_adj(:)
    real (kind=c_float), allocatable       :: interception_ET(:)
    real (kind=c_float), allocatable       :: actual_ET(:)
    real (kind=c_float), allocatable       :: inflow(:)
    real (kind=c_float), allocatable       :: runon(:)
    real (kind=c_float), allocatable       :: runoff(:)
    real (kind=c_float), allocatable       :: runoff_outside(:)
    real (kind=c_float), allocatable       :: outflow(:)
    real (kind=c_float), allocatable       :: infiltration(:)
    real (kind=c_float), allocatable       :: snowmelt(:)
    real (kind=c_float), allocatable       :: interception(:)
     
    real (kind=c_float), allocatable       :: interception_storage(:)
    real (kind=c_float), allocatable       :: snow_storage(:)
    real (kind=c_float), allocatable       :: soil_storage(:)
    real (kind=c_float), allocatable       :: soil_storage_max(:)
    real (kind=c_float), allocatable       :: potential_recharge(:)
    real (kind=c_float), allocatable       :: current_rooting_depth(:)
         
    real (kind=c_float), allocatable       :: gross_precip(:)
    real (kind=c_float), allocatable       :: fog(:)
    real (kind=c_float), allocatable       :: rainfall(:)
    real (kind=c_float), allocatable       :: snowfall(:)
    real (kind=c_float), allocatable       :: irrigation(:)

    real (kind=c_float), allocatable       :: tmin(:)
    real (kind=c_float), allocatable       :: tmax(:)
    real (kind=c_float), allocatable       :: tmean(:)
    real (kind=c_float), allocatable       :: routing_fraction(:)

    integer (kind=c_int), allocatable      :: index_order(:)

    !> declare and initialize procedure pointers such that the default methods are in place
    procedure ( simple_method ), pointer         :: init_interception       => model_initialize_interception_bucket
    procedure ( simple_method ), pointer         :: init_runoff             => model_initialize_runoff_curve_number
    procedure ( simple_method ), pointer         :: init_reference_et       => model_initialize_et_hargreaves
    procedure ( simple_method ), pointer         :: init_routing            => model_initialize_routing_D8
    procedure ( simple_method ), pointer         :: init_soil_moisture      => model_initialize_soil_moisture_thornthwaite_mather
    procedure ( simple_method ), pointer         :: init_snowfall           => model_initialize_snowfall_original
    procedure ( simple_method ), pointer         :: init_snowmelt           => model_initialize_snowmelt_original
    procedure ( simple_method ), pointer         :: init_precipitation_data => model_initialize_precip_normal
    procedure ( simple_method ), pointer         :: init_fog                => model_initialize_fog_none
    procedure ( simple_method ), pointer         :: init_irrigation         => model_initialize_irrigation_none
    procedure ( simple_method ), pointer         :: init_direct_recharge  => model_initialize_direct_recharge_none
    procedure ( simple_method ), pointer         :: init_GDD                => model_initialize_GDD_none

    procedure ( simple_method ), pointer         :: calc_interception      => model_calculate_interception_bucket

    procedure ( simple_method_w_optional ), pointer   :: calc_runoff       => model_calculate_runoff_curve_number
    
    procedure ( simple_method ), pointer         :: calc_reference_et      => model_calculate_et_hargreaves
    procedure ( simple_method ), pointer         :: calc_routing           => model_calculate_routing_D8

    procedure ( simple_method_w_optional ), pointer  :: calc_soil_moisture => model_calculate_soil_moisture_thornthwaite_mather
    
    procedure ( simple_method ), pointer         :: calc_snowfall          => model_calculate_snowfall_original
    procedure ( simple_method ), pointer         :: calc_snowmelt          => model_calculate_snowmelt_original  
    procedure ( simple_method ), pointer         :: calc_fog               => model_calculate_fog_none
    procedure ( simple_method ), pointer         :: calc_irrigation        => model_calculate_irrigation_none
    procedure ( simple_method ), pointer         :: calc_GDD               => model_calculate_GDD_none
    procedure ( simple_method ), pointer         :: calc_direct_recharge => model_calculate_direct_recharge_none    

    procedure (simple_method), pointer           :: output_GDD             => model_output_GDD_none
    procedure (simple_method), pointer           :: output_soil_moisture   => model_output_irrigation_none
    procedure (simple_method), pointer           :: output_irrigation      => model_output_irrigation_none

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

    procedure :: set_method_sub
    generic   :: set_method => set_method_sub

    procedure :: set_inactive_cells_sub
    generic   :: set_inactive_cells => set_inactive_cells_sub

    procedure :: preflight_check_method_pointers
    
    procedure :: calculate_mass_balance_sub
    generic   :: solve => calculate_mass_balance_sub

    procedure :: calculate_interception_mass_balance_sub
    procedure :: calculate_snow_mass_balance_sub
    procedure :: calculate_soil_mass_balance_sub

    procedure :: read_in_available_water_content_sub
    generic   :: get_available_water_content => read_in_available_water_content_sub

    procedure :: read_in_land_use_sub
    generic   :: get_land_use => read_in_land_use_sub

    procedure :: read_in_soil_groups_sub
    generic   :: get_soil_groups => read_in_soil_groups_sub

    procedure :: get_climate_data
    procedure :: write_variables_to_netcdf

    procedure :: initialize_available_water_content_sub
    generic   :: initialize_available_water_content => initialize_available_water_content_sub

    procedure :: initialize_soil_layers_sub
    generic   :: initialize_soil_layers => initialize_soil_layers_sub

    procedure :: initialize_methods_sub
    generic   :: initialize_methods => initialize_methods_sub

    procedure :: initialize_netcdf_output_sub
    generic   :: initialize_netcdf_output => initialize_netcdf_output_sub

    procedure :: initialize_landuse_codes_sub
    generic   :: initialize_landuse => initialize_landuse_codes_sub

    procedure :: initialize_latitude_sub
    generic   :: initialize_latitude => initialize_latitude_sub

    procedure :: initialize_soil_groups_sub
    generic   :: initialize_soil_groups => initialize_soil_groups_sub

    procedure :: summarize_state_variables_sub
    generic   :: summarize => summarize_state_variables_sub
  
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

  type, public :: NETCDF_FILE_COLLECTION_T
    type (T_NETCDF4_FILE), pointer, public :: ncfile
  end type NETCDF_FILE_COLLECTION_T

  ! creating several module-level globals
  type (MODEL_DOMAIN_T), public :: MODEL

  type (NETCDF_FILE_COLLECTION_T), allocatable, public :: OUTPUT(:)

  real (kind=c_float), allocatable  :: MAX_ROOTING_DEPTH(:,:)

  type (GENERAL_GRID_T), pointer    :: pCOORD_GRD

  integer (kind=c_int), parameter   :: NCDF_NUM_OUTPUTS = 17

  enum, bind(c)
    enumerator :: NCDF_GROSS_PRECIPITATION=1, NCDF_RAINFALL, NCDF_SNOWFALL, &
                  NCDF_INTERCEPTION, NCDF_RUNON, NCDF_RUNOFF,               &
                  NCDF_SNOW_STORAGE, NCDF_SOIL_STORAGE,                     &
                  NCDF_REFERENCE_ET0,                                       &
                  NCDF_ACTUAL_ET, NCDF_SNOWMELT, NCDF_TMIN, NCDF_TMAX,      &
                  NCDF_POTENTIAL_RECHARGE, NCDF_INFILTRATION,               &
                  NCDF_IRRIGATION, NCDF_RUNOFF_OUTSIDE
  end enum 

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

    allocate(this%dont_care(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __FILE__, __LINE__)

    allocate(this%array_output(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __FILE__, __LINE__)

  end subroutine initialize_grid_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_arrays_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)        :: this

    ! [ LOCALS ]
    integer (kind=c_int)  :: iCount
    integer (kind=c_int)  :: iStat(31)

    iCount = count( this%active )

    allocate( this%landuse_code(iCount), stat=iStat(1) )
    allocate( this%landuse_index(iCount), stat=iStat(2) )
    allocate( this%soil_group(iCount), stat=iStat(3) )
    allocate( this%num_upslope_connections(iCount), stat=iStat(4) )
    allocate( this%sum_upslope_cells(iCount), stat=iStat(5) )
    allocate( this%awc(iCount), stat=iStat(6) )
    allocate( this%latitude(iCount), stat=iStat(7) )
    allocate( this%reference_ET0(iCount), stat=iStat(8) )
    allocate( this%reference_ET0_adj(iCount), stat=iStat(9) )
    allocate( this%interception_ET(iCount), stat=iStat(10) )
    allocate( this%actual_ET(iCount), stat=iStat(11) )
    allocate( this%inflow(iCount), stat=iStat(12))
    allocate( this%runon(iCount), stat=iStat(13) )
    allocate( this%runoff(iCount), stat=iStat(14) )
    allocate( this%outflow(iCount), stat=iStat(15) )
    allocate( this%infiltration(iCount), stat=iStat(16) )
    allocate( this%snowfall(iCount), stat=iStat(17) )
    allocate( this%snowmelt(iCount), stat=iStat(18) )
    allocate( this%interception(iCount), stat=iStat(19) )
    allocate( this%rainfall(iCount), stat=iStat(20) )
    allocate( this%interception_storage(iCount), stat=iStat(21) )
    allocate( this%snow_storage(iCount), stat=iStat(22) )
    allocate( this%soil_storage(iCount), stat=iStat(23) )
    allocate( this%soil_storage_max(iCount), stat=iStat(24) )
    allocate( this%potential_recharge(iCount), stat=iStat(25) )
    allocate( this%fog(iCount), stat=iStat(26) )
    allocate( this%irrigation(iCount), stat=iStat(27) )
    allocate( this%rooting_depth(iCount), stat=iStat(28) )
    allocate( this%index_order(iCount), stat=iStat(29) )
    allocate( this%gdd(iCount), stat=iStat(30) )
    allocate( this%runoff_outside( iCount ), stat=iStat(31) )

    if ( any( iStat /= 0 ) )  call die("Problem allocating memory", __FILE__, __LINE__)

  end subroutine initialize_arrays_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_methods_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    call this%init_interception
    call this%init_snowfall
    call this%init_snowmelt
    call this%init_fog
    call this%init_runoff
    if ( associated( this%init_routing) )  call this%init_routing
    call this%init_soil_moisture
    call this%init_reference_et
    call this%init_precipitation_data
    call this%init_GDD
    call this%init_irrigation
    call this%init_direct_recharge

  end subroutine initialize_methods_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_netcdf_output_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iIndex

    allocate ( OUTPUT( NCDF_NUM_OUTPUTS ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    do iIndex = 1, ubound(OUTPUT, 1)
      allocate ( OUTPUT(iIndex)%ncfile )
    enddo  

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_GROSS_PRECIPITATION )%ncfile, &
      sVariableName="gross_precipitation", sVariableUnits="inches_per_day",                   &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_RAINFALL )%ncfile,            &
      sVariableName="rainfall", sVariableUnits="inches_per_day",                              &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_INTERCEPTION )%ncfile,        &
      sVariableName="interception", sVariableUnits="inches_per_day",                          &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_RUNOFF )%ncfile,              &
      sVariableName="runoff", sVariableUnits="inches_per_day",                                &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_RUNOFF_OUTSIDE )%ncfile,      &
      sVariableName="runoff_outside", sVariableUnits="inches_per_day",                        &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_RUNON )%ncfile,               &
      sVariableName="runon", sVariableUnits="inches_per_day",                                 &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0   )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_INFILTRATION )%ncfile,        &
      sVariableName="infiltration", sVariableUnits="inches_per_day",                          &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_SNOWFALL )%ncfile,            &
      sVariableName="snowfall", sVariableUnits="inches_per_day",                              &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_SNOWMELT )%ncfile,            &
      sVariableName="snowmelt", sVariableUnits="inches_per_day",                              &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_SNOW_STORAGE )%ncfile,        &
      sVariableName="snow_storage", sVariableUnits="inches",                                  &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_SOIL_STORAGE )%ncfile,        &
      sVariableName="soil_storage", sVariableUnits="inches",                                  &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_POTENTIAL_RECHARGE )%ncfile,  &
      sVariableName="potential_recharge", sVariableUnits="inches",                            &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_REFERENCE_ET0 )%ncfile,       &
      sVariableName="reference_ET0", sVariableUnits="inches",                                 &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_ACTUAL_ET )%ncfile,           &
      sVariableName="actual_ET", sVariableUnits="inches",                                     &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_TMIN )%ncfile,                &
      sVariableName="tmin", sVariableUnits="degrees Fahrenheit",                              &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_TMAX )%ncfile,                &
      sVariableName="tmax", sVariableUnits="degrees Fahrenheit",                              &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT( NCDF_IRRIGATION )%ncfile,          &
      sVariableName="irrigation", sVariableUnits="inches_per_day",                            &
      iNX=this%number_of_columns, iNY=this%number_of_rows,                                    &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,                       &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

      this%dont_care = NC_FILL_FLOAT

  end subroutine initialize_netcdf_output_sub

!--------------------------------------------------------------------------------------------------
  
  subroutine set_inactive_cells_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this
    type (DATA_CATALOG_ENTRY_T), pointer :: pHSG
    type (DATA_CATALOG_ENTRY_T), pointer :: pLULC      
    type (DATA_CATALOG_ENTRY_T), pointer :: pAWC      


    pLULC => DAT%find("LAND_USE")
    pHSG => DAT%find("SOILS_GROUP")
    pAWC => DAT%find("AVAILABLE_WATER_CONTENT")
    
    if ( .not. associated(pHSG) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    if ( .not. associated(pHSG%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    if ( .not. allocated(pHSG%pGrdBase%iData) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of UNALLOCATED variable", __FILE__, __LINE__)

    if ( .not. associated(pAWC) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    if ( .not. associated(pAWC%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    if ( .not. allocated(pAWC%pGrdBase%rData) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of UNALLOCATED variable", __FILE__, __LINE__)

    if ( .not. associated(pLULC) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    if ( .not. associated(pLULC%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    if ( .not. allocated(pLULC%pGrdBase%iData) ) &
      call die("INTERNAL PROGRAMMING ERROR: attempted use of UNALLOCATED variable", __FILE__, __LINE__)

    this%active = .true._c_bool

    where (       ( pHSG%pGrdBase%iData  < 1 )      &
            .or.  ( pLULC%pGrdBase%iData < 0 )      &
            .or.  ( pAWC%pGrdBase%rData < 0.0 ) )

      this%active = .false._c_bool

    end where

    call LOGS%write(asCharacter(count(this%active))//" cells are currently active out of a total of " &
      //asCharacter(size(this%active)), iLinesBefore=1, iLinesAfter=1, iLogLevel=LOG_ALL)

  end subroutine set_inactive_cells_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_landuse_codes_sub( this )

    class (MODEL_DOMAIN_T), intent(inout)     :: this

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
    call PARAMS%get_parameters( slKeys=slList, iValues=iLanduseCodes )

    ! obtain a pointer to the LAND_USE grid
    pLULC => DAT%find("LAND_USE")

    if ( associated(pLULC) ) then

      ! call pLULC%getvalues()

      if (associated( pLULC%pGrdBase) ) then
        this%landuse_code = pack( pLULC%pGrdBase%iData, this%active )
      else
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)
      endif  
    else
      call die("Attempted use of NULL pointer. Failed to find LAND_USE data element.", &
        __FILE__, __LINE__)
    endif

    this%landuse_index = -9999
    iCount = 0
    

    do iIndex = 1, ubound(this%landuse_code,1)

      lMatch = lFALSE

      do iIndex2=1, ubound(iLandUseCodes, 1)

        if (this%landuse_code(iIndex) == iLandUseCodes(iIndex2) ) then
          this%landuse_index(iIndex) = iIndex2
          iCount = iCount + 1
          lMatch = lTRUE
          exit
        endif

      enddo

      if ( .not. lMatch ) &
        call LOGS%write("Failed to match landuse code "//asCharacter(this%landuse_code(iIndex) ) &
          //" with a corresponding landuse code from lookup tables.", iLogLevel=LOG_ALL )

    enddo    

    call LOGS%write("Matches were found between landuse grid value and table value for " &
      //asCharacter(iCount)//" cells out of a total of "//asCharacter(iIndex)//" active cells.", &
      iLinesBefore=1, iLinesAfter=1, iLogLevel=LOG_ALL)

    if ( count(this%landuse_index < 0) > 0 ) &
      call warn(asCharacter(count(this%landuse_index < 0))//" negative values are present" &
      //" in the landuse_index vector.", lFatal=lTRUE, sHints="Negative landuse INDEX values are the " &
      //"result of landuse values for which no match can be found between the grid file and lookup table.")

    call slList%clear()

  end subroutine initialize_landuse_codes_sub

!--------------------------------------------------------------------------------------------------  

  subroutine read_in_soil_groups_sub( this )

    class (MODEL_DOMAIN_T), intent(inout)     :: this

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iIndex
    type (DATA_CATALOG_ENTRY_T), pointer :: pHSG

    pHSG => DAT%find("SOILS_GROUP")
    
    if ( associated(pHSG) ) then

      call pHSG%getvalues()

    else
    
      call warn(sMessage="SOILS_GROUP dataset is flawed or missing.", lFatal=lTRUE,     &
        iLogLevel = LOG_ALL, sHints="Check to see that a valid path and filename have"  &
        //" been ~included in the control file for the SOILS_GROUP dataset.",           &
        lEcho = lTRUE )

    endif    

  end subroutine read_in_soil_groups_sub

!--------------------------------------------------------------------------------------------------  

  subroutine read_in_land_use_sub( this )

    class (MODEL_DOMAIN_T), intent(inout)     :: this

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iIndex
    type (DATA_CATALOG_ENTRY_T), pointer :: pLULC

    pLULC => DAT%find("LAND_USE")
    
    if ( associated(pLULC) ) then

      call pLULC%getvalues()

    else
    
      call warn(sMessage="LAND_USE dataset is flawed or missing.", lFatal=lTRUE,         &
        iLogLevel = LOG_ALL, sHints="Check to see that a valid path and filename have"   &
        //" been ~included in the control file for the LAND_USE dataset.",               &
        lEcho = lTRUE )

    endif    

    call grid_WriteArcGrid("Landuse_land_cover__as_read_into_SWB.asc", pLULC%pGrdBase )

  end subroutine read_in_land_use_sub

!--------------------------------------------------------------------------------------------------  

  subroutine read_in_available_water_content_sub( this )

    class (MODEL_DOMAIN_T), intent(inout)     :: this

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iIndex
    type (DATA_CATALOG_ENTRY_T), pointer :: pAWC

    pAWC => DAT%find("AVAILABLE_WATER_CONTENT")
    
    if ( associated(pAWC) ) then

      call pAWC%getvalues()

    else
    
      call warn(sMessage="AVAILABLE_WATER_CONTENT dataset is flawed or missing.", lFatal=lTRUE,     &
        iLogLevel = LOG_ALL, sHints="Check to see that a valid path and filename have"              &
        //" been ~included in the control file for the AVAILABLE_WATER_CONTENT dataset.",           &
        lEcho = lTRUE )

    endif    

  end subroutine read_in_available_water_content_sub

!--------------------------------------------------------------------------------------------------  

  subroutine initialize_soil_groups_sub( this )

    class (MODEL_DOMAIN_T), intent(inout)     :: this

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iIndex
    type (DATA_CATALOG_ENTRY_T), pointer :: pHSG

    pHSG => DAT%find("SOILS_GROUP")
    
    if ( associated(pHSG) ) then

      if (associated( pHSG%pGrdBase) ) then
        this%soil_group = pack( pHSG%pGrdBase%iData, this%active )
      else
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)
      endif  
    else
      call die("Attempted use of NULL pointer. Failed to find SOILS_GROUP data element.", &
        __FILE__, __LINE__)
    endif

    call LOGS%write("Soils hydrologic groups as read into SWB data structure", iLinesBefore=1, iLinesAfter=1, iLogLevel=LOG_DEBUG)

    do iIndex = 1, maxval(pHSG%pGrdBase%iData)

      call LOGS%write( asCharacter(count(MODEL%soil_group == iIndex) )//" cells belong to soils group " &
        //asCharacter(iIndex), iLogLevel=LOG_DEBUG )
      
    end do    

    call LOGS%write("", iLinesBefore=1, iLogLevel=LOG_DEBUG)

  end subroutine initialize_soil_groups_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_available_water_content_sub( this )

    class (MODEL_DOMAIN_T), intent(inout)     :: this

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iIndex
    type (DATA_CATALOG_ENTRY_T), pointer :: pAWC

    pAWC => DAT%find("AVAILABLE_WATER_CONTENT")
    
    if ( associated(pAWC) ) then

      call pAWC%getvalues()

      if (associated( pAWC%pGrdBase) ) then
        this%awc = pack( pAWC%pGrdBase%rData, this%active )
      else
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)
      endif  
    else
      call die("Attempted use of NULL pointer. Failed to find AVAILABLE_WATER_CONTENT data element.", &
        __FILE__, __LINE__)
    endif
          

  end subroutine initialize_available_water_content_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_latitude_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)     :: this

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iIndex

    pCOORD_GRD => grid_Create( iNX=this%number_of_columns, iNY=this%number_of_rows, &
        rX0=this%X_ll, rY0=this%Y_ll, &
        rGridCellSize=this%gridcellsize, iDataType=GRID_DATATYPE_REAL )  

    allocate ( this%X(this%number_of_columns ) )
    allocate ( this%Y(this%number_of_rows ) )

    ! call the grid routine to populate the X and Y values
    call grid_PopulateXY( pCOORD_GRD )

    ! populating these in order to have them available later for use in writing results to NetCDF
    this%X = pCOORD_GRD%rX( :, 1 )
    this%Y = pCOORD_GRD%rY( 1, : ) 

    ! transform to unprojected (lat/lon) coordinate system
    call grid_Transform(pGrd=pCOORD_GRD, sFromPROJ4=this%PROJ4_string, &
        sToPROJ4="+proj=lonlat +ellps=GRS80 +datum=WGS84 +no_defs" )
    
    this%latitude = pack( pCOORD_GRD%rY, this%active )

    pCOORD_GRD%rData=pCOORD_GRD%rX
    call grid_WriteArcGrid( sFilename="Longitude__calculated.asc", pGrd=pCOORD_GRD )
    pCOORD_GRD%rData=pCOORD_GRD%rY
    call grid_WriteArcGrid( sFilename="Latitude__calculated.asc", pGrd=pCOORD_GRD )


  end subroutine initialize_latitude_sub

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

      ! partition precipitation into rainfall and snowfall fractions
      call this%calc_snowfall()

      ! update growing degree day statistic
      call this%calc_GDD()

    end associate

  end subroutine get_climate_data

!--------------------------------------------------------------------------------------------------

  subroutine write_variables_to_netcdf(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    ! first put out the current time variable for all open NetCDF files
    do iIndex = 1, ubound( OUTPUT, 1 )


      call netcdf_put_variable_vector(NCFILE=OUTPUT(iIndex)%ncfile, &
         iVarID=OUTPUT(iIndex)%ncfile%iVarID(NC_TIME), &
         iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t)], &
         iCount=[1_c_size_t], &
         iStride=[1_c_ptrdiff_t], &
         dpValues=[real(SIM_DT%iNumDaysFromOrigin, kind=c_double)])

    enddo

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_GROSS_PRECIPITATION )%ncfile,     &
            iVarID=OUTPUT( NCDF_GROSS_PRECIPITATION )%ncfile%iVarID(NC_Z),                      &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%gross_precip,                                                          &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_INTERCEPTION )%ncfile,            &
            iVarID=OUTPUT( NCDF_INTERCEPTION )%ncfile%iVarID(NC_Z),                             &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%interception,                                                          &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_RUNOFF )%ncfile,                  &
            iVarID=OUTPUT( NCDF_RUNOFF )%ncfile%iVarID(NC_Z),                                   &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%runoff,                                                                &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_RUNOFF_OUTSIDE )%ncfile,          &
            iVarID=OUTPUT( NCDF_RUNOFF_OUTSIDE )%ncfile%iVarID(NC_Z),                           &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%runoff_outside,                                                        &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_RUNON )%ncfile,                   &
            iVarID=OUTPUT( NCDF_RUNON )%ncfile%iVarID(NC_Z),                                    &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%runon,                                                                 &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_INFILTRATION )%ncfile,            &
            iVarID=OUTPUT( NCDF_INFILTRATION )%ncfile%iVarID(NC_Z),                             &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%infiltration,                                                          &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_SNOWFALL )%ncfile,                &
            iVarID=OUTPUT( NCDF_SNOWFALL )%ncfile%iVarID(NC_Z),                                 &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%snowfall,                                                              &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_SNOWMELT )%ncfile,                &
            iVarID=OUTPUT( NCDF_SNOWMELT )%ncfile%iVarID(NC_Z),                                 &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%snowmelt,                                                              &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_SNOW_STORAGE )%ncfile,            &
            iVarID=OUTPUT( NCDF_SNOW_STORAGE )%ncfile%iVarID(NC_Z),                             &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%snow_storage,                                                          &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_SOIL_STORAGE )%ncfile,            &
            iVarID=OUTPUT( NCDF_SOIL_STORAGE )%ncfile%iVarID(NC_Z),                             &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%soil_storage,                                                          &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_POTENTIAL_RECHARGE )%ncfile,      &
            iVarID=OUTPUT( NCDF_POTENTIAL_RECHARGE )%ncfile%iVarID(NC_Z),                       &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%potential_recharge,                                                    &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_REFERENCE_ET0 )%ncfile,           &
            iVarID=OUTPUT( NCDF_REFERENCE_ET0 )%ncfile%iVarID(NC_Z),                            &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%reference_ET0,                                                         &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_ACTUAL_ET )%ncfile,               &
            iVarID=OUTPUT( NCDF_ACTUAL_ET )%ncfile%iVarID(NC_Z),                                &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%actual_et,                                                             &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_TMIN )%ncfile,                    &
            iVarID=OUTPUT( NCDF_TMIN )%ncfile%iVarID(NC_Z),                                     &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%tmin,                                                                  &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_TMAX )%ncfile,                    &
            iVarID=OUTPUT( NCDF_TMAX )%ncfile%iVarID(NC_Z),                                     &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%tmax,                                                                  &
            rField=this%dont_care )

    call netcdf_put_packed_variable_array(NCFILE=OUTPUT( NCDF_IRRIGATION )%ncfile,              &
            iVarID=OUTPUT( NCDF_IRRIGATION )%ncfile%iVarID(NC_Z),                               &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(this%number_of_rows, kind=c_size_t),                       &
                                int(this%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            lMask=this%active,                                                                  &
            rValues=this%irrigation,                                                            &
            rField=this%dont_care )

    call this%output_GDD()

  end subroutine write_variables_to_netcdf

!--------------------------------------------------------------------------------------------------

  subroutine calculate_mass_balance_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call this%calc_GDD()
    call this%calculate_interception_mass_balance_sub()
    call this%calculate_snow_mass_balance_sub()
    call this%calculate_soil_mass_balance_sub()

  end subroutine calculate_mass_balance_sub

!--------------------------------------------------------------------------------------------------

  subroutine preflight_check_method_pointers(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    if (.not. associated( this%init_interception) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_runoff) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_reference_et) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_snowfall) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_snowmelt) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_irrigation) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_GDD) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_interception) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%init_direct_recharge) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_direct_recharge) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_runoff) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_reference_et) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_snowfall) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_snowmelt) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_irrigation) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_GDD) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%get_precipitation_data ) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%get_minimum_air_temperature_data ) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%get_maximum_air_temperature_data ) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

  end subroutine preflight_check_method_pointers

!--------------------------------------------------------------------------------------------------

  subroutine calculate_interception_mass_balance_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    ! [ LOCALS ]
    real (kind=c_float)  :: fReferenceET_minus_interception

    call this%calc_reference_et()
    call this%calc_fog()
    call this%calc_interception()

!    this%interception_storage = this%interception_storage + this%interception
    this%reference_ET0_adj = max( 0.0_c_float, this%reference_ET0 - this%interception_ET )

  end subroutine calculate_interception_mass_balance_sub

!--------------------------------------------------------------------------------------------------

  subroutine calculate_snow_mass_balance_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    if (.not. allocated(CFGI) ) call initialize_continuous_frozen_ground_index( count( this%active ) )

    call update_continuous_frozen_ground_index( CFGI, this%tmin, this%tmax, this%snow_storage )

    call this%calc_snowmelt

    this%snow_storage = this%snow_storage + this%snowfall
    this%snow_storage = this%snow_storage - this%snowmelt

  end subroutine calculate_snow_mass_balance_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_soil_layers_sub( this )

    use strings
    use string_list

    class (MODEL_DOMAIN_T), intent(inout)   :: this

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

    iNumActiveCells = ubound(this%soil_storage_max,1)

    call slList%append("LU_Code")
    call slList%append("Landuse_Code")
    call slList%append("Landuse_Lookup_Code")

    !> Determine how many soil groups are present

    ! retrieve a string list of all keys associated with root zone depth (i.e. RZ_1, RZ_2, RZ_3, etc.)
    slRZ = PARAMS_DICT%grep_keys("RZ")
    ! Convert the string list to an vector of integers; this call strips off the "RZ_" part of label
    iRZ_SeqNums = slRZ%asInt()
    ! count how many items are present in the vector; this should equal the number of soils groups
    iNumberOfSoilGroups = count( iRZ_SeqNums > 0 )

    !> Determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=slList, iValues=iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes >= 0 )

    allocate( MAX_ROOTING_DEPTH(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for maximum rooting depth table", &
      __FILE__, __LINE__)

    ! we should have the max rooting depth table fully filled out following this block
    do iSoilsIndex = 1, iNumberOfSoilGroups
      sText = "RZ_"//asCharacter(iSoilsIndex)
      call PARAMS%get_parameters( sKey=sText, fValues=RZ )
      MAX_ROOTING_DEPTH(:, iSoilsIndex) = RZ
    enddo  

!     this%awc = pack(pAWC%pGrdBase%rData, this%active)

    call LOGS%WRITE( "Landuse Code |  Soils Code  | Number of Matches",              &
      iLogLevel = LOG_DEBUG, lEcho = lFALSE )
    call LOGS%WRITE( "-------------|--------------|------------------- ",            &
      iLogLevel = LOG_DEBUG, lEcho = lFALSE )

    do iSoilsIndex = 1, iNumberOfSoilGroups
      do iLUIndex = 1, iNumberOfLanduses

        call LOGS%WRITE( asCharacter(iLUIndex)//" | "//asCharacter(iSoilsIndex)//" | "//  &
            asCharacter(count( this%landuse_index == iLUIndex .and. this%soil_group == iSoilsIndex ) ),              &
            iLogLevel = LOG_DEBUG, lEcho = lFALSE )


        do iIndex = 1, ubound(this%soil_storage_max, 1)
    
          if ( this%landuse_index(iIndex) == iLUIndex .and. this%soil_group(iIndex) == iSoilsIndex ) then
            this%soil_storage_max(iIndex) = MAX_ROOTING_DEPTH( iLUIndex, iSoilsIndex ) * this%awc(iIndex)
          endif

        enddo
      enddo
    enddo

    call slList%clear()

  end subroutine initialize_soil_layers_sub

  !------------------------------------------------------------------------------------------------

  subroutine calculate_soil_mass_balance_sub(this)

    use routing__D8

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: index
    integer (kind=c_int) :: orderindex
    integer (kind=c_int) :: targetindex

    call this%calc_irrigation()

    if ( associated(this%calc_routing) ) then

      this%runon = 0.0_c_float

      do index=lbound( ORDER_INDEX, 1 ), ubound( ORDER_INDEX, 1 )

        orderindex = ORDER_INDEX( index )
        targetindex = TARGET_INDEX( index )

        this%inflow( orderindex ) =   this%runon( orderindex )                      &
                                    + this%rainfall( orderindex )                   &
                                    + this%fog( orderindex )                        &
                                    + this%irrigation( orderindex )                 &
                                    + this%snowmelt( orderindex )                   &
                                    - this%interception( orderindex )                      
                             

        if ( this%soil_storage_max( orderindex ) .approxequal. 0.0_c_float ) then

          ! this is an open water cell; special treatment
          this%runoff_outside( orderindex ) =   &
            max( 0.0_c_float, this%inflow( orderindex ) - this%reference_ET0( orderindex ) )
          this%actual_ET( orderindex ) = this%reference_ET0( orderindex )    
          
          this%potential_recharge( orderindex ) = 0.0_c_float

        else

          ! this is a normal (non open water) cell

          call this%calc_runoff( orderindex )
 
          this%infiltration( orderindex ) = this%inflow( orderindex ) - this%runoff( orderindex )

          if ( targetindex > 0)  this%runon( targetindex ) = this%runoff( orderindex )
     
          call this%calc_soil_moisture( orderindex )          

        endif

      enddo

    else   ! no routing

      this%runon = 0.0_c_float

      this%inflow =   this%rainfall                  &
                    + this%fog                       &
                    + this%irrigation                &
                    + this%snowmelt                  &
                    - this%interception

      call this%calc_runoff()
      call this%calc_soil_moisture()

      where ( this%soil_storage_max .approxequal. 0.0_c_float )

        this%runoff_outside = max( 0.0_c_float, this%inflow - this%reference_ET0 )
        this%actual_ET = this%reference_ET0
        this%potential_recharge = 0.0_c_float
        this%runoff = 0.0_c_float
        this%infiltration = 0.0_c_float

      else where

        this%infiltration = this%inflow - this%runoff

      end where

    endif

    ! add any other direct recharge terms to the potential recharge based on the water balance
    call this%calc_direct_recharge()
   
  end subroutine calculate_soil_mass_balance_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_method_sub(this, sCmdText, sMethodName)

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

        call LOGS%WRITE( "==> BUCKET INTERCEPTION submodel selected.", iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      elseif ( sMethodName .strequal. "GASH" ) then

        this%init_interception => model_initialize_interception_gash
        this%calc_interception => model_calculate_interception_gash

        call LOGS%WRITE( "==> GASH INTERCEPTION submodel selected.", iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported INTERCEPTION method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .contains. "RUNOFF" ) then

      if ( ( sMethodName .strequal. "C-N" ) .or. ( sMethodName .strequal. "CURVE_NUMBER" ) ) then

        this%init_runoff => model_initialize_runoff_curve_number
        this%calc_runoff => model_calculate_runoff_curve_number

        call LOGS%WRITE( "==> CURVE NUMBER RUNOFF submodel selected.", iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      elseif ( ( sMethodName .strequal. "RUNOFF_RATIO" ) .or. ( sMethodName .strequal. "MONTHLY_GRID" ) ) then

        this%init_runoff => model_initialize_runoff_gridded_values
        this%calc_runoff => model_calculate_runoff_gridded_values

        call LOGS%WRITE( "==> RUNOFF RATIO submodel selected.", iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported RUNOFF method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .contains. "SNOWFALL" ) then

      if ( ( sMethodName .strequal. "ORIGINAL" ) .or. ( sMethodName .strequal. "ORIGINAL_SWB_METHOD" ) ) then

        this%init_snowfall => model_initialize_snowfall_original
        this%calc_snowfall => model_calculate_snowfall_original

        call LOGS%WRITE( "==> ORIGINAL SNOWFALL submodel selected.", iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      elseif ( sMethodName .strequal. "PRMS" ) then

        this%init_snowfall => model_initialize_snowfall_prms
        this%calc_snowfall => model_calculate_snowfall_prms

        call LOGS%WRITE( "==> PRMS SNOWFALL submodel selected.", iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported SNOWFALL method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif


    elseif ( sCmdText .contains. "FLOW_ROUTING" ) then

      if ( sMethodName .strequal. "D8" ) then

        this%init_routing => model_initialize_routing_D8
        this%calc_routing => model_calculate_routing_D8

        call LOGS%WRITE( "==> D8 FLOW ROUTING submodel selected.", iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      else

        this%init_routing => null()
        this%calc_routing => null()

        call LOGS%WRITE( "==> NULL FLOW ROUTING submodel selected -- NO routing will be performed.", &
            iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      endif

    elseif ( sCmdText .contains. "FOG" ) then

      if ( sMethodName .strequal. "MONTHLY_GRID" ) then

        this%init_fog => model_initialize_fog_monthly_grid
        this%calc_fog => model_calculate_fog_monthly_grid

        call LOGS%WRITE( "==> MONTHLY_GRID FOG submodel selected.", iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      else

        this%init_fog => model_initialize_fog_none
        this%calc_fog => model_calculate_fog_none

        call LOGS%WRITE( "==> NULL FOG submodel selected (i.e. no fog term).", iLogLevel = LOG_DEBUG, lEcho = lFALSE )

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

        call LOGS%WRITE( "==> IRRIGATION will be calculated and applied as needed.", iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      else

        this%init_irrigation => model_initialize_irrigation_none
        this%calc_irrigation => model_calculate_irrigation_none

        call LOGS%WRITE( "==> IRRIGATION will *NOT* be active.", iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      endif

    elseif ( sCmdText .contains. "EVAPOTRANSPIRATION" ) then

      if ( ( sMethodName .strequal. "HARGREAVES" ) &
           .or. ( sMethodName .strequal. "HARGREAVES-SAMANI" ) ) then

        this%init_reference_et => model_initialize_et_hargreaves
        this%calc_reference_et => model_calculate_et_hargreaves

        call LOGS%WRITE( "==> HARGREAVES-SAMANI EVAPOTRANSPIRATION submodel selected.", &
            iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      elseif ( ( sMethodName .strequal. "JENSEN-HAISE" ) &
           .or. ( sMethodName .strequal. "JH" ) ) then

        this%init_reference_et => model_initialize_et_jensen_haise
        this%calc_reference_et => model_calculate_et_jensen_haise

        call LOGS%WRITE( "==> JENSEN_HAISE EVAPOTRANSPIRATION submodel selected.", &
            iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      elseif ( ( sMethodName .strequal. "MONTHLY_GRID" ) &
           .or. ( sMethodName .strequal. "MONTHLY_GRIDDED" ) ) then

        this%init_reference_et => model_initialize_et_monthly_grid
        this%calc_reference_et => model_calculate_et_monthly_grid

        call LOGS%WRITE( "==> MONTHLY_GRID EVAPOTRANSPIRATION submodel selected.", &
            iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      elseif ( ( sMethodName .strequal. "DAILY_GRID" ) &
           .or. ( sMethodName .strequal. "DAILY_GRIDDED" ) ) then

        this%init_reference_et => model_initialize_et_daily_grid
        this%calc_reference_et => model_calculate_et_daily_grid

        call LOGS%WRITE( "==> DAILY_GRID EVAPOTRANSPIRATION submodel selected.", &
            iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported EVAPOTRANSPIRATION method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .contains. "PRECIPITATION" ) then

      if ( ( sMethodName .strequal. "NORMAL" ) &
           .or. ( sMethodName .strequal. "STANDARD" ) ) then

        this%init_precipitation_data => model_initialize_precip_normal
        this%get_precipitation_data => model_get_precip_normal

        call LOGS%WRITE( "==> STANDARD PRECIPITATION submodel selected.", &
            iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      elseif ( ( sMethodName .strequal. "METHOD_OF_FRAGMENTS" ) &
           .or. ( sMethodName .strequal. "FRAGMENTS" ) ) then

        this%init_precipitation_data => model_initialize_precip_method_of_fragments
        this%get_precipitation_data => model_get_precip_method_of_fragments

        call LOGS%WRITE( "==> METHOD OF FRAGMENTS PRECIPITATION submodel selected.", &
            iLogLevel = LOG_DEBUG, lEcho = lFALSE )

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
            iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported DIRECT_RECHARGE method.", &
            lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    elseif ( sCmdText .contains. "SOIL_MOISTURE" ) then

      if ( ( sMethodName .strequal. "T-M" ) .or. ( sMethodName .strequal. "THORNTHWAITE-MATHER" ) ) then

        this%init_soil_moisture => model_initialize_soil_moisture_thornthwaite_mather
        this%calc_soil_moisture => model_calculate_soil_moisture_thornthwaite_mather

        call LOGS%WRITE( "==> THORNTHWAITE-MATHER SOIL MOISTURE submodel selected.", &
            iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      elseif ( ( sMethodName .strequal. "FAO56" ) .or. ( sMethodName .strequal. "FAO-56" ) ) then

        this%init_soil_moisture => model_initialize_soil_moisture_fao_56
        this%calc_soil_moisture => model_calculate_soil_moisture_fao_56

        call LOGS%WRITE( "==> FAO-56 SOIL MOISTURE submodel selected.", &
            iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      else

        call warn("Your control file specifies an unknown or unsupported SOIL_MOISTURE method.", &
          lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

      endif

    else

      call warn("Your control file references an unknown or unsupported method: "//dquote(sCmdText), &
          lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

    endif 

  end subroutine set_method_sub

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_interception_bucket(this)

    use interception__bucket

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call interception_bucket_initialize( )

  end subroutine model_initialize_interception_bucket

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_interception_bucket(this)

    use interception__bucket

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    this%interception = interception_bucket_calculate( this%landuse_index, this%rainfall, this%fog )
    this%interception_ET = this%interception

  end subroutine model_calculate_interception_bucket

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_interception_gash(this)

    use interception__gash

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call interception_gash_initialize( this%active, this%landuse_index )

  end subroutine model_initialize_interception_gash

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_interception_gash(this)

    use interception__gash

    class (MODEL_DOMAIN_T), intent(inout)      :: this

    ! [ LOCALS ]
    real (kind=c_float), allocatable :: fTrunk_Storage_Capacity(:)
    real (kind=c_float), allocatable :: fStemflow_Fraction(:)

! interception_gash_calculate( fRainfall, fFog, fCanopy_Cover_Fraction,       &
!                                                     fTrunk_Storage_Capacity, fStemflow_Fraction,   &
!                                                     fEvaporation_to_Rainfall_Ratio,                &
!                                                     fPrecipitation_at_Saturation, fInterception )

    fTrunk_Storage_Capacity = TRUNK_STORAGE_CAPACITY_TABLE_VALUES( this%landuse_index )
    fStemflow_Fraction = STEMFLOW_FRACTION_TABLE_VALUES( this%landuse_index )
    
    call interception_gash_calculate( this%rainfall,                                      &
                               this%fog,                                                  &
                               CANOPY_COVER_FRACTION,                                     &
                               !TRUNK_STORAGE_CAPACITY_TABLE_VALUES( this%landuse_index ), &
                               fTrunk_Storage_Capacity, &
                               !STEMFLOW_FRACTION_TABLE_VALUES( this%landuse_index ),      &
                               fStemflow_Fraction, &
                               EVAPORATION_TO_RAINFALL_RATIO,                             &
                               P_SAT,                                                     &
                               this%interception )

    this%interception_ET = this%interception

  end subroutine model_calculate_interception_gash

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_routing_D8(this)

    use routing__D8

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call routing_D8_initialize( this%active )

  end subroutine model_initialize_routing_D8  

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_routing_D8(this)

    use routing__D8

    class (MODEL_DOMAIN_T), intent(inout)  :: this

!    call routing_D8_calculate( fRunoff=this%runoff, fRunon=this%runon )


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

!    use snowfall__prms

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_snowfall_prms

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_snowfall_prms(this)

!    use snowfall__prms

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  !  this%snowfall = calculate_snowfall_prms( this%tmax, this%tmin, this%gross_precip )

  end subroutine model_calculate_snowfall_prms

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_snowmelt_original(this)

!    use snowmelt__original

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_snowmelt_original

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_snowmelt_original(this)

    use snowmelt__original

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call snowmelt_original_calculate( fSnowmelt=this%snowmelt, fSnow_storage=this%snow_storage, &
                                      fTMin=this%tmin, fTMax=this%tmax )

  end subroutine model_calculate_snowmelt_original

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_snowmelt_prms(this)

!    use snowmelt__prms

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_initialize_snowmelt_prms

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_snowmelt_prms(this)

!    use snowmelt__prms

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  !  this%snowmelt = calculate_snowmelt_prms( this%tmax, this%tmin, this%gross_precip )

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

    use runoff__curve_number

    class (MODEL_DOMAIN_T), intent(inout)       :: this
    integer (kind=c_int), intent(in), optional  :: index

    if ( present(index) ) then

      this%runoff( index ) = runoff_curve_number_calculate( &
        iLanduseIndex=this%landuse_index( index ), &
        iSoilsIndex=this%soil_group( index ), &
        fSoilStorage=this%soil_storage( index ), &
        fSoilStorage_Max=this%soil_storage_max( index ), &
        fInflow=this%inflow( index ), fCFGI=CFGI( index ) )

    else

      this%runoff = runoff_curve_number_calculate( &
        iLanduseIndex=this%landuse_index, &
        iSoilsIndex=this%soil_group, &
        fSoilStorage=this%soil_storage, &
        fSoilStorage_Max=this%soil_storage_max, &
        fInflow=this%inflow, fCFGI=CFGI )

    endif

  end subroutine model_calculate_runoff_curve_number

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_runoff_gridded_values(this)

    use runoff__gridded_values

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call runoff_gridded_values_initialize( this%active )

  end subroutine model_initialize_runoff_gridded_values

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_runoff_gridded_values(this, index )

    use runoff__gridded_values

    class (MODEL_DOMAIN_T), intent(inout)       :: this
    integer (kind=c_int), intent(in), optional  :: index

    call runoff_gridded_values_calculate( )

!     this%runoff = ( this%rainfall + this%snowmelt ) * RUNOFF_RATIOS

    this%runoff = this%inflow * RUNOFF_RATIOS

  end subroutine model_calculate_runoff_gridded_values

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_soil_moisture_thornthwaite_mather( this )

    use soil_moisture__thornthwaite_mather

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call soil_moisture_thornthwaite_mather_initialize( count( this%active ) )

  end subroutine model_initialize_soil_moisture_thornthwaite_mather

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_soil_moisture_thornthwaite_mather( this, index )

    use soil_moisture__thornthwaite_mather

    class (MODEL_DOMAIN_T), intent(inout)       :: this
    integer (kind=c_int), intent(in), optional  :: index

    if ( present( index ) ) then

      call soil_moisture_thornthwaite_mather_calculate(fAPWL=APWL(index),                                    &
                                                        fSoilStorage=this%soil_storage(index),               &
                                                        fSoilStorage_Excess=this%potential_recharge(index),  &
                                                        fActual_ET=this%actual_ET(index),                    &
                                                        fSoilStorage_Max=this%soil_storage_max(index),       &
                                                        fInfiltration=this%infiltration(index),              &
                                                        fReference_ET=this%reference_ET0_adj(index) )

    else

      call soil_moisture_thornthwaite_mather_calculate(fAPWL=APWL,                                    &
                                                        fSoilStorage=this%soil_storage,               &
                                                        fSoilStorage_Excess=this%potential_recharge,  &
                                                        fActual_ET=this%actual_ET,                    &
                                                        fSoilStorage_Max=this%soil_storage_max,       &
                                                        fInfiltration=this%infiltration,              &
                                                        fReference_ET=this%reference_ET0_adj )

    endif

  end subroutine model_calculate_soil_moisture_thornthwaite_mather

!--------------------------------------------------------------------------------------------------

  subroutine model_output_soil_moisture_none( this )

    class (MODEL_DOMAIN_T), intent(inout)       :: this
    
    ! nothing to do here.

  end subroutine model_output_soil_moisture_none

!--------------------------------------------------------------------------------------------------

  subroutine model_initialize_soil_moisture_fao_56( this )

    use soil_moisture__FAO_56

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call soil_moisture_FAO56_initialize( fSoilStorage=this%soil_storage,        &
                                         iLanduseIndex=this%landuse_index,      &
                                         iSoilGroup=this%soil_group,            &
                                         fMax_Rooting_Depths=MAX_ROOTING_DEPTH, &
                                         fAvailable_Water_Content=this%awc,     &
                                         lActive=this%active )

  end subroutine model_initialize_soil_moisture_fao_56

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_soil_moisture_fao_56( this, index )

    use soil_moisture__FAO_56

    class (MODEL_DOMAIN_T), intent(inout)       :: this
    integer (kind=c_int), intent(in), optional  :: index

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

!  soil_moisture_thornthwaite_mather_calculate(fAPWL, fSoilStorage, fSoilStorage_Excess,                &
!                                             fActual_ET, fSoilStorage_Max, fInfiltration, fReference_ET)

!     real (kind=c_float), intent(inout)   :: fAPWL
!     real (kind=c_float), intent(inout)   :: fSoilStorage
!     real (kind=c_float), intent(out)     :: fSoilStorage_Excess
!     real (kind=c_float), intent(out)     :: fActual_ET
!     real (kind=c_float), intent(in)      :: fSoilStorage_Max
!     real (kind=c_float), intent(in)      :: fInfiltration
!     real (kind=c_float), intent(in)      :: fReference_ET
    
    if ( present( index ) ) then

      call soil_moisture_FAO56_calculate( fSoilStorage=this%soil_storage(index),                             &
                                          fActual_ET=this%actual_ET(index),                                  &
                                          fSoilStorage_Excess=this%potential_recharge(index),                &
                                          fInfiltration=this%infiltration(index),                            &
                                          fGDD=this%gdd(index),                                              &
                                          fAvailableWaterCapacity=this%awc(index),                           &
                                          fReference_ET0=this%reference_ET0_adj(index),                      &
                                          fMaxRootingDepth=MAX_ROOTING_DEPTH( this%landuse_index(index),            &
                                                                      this%soil_group(index) ),              &
                                          iLanduseIndex=this%landuse_index(index),                           &
                                          iSoilGroup=this%soil_group(index) )

    else

      do iIndex=1, ubound(this%soil_storage,1)

!         print *, "LU=",this%landuse_index( iIndex ), &
!                  " Soils=",this%soil_group( iIndex ),    &
!                  " rain:",this%rainfall( iIndex ),      &
!                  " intcp:",this%interception( iIndex ),  &
!                  " fog:",this%fog( iIndex ),           &
!                  " snmelt:",this%snowmelt( iIndex ),      &
!                  " runon:",this%runon( iIndex ),         &
!                  " inflow:", this%inflow( iIndex ),        &
!                  " runoff:",this%runoff( iIndex ),        &
!                  " infil:",this%infiltration( iIndex )

        call soil_moisture_FAO56_calculate( fSoilStorage=this%soil_storage(iIndex),                             &
                                            fActual_ET=this%actual_ET(iIndex),                                  &
                                            fSoilStorage_Excess=this%potential_recharge(iIndex),                &
                                            fInfiltration=this%infiltration(iIndex),                            &
                                            fGDD=this%gdd(iIndex),                                              &
                                            fAvailableWaterCapacity=this%awc(iIndex),                           &
                                            fReference_ET0=this%reference_ET0_adj(iIndex),                      &
                                            fMaxRootingDepth=MAX_ROOTING_DEPTH( this%landuse_Index(iIndex),     &
                                                                                this%soil_group(iIndex) ),      &
                                            iLanduseIndex=this%landuse_index(iIndex),                           &
                                            iSoilGroup=this%soil_group(iIndex) )

      enddo

    endif

  end subroutine model_calculate_soil_moisture_fao_56

!--------------------------------------------------------------------------------------------------

  subroutine model_output_soil_moisture_fao_56( this )

    use soil_moisture__FAO_56

    class (MODEL_DOMAIN_T), intent(inout)       :: this

  end subroutine model_output_soil_moisture_fao_56

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
   
    call irrigation__initialize( this%active )

  end subroutine model_initialize_irrigation

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_irrigation( this )

  use irrigation

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call irrigation__calculate( fIrrigationAmount=this%irrigation,        &
                                iLanduseIndex=this%landuse_index,         &
                                fSoilStorage=this%soil_storage,           & 
                                fSoilStorage_Max=this%soil_storage_max,   &
                                lActive=this%active )

  end subroutine model_calculate_irrigation

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
    !> Nothing here to see. Initialization not really needed for the "normal" method.

    call growing_degree_day_initialize( lActive=this%active,                  &
                                        iLanduseIndex=this%landuse_index,     &
                                        dX=this%X,                            & 
                                        dY=this%Y,                            &
                                        dX_lon=pCOORD_GRD%rX,                 &
                                        dY_lat=pCOORD_GRD%rY  )

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
                                     dX_lon=pCOORD_GRD%rX , dY_lat=pCOORD_GRD%rY )

    print *, __FILE__, ": ", __LINE__

  end subroutine model_initialize_direct_recharge_gridded

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_direct_recharge_gridded ( this )

    use direct_recharge__gridded_data

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call direct_recharge_calculate( iLanduse_Index=this%landuse_index,   &
                                    lActive=this%active,         &
                                    fDont_Care=this%dont_care )

    this%potential_recharge = this%potential_recharge + DIRECT_RECHARGE

  end subroutine model_calculate_direct_recharge_gridded

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

    call fog_monthly_grid_initialize( lActive=this%active, dX=this%X, dY=this%Y, &
      dX_lon=pCOORD_GRD%rX , dY_lat=pCOORD_GRD%rY )

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
      fDont_Care=this%dont_care )

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

    call precipitation_method_of_fragments_initialize( this%active )

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
  
      ! next three statements retrieve the data from the raw or native form
      call pPRCP%getvalues( iMonth, iDay, iYear, iJulianDay )

    end associate

    if (.not. associated(pPRCP%pGrdBase) ) &
      call die("INTERNAL PROGRAMMING ERROR: Call to NULL pointer.", __FILE__, __LINE__)

    call precipitation_method_of_fragments_calculate( this%active )

    this%gross_precip = pack( pPRCP%pGrdBase%rData, this%active ) * FRAGMENT_VALUE * RAINFALL_ADJUST_FACTOR

  end subroutine model_get_precip_method_of_fragments 

!--------------------------------------------------------------------------------------------------

  subroutine minmaxmean_float( variable , varname )

    real (kind=c_float), dimension(:)  :: variable
    character (len=*), intent(in)      :: varname

    ! [ LOCALS ] 
    integer (kind=c_int) :: iCount
    character (len=20)   :: sVarname
    character (len=14)   :: sMin
    character (len=14)   :: sMax
    character (len=14)   :: sMean
    character (len=10)   :: sCount

    write (sVarname, fmt="(a20)") adjustl(varname)

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

    call LOGS%write( adjustl(sVarname)//" | "//adjustl(sMin)//" | "//adjustl(sMax) &
       //" | "//adjustl(sMean)//" | "//adjustl(sCount), iLogLevel=LOG_DEBUG, lEcho=lTRUE )

  end subroutine minmaxmean_float


  subroutine minmaxmean_int( variable , varname )

    integer (kind=c_int), dimension(:)  :: variable
    character (len=*), intent(in)       :: varname

    ! [ LOCALS ] 
    integer (kind=c_int) :: iCount
    character (len=20)   :: sVarname
    character (len=14)   :: sMin
    character (len=14)   :: sMax
    character (len=14)   :: sMean
    character (len=10)   :: sCount

    write (sVarname, fmt="(a20)") adjustl(varname)

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

    call LOGS%write( adjustl(sVarname)//" | "//adjustl(sMin)//" | "//adjustl(sMax) &
       //" | "//adjustl(sMean)//" | "//adjustl(sCount), iLogLevel=LOG_DEBUG, lEcho=lTRUE )

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
    call minmaxmean( this%reference_ET0_adj, "ET0_adn")
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

end module model_domain