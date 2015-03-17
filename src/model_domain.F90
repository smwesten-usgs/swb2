module model_domain

  use iso_c_binding
  use continuous_frozen_ground_index
  use data_catalog
  use data_catalog_entry
  use exceptions
  use simulation_datetime
  use snowfall__original
  use parameters
  use netcdf4_support, only: NC_FILL_FLOAT
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
     
    real (kind=c_float), allocatable       :: latitude(:)
    real (kind=c_float), allocatable       :: reference_ET0(:)
    real (kind=c_float), allocatable       :: reference_ET0_adj(:)
    real (kind=c_float), allocatable       :: interception_ET(:)
    real (kind=c_float), allocatable       :: actual_ET(:)
    real (kind=c_float), allocatable       :: inflow(:)
    real (kind=c_float), allocatable       :: runon(:)
    real (kind=c_float), allocatable       :: runoff(:)
    real (kind=c_float), allocatable       :: outflow(:)
    real (kind=c_float), allocatable       :: infiltration(:)
    real (kind=c_float), allocatable       :: snowmelt(:)
    real (kind=c_float), allocatable       :: interception(:)
    real (kind=c_float), allocatable       :: GDD_28(:)
     
    real (kind=c_float), allocatable       :: interception_storage(:)
    real (kind=c_float), allocatable       :: snow_storage(:)
    real (kind=c_float), allocatable       :: soil_storage(:)
    real (kind=c_float), allocatable       :: soil_storage_max(:)
    real (kind=c_float), allocatable       :: potential_recharge(:)
    real (kind=c_float), allocatable       :: stream_storage(:)
         
    real (kind=c_float), allocatable       :: gross_precip(:)
    real (kind=c_float), allocatable       :: fog(:)
    real (kind=c_float), allocatable       :: rainfall(:)
    real (kind=c_float), allocatable       :: snowfall(:)

    real (kind=c_float), allocatable       :: tmin(:)
    real (kind=c_float), allocatable       :: tmax(:)
    real (kind=c_float), allocatable       :: routing_fraction(:)

    integer (kind=c_int), allocatable      :: index_order(:)

    !> declare and initialize procedure pointers such that the default methods are in place
    procedure ( simple_method ), pointer         :: init_interception       => model_initialize_interception_bucket
    procedure ( simple_method ), pointer         :: init_runoff             => model_initialize_runoff_curve_number
    procedure ( simple_method ), pointer         :: init_reference_et       => model_initialize_et_hargreaves
    procedure ( simple_method ), pointer         :: init_routing            => model_initialize_routing_none
    procedure ( simple_method ), pointer         :: init_soil_moisture      => model_initialize_soil_moisture_thornthwaite_mather
    procedure ( simple_method ), pointer         :: init_snowfall           => model_initialize_snowfall_original
    procedure ( simple_method ), pointer         :: init_snowmelt           => model_initialize_snowmelt_original
    procedure ( simple_method ), pointer         :: init_precipitation_data => model_initialize_precip_normal
    procedure ( simple_method ), pointer         :: init_fog                => model_initialize_fog_none

    procedure ( simple_method ), pointer         :: calc_interception      => model_calculate_interception_bucket

    procedure ( simple_method_w_optional ), pointer   :: calc_runoff       => model_calculate_runoff_curve_number
    
    procedure ( simple_method ), pointer         :: calc_reference_et      => model_calculate_et_hargreaves
    procedure ( simple_method ), pointer         :: calc_routing           => model_calculate_routing_none

    procedure ( simple_method_w_optional ), pointer  :: calc_soil_moisture => model_calculate_soil_moisture_thornthwaite_mather
    
    procedure ( simple_method ), pointer         :: calc_snowfall          => model_calculate_snowfall_original
    procedure ( simple_method ), pointer         :: calc_snowmelt          => model_calculate_snowmelt_original    
    procedure ( simple_method ), pointer         :: get_precipitation_data => model_get_precip_normal
    procedure ( simple_method ), pointer         :: calc_fog               => model_calculate_fog_none

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

    procedure :: iterate_over_simulation_days
    
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

  real (kind=c_float), allocatable  :: ROOTING_DEPTH(:,:)

  type (GENERAL_GRID_T), pointer       :: pCOORD_GRD

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
    integer (kind=c_int)  :: iStat(30)

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
    allocate( this%GDD_28(iCount), stat=iStat(21) )
    allocate( this%interception_storage(iCount), stat=iStat(22) )
    allocate( this%snow_storage(iCount), stat=iStat(23) )
    allocate( this%soil_storage(iCount), stat=iStat(24) )
    allocate( this%soil_storage_max(iCount), stat=iStat(25) )
    allocate( this%potential_recharge(iCount), stat=iStat(26) )
    allocate( this%fog(iCount), stat=iStat(27) )
    allocate( this%stream_storage(iCount), stat=iStat(28) )
    allocate( this%index_order(iCount), stat=iStat(29) )

    allocate( OUTPUT(16), stat=iStat(30) )

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
    call this%init_routing
    call this%init_soil_moisture
    call this%init_reference_et
    call this%init_precipitation_data

  end subroutine initialize_methods_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_netcdf_output_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iIndex

    do iIndex = 1, ubound(OUTPUT, 1)
      allocate ( OUTPUT(iIndex)%ncfile )
    enddo  

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(1)%ncfile, sVariableName="gross_precipitation", &
      sVariableUnits="inches_per_day", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(2)%ncfile, sVariableName="interception", &
      sVariableUnits="inches_per_day", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(3)%ncfile, sVariableName="runoff", &
      sVariableUnits="inches_per_day", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(4)%ncfile, sVariableName="runon", &
      sVariableUnits="inches_per_day", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0   )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(5)%ncfile, sVariableName="infiltration", &
      sVariableUnits="inches_per_day", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(6)%ncfile, sVariableName="snowfall", &
      sVariableUnits="inches_per_day", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(7)%ncfile, sVariableName="snowmelt", &
      sVariableUnits="inches_per_day", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(8)%ncfile, sVariableName="snow_storage", &
      sVariableUnits="inches", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0 )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(9)%ncfile, sVariableName="soil_storage", &
      sVariableUnits="inches", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(10)%ncfile, sVariableName="potential_recharge", &
      sVariableUnits="inches", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(11)%ncfile, sVariableName="reference_ET0", &
      sVariableUnits="inches", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(12)%ncfile, sVariableName="reference_ET0_adj", &
      sVariableUnits="inches", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(13)%ncfile, sVariableName="actual_ET", &
      sVariableUnits="inches", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(14)%ncfile, sVariableName="tmin", &
      sVariableUnits="degrees Fahrenheit", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(15)%ncfile, sVariableName="tmax", &
      sVariableUnits="degrees Fahrenheit", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,&
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(16)%ncfile, sVariableName="interception_storage", &
      sVariableUnits="inches", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end, &
      dpLat=pCOORD_GRD%rY, dpLon=pCOORD_GRD%rX, fValidMin=0.0, fValidMax=2000.0  )

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
    
    !> Determine how many landuse codes are present
    call PARAMS%get_values( sKey="LU_Code", iValues=iLanduseCodes )

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

    deallocate(this%landuse_index)
    allocate( this%landuse_index( ubound( this%landuse_code, 1) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory.", __FILE__, __LINE__ )

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
    
      call die("Error attempting to access SOILS_GROUP data.")

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
    
      call die("Error attempting to access LAND_USE data.")

    endif    


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
    
      call die("Error attempting to access AVAILABLE_WATER_CONTENT data.")

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
        rGridCellSize=this%gridcellsize, iDataType=GRID_DATATYPE_INT )  

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

  end subroutine initialize_latitude_sub

!--------------------------------------------------------------------------------------------------

  !> @todo Move this into a separate module 

  subroutine iterate_over_simulation_days(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    do while ( SIM_DT%curr <= SIM_DT%end )

      call LOGS%write("Calculating: "//SIM_DT%curr%prettydate(), iLogLevel=LOG_ALL, lEcho=.true._c_bool )

      call this%get_climate_data()
      call this%solve()
      call this%write_variables_to_netcdf()
      call SIM_DT%addDay()

    enddo 

  end subroutine iterate_over_simulation_days

!--------------------------------------------------------------------------------------------------

  subroutine get_climate_data(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iJulianDay
    integer (kind=c_int) ::iMonth
    integer (kind=c_int) ::iDay
    integer (kind=c_int) ::iYear
    type (DATA_CATALOG_ENTRY_T), pointer :: pTMAX
    type (DATA_CATALOG_ENTRY_T), pointer :: pTMIN

    pTMAX => DAT%find("TMAX")
    if ( .not. associated(pTMAX) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)


    pTMIN => DAT%find("TMIN")
    if ( .not. associated(pTMIN) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear
  
      ! next two statements retrieve the data from the raw or native form  
      call pTMIN%getvalues( iMonth, iDay, iYear, iJulianDay )

      call pTMAX%getvalues( iMonth, iDay, iYear, iJulianDay )

      ! the following statements process the raw data in order to get it into the 
      ! right units or properly pack the data
      call this%get_precipitation_data()
!      this%gross_precip = pack( PRCP%pGrdBase%rData, this%active )

      this%tmax = pack( pTMAX%pGrdBase%rData, this%active )
      this%tmin = pack( pTMIN%pGrdBase%rData, this%active )

      ! partition preciptation into rainfall and snowfall fractions
      call this%calc_snowfall()

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

    ! next, unpack each vector and output as an array
    this%array_output = unpack(this%gross_precip, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(1)%ncfile, &
                   iVarID=OUTPUT(1)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%interception, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(2)%ncfile, &
                   iVarID=OUTPUT(2)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%runoff, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(3)%ncfile, &
                   iVarID=OUTPUT(3)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%runon, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(4)%ncfile, &
                   iVarID=OUTPUT(4)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%infiltration, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(5)%ncfile, &
                   iVarID=OUTPUT(5)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%snowfall, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(6)%ncfile, &
                   iVarID=OUTPUT(6)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%snowmelt, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(7)%ncfile, &
                   iVarID=OUTPUT(7)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%snow_storage, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(8)%ncfile, &
                   iVarID=OUTPUT(8)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%soil_storage, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(9)%ncfile, &
                   iVarID=OUTPUT(9)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%potential_recharge, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(10)%ncfile, &
                   iVarID=OUTPUT(10)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%reference_ET0, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(11)%ncfile, &
                   iVarID=OUTPUT(11)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%reference_ET0_adj, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(12)%ncfile, &
                   iVarID=OUTPUT(12)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%actual_ET, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(13)%ncfile, &
                   iVarID=OUTPUT(13)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%tmin, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(14)%ncfile, &
                   iVarID=OUTPUT(14)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%tmax, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(15)%ncfile, &
                   iVarID=OUTPUT(15)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

    this%array_output = unpack(this%interception_storage, this%active, this%dont_care)

    call netcdf_put_variable_array(NCFILE=OUTPUT(16)%ncfile, &
                   iVarID=OUTPUT(16)%ncfile%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(this%number_of_rows, kind=c_size_t), int(this%number_of_columns, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=this%array_output )

  end subroutine write_variables_to_netcdf

!--------------------------------------------------------------------------------------------------

  subroutine calculate_mass_balance_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

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


    if (.not. associated( this%calc_interception) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_runoff) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_reference_et) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_snowfall) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_snowmelt) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%get_precipitation_data ) ) &
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
    call slList%append("Landuse_Lookup_Code")

    !> Determine how many soil groups are present

    ! retrieve a string list of all keys associated with root zone depth (i.e. RZ_1, RZ_2, RZ_3, etc.)
    slRZ = PARAMS%grep_keys("RZ")
    ! Convert the string list to an vector of integers; this call strips off the "RZ_" part of label
    iRZ_SeqNums = slRZ%asInt()
    ! count how many items are present in the vector; this should equal the number of soils groups
    iNumberOfSoilGroups = count( iRZ_SeqNums > 0 )

    !> Determine how many landuse codes are present
    call PARAMS%get_values( slList, iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes >= 0 )

    allocate( ROOTING_DEPTH(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for maximum rooting depth table", &
      __FILE__, __LINE__)

    ! we should have the max rooting depth table fully filled out following this block
    do iSoilsIndex = 1, iNumberOfSoilGroups
      sText = "RZ_"//asCharacter(iSoilsIndex)
      call PARAMS%get_values( sText, RZ )
      ROOTING_DEPTH(:, iSoilsIndex) = RZ
    enddo  

!     this%awc = pack(pAWC%pGrdBase%rData, this%active)

 
    do iSoilsIndex = 1, iNumberOfSoilGroups
      do iLUIndex = 1, iNumberOfLanduses

        call LOGS%WRITE( "LU: "//asCharacter(iLUIndex)//"  Soils: "//asCharacter(iSoilsIndex)//"  | # matches = "//  &
            asCharacter(count( this%landuse_index == iLUIndex .and. this%soil_group == iSoilsIndex ) ),              &
            iLogLevel = LOG_DEBUG, lEcho = lFALSE )


        do iIndex = 1, ubound(this%soil_storage_max, 1)
    
          if ( this%landuse_index(iIndex) == iLUIndex .and. this%soil_group(iIndex) == iSoilsIndex ) then
            this%soil_storage_max(iIndex) = ROOTING_DEPTH( iLUIndex, iSoilsIndex ) * this%awc(iIndex)
          endif

        enddo
      enddo
    enddo

  end subroutine initialize_soil_layers_sub

  !------------------------------------------------------------------------------------------------

  subroutine calculate_soil_mass_balance_sub(this)

    use routing__D8

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: index
    integer (kind=c_int) :: orderindex
    integer (kind=c_int) :: targetindex

!     if ( associated(this%calc_routing) ) then

      this%runon = 0.0_c_float

      do index=lbound(this%runon,1), ubound(this%runon,1)

        orderindex = ORDER_INDEX( index )
        targetindex = TARGET_INDEX( index )

        this%inflow( orderindex ) =   this%runon( orderindex )                      &
                                    + this%gross_precip( orderindex )               &
                                    + this%fog( orderindex )                        &
                                    + this%snowmelt( orderindex )                   &
                                    - this%interception( orderindex )                      
                             
        call this%calc_runoff( orderindex )

!        this%runoff( orderindex ) = this%inflow( orderindex ) - this%infiltration( orderindex )
 
        this%infiltration( orderindex ) = this%inflow( orderindex ) - this%runoff( orderindex )

!          print *, orderindex, TARGET_INDEX( orderindex ), this%inflow(orderindex), this%runoff(orderindex)
    
        if ( targetindex > 0) then
          
          this%runon( targetindex ) = this%runoff( orderindex )

!            print *, orderindex, TARGET_INDEX( orderindex ), this%inflow( orderindex ), this%runoff( orderindex ), &
!              this%runon(TARGET_INDEX( orderindex ) )

        endif 

        call this%calc_soil_moisture( orderindex )

      enddo  

!     else

!       this%inflow = this%runon + this%gross_precip - this%interception + this%snowmelt
!       call this%calc_infiltration()
!       this%runoff = this%inflow - this%infiltration

!       call this%calc_soil_moisture

!     endif


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

        this%init_routing => model_initialize_routing_none
        this%calc_routing => model_calculate_routing_none

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

    elseif ( sCmdText .contains. "SOIL_MOISTURE" ) then

      if ( ( sMethodName .strequal. "T-M" ) .or. ( sMethodName .strequal. "THORNTHWAITE-MATHER" ) ) then

        this%init_soil_moisture => model_initialize_soil_moisture_thornthwaite_mather
        this%calc_soil_moisture => model_calculate_soil_moisture_thornthwaite_mather

        call LOGS%WRITE( "==> THORNTHWAITE-MATHER SOIL MOISTURE submodel selected.", &
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

    this%interception = interception_bucket_calculate( this%landuse_index, this%gross_precip, this%fog )
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

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call interception_gash_calculate( this%gross_precip, this%fog, this%interception )
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

  subroutine model_initialize_routing_none(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this
    
  end subroutine model_initialize_routing_none  

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_routing_none(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_calculate_routing_none  

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

    call runoff_gridded_values_calculate(this%inflow, this%runoff, this%active)


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

    call fog_monthly_grid_calculate( fRainfall=this%rainfall, fFog=this%fog, lActive=this%active, &
             fDont_Care=this%dont_care )

  end subroutine model_calculate_fog_monthly_grid  

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
    call minmaxmean( this%GDD_28, "GDD28")
    
    call minmaxmean( this%interception_storage, "intcp_stor")
    call minmaxmean( this%snow_storage, "snow_stor")
    call minmaxmean( this%soil_storage, "soil_stor")
    call minmaxmean( this%soil_storage_max, "soil_stor_max")
    call minmaxmean( this%potential_recharge, "potential_recharge")
    call minmaxmean( this%stream_storage, "stream_storage")

  end subroutine summarize_state_variables_sub

end module model_domain