module model_domain

  use iso_c_binding
  use continuous_frozen_ground_index
  use data_catalog_entry
  use exceptions
  use infiltration__curve_number
  use simulation_datetime
  use parameters
  implicit none

  private

  ! concept: the only state variables that should appear in this module should be
  !          those that are required regardless of what other program options are active

  type, public :: MODEL_DOMAIN_T

    character (len=:), allocatable     :: PROJ4_string
    integer (kind=c_int)               :: number_of_columns
    integer (kind=c_int)               :: number_of_rows
    real (kind=c_double)               :: X_ll, Y_ll
    real (kind=c_double)               :: X_ur, Y_ur
    real (kind=c_double), allocatable  :: X(:), Y(:)
    real (kind=c_double)               :: gridcellsize

    logical (kind=c_bool), allocatable     :: active(:,:)
    real (kind=c_float), allocatable       :: dont_care(:,:)
    real (kind=c_float), allocatable       :: array_output(:,:)

    integer (kind=c_int), allocatable      :: landuse_code(:)
    integer (kind=c_int), allocatable      :: landuse_index(:)
    integer (kind=c_int), allocatable      :: soil_group(:)
    integer (kind=c_int), allocatable      :: num_upslope_connections(:)
    integer (kind=c_int), allocatable      :: sum_upslope_cells(:)
     
    real (kind=c_float), allocatable       :: latitude(:)
    real (kind=c_float), allocatable       :: reference_ET0(:)
    real (kind=c_float), allocatable       :: reference_ET0_adj(:)
    real (kind=c_float), allocatable       :: actual_ET(:)
    real (kind=c_float), allocatable       :: inflow(:)
    real (kind=c_float), allocatable       :: runon(:)
    real (kind=c_float), allocatable       :: runoff(:)
    real (kind=c_float), allocatable       :: outflow(:)
    real (kind=c_float), allocatable       :: infiltration(:)
    real (kind=c_float), allocatable       :: snowfall(:)
    real (kind=c_float), allocatable       :: snowmelt(:)
    real (kind=c_float), allocatable       :: interception(:)
    real (kind=c_float), allocatable       :: net_precip(:)
    real (kind=c_float), allocatable       :: GDD_28(:)
     
    real (kind=c_float), allocatable       :: interception_storage(:)
    real (kind=c_float), allocatable       :: snow_storage(:)
    real (kind=c_float), allocatable       :: soil_storage(:)
    real (kind=c_float), allocatable       :: soil_storage_max(:)
    real (kind=c_float), allocatable       :: stream_storage(:)
         
    real (kind=c_float), allocatable       :: gross_precip(:)
    real (kind=c_float), allocatable       :: tmin(:)
    real (kind=c_float), allocatable       :: tmax(:)
    real (kind=c_float), allocatable       :: routing_fraction(:)

    procedure ( interception_method ), pointer, private        :: calc_interception      => null()
    procedure ( infiltration_method ), pointer, private        :: calc_infiltration      => null()
    procedure ( et_method ), pointer, private                  :: calc_reference_et      => null()
    procedure ( sm_method ), pointer, private                  :: calc_soil_moisture     => null()
    procedure ( precipitation_data_method ), pointer, private  :: get_precipitation_data => model_get_precip_normal

  contains

    procedure :: initialize_arrays_sub
    generic   :: initialize_arrays => initialize_arrays_sub

    procedure :: initialize_grid_sub
    generic   :: initialize_grid => initialize_grid_sub

    procedure :: set_interception_method_sub
    generic   :: set_interception => set_interception_method_sub

    procedure :: set_evapotranspiration_method_sub
    generic   :: set_evapotranspiration => set_evapotranspiration_method_sub

    procedure :: set_infiltration_method_sub
    generic   :: set_infiltration_method => set_infiltration_method_sub

    procedure :: set_soil_moisture_method_sub
    generic   :: set_soil_moisture_method => set_soil_moisture_method_sub 

    procedure :: set_precipitation_data_method_sub
    generic   :: set_precipitation_data_method => set_precipitation_data_method_sub

    procedure :: set_inactive_cells_sub
    generic   :: set_inactive_cells => set_inactive_cells_sub

    procedure :: preflight_check_function_pointers

    procedure :: iterate_over_simulation_days
    
    procedure :: calculate_mass_balance_sub
    generic   :: solve => calculate_mass_balance_sub

    procedure :: calculate_interception_mass_balance_sub
    procedure :: calculate_snow_mass_balance_sub
    procedure :: calculate_soil_mass_balance_sub

    procedure :: get_climate_data
    procedure :: write_variables_to_netcdf

    procedure :: initialize_netcdf_output_sub
    generic   :: initialize_netcdf_output => initialize_netcdf_output_sub

    procedure :: initialize_landuse_codes_sub
    generic   :: initialize_landuse => initialize_landuse_codes_sub

    procedure :: initialize_latitude_sub
    generic   :: initialize_latitude => initialize_latitude_sub

    procedure :: initialize_soil_groups_sub
    generic   :: initialize_soil_groups => initialize_soil_groups_sub


  end type MODEL_DOMAIN_T

  abstract interface
    subroutine interception_method( this ) 
      import :: MODEL_DOMAIN_T
      class ( MODEL_DOMAIN_T ), intent(inout)  :: this
    end subroutine interception_method
  end interface

  abstract interface
    subroutine infiltration_method( this )
      import :: MODEL_DOMAIN_T
      class ( MODEL_DOMAIN_T ), intent(inout)  :: this
    end subroutine infiltration_method
  end interface

  abstract interface
    subroutine et_method( this )
      import :: MODEL_DOMAIN_T
      class ( MODEL_DOMAIN_T ), intent(inout)  :: this
    end subroutine et_method
  end interface    

  abstract interface
    subroutine sm_method( this )
      import :: MODEL_DOMAIN_T
      class ( MODEL_DOMAIN_T ), intent(inout)  :: this
    end subroutine sm_method
  end interface    

  abstract interface
    subroutine precipitation_data_method( this )
      import :: MODEL_DOMAIN_T
      class ( MODEL_DOMAIN_T ), intent(inout)  :: this
    end subroutine precipitation_data_method
  end interface    

  type, public :: NETCDF_FILE_COLLECTION_T
    type (T_NETCDF4_FILE), pointer, public :: ncfile
  end type NETCDF_FILE_COLLECTION_T

  type (MODEL_DOMAIN_T), public :: MODEL

  type (NETCDF_FILE_COLLECTION_T), allocatable, public :: OUTPUT(:)

contains

  !
  ! current concept:
  !
  ! The only 2-D array is the array that contains the mask of active cells.
  !
  ! All remaining state variables and ancillary variables are kept in 1-D vectors
  ! that are PACK-ed and UNPACK-ed as needed by i/o routines. This is cumbersome for fully
  ! active grids, but should amount to significant memory and processing savings when running
  ! SWB for, say, and island domain.
  !

  subroutine initialize_grid_sub(this, iNumCols, iNumRows, dX_ll, dY_ll, dGridCellSize )

    class (MODEL_DOMAIN_T), intent(inout)        :: this
    integer (kind=c_int), intent(in)             :: iNumCols
    integer (kind=c_int), intent(in)             :: iNumRows
    real (kind=c_double), intent(in)             :: dX_ll
    real (kind=c_double), intent(in)             :: dY_ll
    real (kind=c_double), intent(in)             :: dGridcellSize

    ! [ LOCALS ]
    integer (kind=c_int)  :: iCount
    integer (kind=c_int)  :: iStat

    iCount = count( this%active )

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



  subroutine initialize_arrays_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)        :: this

    ! [ LOCALS ]
    integer (kind=c_int)  :: iCount
    integer (kind=c_int)  :: iStat(23)

    iCount = count( this%active )

    allocate( this%landuse_code(iCount), stat=iStat(1) )
    allocate( this%landuse_index(iCount), stat=iStat(2) )
    allocate( this%soil_group(iCount), stat=iStat(3) )
    allocate( this%num_upslope_connections(iCount), stat=iStat(4) )
    allocate( this%sum_upslope_cells(iCount), stat=iStat(5) )
    allocate( this%latitude(iCount), stat=iStat(6) )
    allocate( this%reference_ET0(iCount), stat=iStat(6) )
    allocate( this%reference_ET0_adj(iCount), stat=iStat(7) )
    allocate( this%actual_ET(iCount), stat=iStat(8) )
    allocate( this%inflow(iCount), stat=iStat(9))
    allocate( this%runon(iCount), stat=iStat(10) )
    allocate( this%runoff(iCount), stat=iStat(11) )
    allocate( this%outflow(iCount), stat=iStat(12) )
    allocate( this%infiltration(iCount), stat=iStat(13) )
    allocate( this%snowfall(iCount), stat=iStat(14) )
    allocate( this%snowmelt(iCount), stat=iStat(15) )
    allocate( this%net_precip(iCount), stat=iStat(16) )
    allocate( this%GDD_28(iCount), stat=iStat(17) )
    allocate( this%interception_storage(iCount), stat=iStat(18) )
    allocate( this%snow_storage(iCount), stat=iStat(19) )
    allocate( this%soil_storage(iCount), stat=iStat(20) )
    allocate( this%soil_storage_max(iCount), stat=iStat(21))
    allocate( this%stream_storage(iCount), stat=iStat(22) )

    allocate( OUTPUT(2), stat=iStat(23) )

    if ( any( iStat /= 0 ) )  call die("Problem allocating memory", __FILE__, __LINE__)

  end subroutine initialize_arrays_sub

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
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end )

    call netcdf_open_and_prepare_as_output( NCFILE=OUTPUT(2)%ncfile, sVariableName="interception", &
      sVariableUnits="inches_per_day", iNX=this%number_of_columns, iNY=this%number_of_rows, &
      fX=this%X, fY=this%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end )

      this%dont_care = -9999._c_float

  end subroutine initialize_netcdf_output_sub

!--------------------------------------------------------------------------------------------------
  
  subroutine set_inactive_cells_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    this%active = .true._c_bool

    where (       HSG%pGrdBase%iData < 1 &
            .or.  LULC%pGrdBase%iData < 1 )

      this%active = .false._c_bool

    end where

    call LOGS%write(asCharacter(count(this%active))//" cells are currently active out of a total of " &
      //asCharacter(size(this%active)), iLinesBefore=1, iLinesAfter=1)

  end subroutine set_inactive_cells_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_landuse_codes_sub( this )

    class (MODEL_DOMAIN_T), intent(inout)     :: this

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iIndex
    integer (kind=c_int), allocatable    :: iLandUseCodes(:)
    integer (kind=c_int)                 :: iLandUseIndices(256)
    
    !> Determine how many landuse codes are present
    call PARAMS%get_values( sKey="LU_Code", iValues=iLanduseCodes )

    this%landuse_code = pack( LULC%pGrdBase%iData, this%active )

    do iIndex=1, ubound(iLandUseCodes, 1)

      where ( this%landuse_code == iLandUseCodes(iIndex) )
        this%landuse_index = iIndex
      end where

    enddo  

  end subroutine initialize_landuse_codes_sub

!--------------------------------------------------------------------------------------------------  

  subroutine initialize_soil_groups_sub( this )

    class (MODEL_DOMAIN_T), intent(inout)     :: this

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iIndex
    
    this%soil_group = pack( HSG%pGrdBase%iData, this%active )

    call LOGS%write("Soil hydrologic groups as read into SWB data structure", iLinesBefore=1, iLinesAfter=1, iLogLevel=LOG_DEBUG)

    do iIndex = 1, maxval(HSG%pGrdBase%iData)

      call LOGS%write( asCharacter(count(MODEL%soil_group == iIndex) )//" cells belong to soils group " &
        //asCharacter(iIndex), iLogLevel=LOG_DEBUG )
      
    end do    

    call LOGS%write("", iLinesBefore=1, iLogLevel=LOG_DEBUG)

  end subroutine initialize_soil_groups_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_latitude_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)     :: this

    ! [ LOCALS ]
    type (GENERAL_GRID_T), pointer       :: pGrd
    integer (kind=c_int)                 :: iIndex

    pGrd => grid_Create( iNX=this%number_of_columns, iNY=this%number_of_rows, &
        rX0=this%X_ll, rY0=this%Y_ll, &
        rGridCellSize=this%gridcellsize, iDataType=GRID_DATATYPE_INT )  

    allocate ( this%X(this%number_of_columns ) )
    allocate ( this%Y(this%number_of_rows ) )

    ! call the grid routine to populate the X and Y values
    call grid_PopulateXY(pGrd)

    ! populating these in order to have them available later for use in writing results to NetCDF
    this%X = pGrd%rX( :, 1 )
    this%Y = pGrd%rY( 1, : ) 

    ! transform to unprojected (lat/lon) coordinate system
    call grid_Transform(pGrd=pGrd, sFromPROJ4=this%PROJ4_string, &
        sToPROJ4="+proj=lonlat +ellps=GRS80 +datum=WGS84 +no_defs" )
    
    this%latitude = pack( pGrd%rY, this%active )

    call grid_Destroy(pGrd)

  end subroutine initialize_latitude_sub

!--------------------------------------------------------------------------------------------------

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


    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear
  
      ! next three statements retrieve the data from the raw or native form
      call PRCP%getvalues( iMonth, iDay, iYear, iJulianDay )
  
      call TMIN%getvalues( iMonth, iDay, iYear, iJulianDay )

      call TMAX%getvalues( iMonth, iDay, iYear, iJulianDay )

      ! the following statements process the raw data in order to get it into the 
      ! right units or properly pack the data
      call this%get_precipitation_data()
!      this%gross_precip = pack( PRCP%pGrdBase%rData, this%active )

      this%Tmax = pack( TMAX%pGrdBase%rData, this%active )

      this%Tmin = pack( TMIN%pGrdBase%rData, this%active )

    end associate

  end subroutine get_climate_data

!--------------------------------------------------------------------------------------------------

  subroutine write_variables_to_netcdf(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    do iIndex = 1, ubound( OUTPUT, 1 )

      call netcdf_put_variable_vector(NCFILE=OUTPUT(iIndex)%ncfile, &
         iVarID=OUTPUT(iIndex)%ncfile%iVarID(NC_TIME), &
         iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t)], &
         iCount=[1_c_size_t], &
         iStride=[1_c_ptrdiff_t], &
         dpValues=[real(SIM_DT%iNumDaysFromOrigin, kind=c_double)])

    enddo

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


  end subroutine write_variables_to_netcdf

!--------------------------------------------------------------------------------------------------

  subroutine calculate_mass_balance_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call this%calculate_interception_mass_balance_sub()
    call this%calculate_snow_mass_balance_sub()
    call this%calculate_soil_mass_balance_sub()

  end subroutine calculate_mass_balance_sub

!--------------------------------------------------------------------------------------------------

  subroutine preflight_check_function_pointers(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    if (.not. associated( this%calc_interception) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_infiltration) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%calc_reference_et) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

    if (.not. associated( this%get_precipitation_data ) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__ )

  end subroutine preflight_check_function_pointers

!--------------------------------------------------------------------------------------------------

  subroutine calculate_interception_mass_balance_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    ! [ LOCALS ]
    real (kind=c_float)  :: fReferenceET_minus_interception

    call this%calc_interception()

    call this%calc_reference_et()

    this%interception_storage = this%interception_storage + this%interception

    this%reference_ET0_adj = this%reference_ET0 - this%interception_storage
    where ( this%reference_ET0_adj >= 0.0_c_float ) ! potential ET evaporates all interception storage water
    
       this%interception_storage = 0.0_c_float

    elsewhere ! not enough potential ET to completely dry out interception storage

      this%interception_storage = this%interception_storage - this%reference_ET0
      this%reference_ET0_adj = 0.0_c_float

    end where
    !if ( this%fReference_ET0 > this%fInterceptionStorage ) 

  end subroutine calculate_interception_mass_balance_sub

!--------------------------------------------------------------------------------------------------

  subroutine calculate_snow_mass_balance_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    if (.not. allocated(CFGI) ) call initialize_continuous_frozen_ground_index( count( this%active ) )

    call update_continuous_frozen_ground_index( CFGI, this%Tmin, this%Tmax, this%snow_storage )

  end subroutine calculate_snow_mass_balance_sub

!--------------------------------------------------------------------------------------------------

  subroutine calculate_soil_mass_balance_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

 
    this%inflow = this%runon + this%gross_precip - this%interception + this%snowmelt
    call this%calc_infiltration()
    this%runoff = this%inflow - this%infiltration


    print *, " Precip avg:       ", sum( this%gross_precip ) / ubound( this%gross_precip, 1 )

    print *, " Interception avg: ", sum( this%interception ) / ubound( this%interception, 1 )

    print *, " Inflow avg:       ", sum( this%inflow ) / ubound( this%inflow, 1 )

    print *, " Infiltration avg: ", sum( this%infiltration ) / ubound( this%infiltration, 1 )

    print *, " Runoff avg:       ", sum( this%runoff ) / ubound( this%runoff, 1 )

!     if ( fP_minus_PE < 0.0_c_float ) then

!       this%fAPWL = this%fAPWL + abs( fP_minus_PE )

  end subroutine calculate_soil_mass_balance_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_interception_method_sub(this, sMethodName)

    class (MODEL_DOMAIN_T), intent(inout)   :: this
    character (len=*), intent(in)           :: sMethodName

    if ( sMethodName .strequal. "BUCKET" ) then

      this%calc_interception => model_calculate_interception_bucket

    elseif ( sMethodName .strequal. "GASH" ) then

      this%calc_interception => model_calculate_interception_gash

    endif

  end subroutine set_interception_method_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_infiltration_method_sub(this, sMethodName)

    class (MODEL_DOMAIN_T), intent(inout)   :: this
    character (len=*), intent(in)           :: sMethodName

    if ( ( sMethodName .strequal. "C-N" ) .or. ( sMethodName .strequal. "CURVE_NUMBER" ) ) then

      this%calc_infiltration => model_calculate_infiltration_curve_number

!     elseif ( ( sMethodName .strequal. "G-A" ) .or. ( sMethodName .strequal. "GREEN_AMPT" ) ) then

!       this%calc_infiltration => cell_calculate_infiltration_green_ampt

    endif

  end subroutine set_infiltration_method_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_soil_moisture_method_sub(this, sMethodName)

    class (MODEL_DOMAIN_T), intent(inout)   :: this
    character (len=*), intent(in)           :: sMethodName

    if ( ( sMethodName .strequal. "T-M" ) .or. ( sMethodName .strequal. "THORNTHWAITE_MATHER" ) ) then

      this%calc_soil_moisture => model_calculate_soil_moisture_thornthwaite_mather

!     elseif ( ( sMethodName .strequal. "G-A" ) .or. ( sMethodName .strequal. "GREEN_AMPT" ) ) then

!       this%calc_infiltration => cell_calculate_infiltration_green_ampt

    endif

  end subroutine set_soil_moisture_method_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_evapotranspiration_method_sub(this, sMethodName)

    class (MODEL_DOMAIN_T), intent(inout)   :: this
    character (len=*), intent(in)           :: sMethodName

    if ( ( sMethodName .strequal. "HARGREAVES" ) &
         .or. ( sMethodName .strequal. "HARGREAVES-SAMANI" ) ) then

      this%calc_reference_et => model_calculate_et_hargreaves

    elseif ( ( sMethodName .strequal. "JENSEN-HAISE" ) &
         .or. ( sMethodName .strequal. "JH" ) ) then

      this%calc_reference_et => model_calculate_et_jensen_haise

    endif

  end subroutine set_evapotranspiration_method_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_precipitation_data_method_sub  (this, sMethodName)

    class (MODEL_DOMAIN_T), intent(inout)   :: this
    character (len=*), intent(in)           :: sMethodName

    if ( ( sMethodName .strequal. "NORMAL" ) &
         .or. ( sMethodName .strequal. "STANDARD" ) ) then

      this%get_precipitation_data => model_get_precip_normal

    elseif ( ( sMethodName .strequal. "METHOD_OF_FRAGMENTS" ) &
         .or. ( sMethodName .strequal. "FRAGMENTS" ) ) then

      this%get_precipitation_data => model_get_precip_method_of_fragments

    endif

  end subroutine set_precipitation_data_method_sub

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_interception_bucket(this)

    use interception__bucket

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    this%interception = calculate_interception_bucket( this%landuse_index, this%gross_precip )

  end subroutine model_calculate_interception_bucket

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_interception_gash(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_calculate_interception_gash

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_et_hargreaves(this)

    use et__hargreaves_samani

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    this%reference_ET0 = et_hargreaves_ComputeET( iDayOfYear=SIM_DT%iDOY, iNumDaysInYear=SIM_DT%iDaysInYear,    &
         fLatitude=this%latitude, fTMin=this%Tmin, fTMax=this%Tmax )

  end subroutine model_calculate_et_hargreaves

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_et_jensen_haise(this)

    use et__jensen_haise

    class (MODEL_DOMAIN_T), intent(inout)  :: this

     this%reference_ET0 = et_jh_ComputeET( iDayOfYear=SIM_DT%iDOY, iNumDaysInYear=SIM_DT%iDaysInYear, &
       fLatitude=this%latitude, fTMin=this%Tmin, fTMax=this%Tmax )

  end subroutine model_calculate_et_jensen_haise 

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_infiltration_curve_number(this)


    class (MODEL_DOMAIN_T), intent(inout)  :: this


    this%infiltration = calculate_infiltration__curve_number( &
      iLanduseIndex=this%landuse_index, &
      iSoilsIndex=this%soil_group, &
      fSoilStorage=this%soil_storage, &
      fSoilStorage_Max=this%soil_storage_max, &
      fInflow=this%inflow, fCFGI=CFGI )

  end subroutine model_calculate_infiltration_curve_number

!--------------------------------------------------------------------------------------------------

  subroutine model_calculate_soil_moisture_thornthwaite_mather( this )

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_calculate_soil_moisture_thornthwaite_mather

!--------------------------------------------------------------------------------------------------

  subroutine model_get_precip_normal(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this
      print *, __FILE__, ": ", __LINE__
      print *, shape( this%active )

    this%gross_precip = pack( PRCP%pGrdBase%rData, this%active )

  end subroutine model_get_precip_normal  

!--------------------------------------------------------------------------------------------------
 
  subroutine model_get_precip_method_of_fragments(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this
          print *, __FILE__, ": ", __LINE__


    this%gross_precip = pack( PRCP%pGrdBase%rData, this%active )
      print *, __FILE__, ": ", __LINE__


  end subroutine model_get_precip_method_of_fragments 

end module model_domain