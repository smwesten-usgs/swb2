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

  type, public :: MODEL_DOMAIN_T

    character (len=:), allocatable  :: PROJ4_string
    integer (kind=c_int)            :: number_of_columns
    integer (kind=c_int)            :: number_of_rows
    real (kind=c_double)            :: X_ll, Y_ll
    real (kind=c_double)            :: X_ur, Y_ur
    real (kind=c_double)            :: gridcellsize

    logical (kind=c_bool), allocatable     :: active(:,:)

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
    real (kind=c_float), allocatable       :: GDD_20(:)
     
    real (kind=c_float), allocatable       :: interception_storage(:)
    real (kind=c_float), allocatable       :: snow_storage(:)
    real (kind=c_float), allocatable       :: soil_storage(:)
    real (kind=c_float), allocatable       :: soil_storage_max(:)
    real (kind=c_float), allocatable       :: stream_storage(:)
         
    real (kind=c_float), allocatable       :: gross_precip(:)
    real (kind=c_float), allocatable       :: Tmin(:)
    real (kind=c_float), allocatable       :: Tmax(:)
    real (kind=c_float), allocatable       :: routing_fraction(:)

    procedure ( interception_method ), pointer, private      :: calc_interception => null()
    procedure ( infiltration_method ), pointer, private      :: calc_infiltration => null()
    procedure ( et_method ), pointer, private                :: calc_reference_et => null()
!     procedure ( sm_method ), pointer, private           :: sm_method_ptr => null()

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

  type (MODEL_DOMAIN_T), public :: MODEL

contains


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

  end subroutine initialize_grid_sub



  subroutine initialize_arrays_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)        :: this

    ! [ LOCALS ]
    integer (kind=c_int)  :: iCount
    integer (kind=c_int)  :: iStat(22)

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
    allocate( this%GDD_20(iCount), stat=iStat(17) )
    allocate( this%interception_storage(iCount), stat=iStat(18) )
    allocate( this%snow_storage(iCount), stat=iStat(19) )
    allocate( this%soil_storage(iCount), stat=iStat(20) )
    allocate( this%soil_storage_max(iCount), stat=iStat(21))
    allocate( this%stream_storage(iCount), stat=iStat(22) )

    if ( any( iStat /= 0 ) )  call die("Problem allocating memory", __FILE__, __LINE__)

  end subroutine initialize_arrays_sub

  
  subroutine set_inactive_cells_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    this%active = .true._c_bool

    where (       HSG%pGrdBase%iData < 1 &
            .or.  LULC%pGrdBase%iData < 1 )

      this%active = .false._c_bool

    end where

    call LOGS%write(asCharacter(count(this%active))//" calls are currently active out of a total of " &
      //asCharacter(size(this%active)), iLinesBefore=1, iLinesAfter=1)

  end subroutine set_inactive_cells_sub




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

    pGrd => grid_Create( iNX=this%number_of_columns, iNY=this%number_of_rows, &
        rX0=this%X_ll, rY0=this%Y_ll, &
        rGridCellSize=this%gridcellsize, iDataType=GRID_DATATYPE_INT )  

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
      print *, __FILE__,": ", __LINE__
      call this%solve()
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


      call PRCP%getvalues( iMonth, iDay, iYear, iJulianDay )
      call TMIN%getvalues( iMonth, iDay, iYear, iJulianDay )
      call TMAX%getvalues( iMonth, iDay, iYear, iJulianDay )

      this%gross_precip = pack( PRCP%pGrdBase%rData, this%active )
      this%Tmax = pack( TMAX%pGrdBase%rData, this%active )
      this%Tmin = pack( TMIN%pGrdBase%rData, this%active )

    end associate

  end subroutine get_climate_data

!--------------------------------------------------------------------------------------------------


  subroutine calculate_mass_balance_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    call this%calculate_interception_mass_balance_sub()
    call this%calculate_snow_mass_balance_sub()
    call this%calculate_soil_mass_balance_sub()

  end subroutine calculate_mass_balance_sub



  subroutine preflight_check_function_pointers(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    if (.not. associated( this%calc_interception) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__, &
        "This may be happening because there is no check to see whether the user has specified a valid~" &
        //"method in the control file.")


    if (.not. associated( this%calc_reference_et) ) &
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__, &
        "This may be happening because there is no check to see whether the user has specified a valid~" &
        //"method in the control file.")


  end subroutine preflight_check_function_pointers


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


  subroutine calculate_snow_mass_balance_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    if (.not. allocated(CFGI) ) allocate(CFGI( count(this%active) ), stat=iStat )

    call update_continuous_frozen_ground_index( CFGI, this%Tmin, this%Tmax, this%snow_storage )

  end subroutine calculate_snow_mass_balance_sub



  subroutine calculate_soil_mass_balance_sub(this)

    class (MODEL_DOMAIN_T), intent(inout)   :: this

 
    this%inflow = this%runon + this%gross_precip - this%interception + this%snowmelt

!     if ( fP_minus_PE < 0.0_c_float ) then

!       this%fAPWL = this%fAPWL + abs( fP_minus_PE )

  end subroutine calculate_soil_mass_balance_sub




  subroutine set_interception_method_sub(this, sMethodName)

    class (MODEL_DOMAIN_T), intent(inout)   :: this
    character (len=*), intent(in)           :: sMethodName

    if ( sMethodName .strequal. "BUCKET" ) then

      this%calc_interception => model_calculate_interception_bucket

    elseif ( sMethodName .strequal. "GASH" ) then

      this%calc_interception => model_calculate_interception_gash

    endif

  end subroutine set_interception_method_sub


  subroutine set_infiltration_method_sub(this, sMethodName)

    class (MODEL_DOMAIN_T), intent(inout)   :: this
    character (len=*), intent(in)           :: sMethodName

    if ( ( sMethodName .strequal. "C-N" ) .or. ( sMethodName .strequal. "CURVE_NUMBER" ) ) then

      this%calc_infiltration => model_calculate_infiltration_curve_number

!     elseif ( ( sMethodName .strequal. "G-A" ) .or. ( sMethodName .strequal. "GREEN_AMPT" ) ) then

!       this%calc_infiltration => cell_calculate_infiltration_green_ampt

    endif

  end subroutine set_infiltration_method_sub



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



  subroutine model_calculate_interception_bucket(this)

    use interception__bucket

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    this%interception = calculate_interception_bucket( this%landuse_index, this%gross_precip )

  end subroutine model_calculate_interception_bucket



  subroutine model_calculate_interception_gash(this)

    class (MODEL_DOMAIN_T), intent(inout)  :: this

  end subroutine model_calculate_interception_gash



  subroutine model_calculate_et_hargreaves(this)

    use et__hargreaves_samani

    class (MODEL_DOMAIN_T), intent(inout)  :: this

    this%reference_ET0 = et_hargreaves_ComputeET( iDayOfYear=SIM_DT%iDOY, iNumDaysInYear=SIM_DT%iDaysInYear,    &
         fLatitude=this%latitude, fTMin=this%Tmin, fTMax=this%Tmax )

  end subroutine model_calculate_et_hargreaves


  subroutine model_calculate_et_jensen_haise(this)

    use et__jensen_haise

    class (MODEL_DOMAIN_T), intent(inout)  :: this

     this%reference_ET0 = et_jh_ComputeET( iDayOfYear=SIM_DT%iDOY, iNumDaysInYear=SIM_DT%iDaysInYear, &
       fLatitude=this%latitude, fTMin=this%Tmin, fTMax=this%Tmax )

  end subroutine model_calculate_et_jensen_haise 



  subroutine model_calculate_infiltration_curve_number(this)


    class (MODEL_DOMAIN_T), intent(inout)  :: this


  end subroutine model_calculate_infiltration_curve_number





end module model_domain