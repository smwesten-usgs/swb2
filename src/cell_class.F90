module cell_class

  use iso_c_binding
  use data_catalog_entry
  use constants_and_conversions
  use et_hargreaves
  use et_jensen_haise
  use interception_bucket
  use parameters
  use simulation_datetime
  use strings
  implicit none

  private

  type, public :: CELL_T

    integer (kind=c_int)  :: iSoilGroup = 0_c_int               ! Soil type from soil-type grid
    integer (kind=c_int)  :: iLandUseIndex                      ! Index (row num) of land use table
    integer (kind=c_int)  :: iLandUseCode = 0_c_int                 ! Land use from land-use grid
    real (kind=c_double)  :: dLatitude
    logical (kind=c_bool) :: lActive = lTRUE 

    integer (kind=c_int) :: iSumUpslopeCells = 0_c_int
    integer (kind=c_int) :: iNumUpslopeConnections = 0_c_int

    real (kind=c_float) :: fSoilMoistureStorage = 0.0_c_float    ! Soil moisture in inches of water
    real (kind=c_float) :: fInterceptionStorage = 0.0_c_float    ! This is a reservoir to hold intercepted moisture
    real (kind=c_float) :: fSnowStorage = 0.0_c_float            ! Water present on ground as snow
    real (kind=c_float) :: fStreamStorage = 0.0_c_float          ! Water present in a stream or river
    
    real (kind=c_float) :: fSM_AccumPotentWatLoss = 0.0_c_float  ! Accumulated potential water loss

    real (kind=c_float) :: fReferenceET0 = 0.0_c_float           ! Reference ET0, presumably alfalfa
    real (kind=c_float) :: fReferenceET0_adj = 0.0_c_float       ! ADJUSTED crop ET
    real (kind=c_float) :: fCropETc = 0.0_c_float                ! unadjusted crop ET
    real (kind=c_float) :: fBareSoilEvap = 0.0_c_float
    real (kind=c_float) :: fActualET = 0.0_c_float               ! Actual Evapotranspiration

    real (kind=c_float) :: fInFlow = 0.0_c_float            ! flow in from uphill
    real (kind=c_float) :: fOutFlow = 0.0_c_float           ! flow out downhill
    real (kind=c_float) :: fRouteFraction = 1.0_c_float     ! Fraction of outflow to route downslope
    real (kind=c_float) :: fGrossPrecip = 0.0_c_float       ! Precip - no interception applied
    real (kind=c_float) :: fInterception = 0.0_c_float      ! Interception term
    real (kind=c_float) :: fNetRainfall = 0.0_c_float    ! Net precipitation - precip minus interception
    real (kind=c_float) :: fSnowFall = 0.0_c_float       ! snowfall in inches as SNOW
    real (kind=c_float) :: fSnowMelt = 0.0_c_float       ! snowmelt in inches of water
    real (kind=c_float) :: fTMin = 0.0_c_float           ! Minimum daily temperature
    real (kind=c_float) :: fTMax = 0.0_c_float           ! Maximum daily temperature
    real (kind=c_float) :: fTAvg = 0.0_c_float           ! Average daily temperature
    real (kind=c_float) :: fCFGI = 0.0_c_float           ! Continuous Frozen Ground Index

    real (kind=c_float) :: fGDD = 0.0_c_float              ! Growing Degree Day
    real (kind=c_float) :: fGDD_28F                        ! GDD, base = 29 F
    real (kind=c_float) :: fIrrigationAmount = 0.0_c_float ! total amount of any irrigation
    real (kind=c_float) :: fIrrigationFromGW = 0.0_c_float ! term to hold irrigation term, if any
    real (kind=c_float) :: fIrrigationFromSW = 0.0_c_float ! term to hold irrigation term, if any

    type (CELL_T), pointer :: pDownslope => null()

    procedure ( interception_method ), pointer, private      :: calc_interception => null()

!     procedure ( infiltration_method ), pointer, private :: infiltration_method_ptr => null()
    procedure ( et_method ), pointer, private            :: calc_reference_et => null()
!     procedure ( sm_method ), pointer, private           :: sm_method_ptr => null()

  contains

    procedure :: print_all_sub
    generic   :: print => print_all_sub

    procedure :: cell_set_interception_method_sub
    generic   :: set_interception => cell_set_interception_method_sub

    procedure :: cell_set_evapotranspiration_method_sub
    generic   :: set_evapotranspiration => cell_set_evapotranspiration_method_sub

    procedure :: cell_set_col_row_sub
    generic   :: set_col_row => cell_set_col_row_sub

    procedure :: calculate_mass_balance_sub
    generic   :: solve => calculate_mass_balance_sub

    procedure :: calculate_interception_mass_balance_sub
    procedure :: calculate_soil_mass_balance_sub

    procedure :: cell_set_landuse_index_sub
    generic   :: set_landuse_index => cell_set_landuse_index_sub

!     procedure :: set_infiltration_method_sub
!     generic   :: set_infiltration_method => set_infiltration_method_sub

!     procedure :: set_sm_method_sub
!     generic   :: set_soil_moist_method => set_sm_method_sub


  end type CELL_T

  abstract interface
    subroutine interception_method( this ) 
      import :: CELL_T
      class ( CELL_T ), intent(inout)  :: this
    end subroutine interception_method
  end interface

  abstract interface
    subroutine infiltration_method( this )
      import :: CELL_T
      class ( CELL_T ), intent(inout)  :: this
    end subroutine infiltration_method
  end interface

  abstract interface
    subroutine et_method( this )
      import :: CELL_T
      class ( CELL_T ), intent(inout)  :: this
    end subroutine et_method
  end interface    

  abstract interface
    subroutine sm_method( this )
      import :: CELL_T
      class ( CELL_T ), intent(inout)  :: this
    end subroutine sm_method
  end interface    


contains

  !! implementations of the interfaces above

  subroutine cell_set_landuse_index_sub(this, iLandUseCodes )

    class (CELL_T), intent(inout)      :: this
    integer (kind=c_int), intent(in)   :: iLandUseCodes(:)
        ! [ LOCALS ]
    integer (kind=c_int)                :: iIndex
    logical (kind=c_bool)               :: lMatch

    lMatch = lFALSE

    do iIndex=1, ubound(iLandUseCodes, 1)

      if (this%iLandUseCode == iLandUseCodes(iIndex) ) then
        this%iLandUseIndex = iIndex
        lMatch = lTRUE
        exit
      endif
      
    enddo    
        

    if ( .not. lMatch )     &
      call warn( sMessage="The landuse value "//asCharacter(this%iLandUseCode)//" was not matched with a value from the landuse table",   &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )


  end subroutine cell_set_landuse_index_sub


  subroutine calculate_mass_balance_sub(this)

    class (CELL_T), intent(inout)  :: this

    call this%calculate_interception_mass_balance_sub()
    !call this%calculate_snow_mass_balance_sub()
    call this%calculate_soil_mass_balance_sub()

  end subroutine calculate_mass_balance_sub


  subroutine calculate_interception_mass_balance_sub(this)

    class (CELL_T), intent(inout)   :: this

!    call PRCP%getvalue(this%iCol, this%iRow, this%fGrossPrecip )

    if (associated( this%calc_interception) ) then
      call this%calc_interception()
    else
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__, &
        "This may be happening because there is no check to see whether the user has specified a valid~" &
        //"method in the control file.")
    endif


    if (associated( this%calc_reference_et) ) then
      call this%calc_reference_et()
    else
      call die("INTERNAL PROGRAMMING ERROR--Null procedure pointer.", __FILE__, __LINE__, &
        "This may be happening because there is no check to see whether the user has specified a valid~" &
        //"method in the control file.")
    endif
    
  end subroutine calculate_interception_mass_balance_sub



  subroutine calculate_soil_mass_balance_sub(this)

    class (CELL_T), intent(inout)   :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: i, j
    real (kind=c_double) :: f, g

  end subroutine calculate_soil_mass_balance_sub


  elemental subroutine cell_set_interception_method_sub(this, sMethodName)

    class (CELL_T), intent(inout)   :: this
    character (len=*), intent(in)   :: sMethodName

    if ( sMethodName .strequal. "BUCKET" ) then

      this%calc_interception => cell_calculate_interception_bucket

    elseif ( sMethodName .strequal. "GASH" ) then

      this%calc_interception => cell_calculate_interception_gash

    endif

  end subroutine cell_set_interception_method_sub



  elemental subroutine get_interception_value_sub(this)

    class (CELL_T), intent(inout)   :: this

    call this%calc_interception()

  end subroutine get_interception_value_sub


  elemental subroutine cell_set_evapotranspiration_method_sub(this, sMethodName)

    class (CELL_T), intent(inout)   :: this
    character (len=*), intent(in)   :: sMethodName

    if ( ( sMethodName .strequal. "HARGREAVES" ) &
         .or. ( sMethodName .strequal. "HARGREAVES-SAMANI" ) ) then

      this%calc_reference_et => cell_calculate_et_hargreaves

    elseif ( ( sMethodName .strequal. "JENSEN-HAISE" ) &
         .or. ( sMethodName .strequal. "JH" ) ) then

      this%calc_reference_et => cell_calculate_et_jensen_haise

    endif

  end subroutine cell_set_evapotranspiration_method_sub



  subroutine cell_calculate_et_hargreaves(this)

    class (CELL_T), intent(inout)   :: this

    this%fReferenceET0 = et_hargreaves_ComputeET( iDayOfYear=SIM_DT%iDOY, iNumDaysInYear=SIM_DT%iDaysInYear,    &
         fLatitude=asFloat(this%dLatitude), fTMin=this%fTMin, fTMax=this%fTMax )

  end subroutine cell_calculate_et_hargreaves


   subroutine cell_calculate_et_jensen_haise(this)

     class (CELL_T), intent(inout)   :: this

     this%fReferenceET0 = et_jh_ComputeET( iDayOfYear=SIM_DT%iDOY, iNumDaysInYear=SIM_DT%iDaysInYear, &
       fLatitude=asFloat(this%dLatitude), fTMin=this%fTMin, fTMax=this%fTMax )

   end subroutine cell_calculate_et_jensen_haise 


  subroutine print_all_sub(this)

    class (CELL_T) :: this


  end subroutine print_all_sub



  subroutine cell_calculate_interception_bucket(this)

    use interception_bucket

    class (CELL_T), intent(inout)  :: this

    this%fInterception = calculate_interception_bucket( this%iLandUseIndex, this%fGrossPrecip )

  end subroutine cell_calculate_interception_bucket



  subroutine cell_calculate_interception_gash(this)

    class (CELL_T), intent(inout)  :: this

  end subroutine cell_calculate_interception_gash


  subroutine cell_set_col_row_sub( this, iCol, iRow )

    class (CELL_T), intent(inout)         :: this
    integer (kind=c_int), intent(in)      :: iCol
    integer (kind=c_int), intent(in)      :: iRow

!    this%iRow = iRow
!    this%iCol = iCol

  end subroutine cell_set_col_row_sub  

end module cell_class