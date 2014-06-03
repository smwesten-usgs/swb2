module cell_class

  use iso_c_binding
  use constants_and_conversions
  use et_hargreaves
  implicit none

  private

  type, public :: CELL_BASE_CLASS_T

    integer (kind=c_int) :: iSoilGroup = 0_c_int               ! Soil type from soil-type grid
    integer (kind=c_int) :: iLandUseIndex                      ! Index (row num) of land use table
    integer (kind=c_int) :: iLandUse = 0_c_int                 ! Land use from land-use grid
    real (kind=c_double) :: fLatitude

  contains

    procedure           :: print_all_sub
    generic             :: print => print_all_sub

  end type CELL_BASE_CLASS_T  

  type, extends(CELL_BASE_CLASS_T), public :: CELL_NORMAL_T

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

    type (CELL_NORMAL_T), pointer :: pDownslope

    procedure ( interception_method ), pointer, private      :: interception_method_ptr => null()
    procedure ( interception_init_method ), pointer, private :: interception_init_method_ptr => null()

    procedure ( infiltration_method ), pointer, private :: infiltration_method_ptr => null()
    procedure ( et_method ), pointer, private           :: et_method_ptr => null()
    procedure ( sm_method ), pointer, private           :: sm_method_ptr => null()

  contains

    procedure :: set_interception_method_sub
    generic   :: set_interception => set_interception_method_sub

    procedure :: set_infiltration_method_sub
    generic   :: set_infiltration => set_infiltration_method_sub

    procedure :: set_et_method_sub
    generic   :: set_evapotranspiration => set_et_method_sub

    procedure :: set_sm_method_sub
    generic   :: set_soil_moist => set_sm_sub


  end type CELL_NORMAL_T

  abstract interface
    subroutine interception_method(this)
      import :: CELL_NORMAL_T
      class ( CELL_NORMAL_T ), intent(inout)  :: this
    end subroutine interception_method
  end interface

  abstract interface
    subroutine interception_init_method(this)
      import :: CELL_NORMAL_T
      class ( CELL_NORMAL_T ), intent(inout)  :: this
    end subroutine interception_init_method
  end interface

  abstract interface
    subroutine infiltration_method(this)
      import :: CELL_NORMAL_T
      class ( CELL_NORMAL_T ), intent(inout)  :: this
    end subroutine infiltration_method
  end interface

  abstract interface
    subroutine et_method(this)
      import :: CELL_NORMAL_T
      class ( CELL_NORMAL_T ), intent(inout)  :: this
    end subroutine et_method
  end interface    

  abstract interface
    subroutine sm_method(this)
      import :: CELL_NORMAL_T
      class ( CELL_NORMAL_T ), intent(inout)  :: this
    end subroutine sm_method
  end interface    


contains

  !! implementations of the interfaces above

  subroutine et_hargreaves_samani(this)

    class (CELL_NORMAL_T), intent(inout)   :: this

    this%fReferenceET0 = et_hargreaves_ComputeET( iDayOfYear=180, iNumDaysInYear=365,    &
         fLatitude=asFloat(this%fLatitude), fTMin=this%fTMin, fTMax=this%fTMax )

  end subroutine et_hargreaves_samani 


!   subroutine et_jensen_haise(this)

!     class (CELL_T), intent(inout)   :: this

!     this%fReferenceET0 = et_jh_ComputeET( iDayOfYear, iNumDaysInYear, this%fLatitude, this%fTMin, this%fTMax )

!   end subroutine et_jensen_haise 


  subroutine print_all_sub(this)

    class (CELL_BASE_CLASS_T) :: this

    select type(this)

      type is (CELL_NORMAL_T)

        print *, "Normal cell"

      type is (CELL_BASE_CLASS_T)

        print *, "Inactive cell"

      class default
      
    end select  


  end subroutine print_all_sub

end module cell_class