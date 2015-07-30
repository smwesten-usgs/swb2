!> @file
!> Contains a single module, @ref sm_thornthwaite_mather, which estimates runoff by
!> means of the NRCS/SCS curve number method.


module soil_moisture__thornthwaite_mather

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use exceptions
  use parameters
  use strings
  use string_list
  implicit none

  private

  public :: soil_moisture_thornthwaite_mather_initialize
  public :: soil_moisture_thornthwaite_mather_calculate

  ! parameters that allow the Thornthwaite-Mather tables (1957) to be
  ! represented by a single equation
  real (kind=c_double), parameter :: TM_SLOPE_TERM = 0.478769194198665_c_double
  real (kind=c_double), parameter :: TM_EXP_TERM   = -1.03678439421169_c_double

  real (kind=c_float), allocatable, public  :: APWL(:)
  integer (kind=c_int), allocatable         :: iLanduseCodes(:)
  real (kind=c_float), allocatable          :: MAX_ROOTING_DEPTH(:,:)

contains

!--------------------------------------------------------------------------------------------------

  subroutine soil_moisture_thornthwaite_mather_initialize( iNumActiveCells )

    integer (kind=c_int), intent(in)     :: iNumActiveCells

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat

    allocate( APWL( iNumActiveCells ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    APWL = 0.0_c_float 

  end subroutine soil_moisture_thornthwaite_mather_initialize

!--------------------------------------------------------------------------------------------------

  elemental function calc_soil_storage_given_current_APWL( fSWC, fAPWL)  result(fSoilStorage)

    real (kind=c_float), intent(in) :: fSWC     ! max soil-water capacity (inches)
    real (kind=c_float), intent(in) :: fAPWL    ! accum pot. water loss (inches)
    real (kind=c_float)             :: fSoilStorage

    ! equation as implemented in R;
    ! sm.df$y = maximum soil-water capacity
    ! sm.df$x = APWL
    ! 10^(log10(sm.df$y) - (s.opt[[1]]*sm.df$y^e.opt[[1]]) * sm.df$x)

    if (fSWC > 0.0_c_float ) then

      fSoilStorage = 10.0_c_float**( log10( fSWC )  - ( abs( fAPWL ) * TM_SLOPE_TERM * fSWC**TM_EXP_TERM ) )
   
    else
   
      fSoilStorage = 0.0_c_float
  
    endif

  end function calc_soil_storage_given_current_APWL

!--------------------------------------------------------------------------------------------------

  elemental function calc_APWL_given_soil_storage(fSWC, fSoilStorage)  result(fAPWL)

    real (kind=c_float), intent(in) :: fSWC            ! max soil-water capacity (inches)
    real (kind=c_float), intent(in) :: fSoilStorage  ! curr soil storage (inches)
    real (kind=c_float)             :: fAPWL

    ! equation as implemented in R;
    ! sm.df$y = maximum soil-water capacity
    ! sm.df$x = APWL
    ! (log10(sm.df$y) - log10(sm.df$pred)) / (s.opt[[1]] * sm.df$y^e.opt[[1]])

    if (fSWC > 0.0_c_float .and. fSoilStorage > 0.0_c_float ) then

      fAPWL = -( log10(fSWC) - log10(fSoilStorage)) / ( TM_SLOPE_TERM * fSWC**TM_EXP_TERM )

    else
    
      fAPWL = 0.0_c_float

    endif  

  end function calc_APWL_given_soil_storage

!--------------------------------------------------------------------------------------------------

  elemental subroutine soil_moisture_thornthwaite_mather_calculate(fAPWL, fSoilStorage, fSoilStorage_Excess,                &
                                                                   fSoilStorage_Max, fInfiltration, fReference_ET)

    real (kind=c_float), intent(inout)   :: fAPWL
    real (kind=c_float), intent(inout)   :: fSoilStorage
    real (kind=c_float), intent(out)     :: fSoilStorage_Excess
    real (kind=c_float), intent(in)      :: fSoilStorage_Max
    real (kind=c_float), intent(in)      :: fInfiltration
    real (kind=c_float), intent(in)      :: fReference_ET

    ! [ LOCALS ]
    real (kind=c_float) :: fP_minus_PE
    real (kind=c_float) :: fOldSoilStorage

    fP_minus_PE = fInfiltration - fReference_ET

    ! P - PE < 0: soil is losing moisture; ET exceeds infiltration/precip
    ! must update new APWL, then back-calculate new soil moisture
    !
    ! in this case, Actual ET will be less than Potential or Reference ET because
    ! of resistance in the soil layer to giving up moisture at the current level of
    ! accumulated potential water loss
    !
    if ( fP_minus_PE < 0.0_c_float ) then

      fAPWL = fAPWL - fP_minus_PE
      fSoilStorage = calc_soil_storage_given_current_APWL( fSWC=fSoilStorage_Max, fAPWL=fAPWL )
      fSoilStorage_Excess = 0.0_c_float
    
    else ! P > PE: soil is gaining moisture; add P - PE directly to soil moisture, then
         ! back-calculate a new APWL that corresponds to the new soil moisture
         !
         ! Actual ET is assumed to be equal to potential ET
         !

      fSoilStorage = fSoilStorage + fP_minus_PE

      if (fSoilStorage > fSoilStorage_Max ) then

        fSoilStorage_Excess = fSoilStorage - fSoilStorage_Max
        fSoilStorage = fSoilStorage_Max

      else

        fSoilStorage_Excess = 0.0_c_float

      endif

      fAPWL = calc_APWL_given_soil_storage(fSWC=fSoilStorage_Max, fSoilStorage=fSoilStorage)

    endif

  end subroutine soil_moisture_thornthwaite_mather_calculate

end module soil_moisture__thornthwaite_mather
