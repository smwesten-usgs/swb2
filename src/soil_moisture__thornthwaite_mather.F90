!> @file
!> Contains a single module, @ref sm_thornthwaite_mather, which estimates runoff by
!> means of the NRCS/SCS curve number method.


module soil_moisture__thornthwaite_mather

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use exceptions
  implicit none

  real (kind=c_float), parameter :: APWL_Cap = -40.69_c_float

  ! parameters that allow the Thornthwaite-Mather tables (1957) to be
  ! represented by a single equation
  real (kind=c_double), parameter :: TM_SLOPE_TERM = 0.478769194198665_c_double
  real (kind=c_double), parameter :: TM_EXP_TERM   = -1.03678439421169_c_double

  real (kind=c_float), allocatable  :: APWL(:)

contains

!--------------------------------------------------------------------------------------------------

  subroutine initialize_soil_moisture__thornthwaite_mather( iNumActiveCells )

    integer (kind=c_int), intent(in)  :: iNumActiveCells

    ! [ LOCALS ]
    integer (kind=c_int)  :: iStat

    allocate( APWL( iNumActiveCells ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    APWL = 0.0_c_float 

  end subroutine initialize_soil_moisture__thornthwaite_mather

!--------------------------------------------------------------------------------------------------

  elemental function soil_moisture__thornthwaite_mather_soil_storage( fSWC, fAPWL)  result(fSoilStorage)

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

  end function soil_moisture__thornthwaite_mather_soil_storage

!--------------------------------------------------------------------------------------------------

  function soil_moisture__thornthwaite_mather_update_APWL( fInfiltration, fReference_ET )   result( fAPWL )

    real (kind=c_float), intent(in)  :: fInfiltration
    real (kind=c_float), intent(in)  :: fReference_ET
    real (kind=c_float)              :: fAPWL

    ! [ LOCALS ]
    real (kind=c_float) :: P_minus_PE

    P_minus_PE = fInfiltration - fReference_ET

    if ( P_minus_PE < 0.0_c_float ) then

    else

    endif

  end function soil_moisture__thornthwaite_mather_update_APWL

!--------------------------------------------------------------------------------------------------

  function soil_moisture__thornthwaite_mather_APWL(fSWC, fSoilStorage)  result(fAPWL)

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

  end function soil_moisture__thornthwaite_mather_APWL

!--------------------------------------------------------------------------------------------------

end module soil_moisture__thornthwaite_mather
