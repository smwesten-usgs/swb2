!> @file
!> Contains a single module, @ref sm_thornthwaite_mather, which estimates runoff by
!> means of the NRCS/SCS curve number method.

!> Performs the actual soil-moisture balance once precip, snowmelt, runoff, and ET have
!> been calculated. Soil moisture for a given soil accumulated potential water loss (APWL)
!> is determined by means of Thornthwaite and Mathers' (1957) tables.
module sm_thornthwaite_mather

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types

  implicit none

  real (kind=c_float), parameter :: APWL_Cap = -40.69_c_float

  ! parameters that allow the Thornthwaite-Mather tables (1957) to be
  ! represented by a single equation
  real (kind=c_double), parameter :: rTM_slope_term = 0.478769194198665_c_double
  real (kind=c_double), parameter :: rTM_exp_term = -1.03678439421169_c_double

contains

!------------------------------------------------------------------------------

subroutine sm_thornthwaite_mather_Initialize ( pGrd, pConfig )
  !! Preconfigures soil moisture for the model grid 'pGrd'.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=c_int) :: iRow,iCol
  type ( T_CELL ),pointer :: cel

  ! Initialize the accumulated water loss for each cell according to the
  ! initial soil moisture.

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX

      cel => pGrd%Cells(iCol,iRow)

      if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

      if ( cel%rSoilWaterCap > rNEAR_ZERO ) then

          !! convert input soil moisture (as percent of soil water capacity)
          !! TO soil moisture in inches
          cel%rSoilMoisture = (cel%rSoilMoisturePct/rHUNDRED) * cel%rSoilWaterCap

         ! calculate APWL from equation
         cel%rSM_AccumPotentWatLoss = &
           sm_thornthwaite_mather_APWL(cel%rSoilWaterCap,real(cel%rSoilMoisture, kind=c_double) )

      end if

    end do

  end do

end subroutine sm_thornthwaite_mather_Initialize


!----------------------------------------------------------------------

function sm_thornthwaite_mather_soil_storage(rSWC, rAPWL)  result(dpValue)

  real (kind=c_float), intent(in) :: rSWC     ! max soil-water capacity (inches)
  real (kind=c_float), intent(in) :: rAPWL    ! accum pot. water loss (inches)

  real (kind=c_double) :: dpValue

  ! equation as implemented in R;
  ! sm.df$y = maximum soil-water capacity
  ! sm.df$x = APWL
  ! 10^(log10(sm.df$y) - (s.opt[[1]]*sm.df$y^e.opt[[1]]) * sm.df$x)

  dpValue = dpZERO

  if(rSWC > rZERO ) &

    dpValue = 10_c_double**( log10(REAL(rSWC,kind=c_double)) - &
              ( ABS(REAL(rAPWL,kind=c_double)) * rTM_slope_term &
              * real(rSWC, kind=c_double)**rTM_exp_term ) )


end function sm_thornthwaite_mather_soil_storage

!------------------------------------------------------------------------------

function sm_thornthwaite_mather_APWL(rSWC, dpSoilStorage)  result(dpValue)

  real (kind=c_float), intent(in) :: rSWC          ! max soil-water capacity (inches)
  real (kind=c_double), intent(in) :: dpSoilStorage  ! curr soil storage (inches)

  real (kind=c_double) :: dpValue

  ! equation as implemented in R;
  ! sm.df$y = maximum soil-water capacity
  ! sm.df$x = APWL
  ! (log10(sm.df$y) - log10(sm.df$pred)) / (s.opt[[1]] * sm.df$y^e.opt[[1]])

  dpValue = dpZERO

  if(rSWC > rZERO .and. dpSoilStorage > dpZERO) &

    dpValue = -( log10(REAL(rSWC,kind=c_double)) - log10(dpSoilStorage)) / &
          ( rTM_slope_term * REAL(rSWC,kind=c_double)**rTM_exp_term )

end function sm_thornthwaite_mather_APWL

!------------------------------------------------------------------------------

subroutine sm_thornthwaite_mather_UpdatePctSM( pGrd )

  type ( T_GENERAL_GRID ),pointer :: pGrd

  where(pGrd%Cells%rSoilWaterCap > rNEAR_ZERO )

    pGrd%Cells%rSoilMoisturePct = pGrd%Cells%rSoilMoisture  &
       / pGrd%Cells%rSoilWaterCap * 100.

  elsewhere

    pGrd%Cells%rSoilMoisturePct = rZERO

  endwhere

end subroutine sm_thornthwaite_mather_UpdatePctSM

!------------------------------------------------------------------------------

end module sm_thornthwaite_mather
