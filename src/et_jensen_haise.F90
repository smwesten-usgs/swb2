!> @file
!>  Contains a single module, et_jensen_haise, which
!>  calculates potential evapotranspiration by means of the Jensen-Haise (1963) method.


!>  Calculates potential evapotranspiration by means of the
!>  Jensen-Haise (1963) method.
module et_jensen_haise
!!****h* SWB/et_jensen_haise
! NAME
!   et_jensen_haise.f95 - Evapotranspiration calculation using the
!   Jensen-Haise method.
!
! SYNOPSIS
!   This module calculates evapotranspiration using the Jensen-Haise
!   method.
!
! NOTES
!   Original method is documented in:
!
!   Jensen, M.E., H.R. Haise. 1963. Estimating evapotranspiration from solar
!   radiation. Journal of Irrigation and Drainage Engineering 89(IR4):15-41.
!
!!***

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types

  implicit none

  private

contains


subroutine et_jh_initialize( )
  
end subroutine et_jh_initialize



subroutine et_jh_ComputeET(  )
  !! Computes the potential ET for each cell, based on the meteorological
  !! data given. Stores cell-by-cell PET values in the model grid.
  !! Note: for the T-M model, it's constant scross the grid
  !!
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  integer (kind=c_int),intent(in) :: iDayOfYear

  ! [ LOCALS ]
  real (kind=c_float) :: rSo,rDelta,rOmega_s,rD_r,rS0,rSn,rT
  integer (kind=c_int) :: iCol, iRow
  ! [ CONSTANTS ]
  real (kind=c_float),parameter :: UNIT_CONV = 0.41_c_float / 25.4_c_float

  rD_r = rONE + 0.033_c_float * cos( dpTWOPI * iDayOfYear / 365.0_c_float )

  rDelta = 0.4093_c_float * sin( (dpTWOPI * iDayOfYear / 365.0_c_float) - 1.405_c_float )

  rOmega_s = acos( -tan(rLatitude) * tan(rDelta) )

  rSo = 2.44722_c_float * 15.392_c_float * rD_r * (     rOmega_s  * sin(rLatitude) * sin(rDelta) + &
                                                  sin(rOmega_s) * cos(rLatitude) * cos(rDelta) )
  rSn = rSo * ( rONE-rAlbedo ) * ( rAs + rBS * rSunPct / rHUNDRED )


      if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

      if ( pGrd%Cells(iCol,iRow)%rTAvg <= rFREEZING ) then
        pGrd%Cells(iCol,iRow)%rReferenceET0 = rZERO
      else
        rT = FtoC(pGrd%Cells(iCol,iRow)%rTAvg)
        pGrd%Cells(iCol,iRow)%rReferenceET0 = UNIT_CONV * ( 0.025_c_float * rT + 0.078_c_float ) * rSn
      end if


end subroutine et_jh_ComputeET

end module et_jensen_haise
