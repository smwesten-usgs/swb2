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
  use constants_and_conversions

  implicit none

  private

  public :: et_jh_ComputeET

contains

function et_jh_ComputeET(( iDayOfYear, iNumDaysInYear, fLatitude, fTMin, fTMax, fAlbedo )  result(fReferenceET0)

  integer (kind=c_int),intent(in) :: iDayOfYear
  integer (kind=c_int),intent(in) :: iNumDaysInYear
  real (kind=c_float), intent(in) :: fLatitude
  real (kind=c_float), intent(in) :: fTMin
  real (kind=c_float), intent(in) :: fTMax
  real (kind=c_float), intent(in) :: fAlbedo
  real (kind=c_float)             :: fReferenceET0

  ! [ LOCALS ]
  real (kind=c_float) :: rSo,rDelta,rOmega_s,rD_r,rS0,rSn,rT,fTAvg
  integer (kind=c_int) :: iCol, iRow
  ! [ CONSTANTS ]
  real (kind=c_float),parameter :: UNIT_CONV = 0.41_c_float / 25.4_c_float

  fTAvg = ( fTMin + fTMax ) / 2_c_float

  fD_r = 1.0_c_float + 0.033_c_float * cos( TWOPI * iDayOfYear / iNumDaysInYear )

  fD_r = relative_earth_sun_distance__D_r( iDayOfYear=iDayOfYear, iNumDaysInYear=iNumDaysInYear )

  fDelta = 0.4093_c_float * sin( (TWOPI * iDayOfYear / iNumDaysInYear ) - 1.405_c_float )

  fOmega_s = acos( -tan(fLatitude) * tan(rDelta) )

  fSo = 2.44722_c_float * 15.392_c_float * rD_r * (     rOmega_s  * sin(fLatitude) * sin(rDelta) + &
                                                  sin(rOmega_s) * cos(fLatitude) * cos(rDelta) )
  fSn = fSo * ( 1.0_c_float - fAlbedo ) * ( rAs + rBS * rSunPct / 100.0_c_float )


  if ( fTAvg <= rFREEZING ) then
    fReferenceET0 = 0.0_c_float
  else
    fReferenceET0 = UNIT_CONV * ( 0.025_c_float * rT + 0.078_c_float ) * rSn
  end if


end function et_jh_ComputeET

end module et_jensen_haise
