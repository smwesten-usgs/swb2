!> @file
!>  Contains a single module, et_jensen_haise, which
!>  calculates potential evapotranspiration by means of the Jensen-Haise (1963) method.


!>  Calculates potential evapotranspiration by means of the
!>  Jensen-Haise (1963) method.
module et__jensen_haise
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
  use solar_calculations
  use constants_and_conversions

  implicit none

  private

  public :: et_jh_calculate

contains

elemental function et_jh_calculate( iDayOfYear, iNumDaysInYear, fLatitude, fTMin, fTMax,    &
                                    fAlbedo, fAs, fBs, fSunPct )  result(fReferenceET0)

  integer (kind=c_int), intent(in)          :: iDayOfYear
  integer (kind=c_int), intent(in)          :: iNumDaysInYear
  real (kind=c_float), intent(in)           :: fLatitude
  real (kind=c_float), intent(in)           :: fTMin
  real (kind=c_float), intent(in)           :: fTMax
  real (kind=c_float), intent(in), optional :: fAlbedo
  real (kind=c_float), intent(in), optional :: fAs
  real (kind=c_float), intent(in), optional :: fBs
  real (kind=c_float), intent(in), optional :: fSunPct
  real (kind=c_float)                       :: fReferenceET0

  ! [ LOCALS ]
  real (kind=c_float) :: fSo
  real (kind=c_float) :: fDelta
  real (kind=c_float) :: fOmega_s
  real (kind=c_float) :: fD_r
  real (kind=c_float) :: fSn
  real (kind=c_float) :: fTAvg

  real (kind=c_float) :: fAlbedo_
  real (kind=c_float) :: fAs_
  real (kind=c_float) :: fBs_
  real (kind=c_float) :: fSunPct_
  
  ! [ CONSTANTS ]
  real (kind=c_float),parameter :: UNIT_CONV = 0.41_c_float / 25.4_c_float

  if (present( fAlbedo ) ) then
    fAlbedo_ = fAlbedo
  else
    fAlbedo_ = 0.23_c_float
  endif    

  if (present( fAs ) ) then
    fAs_ = fAs
  else
    fAs_ = 0.25_c_float
  endif    

  if (present( fBs ) ) then
    fBs_ = fBs
  else
    fBs_ = 0.5_c_float
  endif    

  if (present( fSunPct ) ) then
    fSunPct_ = fSunPct
  else
    fSunPct_ = estimate_percent_of_possible_sunshine__psun(fTMax=fTMax, fTMin=fTMin)
  endif    



  fTAvg = ( fTMin + fTMax ) / 2_c_float

!  fD_r = 1.0_c_float + 0.033_c_float * cos( TWOPI * iDayOfYear / iNumDaysInYear )

  fD_r = relative_earth_sun_distance__D_r( iDayOfYear=iDayOfYear, iNumDaysInYear=iNumDaysInYear )

!  fDelta = 0.4093_c_float * sin( (TWOPI * iDayOfYear / iNumDaysInYear ) - 1.405_c_float )

  fDelta = solar_declination_simple__delta( iDayOfYear=iDayOfYear, iNumDaysInYear=iNumDaysInYear )

  fOmega_s = acos( -tan(fLatitude) * tan(fDelta) )

  fSo = 2.44722_c_float * 15.392_c_float * fD_r * (     fOmega_s  * sin(fLatitude) * sin(fDelta) + &
                                                  sin(fOmega_s) * cos(fLatitude) * cos(fDelta) )

  fSn = fSo * ( 1.0_c_float - fAlbedo_ ) * ( fAs_ + fBs_ * fSunPct_ / 100.0_c_float )


  if ( fTAvg <= rFREEZING ) then
    fReferenceET0 = 0.0_c_float
  else
    fReferenceET0 = UNIT_CONV * ( 0.025_c_float * fTAvg + 0.078_c_float ) * fSn
  end if

!> @todo Check these equations against original paper.

!! original paper gives ET0 as:

!! Etp = ( 0.014 * Tavg - 0.37 ) * Rs
!
! where Rs is the solar radiation in inches per day 

end function et_jh_calculate

end module et__jensen_haise
