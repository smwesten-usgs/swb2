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

  use iso_c_binding,               only : c_short, c_int, c_float, c_double
  use solar_calculations,          only : estimate_percent_of_possible_sunshine__psun,   &
                                          relative_earth_sun_distance__D_r,              &
                                          extraterrestrial_radiation__Ra,                &
                                          solar_declination_simple__delta,               &
                                          sunrise_sunset_angle__omega_s,                 &
                                          solar_radiation__Rs
  use meteorological_calculations, only : equivalent_evaporation
  use constants_and_conversions,   only : F_to_K, rFREEZING, MM_PER_IN

  implicit none

  private

  public :: et_jh_calculate

contains

elemental function et_jh_calculate( iDayOfYear, iNumDaysInYear, fLatitude, fTMin, fTMax,    &
                                    fAs, fBs, fSunPct )                      result(fReferenceET0)

  integer (c_int), intent(in)          :: iDayOfYear
  integer (c_int), intent(in)          :: iNumDaysInYear
  real (c_float), intent(in)           :: fLatitude
  real (c_float), intent(in)           :: fTMin
  real (c_float), intent(in)           :: fTMax
  real (c_float), intent(in), optional :: fAs
  real (c_float), intent(in), optional :: fBs
  real (c_float), intent(in), optional :: fSunPct
  real (c_double)                      :: fReferenceET0

  ! [ LOCALS ]
  real (c_double) :: dRa
  real (c_double) :: dDelta
  real (c_double) :: dOmega_s
  real (c_double) :: dD_r
  real (c_double) :: dRs
  real (c_float)  :: fTAvg

  real (c_double) :: dAs_l
  real (c_double) :: dBs_l
  real (c_double) :: dSunPct_l

  if (present( fAs ) ) then
    dAs_l = fAs
  else
    dAs_l = 0.25_c_double
  endif

  if (present( fBs ) ) then
    dBs_l = fBs
  else
    dBs_l = 0.5_c_double
  endif

  if (present( fSunPct ) ) then
    dSunPct_l = fSunPct
  else
    dSunPct_l = estimate_percent_of_possible_sunshine__psun(fTMax=F_to_K( fTMax ), fTMin=F_to_K( fTMin ) )
  endif


  fTAvg = ( fTMin + fTMax ) / 2_c_float

!  fD_r = 1.0_c_float + 0.033_c_float * cos( TWOPI * iDayOfYear / iNumDaysInYear )

  dD_r = relative_earth_sun_distance__D_r( iDayOfYear=iDayOfYear, iNumDaysInYear=iNumDaysInYear )

!  fDelta = 0.4093_c_float * sin( (TWOPI * iDayOfYear / iNumDaysInYear ) - 1.405_c_float )

  dDelta = solar_declination_simple__delta( iDayOfYear=iDayOfYear, iNumDaysInYear=iNumDaysInYear )

!  fOmega_s = acos( -tan(fLatitude) * tan(fDelta) )

  dOmega_s = sunrise_sunset_angle__omega_s( real( fLatitude, c_double), dDelta )

!  dSo = 2.44722_c_float * 15.392_c_float * dD_r * (     dOmega_s  * sin(dLatitude) * sin(dDelta) + &
!                                                  sin(dOmega_s) * cos(dLatitude) * cos(dDelta) )

  dRa =  extraterrestrial_radiation__Ra(dLatitude=real( fLatitude, c_double),    &
                                                        dDelta=dDelta,                &
                                                        dOmega_s=dOmega_s,            &
                                                        dDsubR=dD_r )

!  dSn = dSo * ( 1.0_c_float - fAlbedo_l ) * ( fAs_l + fBs_l * fSunPct_l / 100.0_c_float )

  dRs = solar_radiation__Rs(dRa=dRa, dAs=dAs_l, dBs=dBs_l, fPctSun=dSunPct_l )

  if ( fTAvg <= rFREEZING ) then
    fReferenceET0 = 0.0_c_float
  else
    ! 'equivalent_evaporation' yields the depth of water that could be evaporated for the
    ! given solar radiation in mm
    fReferenceET0 = ( 0.014_c_float * fTavg - 0.37_c_float ) * equivalent_evaporation( dRs ) / MM_PER_IN
!    fReferenceET0 = UNIT_CONV * ( 0.025_c_float * fTAvg + 0.078_c_float ) * fSn
  end if

!> @todo Check these equations against original paper.

!! original paper gives ET0 as:

!! Etp = ( 0.014 * Tavg - 0.37 ) * Rs
!
! where Rs is the solar radiation in inches per day

end function et_jh_calculate

end module et__jensen_haise
