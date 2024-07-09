!> @file
!>  Contains a single module, et_hargreaves, which
!>  calculates potential evapotranspiration by means of the Hargreaves-Samani (1985) method.


!>  Calculates potential evapotranspiration by means of the
!>  Hargreaves-Samani (1985) method.
module et__hargreaves_samani
!!****h* SWB/et_hargreaves
! NAME
!   et_hargreaves.f95 - Evapotranspiration calculation using the
!   Hargreaves method.
!
! SYNOPSIS
!   This module calculates evapotranspiration using the Hargreaves
!   method.
!
! NOTES
!
!  Reference:
!
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
!!***

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use meteorological_calculations
  use parameters, only                : PARAMS
  use solar_calculations

  implicit none


  ! ET parameters -- default values are from Hargreaves and Samani (1985)
  real (c_double) :: ET_SLOPE         ! = 0.0023     
  real (c_double) :: ET_EXPONENT      ! = 0.5
  real (c_double) :: ET_CONSTANT      ! = 17.8

contains

subroutine et_hargreaves_initialize( ) !pConfig, sRecord )
  !! Configures the module, using the command line in 'sRecord'
  ! [ ARGUMENTS ]
!  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
!  character (len=*),intent(inout) :: sRecord

  ! [ LOCALS ]
  character (len=256) :: sOption
  integer (c_int) :: iStat
  real (c_float) :: rValue

  real (kind=c_float), allocatable :: fET_slope(:)
  real (kind=c_float), allocatable :: fET_exponent(:)
  real (kind=c_float), allocatable :: fET_constant(:)

  call PARAMS%get_parameters( sKey="Hargreaves_ET_slope", fValues=fET_slope)
  call PARAMS%get_parameters( sKey="Hargreaves_ET_exponent", fValues=fET_exponent)
  call PARAMS%get_parameters( sKey="Hargreaves_ET_constant", fValues=fET_constant)

  if ( fET_slope(1) > fTINYVAL) then
    ET_SLOPE = real(fET_slope(1), c_double)
  else
    ET_SLOPE = 0.0023_c_double
  endif

  if ( fET_exponent(1) > fTINYVAL) then
    ET_EXPONENT = real(fET_exponent(1), c_double)
  else
    ET_EXPONENT = 0.5_c_double
  endif

  if ( fET_constant(1) > fTINYVAL) then
    ET_CONSTANT = real(fET_constant(1), c_double)
  else
    ET_CONSTANT = 17.8_c_double
  endif

  print *, "****************************************************************************"
  print *, "initializing Hargreaves ET parameters"
  print *, "****************************************************************************"
  print *, "  ET_SLOPE     = ", ET_SLOPE
  print *, "  ET_EXPONENT  = ", ET_EXPONENT
  print *, "  ET_CONSTANT  = ", ET_CONSTANT

  !  write(UNIT=LU_LOG,FMT=*) "Configuring Hargreaves PET model"

!   if (pConfig%rSouthernLatitude <= rNO_DATA_NCDC &
!     .or. pConfig%rNorthernLatitude <= rNO_DATA_NCDC) then

!     call Chomp( sRecord,sOption )
!     read ( unit=sOption, fmt=*, iostat=iStat ) rValue
!     call Assert( iStat == 0, "Could not read the southerly latitude" )
!     pConfig%rSouthernLatitude = dpTWOPI * rValue / 360.0_c_float

!     call Chomp( sRecord,sOption )
!     read ( unit=sOption, fmt=*, iostat=iStat ) rValue
!     call Assert( iStat == 0, "Could not read the northerly latitude" )
!     pConfig%rNorthernLatitude = dpTWOPI * rValue / 360.0_c_float

!   else

!     call echolog("Southern and northern latitude values have been determined" &
!       //"~from project grid bounds and projection parameters. The values supplied" &
!       //"~with the Hargreaves PET option will be ignored...")

!   endif

end subroutine et_hargreaves_initialize

!------------------------------------------------------------------------------

impure elemental function et_hargreaves_calculate( iDayOfYear, iNumDaysInYear, fLatitude, fTMin, fTMax )  result(fReferenceET0)
  !! Computes the potential ET for each cell, based on TMIN and TMAX.
  !! Stores cell-by-cell PET values in the model grid.

  integer (c_int),intent(in) :: iDayOfYear
  integer (c_int),intent(in) :: iNumDaysInYear
  real (c_float), intent(in) :: fLatitude
  real (c_float), intent(in) :: fTMin
  real (c_float), intent(in) :: fTMax
  real (c_double)             :: fReferenceET0

  ! [ LOCALS ]
  real (c_double) :: fDelta, fOmega_s, fD_r, fRa
  real (c_double) :: dLatitude_radians

  dLatitude_radians = fLatitude * DEGREES_TO_RADIANS

  fD_r =relative_earth_sun_distance__D_r(iDayOfYear,iNumDaysInYear)
  fDelta = solar_declination__delta(iDayOfYear, iNumDaysInYear)

  fOmega_s = sunrise_sunset_angle__omega_s(dLatitude_radians, fDelta)

	! NOTE that the following equation returns extraterrestrial radiation in
	! MJ / m**2 / day.  The Hargreaves equation requires extraterrestrial
	! radiation to be expressed in units of mm / day.
	fRa = extraterrestrial_radiation__Ra(dLatitude_radians, fDelta, fOmega_s, fD_r)

  fReferenceET0 = ET0_hargreaves( equivalent_evaporation(fRa), fTMin, fTMax)

end function et_hargreaves_calculate


elemental function ET0_hargreaves( rRa, rTMinF, rTMaxF )   result(rET_0)

  ! [ ARGUMENTS ]
  real (c_double),intent(in) :: rRa
  real (c_float),intent(in)  :: rTMinF
  real (c_float),intent(in)  :: rTMaxF

  ! [ RETURN VALUE ]
  real (c_double) :: rET_0

  ! [ LOCALS ]
  real (c_double) :: rTDelta
  real (c_double) :: rTAvg

  rTAvg = (rTMinF + rTMaxF) / 2.0_c_double

  rTDelta = abs(F_to_K(real(rTMaxF, c_double)) - F_to_K(real(rTMinF, c_double)))

  rET_0 = MAX(rZERO, &
                mm_to_in( ET_SLOPE * rRa * (F_to_C(rTavg) + ET_CONSTANT) * (rTDelta**ET_EXPONENT) ) )
!                mm_to_in( 0.0023_c_float * rRa * (F_to_C(rTavg) + 17.8_c_float) * sqrt(rTDelta)) )

end function ET0_hargreaves


end module et__hargreaves_samani
