!> @file
!>  Contains a single module, et_hargreaves, which
!>  calculates potential evapotranspiration by means of the Hargreaves-Samani (1985) method.


!>  Calculates potential evapotranspiration by means of the
!>  Hargreaves-Samani (1985) method.
module et_hargreaves
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
  use types
  use stats
  use meteorological_functions

  implicit none

  private

  ! ET parameters -- default values are from Hargreaves and Samani (1985)
  real (kind=c_float) :: fET_Slope = 0.0023     
  real (kind=c_float) :: fET_Exponent = 0.5
  real (kind=c_float) :: fET_Constant = 17.8

contains

subroutine et_hargreaves_configure( pConfig, sRecord )
  !! Configures the module, using the command line in 'sRecord'
  ! [ ARGUMENTS ]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  character (len=*),intent(inout) :: sRecord

  ! [ LOCALS ]
  character (len=256) :: sOption
  integer (kind=c_int) :: iStat
  real (kind=c_float) :: rValue

  write(UNIT=LU_LOG,FMT=*) "Configuring Hargreaves PET model"

  if (pConfig%rSouthernLatitude <= rNO_DATA_NCDC &
    .or. pConfig%rNorthernLatitude <= rNO_DATA_NCDC) then

    call Chomp( sRecord,sOption )
    read ( unit=sOption, fmt=*, iostat=iStat ) rValue
    call Assert( iStat == 0, "Could not read the southerly latitude" )
    pConfig%rSouthernLatitude = dpTWOPI * rValue / 360.0_c_float

    call Chomp( sRecord,sOption )
    read ( unit=sOption, fmt=*, iostat=iStat ) rValue
    call Assert( iStat == 0, "Could not read the northerly latitude" )
    pConfig%rNorthernLatitude = dpTWOPI * rValue / 360.0_c_float

  else

    call echolog("Southern and northern latitude values have been determined" &
      //"~from project grid bounds and projection parameters. The values supplied" &
      //"~with the Hargreaves PET option will be ignored...")

  endif

end subroutine et_hargreaves_configure

!------------------------------------------------------------------------------

subroutine et_hargreaves_ComputeET( iDayOfYear, iNumDaysInYear, fLatitude, fTMin, fTMax )
  !! Computes the potential ET for each cell, based on TMIN and TMAX.
  !! Stores cell-by-cell PET values in the model grid.

  integer (kind=c_int),intent(in) :: iDayOfYear
  integer (kind=c_int),intent(in) :: iNumDaysInYear
  real (kind=c_float), intent(in) :: fLatitude
  real (kind=c_float), intent(in) :: fTMin
  real (kind=c_float), intent(in) :: fTMax

  ! [ LOCALS ]
  real (kind=c_double) :: fDelta, fOmega_s, fD_r, fRa

  rD_r =rel_Earth_Sun_dist(iDayOfYear,iNumDaysInYear)
  rDelta = solar_declination(iDayOfYear, iNumDaysInYear)

  rOmega_s = sunset_angle(fLatitude, fDelta)

	! NOTE that the following equation returns extraterrestrial radiation in
	! MJ / m**2 / day.  The Hargreaves equation requires extraterrestrial
	! radiation to be expressed in units of mm / day.
	fRa = extraterrestrial_radiation_Ra(fLatitude, fDelta, fOmega_s, fD_r)

  fReferenceET0 = ET0_hargreaves( equivalent_evaporation(rRa), &
                                  pGrd%Cells(iCol,iRow)%rTMin, &
                                  pGrd%Cells(iCol,iRow)%rTMax)
    
end subroutine et_hargreaves_ComputeET


function ET0_hargreaves( rRa, rTMinF, rTMaxF )   result(rET_0)

  ! [ ARGUMENTS ]
  real (kind=c_double),intent(in) :: rRa
  real (kind=c_float),intent(in) :: rTMinF
  real (kind=c_float),intent(in) :: rTMaxF

  ! [ RETURN VALUE ]
  real (kind=c_float) :: rET_0

  ! [ LOCALS ]
  real (kind=c_double) :: rTDelta
  real (kind=c_double) :: rTAvg

  rTAvg = (rTMinF + rTMaxF) / 2_c_double

  rTDelta = FtoK(rTMaxF) - FtoK(rTMinF)

!  rET_0 = MAX(rZERO, &
!           ( 0.0023_c_float &
!           * rRa &
!           * (FtoC(rTavg) + 17.8_c_float) &
!           * sqrt(rTDelta)) &
!           / rMM_PER_INCH)

  rET_0 = MAX(rZERO, &
           ( fET_Slope &
           * rRa &
           * (FtoC(rTavg) + pConfig%fET_Constant) &
           * (rTDelta**fET_Exponent)) &
           / rMM_PER_INCH)

end function ET0_hargreaves


end module et_hargreaves
