module meteorological_calculations

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use constants_and_conversions
  implicit none

contains

!> Calculate saturation vapor pressure
!!
!! Calculates the saturation vapor pressure for a given temperature.
!!
!! @param[in]  rT  Air temperature, in &deg;C
!! @retval   re_0  Saturation vapor pressure at given air temperature, in kiloPascals
!!
!! @note Implemented as equation 11, Allen and others (1998).
!!
!! @note Reference:
!!    Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
!!    Crop Evapotranspiration (Guidelines for computing crop water
!!    requirements), Food and Agriculture Organization, Rome, Italy.
elemental function sat_vapor_pressure__e_0(rT) result (re_0)

  real (kind=c_float), intent(in)   :: rT
  real (kind=c_float)               :: re_0

  re_0 = 0.6108_c_double * exp (17.27_c_double * rT      &
                                / ( rT + 237.3_c_double)  )

end function sat_vapor_pressure__e_0

!--------------------------------------------------------------------------

!> Calculate dewpoint vapor pressure
!!
!! Calculates the dewpoint vapor pressure for a given temperature.
!!
!! @param[in]  fTMin  Minimum daily air temperature, in &deg;C
!! @retval   fe_a  Dewpoint vapor pressure at given air temperature, in kiloPascals
!!
!! @note
!!
!! @note Reference:
!!
elemental function dewpoint_vapor_pressure__e_a(fTMin) result (fe_a)

  ! [ ARGUMENTS ]
  real (kind=c_float), intent(in) :: fTMin

  ! [ LOCALS ]
  real (kind=c_double) :: fe_a

  fe_a = 0.6108_c_double * exp ( 17.27_c_double * F_to_C(fTMin) &
             / ( F_to_C(fTMin) + 237.3_c_double) )

end function dewpoint_vapor_pressure__e_a

!--------------------------------------------------------------------------
!!****f* meteorological_functions/equivalent_evaporation
! NAME
! equivalent_evaporation - returns a radiation value in terms of equivalent
! evaporation
! SYNOPSIS
! Returns a radiation value in terms of equivalent evaporation (mm/day),
! given an input of radiation in MJ / m**2 / day
!
! INPUTS
! rR - Input radiation, in MJ / m**2 / day
!
! OUTPUTS
! rR_ET - Radiation expressed as equivalent evaporation, in mm / day
!
! SOURCE

elemental function equivalent_evaporation(rR) result(rR_ET)

  ! [ ARGUMENTS ]
  real (kind=c_double), intent(in) :: rR

  ! [ LOCALS ]
  real (kind=c_double) :: rR_ET

    rR_ET = rR * 0.408_c_double

end function equivalent_evaporation



end module meteorological_calculations
