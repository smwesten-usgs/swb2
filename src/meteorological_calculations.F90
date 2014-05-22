module meteorological_calculations

	use iso_c_binding, only : c_int, c_float, c_double, c_bool
  
  use constants_and_conversions
  use types 
  
  implicit none

contains

!> Calculate saturation vapor pressure
!!
!! Calculates the aturation vapor pressure for a given temperature.
!!
!! @param[in]  rT  Air temperature, in &deg;C
!! @retval   re_0  Saturation vapor pressure at given air temperature, in kiloPascals
!!
!! @note Implemented as equation 11, Allen and others (1998).
!!
!! @note Reference:
!!    Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
!!    "Crop Evapotranspiration (Guidelines for computing crop water
!!    requirements)", Food and Agriculture Organization, Rome, Italy.
function sat_vapor_pressure_e_0(rT) result (re_0)

  real (kind=c_float), intent(in)   :: rT
  real (kind=c_float)               :: re_0

  re_0 = 0.6108_c_double * exp (17.27_c_double * rT      &
                                / ( rT + 237.3_c_double)  )

end function sat_vapor_pressure_e_0

!--------------------------------------------------------------------------





end module meteorological_calculations