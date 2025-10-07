module meteorological_calculations
!! Algorithms to estimate dewpoint, vapor pressure, and other values
!! given input air temperature or solar radiation values.

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use constants_and_conversions
  implicit none

contains

elemental function sat_vapor_pressure__e_0(rT) result (re_0)
    !! Calculate saturation vapor pressure
    !! @note Implemented as equation 11 in:
    !!    Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
    !!    Crop Evapotranspiration (Guidelines for computing crop water
    !!    requirements), Food and Agriculture Organization, Rome, Italy.

  real (c_float), intent(in)   :: rT
      !! Air temperature in &deg;C
  real (c_float)               :: re_0
      !! Saturation vapor pressure at given air temperature, in kilopascals

  re_0 = 0.6108_c_double * exp (17.27_c_double * rT      &
                                / ( rT + 237.3_c_double)  )

end function sat_vapor_pressure__e_0

!--------------------------------------------------------------------------

elemental function dewpoint_vapor_pressure__e_a(fTMin) result (fe_a)
    !! Estimate dewpoint vapor pressure given a minimum daily air temperature value

  real (c_float), intent(in) :: fTMin
      !! Minimum daily air temperature, in &deg;C
  real (c_double) :: fe_a
      !! Dewpoint vapor pressure at given air temperature, in kiloPascals

  fe_a = 0.6108_c_double * exp ( 17.27_c_double * F_to_C(fTMin) &
             / ( F_to_C(fTMin) + 237.3_c_double) )

end function dewpoint_vapor_pressure__e_a

!--------------------------------------------------------------------------

elemental function equivalent_evaporation(rR) result(rR_ET)
    !! Calculate a radiation value in terms of equivalent evaporation (depth of water
    !! that would be evaporated for a given radiation value)

  real (c_double), intent(in) :: rR
      !! Input radiation, in \(\frac{MJ}{m^2 \cdot day}\)
  real (c_double) :: rR_ET
      !! Radiation expressed as equivalent evaporation, in \(\frac{mm}{day}\)

  rR_ET = rR * 0.408_c_double

end function equivalent_evaporation



end module meteorological_calculations
