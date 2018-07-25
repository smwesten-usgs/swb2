module test__gash

  use iso_c_binding, only  : c_int, c_float, c_bool
  use constants_and_conversions
  use fruit
  use interception__gash
  implicit none

  integer (kind=c_int)  :: indx
  real (kind=c_float)   :: PSat
  real (kind=c_float)   :: rainfall(20) = [ 0., 0.0057, 0., 0., 0., 0., 0., 0.0189, 0.0057 ,0.0214,          &
                                             0.1121, 0.2172, 0.0082, 0., 0.0007, 0., 0.0007, 0., 0., 0. ]

  real (kind=c_float)   :: expected_interception(20) = [ 0.0, 0.003306, 0.0, 0.0, 0.0, 0.0, 0.0, 0.010962,   &
                                                         0.003306, 0.01241, 0.0650, 0.1063, 0.004756, 0.0,   &
                                                         0.000406, 0.0, 0.000406, 0.0, 0.0, 0.0]

  real (kind=c_float)   :: fog(20) = 0.0
  real (kind=c_float), parameter   :: fE_div_P           = 0.5
  real (kind=c_float), parameter   :: fCanopy_Fraction   = 0.58
  real (kind=c_float), parameter   :: fCanopy_Storage    = 0.05
  real (kind=c_float), parameter   :: fTrunk_Storage     = 0.01
  real (kind=c_float), parameter   :: fStemflow_Fraction = 0.04

  real (kind=c_float), parameter   :: maximum_difference = 1.0e-4

contains

  subroutine test_gash_psat_calculation
  ! gash: test calculation of Gash PSat

    PSat = precipitation_at_saturation(                                        &
           E_div_P=fE_div_P,                                                   &
           canopy_storage_capacity=fCanopy_Storage,                            &
           canopy_cover_fraction=fCanopy_Fraction )

    call assert_equals( 0.119508, PSat, delta=maximum_difference)

  end subroutine test_gash_psat_calculation


  subroutine test_gash_calculation
  ! gash: calculate Gash interception for published case

    real (kind=c_float)  :: fInterception_Value

    PSat = precipitation_at_saturation(                                        &
           E_div_P=fE_div_P,                                                   &
           canopy_storage_capacity=fCanopy_Storage,                            &
           canopy_cover_fraction=fCanopy_Fraction )

    do indx=lbound(rainfall,1), ubound(rainfall,1)

      call interception_gash_calculate( fRainfall=rainfall( indx ),                      &
                                        fFog=fog( indx ),                                &
                                        fCanopy_Cover_Fraction=fCanopy_Fraction,         &
                                        fTrunk_Storage_Capacity=fTrunk_Storage,          &
                                        fStemflow_Fraction=fStemflow_Fraction,           &
                                        fEvaporation_to_Rainfall_Ratio=fE_div_P,         &
                                        fPrecipitation_at_Saturation=PSat,               &
                                        fInterception=fInterception_Value )

      call assert_equals(expected_interception(indx), fInterception_Value, delta=maximum_difference)

    enddo

  end subroutine test_gash_calculation

end module test__gash
