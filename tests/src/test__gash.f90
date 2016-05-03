program test__gash

  use iso_c_binding, only  : c_int, c_float, c_bool
  use constants_and_conversions
  use datetime
  use logfiles
  use parameters, only          : PARAMS, PARAMS_DICT
  use simulation_datetime
  use interception__gash
  use string_list
  implicit none

  integer (kind=c_int)  :: iIndex
  integer (kind=c_int)  :: iLanduseIndex
  real (kind=c_float)   :: fPSat1
  real (kind=c_float)   :: fRainfall(20), fFog(20)
  real (kind=c_float)   :: fE_div_P
  real (kind=c_float)   :: fCanopy_Fraction, fCanopy_Storage, fTrunk_Storage, fStemflow_Fraction
  real (kind=c_float)   :: fInterception_Value
  logical (kind=c_bool) :: lActive(1,1)

  fRainfall = [ 0., 0.0057, 0., 0., 0., 0., 0., 0.0189, 0.0057 ,0.0214 ,0.1121,  &
               0.2172, 0.0082, 0., 0.0007, 0., 0.0007, 0., 0., 0. ]

  fFog = 0.0
  fE_div_P = 0.5
  fCanopy_Fraction = 0.58
  fCanopy_Storage = 0.05
  fTrunk_Storage = 0.01
  fStemflow_Fraction = 0.04
  iLanduseIndex = 17

  lActive = lTRUE

  call PARAMS_DICT%print_all()

	fPSat1 = precipitation_at_saturation(                                                       &
		          E_div_P=fE_div_P,                                                               &
		          canopy_storage_capacity=fCanopy_Storage,  &
	          canopy_cover_fraction=fCanopy_Fraction )

	print *, fPSat1


  do iIndex=lbound(fRainfall,1), ubound(fRainfall,1)

    call interception_gash_calculate( fRainfall=fRainfall( iIndex ),                     &
                                      fFog=fFog( iIndex ),                               &
                                      fCanopy_Cover_Fraction=fCanopy_Fraction,           &
                                      fTrunk_Storage_Capacity=fTrunk_Storage,            &
                                      fStemflow_Fraction=fStemflow_Fraction,             &
                                      fEvaporation_to_Rainfall_Ratio=fE_div_P,           &
                                      fPrecipitation_at_Saturation=fPSat1,               &
                                      fInterception=fInterception_Value )

    write(*, fmt="(f12.3,3x,f12.4)") fRainfall( iIndex ), fInterception_Value

  enddo

!  precipitation_at_saturation( E_div_P, canopy_storage_capacity, canopy_cover_fraction )  &
!                                                                               result( Psat )

  

end program test__gash