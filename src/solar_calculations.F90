module solar_calculations

  use iso_c_binding 
  use constants_and_conversions
  use meteorological_calculations
  use exceptions
  implicit none

  real (kind=c_float) :: EARTH_SUN_DIST_Dr
  real (kind=c_float) :: SOLAR_DECLINATION_Delta

contains  

  !> Calculate the number of daylight hours at a location.
  !! 
  !! @param[in]  dOmega_s   Sunset hour angle in Radians.
  !! @retval           rN   Number of daylight hours.
  !!
  !! @note Implementation follows equation 34, Allen and others (1998).
  !!
  !! @note Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!       "Crop Evapotranspiration (Guidelines for computing crop water
  !!       requirements)", Food and Agriculture Organization, Rome, Italy.
  function daylight_hours( dOmega_s )    result(dN)   bind(c)

    real (kind=c_double), intent(in)   :: dOmega_s
    real (kind=c_double)               :: dN

    dN = 24_c_double / PI * dOmega_s

  end function daylight_hours


! ***** TO ACCESS THE ABOVE FUNCTION FROM PYTHON VIA CTYPES (once the proper shared library has been created):
! from ctypes import *
! libf = CDLL("libsntemp_lib.dylib")
! libf.daylight_hours.restype = c_double
! libf.daylight_hours(byref(c_double(0.8)))

! IF one does not wish to add the 'byref' function call as shown above, the Fortran code itself
! may be modified to force values to be passed by value rather than by reference:
!
!  subroutine daylight_hours_c(dOmega_s, dN)  bind(c, name="daylight_hours_c")
!    
!    real (kind=c_double), intent(in), value        :: dOmega_s
!    real (kind=c_double), intent(out)              :: dN 
!
!    dN = daylight_hours(dOmega_s)
!
!  end subroutine daylight_hours_c

  !------------------------------------------------------------------------------------------------

  !>  Calculate extraterrestrial radiation given latitude and time of year.
  !!
  !! @param[in]  dLatitude   Latitude of grid cell in RADIANS.
  !! @param[in]     dDelta   Solar declination in RADIANS.
  !! @param[in]   dOmega_s   Sunset hour angle in RADIANS.
  !! @param[in]     dDsubR   Inverse relative distance Earth-Sun.
  !!
  !! @retval           rRa   Extraterrestrial radiation in MJ / m**2 / day.
  !!
  !! @note  1 MJ = 1e6 Joules; 1 Joule = 1 Watt / sec.
  !! @note   Therefore, multiply by 1e6 and divide by 86400 to get W/m*2-day.
  !!
  !! @note Source 
  !!        Equation 21, Allen and others (1998).
  !!
  !! @note Reference
  !!        Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!        "Crop Evapotranspiration (Guidelines for computing crop water
  !!        requirements)", Food and Agriculture Organization, Rome, Italy.
  !!
  !! @sa http://www.fao.org/docrep/x0490e/x0490e07.htm#solar%20radiation
  elemental function extraterrestrial_radiation__Ra(dLatitude, dDelta, dOmega_s, dDsubR)    result(dRa)

    ! [ ARGUMENTS ]
    real (kind=c_double), intent(in) :: dLatitude
    real (kind=c_double), intent(in) :: dDelta
    real (kind=c_double), intent(in) :: dOmega_s
    real (kind=c_double), intent(in) :: dDsubR

    ! [ LOCALS ]
    real (kind=c_double) :: dRa
    real (kind=c_double) :: dPartA, dPartB
    real (kind=c_double), parameter :: dGsc = 0.0820_c_double  ! MJ / m**2 / min

    dPartA = dOmega_s * sin( dLatitude ) * sin( dDelta )
    dPartB = cos( dLatitude ) * cos( dDelta ) * sin( dOmega_s )

    dRa = 24_c_double * 60_c_double * dGsc * dDsubR * ( dPartA + dPartB ) / PI

  end function extraterrestrial_radiation__Ra


  !------------------------------------------------------------------------------------------------

  !> Calculate net shortwave radiation
  !!
  !! @param[in]      dRs   Incoming shortwave solar radiation, in MJ / m**2 / day
  !! @param[in]  dAlbedo   Albedo or canopy reflection coefficient; 0.23 for grass reference crop
  !! @retval        dRns   Net shortwave radiation, in MJ / m**2 / day
  !!
  !! @note Implementation follows equation 38, Allen and others (1998).
  !!
  !! @note   Reference:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  function net_shortwave_radiation__Rns(dRs, dAlbedo)  result(dRns)

    real(kind=c_double), intent(in) :: dRs
    real(kind=c_double), intent(in) :: dAlbedo

    ! [ LOCALS ]
    real(kind=c_double) :: dRns

    dRns = (1_c_double - dAlbedo) * dRs

  end function net_shortwave_radiation__Rns

  !------------------------------------------------------------------------------------------------

  !> Calculate the solar declination for a given day of the year.
  !!
  !! @param[in]      iDayOfYear   Integer day of the year (January 1 = 1)
  !! @param[in]  iNumDaysInYear   Number of days in the current year
  !! @retval             dDelta   Solar declination, in RADIANS
  !!
  !! @note Implementation follows equation XXX? in:
  !!
  !! @note Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  elemental function solar_declination_simple__delta(iDayOfYear, iNumDaysInYear) result(dDelta)

    integer (kind=c_int), intent(in) :: iDayOfYear
    integer (kind=c_int), intent(in) :: iNumDaysInYear
    real (kind=c_double) :: dDelta

    dDelta = 0.409_c_double &
             * sin( (TWOPI * real(iDayOfYear, kind=c_double) / real(iNumDaysInYear, kind=c_double) ) &
  		          - 1.39_c_double)

  end function solar_declination_simple__delta


  !------------------------------------------------------------------------------------------------

  !> Calculate the solar declination for a given day of the year.
  !!
  !! @param[in]      iDayOfYear   Integer day of the year (January 1 = 1)
  !! @param[in]  iNumDaysInYear   Number of days in the current year
  !! @retval             dDelta   Solar declination, in RADIANS
  !!
  !! @note Implementation follows equation 1.3.1 in Iqbal (1983).
  !!
  !! @note Iqbal (1983) reports maximum error of 0.0006 radians; if the last two terms are omitted,
  !!       the reported accuracy drops to 0.0035 radians.
  !!
  !! @note Reference:
  !!       Iqbal, Muhammad (1983-09-28). An Introduction To Solar Radiation (p. 10). Elsevier Science. Kindle Edition. 
  elemental function solar_declination__delta(iDayOfYear, iNumDaysInYear) result(dDelta)

    integer (kind=c_int), intent(in) :: iDayOfYear
    integer (kind=c_int), intent(in) :: iNumDaysInYear
    real (kind=c_double) :: dDelta

    ! [ LOCALS ]
    real (kind=c_double) :: dGamma

    dGamma = day_angle__gamma( iDayOfYear, iNumDaysInYear )
    
    dDelta =   0.006918_c_double                                       &
             - 0.399912_c_double * cos( dGamma )                       &
             + 0.070257_c_double * sin( dGamma )                       &
             - 0.006758_c_double * cos( 2_c_double * dGamma )          &
             + 0.000907_c_double * sin( 2_c_double * dGamma )          &
             - 0.002697_c_double * cos( 3_c_double * dGamma )          &
             + 0.00148_c_double  * sin( 3_c_double * dGamma )


  end function solar_declination__delta


  !------------------------------------------------------------------------------------------------

  !> Calculate the inverse relative Earth-Sun distance for a given day of the year.
  !!
  !! @param[in]      iDayOfYear   Integer day of the year (January 1 = 1)
  !! @param[in]  iNumDaysInYear   Number of days in the current year
  !! @retval             dDsubR   Relative Earth-Sun distance
  !!
  !! @note Implementation follows equation 23, Allen and others (1998): <BR>
  !! @f$ d_r = 1 + 0.033 \cos \left( \frac{ 2 \pi }{365} J \right) @f$  <BR><BR>
  !! where: <BR>
  !!     @f$ d_r @f$ is the inverse relative distance between Earth and the Sun <BR>
  !!     @f$ J @f$ is the current day of the year <BR>
  !!
  !! @note References:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  !!
  !! @note Duffie, J.A., Beckman, W.A. Solar Engineering of Thermal Processes.”. New York: Wiley, 1980.
  !! 
  !! @note Equation 1.2.3 in Iqbal, Muhammad (1983-09-28). An Introduction To Solar Radiation (p. 28).
  !!       Elsevier Science. Kindle Edition. 
  elemental function relative_earth_sun_distance__D_r( iDayOfYear, iNumDaysInYear )   result( dDsubR ) 

    ! [ ARGUMENTS ]
    integer (kind=c_int), intent(in) :: iDayOfYear
    integer (kind=c_int), intent(in) :: iNumDaysInYear
    real (kind=c_double) :: dDsubR

    dDsubR = 1_c_double + 0.033_c_double &
             * cos( TWOPI * real( iDayOfYear, kind=c_double )          &
                                      / real( iNumDaysInYear, kind=c_double ) )

  end function relative_earth_sun_distance__D_r

  !------------------------------------------------------------------------------------------------

  !> Calculate sunrise/sunset angle, in RADIANS.
  !!
  !! @param[in]  dLatitude   Latitude, in RADIANS
  !! @param[in]     dDelta   Solar declination, in RADIANS
  !! @retval      dOmega_s   Sunset angle, in RADIANS
  !!
  !! @note Implementation follows equation 25, Allen and others (1998).
  !!
  !! @note Hour angle is zero at solar noon. Definition in Iqbal (1983) yields positive values before solar noon, 
  !!       and negative values following solar noon.
  !!
  !! @note   Reference:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
   elemental function sunrise_sunset_angle__omega_s( dLatitude, dDelta ) result( dOmega_s ) 

    real (kind=c_double), intent(in) :: dLatitude
    real (kind=c_double), intent(in) :: dDelta
    real (kind=c_double) :: dOmega_s

    dOmega_s = acos( - tan(dLatitude) * tan(dDelta) )

  end function sunrise_sunset_angle__omega_s

  !------------------------------------------------------------------------------------------------

  !> Calculate shortwave solar radiation.
  !!
  !! Calculates the solar radiation using Hargreave's radiation formula.
  !! For use when percent possible daily sunshine value is not available.
  !!
  !! @param[in]    dRa   Extraterrestrial radiation, in MJ / m**2 / day
  !! @param[in]  rTMin   Minimum daily air temperature, in &deg;C
  !! @param[in]  rTMax   Maximum daily air temperature, in &deg;C
  !! @retval       dRa   Solar radiation, in MJ / m**2 / day
  !!
  !! @note Implementation follows equation 50, Allen and others (1998).
  !!
  !! @note  Reference:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  function solar_radiation_Hargreaves__Rs( dRa, fTMin, fTMax )   result( dRs )   bind(c)

    real (kind=c_double), intent(in) :: dRa
    real (kind=c_float), intent(in)  :: fTMin
    real (kind=c_float), intent(in)  :: fTMax
    real (kind=c_double)             :: dRs

    ! [ LOCALS ]
    real (kind=c_double), parameter :: dKRs = 0.175

    dRs = dKRs * sqrt( C_to_K(fTMax) - C_to_K(fTMin) ) * dRa

  end function solar_radiation_Hargreaves__Rs

  !------------------------------------------------------------------------------------------------

  !> Estimate percent of possible sunshine.
  !! 
  !! This function follows from equation 5 in "The Rational Use of the FAO Blaney-Criddle
  !! substituting the rearranged Hargreaves solar radiation formula into
  !! equation 5 results in the formulation below
  !!
  !! @param[in]  fTMax   Maximum daily air temperature, in &deg;C
  !! @param[in]  fTMin   Minimum daily air temperature, in &deg;C
  !! @retval     fPsun   Percentage of possible sunshine, dimensionless percentage
  !!
  !! @todo [Need to add reference here...]
  elemental function estimate_percent_of_possible_sunshine__psun(fTMax, fTMin)  result(fPsun)

    real (kind=c_float), intent(in) :: fTMax
    real (kind=c_float), intent(in) :: fTMin
    real (kind=c_float) :: fPsun

    ! [ LOCALS ]
    real (kind=c_float), parameter :: fKRs = 0.175

    fPsun = ( 2_c_float * fKRs * sqrt( C_to_K( fTMAX ) - C_to_K( fTMIN ) ) ) - 0.5_c_float

    if ( fPsun < 0_c_float ) then
      fPsun = 0_c_float
    elseif ( fPsun > 1.0_c_float ) then
      fPsun = 100_c_float
    else
      fPsun = fPsun * 100_c_float
    endif

  end function estimate_percent_of_possible_sunshine__psun

  !------------------------------------------------------------------------------------------------

  !> Calculate clear sky solar radiation.
  !!
  !! Calculate the clear sky solar radiation (i.e. when rPctSun = 100,
  !!   n/N=1.  Required for computing net longwave radiation.
  !!
  !! @param[in]  dRa   Extraterrestrial radiation, in MJ / m**2 / day
  !! @param[in]  dAs   Solar radiation regression constant, expressing the fraction
  !!                     of extraterrestrial radiation that reaches earth on OVERCAST days.
  !! @param[in]  sBs   Solar radiation regression constant. As + Bs express the fraction
  !!                     of extraterrestrial radiation that reaches earth on CLEAR days.
  !! @retval    dRso   Clear sky solar radiation, in MJ / m**2 / day
  !!
  !! @note Implementation follows equation 36, Allen and others (1998).
  !!
  !! @note Reference:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  function clear_sky_solar_radiation__Rso( dRa, dAs, dBs )   result( dRso )   bind(c)

    real (kind=c_double), intent(in)           :: dRa
    real (kind=c_double), intent(in), optional :: dAs
    real (kind=c_double), intent(in),optional  :: dBs
    real (kind=c_double)                       :: dRso

    ! [ LOCALS ]
    real (kind=c_double) :: dAs_
    real (kind=c_double) :: dBs_

    ! assign default value to As if none is provided
    if ( present( dAs ) ) then
      dAs_ = dAs
    else
      dAs_ = 0.25_c_double
    end if

    ! assign default value to Bs if none is provided
    if ( present( dBs ) ) then
      dBs_ = dBs
    else
      dBs_ = 0.5_c_double
    end if

    dRso = (dAs_ + dBs_) * dRa

  end function clear_sky_solar_radiation__Rso

  !------------------------------------------------------------------------------------------------

  !> Calculate the clear sky solar radiation.
  !!
  !! Calculate the clear sky solar radiation (i.e. when rPctSun = 100,
  !!   n/N=1.  Required for computing net longwave radiation.
  !!   For use when no regression coefficients (A, B) are known.
  !!
  !! @param[in]         dRa   Extraterrestrial radiation, in MJ / m**2 / day
  !! @param[in]  fElevation   Elevation, in METERS above sea level
  !! @retval           dRso   Clear sky solar radiation, in MJ / m**2 / day
  !!
  !! @note Implementation follows equation 37, Allen and others (1998).
  !!
  !! @note Reference:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  function clear_sky_solar_radiation_noAB__Rso(dRa, fElevation) result(dRso)

    real (kind=c_double), intent(in) :: dRa
    real (kind=c_float), intent(in)  :: fElevation
    real (kind=c_double)             :: dRso

    dRso = ( 0.75_c_double + 1.0E-5_c_double * fElevation ) * dRa

  end function clear_sky_solar_radiation_noAB__Rso

  !------------------------------------------------------------------------------------------------

  !> Calculate solar radiation by means of the Angstrom formula.
  !!
  !! @param[in]      dRa   Extraterrestrial radiation in MJ / m**2 / day
  !! @param[in]      dAs   Solar radiation regression constant, expressing the fraction
  !!                         of extraterrestrial radiation that reaches earth on OVERCAST days.
  !! @param[in]      dBs   Solar radiation regression constant. As + Bs express the fraction
  !!                         of extraterrestrial radiation that reaches earth on CLEAR days.
  !! @param[in]  fPctSun   Percent of TOTAL number of sunshine hours during which the
  !!                         sun actually shown.
  !! @retval         fRs   Solar radiation in MJ / m**2 / day
  !!
  !! @notes
  !!  Implementation follows equation 35, Allen and others (1998).
  !!
  !!   Reference:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  function solar_radiation__Rs(dRa, dAs, dBs, fPctSun) result(dRs)

    real (kind=c_double), intent(in) :: dRa
    real (kind=c_double), intent(in) :: dAs
    real (kind=c_double), intent(in) :: dBs
    real (kind=c_double), intent(in) :: fPctSun
    real (kind=c_double)             :: dRs

    dRs = ( dAs + (dBs * fPctSun / 100_c_float ) ) * dRa

  end function solar_radiation__Rs

  !------------------------------------------------------------------------------------------------

  !> Calculate net longwave radiation flux.
  !!
  !! @param[in]  fTMin   Minimum daily air temperature, in &deg;C
  !! @param[in]  fTMax   Maximum daily air temperature, in &deg;C
  !! @param[in]    dRs   Measured or calculated shortwave solar radiation, in MJ / m**2 / day
  !! @param[in]   dRso   Calculated clear-sky radiation, in MJ / m**2 / day
  !! @retval      dRnl   Net longwave solar radiation flux (incoming minus outgoing), in MJ / m**2 / day
  !!
  !! @note
  !! Implementation follows equation 39, Allen and others (1998).
  !!
  !! @note
  !!   Reference:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  function net_longwave_radiation__Rnl(fTMin, fTMax, dRs, dRso)  result(dRnl)

    real(kind=c_float), intent(in)  :: fTMin
    real(kind=c_float), intent(in)  :: fTMax
    real(kind=c_double), intent(in) :: dRs
    real(kind=c_double), intent(in) :: dRso
    real(kind=c_double)             :: dRnl

    ! [ LOCALS ]
    real(kind=c_double)             :: dTAvg_K
    real(kind=c_double)             :: dTAvg_4
    real (kind=c_double)            :: d_ea
    real (kind=c_double)            :: dCloudFrac
    real (kind=c_double), parameter :: dSIGMA = 4.903E-9_c_double

    dTAvg_K = C_to_K((fTMin + fTMax ) / 2.0_c_float )

    dTAvg_4 = dTAvg_K * dTAvg_K * dTAvg_K * dTAvg_K * dSIGMA
    d_ea = dewpoint_vapor_pressure__e_a( fTMin )

    dCloudFrac = min( dRs / dRso, 1.0_c_double )

    dRnl = dTAvg_4 * ( 0.34_c_double - 0.14_c_double * sqrt( d_ea ) ) &
            * ( 1.35_c_double * dCloudFrac - 0.35_c_double )

  end function net_longwave_radiation__Rnl

  !------------------------------------------------------------------------------------------------

  !> Calculate the day angle in RADIANS.
  !!
  !! This function expresses the integer day number as an angle (in
  !! radians). Output values range from 0 (on January 1st) to 
  !! just less than @f$ 2\pi @f$ on December 31st.
  !!
  !! @param[in]        iDayOfYear   Current day of the year.
  !! @param[in]    iNumDaysInYear   Number of days in the current year.
  !! @retval               dGamma   Day angle in RADIANS.
  !!
  !! @note Implementation follows equation 1.2.2 in Iqbal (1983)
  !! 
  !! @note Reference: 
  !!       Iqbal, Muhammad (1983-09-28). An Introduction To Solar Radiation (p. 3). Elsevier Science. Kindle Edition. 
  elemental function day_angle__gamma(iDayOfYear, iNumDaysInYear)     result(dGamma)

    integer (kind=c_int), intent(in)   :: iDayOfYear
    integer (kind=c_int), intent(in)   :: iNumDaysInYear
    real (kind=c_double)               :: dGamma

    dGamma = TWOPI * ( iDayOfYear - 1 ) / iNumDaysInYear

  end function day_angle__gamma  

  !------------------------------------------------------------------------------------------------


  !> Calculate the solar altitude given the zenith angle.
  !!
  !! @param[in]      rTheta_z   Solar zenith angle for given location and time, in RADIANS
  !!
  !! @retval           rAlpha   Solar altitude angle for given location and time, in RADIANS  
  function solar_altitude__alpha( dTheta_z )    result( dAlpha )   bind(c)

    real (kind=c_double), intent(in)   :: dTheta_z
    real (kind=c_double)               :: dAlpha

    call assert( dTheta_z >= 0.0_c_double .and. dTheta_z <= HALFPI, &
      "Internal programming error: solar zenith angle must be in radians and in the range 0 to pi/2", &
      __FILE__, __LINE__) 

    dAlpha = HALFPI - dAlpha

  end function solar_altitude__alpha  


  !------------------------------------------------------------------------------------------------

	!> Calculate solar zenith angle given latitude, declination, and (optionally) hour angle.
	!!
  !! Calculate solar zenith. Solar zenith angle is the angle between a point directly overhead and
	!! the center of the sun's disk.
	!!
  !! @param[in]     rLatitude   Latitude of location for which estimate is being made, in RADIANS
	!! @param[in]        rDelta   Solar declination angle, in RADIANS
  !! @param[in]        rOmega   [OPTIONAL] Hour angle, measured at the celestial pole between the observer's meridian
  !!                            and the solar meridian, in RADIANS. 
  !! 
	!! @retval         rTheta_z   Solar zenith angle for given location and time, in RADIANS
	!!
  !! @note Implementation follows equation 9.67, Jacobson, 2005
  !!
  !! @note
  !!   Reference:
  !!   Jacobson, M.Z., 2005, Fundamentals of atmospheric modeling, Second Edition:
  !!   Cambridge University Press.
	function zenith_angle__theta_z( dLatitude, dDelta, dOmega ) result( dTheta_z )     bind(c)

	  real (kind=c_double), intent(in)            :: dLatitude
	  real (kind=c_double), intent(in)            :: dDelta
    real (kind=c_double), intent(in), optional  :: dOmega

	  ! [ LOCALS ]
	  real (kind=c_double) :: dTheta_z
    real (kind=c_double) :: dOmega_

    if ( present( dOmega ) ) then
      dOmega_ = dOmega_
    else
      dOmega_ = 0.0_c_double
    endif 

	  call assert( dLatitude <= HALFPI .and. dLatitude >= -HALFPI , &
	    "Internal programming error: Latitude must be expressed in RADIANS and range from -pi/2 to pi/2", &
	    __FILE__, __LINE__ )


	  dTheta_z = acos( sin(dLatitude) * sin(dDelta) + cos(dLatitude) * cos(dDelta) * cos(dOmega_) )

	end function zenith_angle__theta_z

  !------------------------------------------------------------------------------------------------

  !> Calculate solar azimuth angle.
  !!
  !! @param[in]     rAlpha   Solar altitude angle, in RADIANS.
  !! @param[in]  rLatitude   Latitude of location for which estimate is being made, in RADIANS.
  !! @param[in]     rDelta   Solar declination angle, in RADIANS.
  !!
  !! @retval          rPsi   Solar azimuth angle, in RADIANS.
  !!
  !! @note   Implementation follows equation 1.5.2a in Iqbal (1983).
  !!
  !! @note Reference: 
  !!       Iqbal, Muhammad (1983-09-28). An Introduction To Solar Radiation (p. 15). Elsevier Science. Kindle Edition. 
  function azimuth_angle__psi(rAlpha, rLatitude, rDelta)   result(rPsi)    bind(c)

    real (kind=c_double), intent(in)      :: rAlpha
    real (kind=c_double), intent(in)      :: rLatitude
    real (kind=c_double), intent(in)      :: rDelta
    real (kind=c_double)                  :: rPsi

    ! [ LOCALS ]
    real (kind=c_double) :: rTempval

    rTempval = ( sin( rAlpha ) * sin( rLatitude ) - sin( rDelta ) )          &
              /   ( cos( rAlpha ) * cos( rLatitude ) )

    ! avoid a NaN; cap value at 1.0
    if ( abs( rTempval ) > 1.0_c_double ) rTempval = sign( 1.0_c_double, rTempval )

    rPsi = acos( rTempval )

  end function azimuth_angle__psi

end module solar_calculations