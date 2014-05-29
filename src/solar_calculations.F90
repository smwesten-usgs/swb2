module solar_calculations

  use iso_c_binding 
  use constants_and_conversions
  use exceptions
  implicit none

  real (kind=c_float) :: EARTH_SUN_DIST_Dr
  real (kind=c_float) :: SOLAR_DECLINATION_Delta

contains  

  !> Calculate the number of daylight hours at a location.
  !! 
  !! @param[in]  rOmega_s   Sunset hour angle in Radians.
  !! @retval           rN   Number of daylight hours.
  !!
  !! @note Implementation follows equation 34, Allen and others (1998).
  !!
  !! @note Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!       "Crop Evapotranspiration (Guidelines for computing crop water
  !!       requirements)", Food and Agriculture Organization, Rome, Italy.
  function daylight_hours(rOmega_s) result(rN)  bind(c)

    ! [ ARGUMENTS ]
    real (kind=c_double), intent(in) :: rOmega_s

    ! [ LOCALS ]
    real (kind=c_double) :: rN

    rN = 24_c_double / PI * rOmega_s

  end function daylight_hours


! ***** TO ACCESS THE ABOVE FUNCTION FROM PYTHON VIA CTYPES (once the proper shared library has been created):
! from ctypes import *
! libf = CDLL("libsntemp_lib.dylib")
! libf.daylight_hours.restype = c_double
! libf.daylight_hours(byref(c_double(0.8)))

! IF one does not wish to add the 'byref' function call as shown above, the Fortran code itself
! may be modified to force values to be passed by value rather than by reference:
!
!  subroutine daylight_hours_c(rOmega_s, rN)  bind(c, name="daylight_hours_c")
!    
!    real (kind=c_double), intent(in), value        :: rOmega_s
!    real (kind=c_double), intent(out)              :: rN 
!
!    rN = daylight_hours(rOmega_s)
!
!  end subroutine daylight_hours_c

  !------------------------------------------------------------------------------------------------

  !>  Calculate extraterrestrial radiation given latitude and time of year.
  !!
  !! @param[in]  rLatitude   Latitude of grid cell in RADIANS.
  !! @param[in]     rDelta   Solar declination in RADIANS.
  !! @param[in]   rOmega_s   Sunset hour angle in RADIANS.
  !! @param[in]     rDsubR   Inverse relative distance Earth-Sun.
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
  function extraterrestrial_radiation_Ra(rLatitude,rDelta,rOmega_s,rDsubR) result(rRa)

    ! [ ARGUMENTS ]
    real (kind=c_double), intent(in) :: rLatitude
    real (kind=c_double), intent(in) :: rDelta
    real (kind=c_double), intent(in) :: rOmega_s
    real (kind=c_double), intent(in) :: rDsubR

    ! [ LOCALS ]
    real (kind=c_double) :: rRa
    real (kind=c_double) :: rPartA, rPartB
    real (kind=c_double), parameter :: rGsc = 0.0820_c_double  ! MJ / m**2 / min

    rPartA = rOmega_s * sin(rLatitude) * sin(rDelta)
    rPartB = cos(rLatitude) * cos(rDelta) * sin(rOmega_s)

    rRa = 24_c_double * 60_c_double * rGsc * rDsubR * (rPartA + rPartB) / PI

  end function extraterrestrial_radiation_Ra


  !------------------------------------------------------------------------------------------------

  !> Calculate net shortwave radiation
  !!
  !! @param[in]      rRs   Incoming shortwave solar radiation, in MJ / m**2 / day
  !! @param[in]  rAlbedo   Albedo or canopy reflection coefficient; 0.23 for grass reference crop
  !! @retval        rRns   Net shortwave radiation, in MJ / m**2 / day
  !!
  !! @note Implementation follows equation 38, Allen and others (1998).
  !!
  !! @note   Reference:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  function net_shortwave_radiation_Rns(rRs, rAlbedo)  result(rRns)

    real(kind=c_double), intent(in) :: rRs
    real(kind=c_double), intent(in) :: rAlbedo

    ! [ LOCALS ]
    real(kind=c_double) :: rRns

    rRns = (1_c_double - rAlbedo) * rRs

  end function net_shortwave_radiation_Rns

  !------------------------------------------------------------------------------------------------

  !> Calculate the solar declination for a given day of the year.
  !!
  !! @param[in]      iDayOfYear   Integer day of the year (January 1 = 1)
  !! @param[in]  iNumDaysInYear   Number of days in the current year
  !! @retval             rDelta   Solar declination, in RADIANS
  !!
  !! @note Implementation follows equation XXX? in:
  !!
  !! @note Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  function solar_declination_simple(iDayOfYear, iNumDaysInYear) result(rDelta)  bind(c)

    integer (kind=c_int), intent(in) :: iDayOfYear
    integer (kind=c_int), intent(in) :: iNumDaysInYear
    real (kind=c_double) :: rDelta

    rDelta = 0.409_c_double &
             * sin( (TWOPI * real(iDayOfYear, kind=c_double) / real(iNumDaysInYear, kind=c_double) ) &
  		          - 1.39_c_double)

  end function solar_declination_simple


  !------------------------------------------------------------------------------------------------

  !> Calculate the solar declination for a given day of the year.
  !!
  !! @param[in]      iDayOfYear   Integer day of the year (January 1 = 1)
  !! @param[in]  iNumDaysInYear   Number of days in the current year
  !! @retval             rDelta   Solar declination, in RADIANS
  !!
  !! @note Implementation follows equation 1.3.1 in Iqbal (1983).
  !!
  !! @note Iqbal (1983) reports maximum error of 0.0006 radians; if the last two terms are omitted,
  !!       the reported accuracy drops to 0.0035 radians.
  !!
  !! @note Reference:
  !!       Iqbal, Muhammad (1983-09-28). An Introduction To Solar Radiation (p. 10). Elsevier Science. Kindle Edition. 
  function solar_declination(iDayOfYear, iNumDaysInYear) result(rDelta)  bind(c)

    integer (kind=c_int), intent(in) :: iDayOfYear
    integer (kind=c_int), intent(in) :: iNumDaysInYear
    real (kind=c_double) :: rDelta

    ! [ LOCALS ]
    real (kind=c_double) :: rGamma

    rGamma = day_angle_gamma(iDayOfYear, iNumDaysInYear)
    
    rDelta =   0.006918_c_double                                       &
             - 0.399912_c_double * cos( rGamma )                       &
             + 0.070257_c_double * sin( rGamma )                       &
             - 0.006758_c_double * cos( 2_c_double * rGamma )          &
             + 0.000907_c_double * sin( 2_c_double * rGamma )          &
             - 0.002697_c_double * cos( 3_c_double * rGamma )          &
             + 0.00148_c_double  * sin( 3_c_double * rGamma )


  end function solar_declination


  !------------------------------------------------------------------------------------------------

  !> Calculate the inverse relative Earth-Sun distance for a given day of the year.
  !!
  !! @param[in]      iDayOfYear   Integer day of the year (January 1 = 1)
  !! @param[in]  iNumDaysInYear   Number of days in the current year
  !! @retval             rDsubR   Relative Earth-Sun distance
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
  function rel_Earth_Sun_dist(iDayOfYear,iNumDaysInYear) result(rDsubR)   bind(c)

    ! [ ARGUMENTS ]
    integer (kind=c_int), intent(in) :: iDayOfYear
    integer (kind=c_int), intent(in) :: iNumDaysInYear
    real (kind=c_float) :: rDsubR

    rDsubR = 1_c_double + 0.033_c_double &
             * cos( TWOPI * real(iDayOfYear, kind=c_double)          &
                                      / real(iNumDaysInYear, kind=c_double ) )

  end function rel_Earth_Sun_dist

  !------------------------------------------------------------------------------------------------

  !> Calculate sunrise/sunset angle, in RADIANS.
  !!
  !! @param[in]  rLatitude   Latitude, in RADIANS
  !! @param[in]     rDelta   Solar declination, in RADIANS
  !! @retval      rOmega_s   Sunset angle, in RADIANS
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
   function sunrise_sunset_angle(rLatitude, rDelta) result(rOmega_s)    bind(c)

    real (kind=c_double), intent(in) :: rLatitude
    real (kind=c_double), intent(in) :: rDelta
    real (kind=c_double) :: rOmega_s

    call assert(rLatitude <1.58 .and. rLatitude > -1.58, &
      "Internal programming error: Latitude must be expressed in RADIANS", &
      TRIM(__FILE__),__LINE__)

    rOmega_s = acos( - tan(rLatitude) * tan(rDelta) )

  end function sunrise_sunset_angle


  !------------------------------------------------------------------------------------------------

  !> Calculate shortwave solar radiation.
  !!
  !! Calculates the solar radiation using Hargreave's radiation formula.
  !! For use when percent possible daily sunshine value is not available.
  !!
  !! @param[in]    rRa   Extraterrestrial radiation, in MJ / m**2 / day
  !! @param[in]  rTMin   Minimum daily air temperature, in &deg;C
  !! @param[in]  rTMax   Maximum daily air temperature, in &deg;C
  !! @retval       rRa   Solar radiation, in MJ / m**2 / day
  !!
  !! @note Implementation follows equation 50, Allen and others (1998).
  !!
  !! @note  Reference:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  function solar_radiation_Hargreaves_Rs(rRa, rTMin, rTMax)   result(rRs)

    real (kind=c_double), intent(in) :: rRa
    real (kind=c_float), intent(in) :: rTMin
    real (kind=c_float), intent(in) :: rTMax
    real (kind=c_double) :: rRs

    ! [ LOCALS ]
    real (kind=c_double), parameter :: rKRs = 0.175

    rRs = rKRs * sqrt( C_to_K(rTMax) - C_to_K(rTMin) ) * rRa

  end function solar_radiation_Hargreaves_Rs

  !------------------------------------------------------------------------------------------------

  !> Estimate percent of possible sunshine.
  !! 
  !! This function follows from equation 5 in "The Rational Use of the FAO Blaney-Criddle
  !! substituting the rearranged Hargreaves solar radiation formula into
  !! equation 5 results in the formulation below
  !!
  !! @param[in]  rTMax   Maximum daily air temperature, in &deg;C
  !! @param[in]  rTMin   Minimum daily air temperature, in &deg;C
  !! @retval     rPsun   Percentage of possible sunshine, dimensionless percentage
  !!
  !! @todo [Need to add reference here...]
  function estimate_percent_of_possible_sunshine(rTMax, rTMin)  result(rPsun)

    real (kind=c_float), intent(in) :: rTMax
    real (kind=c_float), intent(in) :: rTMin
    real (kind=c_float) :: rPsun

    ! [ LOCALS ]
    real (kind=c_float), parameter :: rKRs = 0.175

    rPsun = ( 2_c_float * rKRs * sqrt( C_to_K( rTMAX ) - C_to_K( rTMIN ) ) ) - 0.5_c_float

    if ( rPsun < 0_c_float ) then
      rPsun = 0_c_float
    elseif ( rPsun > 1.0_c_float ) then
      rPsun = 100_c_float
    else
      rPsun = rPsun * 100_c_float
    endif

  end function estimate_percent_of_possible_sunshine

  !------------------------------------------------------------------------------------------------

  !> Calculate clear sky solar radiation.
  !!
  !! Calculate the clear sky solar radiation (i.e. when rPctSun = 100,
  !!   n/N=1.  Required for computing net longwave radiation.
  !!
  !! @param[in]  rRa   Extraterrestrial radiation, in MJ / m**2 / day
  !! @param[in]  rAs   Solar radiation regression constant, expressing the fraction
  !!                     of extraterrestrial radiation that reaches earth on OVERCAST days.
  !! @param[in]  rBs   Solar radiation regression constant. As + Bs express the fraction
  !!                     of extraterrestrial radiation that reaches earth on CLEAR days.
  !! @retval    rRso   Clear sky solar radiation, in MJ / m**2 / day
  !!
  !! @note Implementation follows equation 36, Allen and others (1998).
  !!
  !! @note Reference:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  function clear_sky_solar_radiation_Rso(rRa, rAs_in, rBs_in) result(rRso)

    real (kind=c_double), intent(in) :: rRa
    real (kind=c_double), intent(in), optional :: rAs_in
    real (kind=c_double), intent(in),optional :: rBs_in

    ! [ LOCALS ]
    real (kind=c_double) :: rRso
    real (kind=c_double) :: rAs
    real (kind=c_double) :: rBs

    ! assign default value to As if none is provided
    if(present(rAs_in)) then
      rAs = rAs_in
    else
      rAs = 0.25_c_double

    end if

    ! assign default value to Bs if none is provided
    if(present(rBs_in)) then
      rBs = rBs_in
    else
      rBs = 0.5_c_double
    end if

    rRso = (rAs + rBs) * rRa

  end function clear_sky_solar_radiation_Rso

  !------------------------------------------------------------------------------------------------

  !> Calculate the clear sky solar radiation.
  !!
  !! Calculate the clear sky solar radiation (i.e. when rPctSun = 100,
  !!   n/N=1.  Required for computing net longwave radiation.
  !!   For use when no regression coefficients (A, B) are known.
  !!
  !! @param[in]         rRa   Extraterrestrial radiation, in MJ / m**2 / day
  !! @param[in]  rElevation   Elevation, in METERS above sea level
  !! @retval           rRso   Clear sky solar radiation, in MJ / m**2 / day
  !!
  !! @note Implementation follows equation 37, Allen and others (1998).
  !!
  !! @note Reference:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  function clear_sky_solar_radiation_noAB_Rso(rRa, rElevation) result(rRso)

    real (kind=c_double), intent(in) :: rRa
    real (kind=c_double), intent(in) :: rElevation
    real (kind=c_float) :: rRso

    rRso = ( 0.75_c_double + 1.0E-5_c_double * rElevation ) * rRa

  end function clear_sky_solar_radiation_noAB_Rso

  !------------------------------------------------------------------------------------------------

  !> Calculate solar radiation by means of the Angstrom formula.
  !!
  !! @param[in]      rRa   Extraterrestrial radiation in MJ / m**2 / day
  !! @param[in]      rAs   Solar radiation regression constant, expressing the fraction
  !!                         of extraterrestrial radiation that reaches earth on OVERCAST days.
  !! @param[in]      rBs   Solar radiation regression constant. As + Bs express the fraction
  !!                         of extraterrestrial radiation that reaches earth on CLEAR days.
  !! @param[in]  rPctSun   Percent of TOTAL number of sunshine hours during which the
  !!                         sun actually shown.
  !! @retval         rRs   Solar radiation in MJ / m**2 / day
  !!
  !! @notes
  !!  Implementation follows equation 35, Allen and others (1998).
  !!
  !!   Reference:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  function solar_radiation_Rs(rRa, rAs, rBs, rPctSun) result(rRs)

    real (kind=c_double), intent(in) :: rRa
    real (kind=c_double), intent(in) :: rAs
    real (kind=c_double), intent(in) :: rBs
    real (kind=c_double), intent(in) :: rPctSun

    ! [ LOCALS ]
    real (kind=c_double) :: rRs

    rRs = ( rAs + (rBs * rPctSun / 100_c_float ) ) * rRa

  end function solar_radiation_Rs

  !------------------------------------------------------------------------------------------------

  !> Calculate net longwave radiation flux.
  !!
  !! @param[in]  rTMin   Minimum daily air temperature, in &deg;C
  !! @param[in]  rTMax   Maximum daily air temperature, in &deg;C
  !! @param[in]    rRs   Measured or calculated shortwave solar radiation, in MJ / m**2 / day
  !! @param[in]   rRso   Calculated clear-sky radiation, in MJ / m**2 / day
  !! @retval      rRnl   Net longwave solar radiation flux (incoming minus outgoing), in MJ / m**2 / day
  !!
  !! @note
  !! Implementation follows equation 39, Allen and others (1998).
  !!
  !! @note
  !!   Reference:
  !!   Allen, R.G., and others, 1998, FAO Irrigation and Drainage Paper No. 56,
  !!    "Crop Evapotranspiration (Guidelines for computing crop water
  !!    requirements)", Food and Agriculture Organization, Rome, Italy.
  function net_longwave_radiation_Rnl(rTMin, rTMax, rRs, rRso)  result(rRnl)

    real(kind=c_float), intent(in)  :: rTMin
    real(kind=c_float), intent(in)  :: rTMax
    real(kind=c_double), intent(in) :: rRs
    real(kind=c_double), intent(in) :: rRso

    ! [ LOCALS ]
    real(kind=c_double) :: rRnl
    real(kind=c_double) :: rTAvg_K
    real(kind=c_double) :: rTAvg_4

    real (kind=c_double)            :: r_ea
    real (kind=c_double)            :: rCloudFrac
    real (kind=c_double), parameter :: rSIGMA = 4.903E-9_c_double

    rTAvg_K = C_to_K((rTMin + rTMax ) / 2.)

    rTAvg_4 = rTAvg_K * rTAvg_K * rTAvg_K * rTAvg_K * rSIGMA
!    r_ea = dewpoint_vapor_pressure_ea( rTMin )

    rCloudFrac = min(rRs / rRso, 1.0)

    rRnl = rTAvg_4 * ( 0.34_c_double - 0.14_c_double * sqrt( r_ea ) ) &
            * ( 1.35_c_double * rCloudFrac - 0.35_c_double )

  end function net_longwave_radiation_Rnl

  !------------------------------------------------------------------------------------------------

  !> Calculate the day angle in RADIANS.
  !!
  !! This function expresses the integer day number as an angle (in
  !! radians). Output values range from 0 (on January 1st) to 
  !! just less than @f$ 2\pi @f$ on December 31st.
  !!
  !! @param[in]        iDayOfYear   Current day of the year.
  !! @param[in]    iNumDaysInYear   Number of days in the current year.
  !! @retval               rGamma   Day angle in RADIANS.
  !!
  !! @note Implementation follows equation 1.2.2 in Iqbal (1983)
  !! 
  !! @note Reference: 
  !!       Iqbal, Muhammad (1983-09-28). An Introduction To Solar Radiation (p. 3). Elsevier Science. Kindle Edition. 
  function day_angle_gamma(iDayOfYear, iNumDaysInYear)     result(rGamma)  bind(c)

    integer (kind=c_int), intent(in)   :: iDayOfYear
    integer (kind=c_int), intent(in)   :: iNumDaysInYear
    real (kind=c_double)               :: rGamma

    rGamma = TWOPI * ( iDayOfYear - 1 ) / iNumDaysInYear

  end function day_angle_gamma  

  !------------------------------------------------------------------------------------------------


  !> Calculate the solar altitude given the zenith angle.
  !!
  !! @param[in]      rTheta_z   Solar zenith angle for given location and time, in RADIANS
  !!
  !! @retval           rAlpha   Solar altitude angle for given location and time, in RADIANS  
  function solar_altitude(rTheta_z)    result(rAlpha)   bind(c)

    real (kind=c_double), intent(in)   :: rTheta_z
    real (kind=c_double)               :: rAlpha

    call assert( rTheta_z >= rZERO .and. rTheta_z <= HALFPI, &
      "Internal programming error: solar zenith angle must be in radians and in the range 0 to pi/2", &
      __FILE__, __LINE__) 

    rAlpha = HALFPI - rAlpha

  end function solar_altitude  


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
	function zenith_angle(rLatitude, rDelta, rOmega ) result(rTheta_z)     bind(c)

	  real (kind=c_double), intent(in)            :: rLatitude
	  real (kind=c_double), intent(in)            :: rDelta
    real (kind=c_double), intent(in), optional  :: rOmega

	  ! [ LOCALS ]
	  real (kind=c_double) :: rTheta_z
    real (kind=c_double) :: rOmega_

    if (present(rOmega) ) then
      rOmega_ = rOmega_
    else
      rOmega_ = rZERO
    endif 

	  call assert( rLatitude <= HALFPI .and. rLatitude >= -HALFPI , &
	    "Internal programming error: Latitude must be expressed in RADIANS and range from -pi/2 to pi/2", &
	    __FILE__, __LINE__ )


	  rTheta_z = acos( sin(rLatitude) * sin(rDelta) + cos(rLatitude) * cos(rDelta) * cos(rOmega_) )

	end function zenith_angle

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
  function azimuth_angle(rAlpha, rLatitude, rDelta)   result(rPsi)    bind(c)

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

  end function azimuth_angle

end module solar_calculations