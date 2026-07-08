module test_solar_calculations
  !! Unit tests for solar radiation calculations.
  !!
  !! Reference values from:
  !!   Allen, R.G., Pereira, L.S., Raes, D., and Smith, M., 1998,
  !!   Crop evapotranspiration-Guidelines for computing crop water
  !!   requirements-FAO Irrigation and drainage paper 56: Food and
  !!   Agriculture Organization of the United Nations, Rome, 333 pp.
  !!
  !!   Iqbal, Muhammad, 1983, An Introduction To Solar Radiation,
  !!   Elsevier Science.
  !!
  !! No environment setup needed — all functions are pure/elemental.

  use iso_c_binding, only: c_int, c_float, c_double
  use testdrive, only: check, error_type, new_unittest, unittest_type, test_failed
  use solar_calculations
  use constants_and_conversions, only: PI, TWOPI, DEGREES_TO_RADIANS
  implicit none
  private

  public :: collect_solar_calculations

  ! FAO-56 Example 8 parameters (20°S latitude, DOY 246 = Sept 3)
  real(c_double), parameter :: FAO56_EX8_LAT_DEG = -20.0_c_double
  real(c_double), parameter :: FAO56_EX8_LAT_RAD = FAO56_EX8_LAT_DEG * DEGREES_TO_RADIANS
  integer(c_int), parameter :: DOY_SEPT3 = 246
  integer(c_int), parameter :: DAYS_IN_YEAR = 365

contains

  !---------------------------------------------------------------------------
  !> @brief Register all solar calculation tests.
  !---------------------------------------------------------------------------
  subroutine collect_solar_calculations(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      ! --- Day angle ---
      new_unittest("day_angle_jan1", test_day_angle_jan1), &
      new_unittest("day_angle_dec31", test_day_angle_dec31), &
      ! --- Relative Earth-Sun distance (eq. 23) ---
      new_unittest("earth_sun_distance_jan1", test_earth_sun_distance_jan1), &
      new_unittest("earth_sun_distance_july1", test_earth_sun_distance_july1), &
      ! --- Solar declination (eq. 24) ---
      new_unittest("solar_declination_equinox", test_solar_declination_equinox), &
      new_unittest("solar_declination_summer_solstice", test_solar_declination_summer_solstice), &
      new_unittest("solar_declination_sept3", test_solar_declination_sept3), &
      ! --- Sunset hour angle (eq. 25) ---
      new_unittest("sunset_angle_equator_equinox", test_sunset_angle_equator_equinox), &
      new_unittest("sunset_angle_20S_sept3", test_sunset_angle_20S_sept3), &
      ! --- Daylight hours (eq. 34) ---
      new_unittest("daylight_hours_equator_equinox", test_daylight_hours_equator_equinox), &
      new_unittest("daylight_hours_20S_sept3", test_daylight_hours_20S_sept3), &
      ! --- Extraterrestrial radiation (eq. 21) ---
      new_unittest("Ra_20S_sept3", test_Ra_20S_sept3), &
      ! --- Net shortwave radiation (eq. 38) ---
      new_unittest("Rns_grass_reference", test_Rns_grass_reference), &
      ! --- Clear sky radiation (eq. 37) ---
      new_unittest("Rso_no_AB", test_Rso_no_AB), &
      ! --- Hargreaves solar radiation (eq. 50) ---
      new_unittest("Rs_hargreaves", test_Rs_hargreaves) &
    ]
  end subroutine collect_solar_calculations

  !---------------------------------------------------------------------------
  ! DAY ANGLE
  !---------------------------------------------------------------------------

  !> @brief Day angle on Jan 1 (DOY=1) should be ~0 radians.
  subroutine test_day_angle_jan1(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: gamma

    gamma = day_angle__gamma(1, 365)

    call check(error, abs(gamma) < 0.02, &
               "Day angle on Jan 1 should be ~0 radians")
  end subroutine test_day_angle_jan1

  !> @brief Day angle on Dec 31 (DOY=365) should be just under 2*pi.
  subroutine test_day_angle_dec31(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: gamma

    gamma = day_angle__gamma(365, 365)

    call check(error, abs(gamma - TWOPI * (364.0_c_double / 365.0_c_double)) < 0.01, &
               "Day angle on Dec 31 should be ~2*pi*(364/365)")
  end subroutine test_day_angle_dec31

  !---------------------------------------------------------------------------
  ! RELATIVE EARTH-SUN DISTANCE (FAO-56 eq. 23)
  !---------------------------------------------------------------------------

  !> @brief Earth-Sun distance on Jan 1 should be ~1.033 (perihelion).
  subroutine test_earth_sun_distance_jan1(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: dr

    dr = relative_earth_sun_distance__D_r(1, 365)

    ! Near perihelion (Jan 3), dr should be close to 1.033
    call check(error, abs(dr - 1.033_c_double) < 0.001, &
               "Earth-Sun distance on Jan 1 should be ~1.033")
  end subroutine test_earth_sun_distance_jan1

  !> @brief Earth-Sun distance on July 1 (DOY 182) should be ~0.967 (aphelion).
  subroutine test_earth_sun_distance_july1(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: dr

    dr = relative_earth_sun_distance__D_r(182, 365)

    ! Near aphelion (July 4), dr should be close to 0.967
    call check(error, abs(dr - 0.967_c_double) < 0.002, &
               "Earth-Sun distance on July 1 should be ~0.967")
  end subroutine test_earth_sun_distance_july1

  !---------------------------------------------------------------------------
  ! SOLAR DECLINATION (FAO-56 eq. 24)
  !---------------------------------------------------------------------------

  !> @brief Solar declination near spring equinox (DOY ~80) should be ~0.
  subroutine test_solar_declination_equinox(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: delta

    delta = solar_declination_simple__delta(80, 365)

    call check(error, abs(delta) < 0.03, &
               "Solar declination near equinox should be ~0 radians")
  end subroutine test_solar_declination_equinox

  !> @brief Solar declination at summer solstice (DOY ~172) should be ~+0.409 rad (~23.45°).
  subroutine test_solar_declination_summer_solstice(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: delta

    delta = solar_declination_simple__delta(172, 365)

    ! Maximum declination is ~0.409 radians (23.45 degrees)
    call check(error, abs(delta - 0.409_c_double) < 0.02, &
               "Solar declination at summer solstice should be ~0.409 rad")
  end subroutine test_solar_declination_summer_solstice

  !> @brief Solar declination on Sept 3 (DOY 246) for FAO-56 Example 8.
  !!        Expected: 0.120 rad (from FAO-56 table)
  subroutine test_solar_declination_sept3(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: delta

    delta = solar_declination_simple__delta(DOY_SEPT3, DAYS_IN_YEAR)

    ! FAO-56 Example 8 gives delta = 0.120 rad for Sept 3
    call check(error, abs(delta - 0.120_c_double) < 0.005, &
               "Solar declination on Sept 3 should be ~0.120 rad (FAO-56 Ex. 8)")
  end subroutine test_solar_declination_sept3

  !---------------------------------------------------------------------------
  ! SUNSET HOUR ANGLE (FAO-56 eq. 25)
  !---------------------------------------------------------------------------

  !> @brief At the equator during equinox, sunset angle = pi/2 (12h daylight).
  subroutine test_sunset_angle_equator_equinox(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: omega_s, delta

    delta = 0.0_c_double  ! equinox: declination = 0
    omega_s = sunrise_sunset_angle__omega_s(0.0_c_double, delta)

    ! At equator with zero declination: omega_s = acos(0) = pi/2
    call check(error, abs(omega_s - PI / 2.0_c_double) < 0.001, &
               "Sunset angle at equator during equinox should be pi/2")
  end subroutine test_sunset_angle_equator_equinox

  !> @brief Sunset hour angle for 20°S on Sept 3 (DOY 246).
  !!        FAO-56 Example 8: omega_s = 1.527 rad
  subroutine test_sunset_angle_20S_sept3(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: omega_s, delta

    delta = solar_declination_simple__delta(DOY_SEPT3, DAYS_IN_YEAR)
    omega_s = sunrise_sunset_angle__omega_s(FAO56_EX8_LAT_RAD, delta)

    ! FAO-56 Example 8: omega_s = 1.527 rad
    call check(error, abs(omega_s - 1.527_c_double) < 0.01, &
               "Sunset angle 20S Sept 3 should be ~1.527 rad (FAO-56 Ex. 8)")
  end subroutine test_sunset_angle_20S_sept3

  !---------------------------------------------------------------------------
  ! DAYLIGHT HOURS (FAO-56 eq. 34)
  !---------------------------------------------------------------------------

  !> @brief At equator during equinox, daylight = 12 hours.
  subroutine test_daylight_hours_equator_equinox(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: N, omega_s

    omega_s = PI / 2.0_c_double  ! equinox at equator
    N = daylight_hours(omega_s)

    call check(error, abs(N - 12.0_c_double) < 0.01, &
               "Daylight hours at equator during equinox should be 12.0")
  end subroutine test_daylight_hours_equator_equinox

  !> @brief Daylight hours for 20°S on Sept 3.
  !!        N = 24/pi * omega_s = 24/pi * 1.527 = 11.66 hours
  subroutine test_daylight_hours_20S_sept3(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: N, omega_s, delta

    delta = solar_declination_simple__delta(DOY_SEPT3, DAYS_IN_YEAR)
    omega_s = sunrise_sunset_angle__omega_s(FAO56_EX8_LAT_RAD, delta)
    N = daylight_hours(omega_s)

    ! N = 24/pi * 1.527 = 11.66 hours (southern hemisphere winter → short days)
    call check(error, abs(N - 11.66_c_double) < 0.1, &
               "Daylight hours 20S Sept 3 should be ~11.66 h")
  end subroutine test_daylight_hours_20S_sept3

  !---------------------------------------------------------------------------
  ! EXTRATERRESTRIAL RADIATION (FAO-56 eq. 21)
  !---------------------------------------------------------------------------

  !> @brief Ra for 20°S on Sept 3 (DOY 246).
  !!        FAO-56 Example 8: Ra = 32.2 MJ/m2/day
  subroutine test_Ra_20S_sept3(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: Ra, delta, omega_s, dr

    delta = solar_declination_simple__delta(DOY_SEPT3, DAYS_IN_YEAR)
    dr = relative_earth_sun_distance__D_r(DOY_SEPT3, DAYS_IN_YEAR)
    omega_s = sunrise_sunset_angle__omega_s(FAO56_EX8_LAT_RAD, delta)

    Ra = extraterrestrial_radiation__Ra(FAO56_EX8_LAT_RAD, delta, omega_s, dr)

    ! FAO-56 Example 8: Ra = 32.2 MJ/m2/day
    call check(error, abs(Ra - 32.2_c_double) < 0.5, &
               "Ra 20S Sept 3 should be ~32.2 MJ/m2/day (FAO-56 Ex. 8)")
  end subroutine test_Ra_20S_sept3

  !---------------------------------------------------------------------------
  ! NET SHORTWAVE RADIATION (FAO-56 eq. 38)
  !---------------------------------------------------------------------------

  !> @brief Net shortwave with grass albedo (0.23) and Rs=14.5 → Rns=11.2.
  subroutine test_Rns_grass_reference(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: Rns

    Rns = net_shortwave_radiation__Rns(14.5_c_double, 0.23_c_double)

    ! (1 - 0.23) * 14.5 = 11.165
    call check(error, abs(Rns - 11.165_c_double) < 0.01, &
               "Rns = (1-0.23)*14.5 should be ~11.165")
  end subroutine test_Rns_grass_reference

  !---------------------------------------------------------------------------
  ! CLEAR SKY RADIATION (FAO-56 eq. 37)
  !---------------------------------------------------------------------------

  !> @brief Clear sky radiation with elevation = 100m, Ra = 36.5.
  !!        Rso = (0.75 + 2e-5 * 100) * 36.5 = 27.45
  subroutine test_Rso_no_AB(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: Rso

    Rso = clear_sky_solar_radiation_noAB__Rso(36.5_c_double, 100.0_c_float)

    ! (0.75 + 1e-5 * 100) * 36.5 = (0.75 + 0.001) * 36.5 = 27.41
    call check(error, abs(Rso - 27.41_c_double) < 0.1, &
               "Rso at 100m elevation with Ra=36.5 should be ~27.41")
  end subroutine test_Rso_no_AB

  !---------------------------------------------------------------------------
  ! HARGREAVES SOLAR RADIATION (FAO-56 eq. 50)
  !---------------------------------------------------------------------------

  !> @brief Hargreaves Rs estimate with known inputs.
  !!        Rs = kRs * sqrt(Tmax_K - Tmin_K) * Ra
  subroutine test_Rs_hargreaves(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: Rs

    ! Tmin=20C, Tmax=35C (in Celsius), Ra=36.5
    ! Tmin_K = 293.15, Tmax_K = 308.15
    ! sqrt(308.15 - 293.15) = sqrt(15) = 3.873
    ! Rs = 0.175 * 3.873 * 36.5 = 24.73
    Rs = solar_radiation_Hargreaves__Rs(36.5_c_double, 20.0_c_float, 35.0_c_float)

    call check(error, abs(Rs - 24.73_c_double) < 0.2, &
               "Hargreaves Rs with Tmin=20C, Tmax=35C, Ra=36.5 should be ~24.73")
  end subroutine test_Rs_hargreaves

end module test_solar_calculations
