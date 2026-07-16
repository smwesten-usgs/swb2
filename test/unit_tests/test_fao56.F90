module test_fao56
  !! Unit tests for FAO-56 crop coefficient and two-stage evaporation functions.
  !!
  !! References:
  !!   Allen, R.G., Pereira, L.S., Raes, D., and Smith, M., 1998,
  !!   Crop evapotranspiration — Guidelines for computing crop water
  !!   requirements: FAO Irrigation and Drainage Paper 56.
  !!
  !!   Wilson, L.T., and Barnett, W.W., 1983, Degree-days: an aid in crop
  !!   and pest management: California Agriculture, January-February, p. 4.
  !!
  !! Requires Tier 3 (FAO56_GDD) environment from test_fixtures.

  use iso_c_binding, only: c_int, c_float, c_double
  use testdrive, only: check, error_type, new_unittest, unittest_type, test_failed
  use constants_and_conversions, only: clip
  use crop_coefficients__FAO56, only: &
       crop_coefficients_FAO56_interpolate_Kcb, &
       crop_coefficients_FAO56_calculate_Kcb_Max, &
       KCB_METHOD, KCB_METHOD_GDD
  use phenology, only: GROWTH_STAGE_DORMANT, GROWTH_STAGE_INI, GROWTH_STAGE_DEV, &
                        GROWTH_STAGE_MID, GROWTH_STAGE_LATE
  use growing_degree_day, only: growing_degree_day_calculate
  use simulation_datetime, only: SIM_DT
  use fstring, only: as_character
  use test_fixtures, only: setup_environment_fao56_gdd
  use parameters, only: PARAMS
  implicit none
  private

  public :: collect_fao56

contains

  !---------------------------------------------------------------------------
  !> @brief Register all FAO-56 tests into the test suite array.
  !---------------------------------------------------------------------------
  subroutine collect_fao56(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    call setup_environment_fao56_gdd()

    testsuite = [ &
      ! --- Crop coefficient method detection ---
      new_unittest("kcb_method_gdd_detected", test_kcb_method_gdd_detected), &
      ! --- Kcb interpolation (new interface) ---
      new_unittest("Kcb_dormant_is_min", test_Kcb_dormant_is_min), &
      new_unittest("Kcb_ini_stage", test_Kcb_ini_stage), &
      new_unittest("Kcb_dev_midpoint", test_Kcb_dev_midpoint), &
      new_unittest("Kcb_mid_stage", test_Kcb_mid_stage), &
      new_unittest("Kcb_late_midpoint", test_Kcb_late_midpoint), &
      ! --- GDD accumulation (Wilson & Barnett, 1983) ---
      new_unittest("gdd_simple_cases", test_gdd_simple_cases), &
      ! --- Equation 72: Kcb_max ---
      new_unittest("eq72_example_35", test_eq72_example_35), &
      new_unittest("eq72_vs_R_implementation", test_eq72_vs_R_implementation), &
      ! --- Example 35: two-stage evaporation ---
      new_unittest("example_35_Kr_Ke_10day", test_example_35_Kr_Ke_10day) &
    ]
  end subroutine collect_fao56

  !---------------------------------------------------------------------------
  ! CROP COEFFICIENT METHOD DETECTION
  !---------------------------------------------------------------------------

  !> @brief Corn (index 5) should be detected as GDD-based Kcb method.
  subroutine test_kcb_method_gdd_detected(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, KCB_METHOD(5) == KCB_METHOD_GDD, &
               "Index 5 (Corn) should use KCB_METHOD_GDD")
  end subroutine test_kcb_method_gdd_detected

  !---------------------------------------------------------------------------
  ! Kcb INTERPOLATION — tests the new pure interpolation function
  ! Using index 5 (Corn): Kcb_ini=0.20, Kcb_mid=0.96, Kcb_end=0.60, Kcb_min=0.19
  !---------------------------------------------------------------------------

  !> @brief DORMANT stage → Kcb = Kcb_min
  subroutine test_Kcb_dormant_is_min(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: Kcb

    Kcb = crop_coefficients_FAO56_interpolate_Kcb( &
      landuse_index=5, growth_stage=GROWTH_STAGE_DORMANT, &
      stage_fraction=0.0_c_float, current_month=1)

    call check(error, abs(Kcb - 0.19) < 1.0e-4, &
               "DORMANT: Kcb should be Kcb_min (0.19)")
  end subroutine test_Kcb_dormant_is_min

  !> @brief INI stage → Kcb = Kcb_ini
  subroutine test_Kcb_ini_stage(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: Kcb

    Kcb = crop_coefficients_FAO56_interpolate_Kcb( &
      landuse_index=5, growth_stage=GROWTH_STAGE_INI, &
      stage_fraction=0.5_c_float, current_month=6)

    call check(error, abs(Kcb - 0.20) < 1.0e-4, &
               "INI stage: Kcb should be Kcb_ini (0.20)")
  end subroutine test_Kcb_ini_stage

  !> @brief DEV stage at 50% → Kcb = Kcb_ini + 0.5*(Kcb_mid - Kcb_ini) = 0.58
  subroutine test_Kcb_dev_midpoint(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: Kcb, expected

    expected = 0.20 + 0.5 * (0.96 - 0.20)  ! = 0.58

    Kcb = crop_coefficients_FAO56_interpolate_Kcb( &
      landuse_index=5, growth_stage=GROWTH_STAGE_DEV, &
      stage_fraction=0.5_c_float, current_month=6)

    call check(error, abs(Kcb - expected) < 1.0e-3, &
               "DEV at 50%: Kcb should be ~0.58")
  end subroutine test_Kcb_dev_midpoint

  !> @brief MID stage → Kcb = Kcb_mid
  subroutine test_Kcb_mid_stage(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: Kcb

    Kcb = crop_coefficients_FAO56_interpolate_Kcb( &
      landuse_index=5, growth_stage=GROWTH_STAGE_MID, &
      stage_fraction=0.5_c_float, current_month=7)

    call check(error, abs(Kcb - 0.96) < 1.0e-4, &
               "MID stage: Kcb should be Kcb_mid (0.96)")
  end subroutine test_Kcb_mid_stage

  !> @brief LATE stage at 50% → Kcb = Kcb_mid + 0.5*(Kcb_end - Kcb_mid) = 0.78
  subroutine test_Kcb_late_midpoint(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: Kcb, expected

    expected = 0.96 + 0.5 * (0.60 - 0.96)  ! = 0.78

    Kcb = crop_coefficients_FAO56_interpolate_Kcb( &
      landuse_index=5, growth_stage=GROWTH_STAGE_LATE, &
      stage_fraction=0.5_c_float, current_month=8)

    call check(error, abs(Kcb - expected) < 1.0e-3, &
               "LATE at 50%: Kcb should be ~0.78")
  end subroutine test_Kcb_late_midpoint

  !---------------------------------------------------------------------------
  ! GDD ACCUMULATION (Wilson & Barnett, 1983)
  !---------------------------------------------------------------------------

  !> @brief GDD calculation for 6 cases from Wilson & Barnett (1983) Table 1.
  !!        Base temperature = 50°F. Uses table-driven accumulation pattern.
  subroutine test_gdd_simple_cases(error)
    type(error_type), allocatable, intent(out) :: error

    real(c_float) :: TMIN(6), TMAX(6), GDD_EXPECTED(6)
    real(c_float) :: gdd
    integer(c_int) :: i, num_failures
    character(len=512) :: failure_msg
    character(len=80) :: line

    TMIN         = [55., 55., 60., 60., 65., 65.]
    TMAX         = [65., 70., 70., 75., 80., 85.]
    ! FAO-56 formula: max((max(tmin,base) + min(tmax,gdd_max))/2, base) - base
    ! base=50, gdd_max=130
    ! Case 1: max((max(55,50)+min(65,130))/2, 50) - 50 = max((55+65)/2, 50) - 50 = 60 - 50 = 10
    ! Case 2: max((max(55,50)+min(70,130))/2, 50) - 50 = max((55+70)/2, 50) - 50 = 62.5 - 50 = 12.5
    ! Case 3: max((max(60,50)+min(70,130))/2, 50) - 50 = max((60+70)/2, 50) - 50 = 65 - 50 = 15
    ! Case 4: max((max(60,50)+min(75,130))/2, 50) - 50 = max((60+75)/2, 50) - 50 = 67.5 - 50 = 17.5
    ! Case 5: max((max(65,50)+min(80,130))/2, 50) - 50 = max((65+80)/2, 50) - 50 = 72.5 - 50 = 22.5
    ! Case 6: max((max(65,50)+min(85,130))/2, 50) - 50 = max((65+85)/2, 50) - 50 = 75 - 50 = 25
    GDD_EXPECTED = [10., 12.5, 15., 17.5, 22.5, 25.]

    num_failures = 0
    failure_msg = ""

    do i = 1, 6
      gdd = 0.0
      call growing_degree_day_calculate(gdd, TMIN(i), TMAX(i), 1)
      if (abs(gdd - GDD_EXPECTED(i)) > 1.0e-4) then
        num_failures = num_failures + 1
        write(line, '("  Case ", i1, ": Tmin=", f4.0, " Tmax=", f4.0, &
              &" got GDD=", f6.2, " expected ", f6.2, a)') &
              i, TMIN(i), TMAX(i), gdd, GDD_EXPECTED(i), new_line('a')
        failure_msg = trim(failure_msg) // trim(line)
      end if
    end do

    if (num_failures > 0) call test_failed(error, trim(failure_msg))
  end subroutine test_gdd_simple_cases

  !---------------------------------------------------------------------------
  ! EQUATION 72: Kcb_max
  !---------------------------------------------------------------------------

  !> @brief Kcb_max from FAO-56 Example 35 inputs.
  !!        Value verified against R implementation (publication has round-off).
  subroutine test_eq72_example_35(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: kcb_max

    kcb_max = crop_coefficients_FAO56_calculate_Kcb_Max( &
                wind_speed_meters_per_sec=1.6, &
                relative_humidity_min_pct=35., &
                Kcb=0.35, &
                plant_height_meters=0.3)

    call check(error, abs(kcb_max - 1.212028) < 1.0e-4, &
               "Kcb_max should be 1.212028 (Example 35 inputs)")
  end subroutine test_eq72_example_35

  !> @brief Kcb_max for multiple parameter combinations vs. R implementation.
  !!        Test data loaded from FAO56_equation_72_calcs.txt.
  subroutine test_eq72_vs_R_implementation(error)
    type(error_type), allocatable, intent(out) :: error

    real(c_float), allocatable :: wind_spd(:), RHmin(:), plant_height_m(:), kcb_max_expected(:)
    real(c_float) :: test_val
    integer(c_int) :: n, num_failures
    character(len=2048) :: failure_msg
    character(len=120) :: line

    call PARAMS%get_parameters(wind_spd, sKey="eq72_u2")
    call PARAMS%get_parameters(RHmin, sKey="eq72_rhmin")
    call PARAMS%get_parameters(plant_height_m, sKey="eq72_plant_height")
    call PARAMS%get_parameters(kcb_max_expected, sKey="eq72_kc_max")

    num_failures = 0
    failure_msg = ""

    do n = 1, size(RHmin)
      test_val = crop_coefficients_FAO56_calculate_Kcb_Max( &
                   wind_spd(n), RHmin(n), 0.5, plant_height_m(n))

      if (abs(test_val - kcb_max_expected(n)) > 1.0e-4) then
        num_failures = num_failures + 1
        write(line, '("  Case ", i2, ": u2=", f5.2, " RHmin=", f5.1, &
              &" h=", f5.2, " got=", f7.4, " expected=", f7.4, a)') &
              n, wind_spd(n), RHmin(n), plant_height_m(n), &
              test_val, kcb_max_expected(n), new_line('a')
        failure_msg = trim(failure_msg) // trim(line)
      end if
    end do

    if (num_failures > 0) call test_failed(error, trim(failure_msg))
  end subroutine test_eq72_vs_R_implementation

  !---------------------------------------------------------------------------
  ! EXAMPLE 35: TWO-STAGE EVAPORATION (Kr and Ke over 10 days)
  !---------------------------------------------------------------------------

  !> @brief Reproduce FAO-56 Example 35 Kr and Ke values over 10 days.
  !!        Verifies the evaporation reduction coefficient (Kr) and
  !!        soil evaporation coefficient (Ke) calculations.
  subroutine test_example_35_Kr_Ke_10day(error)
    type(error_type), allocatable, intent(out) :: error

    real(c_float), allocatable :: ex35_kr(:), ex35_ke(:)
    real(c_float), allocatable :: ex35_ET0(:), ex35_Kcb(:)
    real(c_float), allocatable :: ex35_p_minus_ro(:), ex35_irrigation(:)
    real(c_float), allocatable :: ex35_few(:), ex35_fw(:)

    real(c_float) :: Kcb_max, evaporable_water_storage, evaporable_water_deficit
    real(c_float) :: TEW_, REW_
    real(c_float) :: infiltration, Kr, Ke, Ks, Kcb, fw, few
    real(c_float) :: bare_soil_evap, actual_et, crop_etc
    real(c_double) :: reference_et0
    integer(c_int) :: day, num_failures
    character(len=2048) :: failure_msg
    character(len=120) :: line

    TEW_ = 18.0
    REW_ = 8.0
    evaporable_water_storage = 0.0
    evaporable_water_deficit = TEW_

    call PARAMS%get_parameters(ex35_kr, sKey="EX35_KR")
    call PARAMS%get_parameters(ex35_ke, sKey="EX35_ke")
    call PARAMS%get_parameters(ex35_ET0, sKey="EX35_ET0")
    call PARAMS%get_parameters(ex35_Kcb, sKey="EX35_Kcb")
    call PARAMS%get_parameters(ex35_p_minus_ro, sKey="ex35_p_minus_ro")
    call PARAMS%get_parameters(ex35_irrigation, sKey="ex35_irrigation")
    call PARAMS%get_parameters(ex35_few, sKey="ex35_few")
    call PARAMS%get_parameters(ex35_fw, sKey="ex35_fw")

    Kcb_max = crop_coefficients_FAO56_calculate_Kcb_Max( &
                wind_speed_meters_per_sec=1.5, &
                relative_humidity_min_pct=35., &
                Kcb=0.35, &
                plant_height_meters=0.30)

    num_failures = 0
    failure_msg = ""

    do day = 1, 10
      fw = ex35_fw(day)
      few = ex35_few(day)
      infiltration = ex35_p_minus_ro(day) + ex35_irrigation(day)
      Kcb = ex35_Kcb(day)
      reference_et0 = ex35_ET0(day)

      evaporable_water_storage = clip(evaporable_water_storage + infiltration, &
                                      minval=0., maxval=TEW_)
      evaporable_water_deficit = clip(TEW_ - evaporable_water_storage, &
                                      minval=0., maxval=TEW_)

      if (evaporable_water_deficit <= REW_) then
        Kr = 1.0_c_float
      else if (evaporable_water_deficit < TEW_) then
        Kr = real((real(TEW_, c_double) - evaporable_water_deficit) &
            / (real(TEW_, c_double) - real(REW_, c_double) + 1.0e-8_c_double), c_float)
      else
        Kr = 0.0_c_float
      end if

      Ks = Kr
      Ke = real(Kr * (real(Kcb_max, c_double) - real(Kcb, c_double)), c_float)

      ! Check Kr
      if (abs(Kr - ex35_kr(day)) > 1.0e-2) then
        num_failures = num_failures + 1
        write(line, '("  Day ", i2, " Kr: got ", f6.4, " expected ", f6.4, a)') &
              day, Kr, ex35_kr(day), new_line('a')
        failure_msg = trim(failure_msg) // trim(line)
      end if

      ! Check Ke
      if (abs(Ke - ex35_ke(day)) > 1.0e-2) then
        num_failures = num_failures + 1
        write(line, '("  Day ", i2, " Ke: got ", f6.4, " expected ", f6.4, a)') &
              day, Ke, ex35_ke(day), new_line('a')
        failure_msg = trim(failure_msg) // trim(line)
      end if

      ! Advance the water balance for next day
      bare_soil_evap = real(reference_et0 * Ke / few, c_float)
      crop_etc = real(reference_et0 * Kcb * Ks * 0.0, c_float)
      actual_et = crop_etc + bare_soil_evap
      evaporable_water_storage = clip(evaporable_water_storage - actual_et, &
                                      minval=0.0, maxval=TEW_)
    end do

    if (num_failures > 0) call test_failed(error, trim(failure_msg))
  end subroutine test_example_35_Kr_Ke_10day

end module test_fao56
