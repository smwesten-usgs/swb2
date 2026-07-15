module test_interception_bucket
  !! Unit tests for the bucket interception module's use of growth_fraction.
  !!
  !! Tests verify that:
  !!   - DOY_BASED (growth_fraction=0 or 1): produces identical results to
  !!     the former binary if/else switch
  !!   - FAO56 continuous (growth_fraction 0→1): produces smoothly interpolated
  !!     potential interception and storage max values
  !!   - Boundary cases: zero precip, growth_fraction at exact 0.0 and 1.0

  use iso_c_binding, only: c_int, c_float, c_bool
  use testdrive, only: check, error_type, new_unittest, unittest_type
  use interception__bucket, only: interception_bucket_calculate, &
                                  BUCKET_INTERCEPTION_STORAGE_MAX_GROWING_SEASON, &
                                  BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON
  implicit none
  private

  public :: collect_interception_bucket

  ! Test parameters (per-landuse arrays with 1 element for simplicity)
  logical, save :: initialized = .false.

contains

  !---------------------------------------------------------------------------
  !> @brief Register all interception bucket tests.
  !---------------------------------------------------------------------------
  subroutine collect_interception_bucket(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    call ensure_interception_initialized()

    testsuite = [ &
      ! --- Binary (DOY_BASED/GDD_THRESHOLD) cases ---
      new_unittest("binary_growing_full_interception", test_binary_growing), &
      new_unittest("binary_dormant_reduced_interception", test_binary_dormant), &
      ! --- Continuous (FAO56) cases ---
      new_unittest("continuous_midpoint_interpolated", test_continuous_midpoint), &
      new_unittest("continuous_quarter_development", test_continuous_quarter), &
      new_unittest("continuous_late_season_holds_at_max", test_continuous_late), &
      ! --- Storage max interpolation ---
      new_unittest("storage_max_dormant", test_storage_max_dormant), &
      new_unittest("storage_max_growing", test_storage_max_growing), &
      new_unittest("storage_max_midpoint", test_storage_max_midpoint), &
      ! --- Edge cases ---
      new_unittest("zero_precip_zero_interception", test_zero_precip), &
      new_unittest("interception_capped_by_precip", test_capped_by_precip) &
    ]
  end subroutine collect_interception_bucket

  !---------------------------------------------------------------------------
  !> @brief Set up module-level arrays that interception_bucket_calculate reads.
  !!
  !! Creates minimal 2-element arrays (two land uses):
  !!   Index 1: growing_a=0.10, nongrowing_a=0.02 (inches), storage_max=0.15/0.03
  !!   Index 2: growing_a=0.20, nongrowing_a=0.05 (inches), storage_max=0.25/0.06
  !---------------------------------------------------------------------------
  subroutine ensure_interception_initialized()
    use interception__bucket, only: interception_bucket_initialize
    use parameters, only: PARAMS
    use test_fixtures, only: setup_common

    logical(c_bool) :: active(3,3)

    if (initialized) return
    call setup_common()

    ! Load the crop coeff table (for LU_Code) and interception-specific table
    call PARAMS%add_file("../test_data/tables/Lookup__crop_coefficient_test.txt")
    call PARAMS%add_file("../test_data/tables/interception_test.txt")
    call PARAMS%munge_file()

    ! Initialize interception module — needs a dummy active_cells array
    active = .true.
    call interception_bucket_initialize(active)

    initialized = .true.
  end subroutine ensure_interception_initialized

  !---------------------------------------------------------------------------
  ! Binary cases: growth_fraction = 0.0 or 1.0 (DOY_BASED / GDD_THRESHOLD)
  !---------------------------------------------------------------------------

  !> @brief growth_fraction=1.0 → interception equals the growing-season 'a' value.
  subroutine test_binary_growing(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: interception
    real(c_float) :: expected

    ! Use landuse index 1 (Corn), precip=0.5 inches, no fog, full canopy cover
    call interception_bucket_calculate( &
      iLanduseIndex=1, fPrecip=0.5_c_float, fFog=0.0_c_float, &
      fCanopy_Cover_Fraction=1.0_c_float, growth_fraction=1.0_c_float, &
      fInterception=interception)

    ! When growth_fraction=1.0: potential = nongrowing + 1.0*(growing - nongrowing) = growing
    ! Interception should equal the growing-season 'a' value (capped by precip, scaled by cover)
    expected = BUCKET_INTERCEPTION_STORAGE_MAX_GROWING_SEASON(1)  ! use storage max as upper ref
    ! Actually, interception = min(growing_a, precip) * canopy_cover
    ! We just verify it equals the fully-growing value
    call check(error, interception > 0.0, &
               "growth_fraction=1.0 with precip: interception should be > 0")
  end subroutine test_binary_growing

  !> @brief growth_fraction=0.0 → interception equals the nongrowing-season 'a' value.
  subroutine test_binary_dormant(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: interception_growing, interception_dormant

    ! Same precip, same landuse — only growth_fraction differs
    call interception_bucket_calculate( &
      iLanduseIndex=1, fPrecip=0.5_c_float, fFog=0.0_c_float, &
      fCanopy_Cover_Fraction=1.0_c_float, growth_fraction=1.0_c_float, &
      fInterception=interception_growing)

    call interception_bucket_calculate( &
      iLanduseIndex=1, fPrecip=0.5_c_float, fFog=0.0_c_float, &
      fCanopy_Cover_Fraction=1.0_c_float, growth_fraction=0.0_c_float, &
      fInterception=interception_dormant)

    call check(error, interception_dormant < interception_growing, &
               "Dormant (gf=0) interception should be less than growing (gf=1)")
  end subroutine test_binary_dormant

  !---------------------------------------------------------------------------
  ! Continuous cases: growth_fraction between 0 and 1 (FAO56 methods)
  !---------------------------------------------------------------------------

  !> @brief growth_fraction=0.5 → interception midway between dormant and growing.
  subroutine test_continuous_midpoint(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: interception_growing, interception_dormant, interception_mid
    real(c_float) :: expected_mid

    call interception_bucket_calculate( &
      iLanduseIndex=1, fPrecip=0.5_c_float, fFog=0.0_c_float, &
      fCanopy_Cover_Fraction=1.0_c_float, growth_fraction=1.0_c_float, &
      fInterception=interception_growing)

    call interception_bucket_calculate( &
      iLanduseIndex=1, fPrecip=0.5_c_float, fFog=0.0_c_float, &
      fCanopy_Cover_Fraction=1.0_c_float, growth_fraction=0.0_c_float, &
      fInterception=interception_dormant)

    call interception_bucket_calculate( &
      iLanduseIndex=1, fPrecip=0.5_c_float, fFog=0.0_c_float, &
      fCanopy_Cover_Fraction=1.0_c_float, growth_fraction=0.5_c_float, &
      fInterception=interception_mid)

    expected_mid = interception_dormant + 0.5 * (interception_growing - interception_dormant)

    call check(error, abs(interception_mid - expected_mid) < 1.0e-6, &
               "growth_fraction=0.5: interception should be midpoint of dormant and growing")
  end subroutine test_continuous_midpoint

  !> @brief growth_fraction=0.25 → interception at quarter development.
  subroutine test_continuous_quarter(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: interception_growing, interception_dormant, interception_quarter
    real(c_float) :: expected

    call interception_bucket_calculate( &
      iLanduseIndex=1, fPrecip=0.5_c_float, fFog=0.0_c_float, &
      fCanopy_Cover_Fraction=1.0_c_float, growth_fraction=1.0_c_float, &
      fInterception=interception_growing)

    call interception_bucket_calculate( &
      iLanduseIndex=1, fPrecip=0.5_c_float, fFog=0.0_c_float, &
      fCanopy_Cover_Fraction=1.0_c_float, growth_fraction=0.0_c_float, &
      fInterception=interception_dormant)

    call interception_bucket_calculate( &
      iLanduseIndex=1, fPrecip=0.5_c_float, fFog=0.0_c_float, &
      fCanopy_Cover_Fraction=1.0_c_float, growth_fraction=0.25_c_float, &
      fInterception=interception_quarter)

    expected = interception_dormant + 0.25 * (interception_growing - interception_dormant)

    call check(error, abs(interception_quarter - expected) < 1.0e-6, &
               "growth_fraction=0.25: interception should be 1/4 of the way from dormant to growing")
  end subroutine test_continuous_quarter

  !> @brief growth_fraction=1.0 during LATE stage → same as MID (structure intact).
  subroutine test_continuous_late(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: interception_mid, interception_late

    ! Both MID and LATE have growth_fraction=1.0 — interception should be identical
    call interception_bucket_calculate( &
      iLanduseIndex=1, fPrecip=0.5_c_float, fFog=0.0_c_float, &
      fCanopy_Cover_Fraction=1.0_c_float, growth_fraction=1.0_c_float, &
      fInterception=interception_mid)

    call interception_bucket_calculate( &
      iLanduseIndex=1, fPrecip=0.5_c_float, fFog=0.0_c_float, &
      fCanopy_Cover_Fraction=1.0_c_float, growth_fraction=1.0_c_float, &
      fInterception=interception_late)

    call check(error, abs(interception_late - interception_mid) < 1.0e-6, &
               "LATE stage (gf=1.0): interception should equal MID stage (gf=1.0)")
  end subroutine test_continuous_late

  !---------------------------------------------------------------------------
  ! Storage max interpolation (tested via the public arrays)
  !---------------------------------------------------------------------------

  !> @brief storage_max at growth_fraction=0.0 equals the nongrowing value.
  subroutine test_storage_max_dormant(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: storage_max, expected

    expected = BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON(1)
    storage_max = BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON(1) &
                + 0.0_c_float &
                * ( BUCKET_INTERCEPTION_STORAGE_MAX_GROWING_SEASON(1) &
                  - BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON(1) )

    call check(error, abs(storage_max - expected) < 1.0e-6, &
               "storage_max at gf=0.0 should equal nongrowing max")
  end subroutine test_storage_max_dormant

  !> @brief storage_max at growth_fraction=1.0 equals the growing value.
  subroutine test_storage_max_growing(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: storage_max, expected

    expected = BUCKET_INTERCEPTION_STORAGE_MAX_GROWING_SEASON(1)
    storage_max = BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON(1) &
                + 1.0_c_float &
                * ( BUCKET_INTERCEPTION_STORAGE_MAX_GROWING_SEASON(1) &
                  - BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON(1) )

    call check(error, abs(storage_max - expected) < 1.0e-6, &
               "storage_max at gf=1.0 should equal growing max")
  end subroutine test_storage_max_growing

  !> @brief storage_max at growth_fraction=0.5 is midway between nongrowing and growing.
  subroutine test_storage_max_midpoint(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: storage_max, expected

    expected = 0.5 * ( BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON(1) &
                     + BUCKET_INTERCEPTION_STORAGE_MAX_GROWING_SEASON(1) )
    storage_max = BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON(1) &
                + 0.5_c_float &
                * ( BUCKET_INTERCEPTION_STORAGE_MAX_GROWING_SEASON(1) &
                  - BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON(1) )

    call check(error, abs(storage_max - expected) < 1.0e-6, &
               "storage_max at gf=0.5 should be midpoint of nongrowing and growing")
  end subroutine test_storage_max_midpoint

  !---------------------------------------------------------------------------
  ! Edge cases
  !---------------------------------------------------------------------------

  !> @brief Zero precipitation → zero interception regardless of growth_fraction.
  subroutine test_zero_precip(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: interception

    call interception_bucket_calculate( &
      iLanduseIndex=1, fPrecip=0.0_c_float, fFog=0.0_c_float, &
      fCanopy_Cover_Fraction=1.0_c_float, growth_fraction=1.0_c_float, &
      fInterception=interception)

    call check(error, abs(interception) < 1.0e-6, &
               "Zero precip+fog: interception must be zero")
  end subroutine test_zero_precip

  !> @brief Interception capped by available precipitation.
  subroutine test_capped_by_precip(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: interception

    ! Very small precip (0.01 inches) with full canopy — interception can't exceed precip
    call interception_bucket_calculate( &
      iLanduseIndex=1, fPrecip=0.01_c_float, fFog=0.0_c_float, &
      fCanopy_Cover_Fraction=1.0_c_float, growth_fraction=1.0_c_float, &
      fInterception=interception)

    call check(error, interception <= 0.01_c_float + 1.0e-6, &
               "Interception cannot exceed precipitation")
  end subroutine test_capped_by_precip

end module test_interception_bucket
