module test_constants_and_conversions
  !! Unit tests for constants_and_conversions module.
  !!
  !! Tests clip (bounds enforcement), approximate equality operator,
  !! temperature/distance/angle conversions, is_numeric, and type coercion.
  !! All pure math — no environment setup needed.

  use iso_c_binding, only: c_int, c_float, c_double, c_bool
  use testdrive, only: check, error_type, new_unittest, unittest_type, test_failed
  use constants_and_conversions
  implicit none
  private

  public :: collect_constants_and_conversions

contains

  !---------------------------------------------------------------------------
  !> @brief Register all constants_and_conversions tests.
  !---------------------------------------------------------------------------
  subroutine collect_constants_and_conversions(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      ! --- clip (bounds enforcement) ---
      new_unittest("clip_int_within_bounds", test_clip_int_within_bounds), &
      new_unittest("clip_int_below_min", test_clip_int_below_min), &
      new_unittest("clip_int_above_max", test_clip_int_above_max), &
      new_unittest("clip_float_within_bounds", test_clip_float_within_bounds), &
      new_unittest("clip_float_below_min", test_clip_float_below_min), &
      new_unittest("clip_float_above_max", test_clip_float_above_max), &
      new_unittest("clip_float_at_boundaries", test_clip_float_at_boundaries), &
      ! --- .approxequal. operator ---
      new_unittest("approxequal_identical", test_approxequal_identical), &
      new_unittest("approxequal_within_tol", test_approxequal_within_tol), &
      new_unittest("approxequal_outside_tol", test_approxequal_outside_tol), &
      new_unittest("approxequal_double", test_approxequal_double), &
      ! --- temperature conversions ---
      new_unittest("C_to_F_freezing", test_C_to_F_freezing), &
      new_unittest("F_to_C_boiling", test_F_to_C_boiling), &
      new_unittest("F_to_K_freezing", test_F_to_K_freezing), &
      new_unittest("C_to_K_zero", test_C_to_K_zero), &
      ! --- distance conversions ---
      new_unittest("mm_to_in_one_inch", test_mm_to_in_one_inch), &
      new_unittest("in_to_mm_one_inch", test_in_to_mm_one_inch), &
      new_unittest("mm_in_roundtrip", test_mm_in_roundtrip), &
      ! --- angle conversions ---
      new_unittest("deg_to_rad_180", test_deg_to_rad_180), &
      new_unittest("rad_to_deg_pi", test_rad_to_deg_pi), &
      new_unittest("deg_rad_roundtrip", test_deg_rad_roundtrip), &
      ! --- is_numeric ---
      new_unittest("is_numeric_integer", test_is_numeric_integer), &
      new_unittest("is_numeric_float", test_is_numeric_float), &
      new_unittest("is_numeric_text_only", test_is_numeric_text_only), &
      new_unittest("is_numeric_mixed", test_is_numeric_mixed) &
    ]
  end subroutine collect_constants_and_conversions

  !---------------------------------------------------------------------------
  ! CLIP TESTS
  !---------------------------------------------------------------------------

  !> @brief Value within bounds is unchanged.
  subroutine test_clip_int_within_bounds(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, clip(value=5_c_int, minval=0_c_int, maxval=10_c_int) == 5, &
               "clip(5, 0, 10) should return 5")
  end subroutine test_clip_int_within_bounds

  !> @brief Value below min is clamped to min.
  subroutine test_clip_int_below_min(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, clip(value=-3_c_int, minval=0_c_int, maxval=10_c_int) == 0, &
               "clip(-3, 0, 10) should return 0")
  end subroutine test_clip_int_below_min

  !> @brief Value above max is clamped to max.
  subroutine test_clip_int_above_max(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, clip(value=15_c_int, minval=0_c_int, maxval=10_c_int) == 10, &
               "clip(15, 0, 10) should return 10")
  end subroutine test_clip_int_above_max

  !> @brief Float value within bounds is unchanged.
  subroutine test_clip_float_within_bounds(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: result
    result = clip(value=5.5_c_float, minval=0.0_c_float, maxval=10.0_c_float)
    call check(error, abs(result - 5.5) < 1.0e-6, &
               "clip(5.5, 0, 10) should return 5.5")
  end subroutine test_clip_float_within_bounds

  !> @brief Float value below min is clamped.
  subroutine test_clip_float_below_min(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: result
    result = clip(value=-0.001_c_float, minval=0.0_c_float, maxval=1.0_c_float)
    call check(error, abs(result) < 1.0e-6, &
               "clip(-0.001, 0, 1) should return 0.0")
  end subroutine test_clip_float_below_min

  !> @brief Float value above max is clamped.
  subroutine test_clip_float_above_max(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: result
    result = clip(value=100.0_c_float, minval=0.0_c_float, maxval=18.0_c_float)
    call check(error, abs(result - 18.0) < 1.0e-6, &
               "clip(100, 0, 18) should return 18.0")
  end subroutine test_clip_float_above_max

  !> @brief Value exactly at min and max boundaries returns that boundary.
  subroutine test_clip_float_at_boundaries(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: result_min, result_max

    result_min = clip(value=0.0_c_float, minval=0.0_c_float, maxval=10.0_c_float)
    result_max = clip(value=10.0_c_float, minval=0.0_c_float, maxval=10.0_c_float)

    call check(error, abs(result_min) < 1.0e-6, &
               "clip(0.0, 0, 10) should return 0.0")
    if (allocated(error)) return
    call check(error, abs(result_max - 10.0) < 1.0e-6, &
               "clip(10.0, 0, 10) should return 10.0")
  end subroutine test_clip_float_at_boundaries

  !---------------------------------------------------------------------------
  ! .APPROXEQUAL. TESTS
  !---------------------------------------------------------------------------

  !> @brief Identical float values are approximately equal.
  subroutine test_approxequal_identical(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, logical(1.0_c_float .approxequal. 1.0_c_float), &
               "1.0 .approxequal. 1.0 should be true")
  end subroutine test_approxequal_identical

  !> @brief Values differing by less than TOLERANCE_FLOAT are approximately equal.
  subroutine test_approxequal_within_tol(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: a, b
    a = 1.0_c_float
    b = a + 0.5e-7_c_float  ! well within 1.0e-6 tolerance
    call check(error, logical(a .approxequal. b), &
               "values within TOLERANCE_FLOAT should be approximately equal")
  end subroutine test_approxequal_within_tol

  !> @brief Values differing by more than TOLERANCE_FLOAT are not approximately equal.
  subroutine test_approxequal_outside_tol(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: a, b
    a = 1.0_c_float
    b = a + 1.0e-4_c_float  ! well outside 1.0e-6 tolerance
    call check(error, .not. logical(a .approxequal. b), &
               "values outside TOLERANCE_FLOAT should NOT be approximately equal")
  end subroutine test_approxequal_outside_tol

  !> @brief Double precision approximate equality works.
  subroutine test_approxequal_double(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: a, b
    a = 1.0_c_double
    b = a + 0.5e-10_c_double  ! within 1.0e-9 tolerance
    call check(error, logical(a .approxequal. b), &
               "double values within TOLERANCE_DOUBLE should be approximately equal")
  end subroutine test_approxequal_double

  !---------------------------------------------------------------------------
  ! TEMPERATURE CONVERSION TESTS
  !---------------------------------------------------------------------------

  !> @brief 0°C = 32°F.
  subroutine test_C_to_F_freezing(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(C_to_F(0.0_c_float) - 32.0_c_float) < 1.0e-5, &
               "0 C should equal 32 F")
  end subroutine test_C_to_F_freezing

  !> @brief 212°F = 100°C.
  subroutine test_F_to_C_boiling(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(F_to_C(212.0_c_float) - 100.0_c_float) < 1.0e-4, &
               "212 F should equal 100 C")
  end subroutine test_F_to_C_boiling

  !> @brief 32°F = 273.15 K.
  subroutine test_F_to_K_freezing(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(F_to_K(32.0_c_float) - 273.15) < 0.01, &
               "32 F should equal 273.15 K")
  end subroutine test_F_to_K_freezing

  !> @brief 0°C = 273.15 K.
  subroutine test_C_to_K_zero(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(C_to_K(0.0_c_float) - 273.15) < 0.01, &
               "0 C should equal 273.15 K")
  end subroutine test_C_to_K_zero

  !---------------------------------------------------------------------------
  ! DISTANCE CONVERSION TESTS
  !---------------------------------------------------------------------------

  !> @brief 25.4 mm = 1 inch.
  subroutine test_mm_to_in_one_inch(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(mm_to_in(25.4_c_float) - 1.0) < 1.0e-5, &
               "25.4 mm should equal 1.0 inch")
  end subroutine test_mm_to_in_one_inch

  !> @brief 1 inch = 25.4 mm.
  subroutine test_in_to_mm_one_inch(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(in_to_mm(1.0_c_float) - 25.4) < 1.0e-4, &
               "1.0 inch should equal 25.4 mm")
  end subroutine test_in_to_mm_one_inch

  !> @brief Roundtrip: in_to_mm(mm_to_in(x)) == x.
  subroutine test_mm_in_roundtrip(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: original, roundtrip
    original = 42.7_c_float
    roundtrip = in_to_mm(mm_to_in(original))
    call check(error, abs(roundtrip - original) < 1.0e-4, &
               "mm→in→mm roundtrip should preserve value")
  end subroutine test_mm_in_roundtrip

  !---------------------------------------------------------------------------
  ! ANGLE CONVERSION TESTS
  !---------------------------------------------------------------------------

  !> @brief 180° = π radians.
  subroutine test_deg_to_rad_180(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(deg_to_rad(180.0_c_float) - real(PI, c_float)) < 1.0e-5, &
               "180 degrees should equal PI radians")
  end subroutine test_deg_to_rad_180

  !> @brief π radians = 180°.
  subroutine test_rad_to_deg_pi(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(rad_to_deg(real(PI, c_float)) - 180.0_c_float) < 1.0e-4, &
               "PI radians should equal 180 degrees")
  end subroutine test_rad_to_deg_pi

  !> @brief Roundtrip: rad_to_deg(deg_to_rad(x)) == x.
  subroutine test_deg_rad_roundtrip(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: original, roundtrip
    original = 45.0_c_float
    roundtrip = rad_to_deg(deg_to_rad(original))
    call check(error, abs(roundtrip - original) < 1.0e-4, &
               "deg→rad→deg roundtrip should preserve value")
  end subroutine test_deg_rad_roundtrip

  !---------------------------------------------------------------------------
  ! IS_NUMERIC TESTS
  !---------------------------------------------------------------------------

  !> @brief "42" is numeric.
  subroutine test_is_numeric_integer(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, logical(is_numeric("42")), &
               "is_numeric('42') should be true")
  end subroutine test_is_numeric_integer

  !> @brief "3.14" is numeric.
  subroutine test_is_numeric_float(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, logical(is_numeric("3.14")), &
               "is_numeric('3.14') should be true")
  end subroutine test_is_numeric_float

  !> @brief "hello" is not numeric.
  subroutine test_is_numeric_text_only(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, .not. logical(is_numeric("hello")), &
               "is_numeric('hello') should be false")
  end subroutine test_is_numeric_text_only

  !> @brief "3.5inches" is numeric (contains digits).
  subroutine test_is_numeric_mixed(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, logical(is_numeric("3.5inches")), &
               "is_numeric('3.5inches') should be true (contains numeric characters)")
  end subroutine test_is_numeric_mixed

end module test_constants_and_conversions
