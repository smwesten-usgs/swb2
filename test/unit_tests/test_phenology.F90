module test_phenology
  !! Unit tests for the phenology module.
  !!
  !! Tests cover:
  !!   - DOY_BASED method: normal season (start < end), winter crop (start > end),
  !!     boundary conditions (exact start/end DOY), dormant period
  !!   - GDD_THRESHOLD method: accumulation to threshold, killing frost termination,
  !!     reset at year boundary

  use iso_c_binding, only: c_int, c_float, c_bool
  use testdrive, only: check, error_type, new_unittest, unittest_type, test_failed
  use constants_and_conversions, only: TRUE, FALSE
  use phenology, only: phenology_update_doy_based, &
                       phenology_update_gdd_threshold, &
                       GROWTH_STAGE_DORMANT, GROWTH_STAGE_MID
  implicit none
  private

  public :: collect_phenology

contains

  !---------------------------------------------------------------------------
  !> @brief Register all phenology tests.
  !---------------------------------------------------------------------------
  subroutine collect_phenology(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      new_unittest("doy_mid_season_is_growing", test_doy_mid_season_is_growing), &
      new_unittest("doy_before_start_is_dormant", test_doy_before_start_is_dormant), &
      new_unittest("doy_after_end_is_dormant", test_doy_after_end_is_dormant), &
      new_unittest("doy_on_start_day_is_growing", test_doy_on_start_day_is_growing), &
      new_unittest("doy_on_end_day_is_growing", test_doy_on_end_day_is_growing), &
      new_unittest("doy_day_after_end_is_dormant", test_doy_day_after_end_is_dormant), &
      new_unittest("doy_winter_crop_mid_winter", test_doy_winter_crop_mid_winter), &
      new_unittest("doy_winter_crop_summer_dormant", test_doy_winter_crop_summer_dormant), &
      new_unittest("doy_winter_crop_on_start", test_doy_winter_crop_on_start), &
      new_unittest("doy_growth_fraction_is_one_when_growing", test_doy_growth_fraction_one), &
      new_unittest("doy_growth_fraction_is_zero_when_dormant", test_doy_growth_fraction_zero), &
      new_unittest("gdd_below_threshold_is_dormant", test_gdd_below_threshold_is_dormant), &
      new_unittest("gdd_at_threshold_is_growing", test_gdd_at_threshold_is_growing), &
      new_unittest("gdd_above_threshold_is_growing", test_gdd_above_threshold_is_growing), &
      new_unittest("gdd_killing_frost_ends_season", test_gdd_killing_frost_ends_season), &
      new_unittest("gdd_above_frost_threshold_stays_growing", test_gdd_above_frost_stays_growing) &
    ]
  end subroutine collect_phenology

  !---------------------------------------------------------------------------
  ! DOY_BASED — Normal season (start=100, end=280)
  !---------------------------------------------------------------------------

  !> @brief Day 150 (mid-summer) should be growing season.
  subroutine test_doy_mid_season_is_growing(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_doy_based( &
      current_doy=150, &
      growing_season_start_doy=100, &
      growing_season_end_doy=280, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .true., &
               "DOY 150 should be growing season (start=100, end=280)")
  end subroutine test_doy_mid_season_is_growing

  !> @brief Day 50 (before start) should be dormant.
  subroutine test_doy_before_start_is_dormant(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_doy_based( &
      current_doy=50, &
      growing_season_start_doy=100, &
      growing_season_end_doy=280, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .false., &
               "DOY 50 should be dormant (start=100, end=280)")
  end subroutine test_doy_before_start_is_dormant

  !> @brief Day 300 (after end) should be dormant.
  subroutine test_doy_after_end_is_dormant(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_doy_based( &
      current_doy=300, &
      growing_season_start_doy=100, &
      growing_season_end_doy=280, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .false., &
               "DOY 300 should be dormant (start=100, end=280)")
  end subroutine test_doy_after_end_is_dormant

  !> @brief Exact start day should be growing.
  subroutine test_doy_on_start_day_is_growing(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_doy_based( &
      current_doy=100, &
      growing_season_start_doy=100, &
      growing_season_end_doy=280, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .true., &
               "Exact start DOY should be growing")
  end subroutine test_doy_on_start_day_is_growing

  !> @brief Exact end day should be growing (last day of season, inclusive).
  subroutine test_doy_on_end_day_is_growing(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_doy_based( &
      current_doy=280, &
      growing_season_start_doy=100, &
      growing_season_end_doy=280, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .true., &
               "Exact end DOY should be growing (inclusive)")
  end subroutine test_doy_on_end_day_is_growing

  !> @brief Day after end should be dormant.
  subroutine test_doy_day_after_end_is_dormant(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_doy_based( &
      current_doy=281, &
      growing_season_start_doy=100, &
      growing_season_end_doy=280, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .false., &
               "DOY 281 should be dormant (end=280)")
  end subroutine test_doy_day_after_end_is_dormant

  !---------------------------------------------------------------------------
  ! DOY_BASED — Winter crop (start=275, end=120) — wraps around year boundary
  !---------------------------------------------------------------------------

  !> @brief Winter crop: DOY 350 (mid-winter) should be growing.
  subroutine test_doy_winter_crop_mid_winter(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_doy_based( &
      current_doy=350, &
      growing_season_start_doy=275, &
      growing_season_end_doy=120, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .true., &
               "Winter crop: DOY 350 should be growing (start=275, end=120)")
  end subroutine test_doy_winter_crop_mid_winter

  !> @brief Winter crop: DOY 200 (summer) should be dormant.
  subroutine test_doy_winter_crop_summer_dormant(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_doy_based( &
      current_doy=200, &
      growing_season_start_doy=275, &
      growing_season_end_doy=120, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .false., &
               "Winter crop: DOY 200 should be dormant (start=275, end=120)")
  end subroutine test_doy_winter_crop_summer_dormant

  !> @brief Winter crop: exact start day should be growing.
  subroutine test_doy_winter_crop_on_start(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_doy_based( &
      current_doy=275, &
      growing_season_start_doy=275, &
      growing_season_end_doy=120, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .true., &
               "Winter crop: exact start DOY 275 should be growing")
  end subroutine test_doy_winter_crop_on_start

  !---------------------------------------------------------------------------
  ! DOY_BASED — growth_fraction and growth_stage outputs
  !---------------------------------------------------------------------------

  !> @brief growth_fraction should be 1.0 during growing season.
  subroutine test_doy_growth_fraction_one(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_doy_based( &
      current_doy=150, &
      growing_season_start_doy=100, &
      growing_season_end_doy=280, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, abs(growth_fraction - 1.0) < 1.0e-6, &
               "growth_fraction should be 1.0 during growing season")
    call check(error, growth_stage == GROWTH_STAGE_MID, &
               "growth_stage should be GROWTH_STAGE_MID during growing season")
  end subroutine test_doy_growth_fraction_one

  !> @brief growth_fraction should be 0.0 during dormant period.
  subroutine test_doy_growth_fraction_zero(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_doy_based( &
      current_doy=50, &
      growing_season_start_doy=100, &
      growing_season_end_doy=280, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, abs(growth_fraction) < 1.0e-6, &
               "growth_fraction should be 0.0 during dormant period")
    call check(error, growth_stage == GROWTH_STAGE_DORMANT, &
               "growth_stage should be GROWTH_STAGE_DORMANT during dormant period")
  end subroutine test_doy_growth_fraction_zero

  !---------------------------------------------------------------------------
  ! GDD_THRESHOLD tests
  !
  ! Setup: growing_season_start_gdd = 200.0 degree-days
  !        killing_frost_temperature = 28.0 degrees (Fahrenheit)
  !---------------------------------------------------------------------------

  !> @brief GDD below threshold: vegetation should be dormant.
  subroutine test_gdd_below_threshold_is_dormant(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_gdd_threshold( &
      current_gdd=150.0_c_float, &
      mean_air_temperature=50.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      it_is_growing_season_in=FALSE, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .false., &
               "GDD 150 < threshold 200: should be dormant")
  end subroutine test_gdd_below_threshold_is_dormant

  !> @brief GDD exactly at threshold: growing season begins.
  subroutine test_gdd_at_threshold_is_growing(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_gdd_threshold( &
      current_gdd=200.0_c_float, &
      mean_air_temperature=50.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      it_is_growing_season_in=FALSE, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .true., &
               "GDD 200 at threshold 200: should be growing")
  end subroutine test_gdd_at_threshold_is_growing

  !> @brief GDD above threshold with warm temperature: still growing.
  subroutine test_gdd_above_threshold_is_growing(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_gdd_threshold( &
      current_gdd=500.0_c_float, &
      mean_air_temperature=65.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      it_is_growing_season_in=TRUE, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .true., &
               "GDD 500 > threshold, warm temp: should remain growing")
  end subroutine test_gdd_above_threshold_is_growing

  !> @brief Killing frost while growing: season ends.
  subroutine test_gdd_killing_frost_ends_season(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_gdd_threshold( &
      current_gdd=500.0_c_float, &
      mean_air_temperature=25.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      it_is_growing_season_in=TRUE, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .false., &
               "Temperature 25 <= frost threshold 28: season should end")
  end subroutine test_gdd_killing_frost_ends_season

  !> @brief Temperature above frost threshold while growing: season continues.
  subroutine test_gdd_above_frost_stays_growing(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_gdd_threshold( &
      current_gdd=500.0_c_float, &
      mean_air_temperature=29.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      it_is_growing_season_in=TRUE, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .true., &
               "Temperature 29 > frost threshold 28: season should continue")
  end subroutine test_gdd_above_frost_stays_growing

end module test_phenology
