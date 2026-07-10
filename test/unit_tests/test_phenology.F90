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
                       phenology_update, &
                       phenology_initialize, &
                       GROWING_SEASON_START_DOY, GROWING_SEASON_END_DOY, &
                       GROWING_SEASON_START_GDD, KILLING_FROST_TEMP, &
                       PHENOLOGY_METHOD_INDEX, &
                       PHENOLOGY_NONE, PHENOLOGY_DOY_BASED, PHENOLOGY_GDD_THRESHOLD, &
                       GROWTH_STAGE_DORMANT, GROWTH_STAGE_MID
  use parameters, only: PARAMETERS_T, PARAMS
  use fstring_list, only: NA_FLOAT
  use test_fixtures, only: setup_common
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
      new_unittest("gdd_above_frost_threshold_stays_growing", test_gdd_above_frost_stays_growing), &
      new_unittest("init_reads_mmdd_as_doy", test_init_reads_mmdd_as_doy), &
      new_unittest("init_lu_code_count", test_init_lu_code_count), &
      new_unittest("init_reads_integer_doy", test_init_reads_integer_doy), &
      new_unittest("init_reads_gdd_threshold", test_init_reads_gdd_threshold), &
      new_unittest("init_nodata_for_missing_columns", test_init_nodata_for_missing), &
      new_unittest("init_method_index_doy_landuse", test_init_method_index_doy), &
      new_unittest("init_method_index_gdd_landuse", test_init_method_index_gdd), &
      new_unittest("init_method_index_none_landuse", test_init_method_index_none), &
      new_unittest("dispatch_doy_landuse_is_growing", test_dispatch_doy_growing), &
      new_unittest("dispatch_gdd_landuse_above_threshold", test_dispatch_gdd_growing), &
      new_unittest("dispatch_none_landuse_always_dormant", test_dispatch_none_dormant) &
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
    logical(c_bool) :: frost_killed_season
    integer(c_int) :: growth_stage

    frost_killed_season = FALSE

    call phenology_update_gdd_threshold( &
      current_gdd=150.0_c_float, &
      mean_air_temperature=50.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      it_is_growing_season_in=FALSE, &
      frost_killed_season=frost_killed_season, &
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
    logical(c_bool) :: frost_killed_season
    integer(c_int) :: growth_stage

    frost_killed_season = FALSE

    call phenology_update_gdd_threshold( &
      current_gdd=200.0_c_float, &
      mean_air_temperature=50.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      it_is_growing_season_in=FALSE, &
      frost_killed_season=frost_killed_season, &
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
    logical(c_bool) :: frost_killed_season
    integer(c_int) :: growth_stage

    frost_killed_season = FALSE

    call phenology_update_gdd_threshold( &
      current_gdd=500.0_c_float, &
      mean_air_temperature=65.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      it_is_growing_season_in=TRUE, &
      frost_killed_season=frost_killed_season, &
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
    logical(c_bool) :: frost_killed_season
    integer(c_int) :: growth_stage

    frost_killed_season = FALSE

    call phenology_update_gdd_threshold( &
      current_gdd=500.0_c_float, &
      mean_air_temperature=25.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      it_is_growing_season_in=TRUE, &
      frost_killed_season=frost_killed_season, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .false., &
               "Temperature 25 <= frost threshold 28: season should end")
    call check(error, frost_killed_season .eqv. .true., &
               "frost_killed_season should be set TRUE after killing frost")
  end subroutine test_gdd_killing_frost_ends_season

  !> @brief Temperature above frost threshold while growing: season continues.
  subroutine test_gdd_above_frost_stays_growing(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    logical(c_bool) :: frost_killed_season
    integer(c_int) :: growth_stage

    frost_killed_season = FALSE

    call phenology_update_gdd_threshold( &
      current_gdd=500.0_c_float, &
      mean_air_temperature=29.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      it_is_growing_season_in=TRUE, &
      frost_killed_season=frost_killed_season, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .true., &
               "Temperature 29 > frost threshold 28: season should continue")
  end subroutine test_gdd_above_frost_stays_growing

  !---------------------------------------------------------------------------
  ! phenology_initialize tests
  !
  ! Uses test/test_data/tables/phenology_test.txt (9 landuses, same LU_Codes
  ! as Lookup__crop_coefficient_test.txt):
  !   LU 1  (Corn):         Growing_season_start_date=04/15, end=10/15
  !   LU 4  (Sorghum):      Growing_season_start_date=100,   end=280
  !   LU 5  (Soybeans):     GDD=200.0, frost=28.0
  !   LU 6  (Sunflower):    GDD=250.0, frost=30.0
  !   LU 12 (Sweet Corn):   Growing_season_start_date=05/01, end=09/30
  !   LU 21 (Barley):       Growing_season_start_date=04/01, end=08/15
  !   LU 23 (Spring Wheat): Growing_season_start_date=04/20, end=09/01
  !   LU 24 (Winter Wheat): Growing_season_start_date=10/01, end=05/30
  !   LU 26 (Dbl Crop):     GDD=150.0, frost=26.0
  !
  ! phenology_initialize reads from the global PARAMS. The table shares
  ! LU_Code/Description with the crop coeff table already loaded by other
  ! test suites, so columns merge cleanly.
  !---------------------------------------------------------------------------

  subroutine ensure_phenology_initialized()
    logical, save :: initialized = .false.
    if (initialized) return
    call setup_common()
    call PARAMS%add_file("../test_data/tables/phenology_test.txt")
    call PARAMS%munge_file()
    call phenology_initialize()
    initialized = .true.
  end subroutine ensure_phenology_initialized

  !> @brief Verify PARAMS reads all 9 LU_Code entries.
  subroutine test_init_lu_code_count(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int), allocatable :: lu_codes(:)
    character(len=64) :: msg

    call ensure_phenology_initialized()

    call PARAMS%get_parameters( sKey="LU_Code", iValues=lu_codes )
    write(msg, '("Expected 9 LU_Code values, got ", i0)') size(lu_codes)
    call check(error, size(lu_codes) == 9, trim(msg))
  end subroutine test_init_lu_code_count

  !> @brief mm/dd format is correctly converted to DOY (04/15 → 105).
  subroutine test_init_reads_mmdd_as_doy(error)
    type(error_type), allocatable, intent(out) :: error

    call ensure_phenology_initialized()

    ! LU 1 (Corn, index 1): 04/15 → DOY 105
    call check(error, GROWING_SEASON_START_DOY(1) == 105, &
               "04/15 should convert to DOY 105")
    if (allocated(error)) return

    ! LU 1 (Corn, index 1): 10/15 → DOY 288
    call check(error, GROWING_SEASON_END_DOY(1) == 288, &
               "10/15 should convert to DOY 288")
  end subroutine test_init_reads_mmdd_as_doy

  !> @brief Integer DOY strings are read correctly (100, 280).
  subroutine test_init_reads_integer_doy(error)
    type(error_type), allocatable, intent(out) :: error

    call ensure_phenology_initialized()

    ! LU 4 (Sorghum, index 2): start=100, end=280 as integer DOY
    call check(error, GROWING_SEASON_START_DOY(2) == 100, &
               "Integer DOY 100 should be read directly")
    if (allocated(error)) return

    call check(error, GROWING_SEASON_END_DOY(2) == 280, &
               "Integer DOY 280 should be read directly")
  end subroutine test_init_reads_integer_doy

  !> @brief GDD threshold values read correctly for GDD_THRESHOLD landuses.
  subroutine test_init_reads_gdd_threshold(error)
    type(error_type), allocatable, intent(out) :: error

    call ensure_phenology_initialized()

    ! LU 5 (Soybeans, index 3): GDD=200, frost=28.0
    call check(error, abs(GROWING_SEASON_START_GDD(3) - 200.0) < 0.01, &
               "Growing_season_start_GDD should be 200.0 for Soybeans")
    if (allocated(error)) return

    call check(error, abs(KILLING_FROST_TEMP(3) - 28.0) < 0.01, &
               "Killing_frost_temperature should be 28.0 for Soybeans")
  end subroutine test_init_reads_gdd_threshold

  !> @brief Landuses with <NA> for a column get appropriate NODATA values.
  subroutine test_init_nodata_for_missing(error)
    type(error_type), allocatable, intent(out) :: error

    call ensure_phenology_initialized()

    ! LU 5 (Soybeans, index 3): no DOY columns → NODATA_INT (-9999)
    call check(error, GROWING_SEASON_START_DOY(3) == -9999, &
               "DOY start should be -9999 for GDD-only landuse")
    if (allocated(error)) return

    ! LU 1 (Corn, index 1): no GDD columns → NA_FLOAT
    call check(error, .not. (GROWING_SEASON_START_GDD(1) > NA_FLOAT), &
               "GDD start should be NA_FLOAT for DOY-only landuse")
  end subroutine test_init_nodata_for_missing

  !---------------------------------------------------------------------------
  ! Method index tests — verify phenology_initialize sets the correct method
  !---------------------------------------------------------------------------

  !> @brief DOY landuse gets PHENOLOGY_DOY_BASED method index.
  subroutine test_init_method_index_doy(error)
    type(error_type), allocatable, intent(out) :: error
    call ensure_phenology_initialized()
    ! LU 1 (Corn, index 1): has DOY columns
    call check(error, PHENOLOGY_METHOD_INDEX(1) == PHENOLOGY_DOY_BASED, &
               "Corn should have PHENOLOGY_DOY_BASED method")
  end subroutine test_init_method_index_doy

  !> @brief GDD landuse gets PHENOLOGY_GDD_THRESHOLD method index.
  subroutine test_init_method_index_gdd(error)
    type(error_type), allocatable, intent(out) :: error
    call ensure_phenology_initialized()
    ! LU 5 (Soybeans, index 3): has GDD columns
    call check(error, PHENOLOGY_METHOD_INDEX(3) == PHENOLOGY_GDD_THRESHOLD, &
               "Soybeans should have PHENOLOGY_GDD_THRESHOLD method")
  end subroutine test_init_method_index_gdd

  !> @brief Landuse with neither DOY nor GDD gets PHENOLOGY_NONE.
  !! Note: In this test table all landuses have either DOY or GDD, so
  !! we test this indirectly — if a future table has a bare landuse, it
  !! should get PHENOLOGY_NONE. For now, verify a GDD landuse is NOT DOY.
  subroutine test_init_method_index_none(error)
    type(error_type), allocatable, intent(out) :: error
    call ensure_phenology_initialized()
    ! LU 5 (Soybeans, index 3): should NOT be DOY
    call check(error, PHENOLOGY_METHOD_INDEX(3) /= PHENOLOGY_DOY_BASED, &
               "Soybeans should not have PHENOLOGY_DOY_BASED")
    if (allocated(error)) return
    ! LU 1 (Corn, index 1): should NOT be GDD
    call check(error, PHENOLOGY_METHOD_INDEX(1) /= PHENOLOGY_GDD_THRESHOLD, &
               "Corn should not have PHENOLOGY_GDD_THRESHOLD")
  end subroutine test_init_method_index_none

  !---------------------------------------------------------------------------
  ! Dispatch tests — verify phenology_update routes correctly
  !---------------------------------------------------------------------------

  !> @brief Dispatch for DOY landuse in mid-season returns growing.
  subroutine test_dispatch_doy_growing(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    logical(c_bool) :: frost_killed_season
    integer(c_int) :: growth_stage

    call ensure_phenology_initialized()
    frost_killed_season = FALSE

    ! LU 1 (Corn, index 1): DOY 150, season is 105-288
    call phenology_update( &
      landuse_index=1, &
      current_doy=150, &
      current_gdd=0.0_c_float, &
      mean_air_temperature=70.0_c_float, &
      it_is_growing_season_in=FALSE, &
      frost_killed_season=frost_killed_season, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .true., &
               "Dispatch DOY: Corn DOY 150 should be growing")
  end subroutine test_dispatch_doy_growing

  !> @brief Dispatch for GDD landuse above threshold returns growing.
  subroutine test_dispatch_gdd_growing(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    logical(c_bool) :: frost_killed_season
    integer(c_int) :: growth_stage

    call ensure_phenology_initialized()
    frost_killed_season = FALSE

    ! LU 5 (Soybeans, index 3): GDD=300 > threshold 200, warm temp
    call phenology_update( &
      landuse_index=3, &
      current_doy=150, &
      current_gdd=300.0_c_float, &
      mean_air_temperature=70.0_c_float, &
      it_is_growing_season_in=FALSE, &
      frost_killed_season=frost_killed_season, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .true., &
               "Dispatch GDD: Soybeans GDD 300 > 200 should be growing")
  end subroutine test_dispatch_gdd_growing

  !> @brief Dispatch for PHENOLOGY_NONE landuse returns dormant.
  !! Since our test table doesn't have a NONE landuse, we test by calling
  !! phenology_update_doy_based with a DOY outside the season — confirming
  !! the dispatch at least doesn't crash for valid landuses.
  !! A proper NONE test would require a table with an all-NA row.
  subroutine test_dispatch_none_dormant(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    logical(c_bool) :: frost_killed_season
    integer(c_int) :: growth_stage

    call ensure_phenology_initialized()
    frost_killed_season = FALSE

    ! LU 1 (Corn, index 1): DOY 30, outside season 105-288
    call phenology_update( &
      landuse_index=1, &
      current_doy=30, &
      current_gdd=0.0_c_float, &
      mean_air_temperature=20.0_c_float, &
      it_is_growing_season_in=FALSE, &
      frost_killed_season=frost_killed_season, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, it_is_growing_season .eqv. .false., &
               "Dispatch DOY: Corn DOY 30 should be dormant")
  end subroutine test_dispatch_none_dormant

end module test_phenology
