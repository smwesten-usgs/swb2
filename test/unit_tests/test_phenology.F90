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
                       phenology_update_fao56_dates, &
                       phenology_update_fao56_gdd, &
                       phenology_update, &
                       phenology_initialize, &
                       GROWING_SEASON_START_DOY, GROWING_SEASON_END_DOY, &
                       GROWING_SEASON_START_GDD, KILLING_FROST_TEMP, &
                       PHENOLOGY_METHOD_INDEX, &
                       PHENOLOGY_NONE, PHENOLOGY_DOY_BASED, PHENOLOGY_GDD_THRESHOLD, &
                       PHENOLOGY_FAO56_DATES, PHENOLOGY_FAO56_GDD, &
                       GROWTH_STAGE_DORMANT, GROWTH_STAGE_INI, GROWTH_STAGE_DEV, &
                       GROWTH_STAGE_MID, GROWTH_STAGE_LATE
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
      new_unittest("dispatch_none_landuse_always_dormant", test_dispatch_none_dormant), &
      new_unittest("fao56_dates_ini_stage", test_fao56_dates_ini_stage), &
      new_unittest("fao56_dates_dev_stage_midpoint", test_fao56_dates_dev_stage_midpoint), &
      new_unittest("fao56_dates_mid_stage", test_fao56_dates_mid_stage), &
      new_unittest("fao56_dates_late_stage", test_fao56_dates_late_stage), &
      new_unittest("fao56_dates_dormant_after_season", test_fao56_dates_dormant_after), &
      new_unittest("fao56_dates_winter_crop_wrap", test_fao56_dates_winter_crop), &
      new_unittest("fao56_gdd_ini_stage", test_fao56_gdd_ini_stage), &
      new_unittest("fao56_gdd_mid_stage", test_fao56_gdd_mid_stage), &
      new_unittest("fao56_gdd_frost_kills_and_latches", test_fao56_gdd_frost_latch), &
      new_unittest("fao56_gdd_dormant_after_all_stages", test_fao56_gdd_dormant_after), &
      ! --- growth_fraction trajectory tests ---
      new_unittest("gf_doy_is_binary_one", test_gf_doy_is_binary_one), &
      new_unittest("gf_doy_is_binary_zero", test_gf_doy_is_binary_zero), &
      new_unittest("gf_gdd_threshold_is_binary_one", test_gf_gdd_threshold_is_binary_one), &
      new_unittest("gf_gdd_threshold_is_binary_zero", test_gf_gdd_threshold_is_binary_zero), &
      new_unittest("gf_dates_ini_start", test_gf_dates_ini_start), &
      new_unittest("gf_dates_ini_end", test_gf_dates_ini_end), &
      new_unittest("gf_dates_dev_start", test_gf_dates_dev_start), &
      new_unittest("gf_dates_dev_midpoint", test_gf_dates_dev_midpoint), &
      new_unittest("gf_dates_mid_start", test_gf_dates_mid_start), &
      new_unittest("gf_dates_mid_holds", test_gf_dates_mid_holds), &
      new_unittest("gf_dates_late_start", test_gf_dates_late_start), &
      new_unittest("gf_dates_late_midpoint", test_gf_dates_late_midpoint), &
      new_unittest("gf_dates_late_end", test_gf_dates_late_end), &
      new_unittest("gf_dates_dormant", test_gf_dates_dormant), &
      new_unittest("gf_gdd_ini_midpoint", test_gf_gdd_ini_midpoint), &
      new_unittest("gf_gdd_dev_midpoint", test_gf_gdd_dev_midpoint), &
      new_unittest("gf_gdd_mid", test_gf_gdd_mid), &
      new_unittest("gf_gdd_late_midpoint", test_gf_gdd_late_midpoint), &
      new_unittest("gf_gdd_dormant_after", test_gf_gdd_dormant_after) &
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
    call PARAMS%add_file("../test_data/tables/Lookup__crop_coefficient_test.txt")
    call PARAMS%add_file("../test_data/tables/phenology_test.txt")
    call PARAMS%munge_file()
    call phenology_initialize(PARAMS)
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

  !> @brief mm/dd format is correctly converted to DOY (05/01 → 121).
  subroutine test_init_reads_mmdd_as_doy(error)
    type(error_type), allocatable, intent(out) :: error

    call ensure_phenology_initialized()

    ! LU 1 (Corn, index 1): 05/01 → DOY 121
    call check(error, GROWING_SEASON_START_DOY(1) == 121, &
               "05/01 should convert to DOY 121")
    if (allocated(error)) return

    ! LU 1 (Corn, index 1): 10/15 → DOY 288
    call check(error, GROWING_SEASON_END_DOY(1) == 288, &
               "10/15 should convert to DOY 288")
  end subroutine test_init_reads_mmdd_as_doy

  !> @brief Integer DOY strings are read correctly (280 for Sorghum end date).
  subroutine test_init_reads_integer_doy(error)
    type(error_type), allocatable, intent(out) :: error

    call ensure_phenology_initialized()

    ! LU 4 (Sorghum, index 2): start=05/01 → DOY 121, end=280 as integer DOY
    call check(error, GROWING_SEASON_START_DOY(2) == 121, &
               "Sorghum start (05/01) should be DOY 121")
    if (allocated(error)) return

    call check(error, GROWING_SEASON_END_DOY(2) == 280, &
               "Integer DOY 280 should be read directly for end date")
  end subroutine test_init_reads_integer_doy

  !> @brief GDD threshold and killing frost values read correctly.
  subroutine test_init_reads_gdd_threshold(error)
    type(error_type), allocatable, intent(out) :: error

    call ensure_phenology_initialized()

    ! LU 6 (Sunflower, index 4): GDD=250, frost=30.0
    call check(error, abs(GROWING_SEASON_START_GDD(4) - 250.0) < 0.01, &
               "Growing_season_start_GDD should be 250.0 for Sunflower")
    if (allocated(error)) return

    call check(error, abs(KILLING_FROST_TEMP(4) - 30.0) < 0.01, &
               "Killing_frost_temperature should be 30.0 for Sunflower")
  end subroutine test_init_reads_gdd_threshold

  !> @brief Landuses with <NA> for a column get appropriate NODATA values.
  subroutine test_init_nodata_for_missing(error)
    type(error_type), allocatable, intent(out) :: error

    call ensure_phenology_initialized()

    ! LU 12 (Sweet Corn, index 5): all <NA> for DOY and GDD in phenology table
    ! Start DOY comes from crop coeff table (empty → NODATA)
    call check(error, GROWING_SEASON_END_DOY(5) == -9999, &
               "DOY end should be -9999 for Sweet Corn (no end date)")
    if (allocated(error)) return

    ! LU 24 (Winter Wheat, index 8): no GDD columns → NA_FLOAT
    call check(error, .not. (GROWING_SEASON_START_GDD(8) > NA_FLOAT), &
               "GDD start should be NA_FLOAT for DOY-only landuse (Winter Wheat)")
  end subroutine test_init_nodata_for_missing

  !---------------------------------------------------------------------------
  ! Method index tests — verify phenology_initialize sets the correct method
  !---------------------------------------------------------------------------

  !> @brief DOY landuse with FAO56 stage lengths gets PHENOLOGY_FAO56_DATES method index.
  subroutine test_init_method_index_doy(error)
    type(error_type), allocatable, intent(out) :: error
    call ensure_phenology_initialized()
    ! LU 1 (Corn, index 1): has DOY columns AND L_ini/L_dev/L_mid/L_late
    ! from the merged crop coefficient table → FAO56_DATES takes priority
    call check(error, PHENOLOGY_METHOD_INDEX(1) == PHENOLOGY_FAO56_DATES, &
               "Corn should have PHENOLOGY_FAO56_DATES method (DOY + stage lengths present)")
  end subroutine test_init_method_index_doy

  !> @brief GDD landuse gets a GDD-based method (FAO56_GDD when stage GDDs present).
  subroutine test_init_method_index_gdd(error)
    type(error_type), allocatable, intent(out) :: error
    call ensure_phenology_initialized()
    ! LU 12 (Sweet Corn, index 5): has Growing_season_start_GDD + GDD_ini/dev/mid/late
    ! from crop coeff table → FAO56_GDD
    call check(error, PHENOLOGY_METHOD_INDEX(5) == PHENOLOGY_FAO56_GDD, &
               "Sweet Corn should have PHENOLOGY_FAO56_GDD method")
  end subroutine test_init_method_index_gdd

  !> @brief Verify a non-DOY landuse is not tagged DOY_BASED.
  subroutine test_init_method_index_none(error)
    type(error_type), allocatable, intent(out) :: error
    call ensure_phenology_initialized()
    ! LU 26 (Dbl Crop, index 9): should NOT be DOY
    call check(error, PHENOLOGY_METHOD_INDEX(9) /= PHENOLOGY_DOY_BASED, &
               "Dbl Crop should not have PHENOLOGY_DOY_BASED")
    if (allocated(error)) return
    ! LU 1 (Corn, index 1): should NOT be plain GDD_THRESHOLD
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
    real(c_float) :: stage_fraction
    logical(c_bool) :: it_is_growing_season
    logical(c_bool) :: frost_killed_season
    integer(c_int) :: growth_stage

    call ensure_phenology_initialized()
    frost_killed_season = FALSE

    ! LU 1 (Corn, index 1): DOY 150, season is 105-288
    call phenology_update( &
      landuse_index=1, &
      current_doy=150, &
      days_in_year=365, &
      current_gdd=0.0_c_float, &
      mean_air_temperature=70.0_c_float, &
      it_is_growing_season_in=FALSE, &
      frost_killed_season=frost_killed_season, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction)

    call check(error, it_is_growing_season .eqv. .true., &
               "Dispatch DOY: Corn DOY 150 should be growing")
  end subroutine test_dispatch_doy_growing

  !> @brief Dispatch for GDD landuse above threshold returns growing.
  subroutine test_dispatch_gdd_growing(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    real(c_float) :: stage_fraction
    logical(c_bool) :: it_is_growing_season
    logical(c_bool) :: frost_killed_season
    integer(c_int) :: growth_stage

    call ensure_phenology_initialized()
    frost_killed_season = FALSE

    ! LU 5 (Soybeans, index 3): GDD=300 > threshold 200, warm temp
    call phenology_update( &
      landuse_index=3, &
      current_doy=150, &
      days_in_year=365, &
      current_gdd=300.0_c_float, &
      mean_air_temperature=70.0_c_float, &
      it_is_growing_season_in=FALSE, &
      frost_killed_season=frost_killed_season, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction)

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
    real(c_float) :: stage_fraction
    logical(c_bool) :: it_is_growing_season
    logical(c_bool) :: frost_killed_season
    integer(c_int) :: growth_stage

    call ensure_phenology_initialized()
    frost_killed_season = FALSE

    ! LU 1 (Corn, index 1): DOY 30, outside season 105-288
    call phenology_update( &
      landuse_index=1, &
      current_doy=30, &
      days_in_year=365, &
      current_gdd=0.0_c_float, &
      mean_air_temperature=20.0_c_float, &
      it_is_growing_season_in=FALSE, &
      frost_killed_season=frost_killed_season, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction)

    call check(error, it_is_growing_season .eqv. .false., &
               "Dispatch DOY: Corn DOY 30 should be dormant")
  end subroutine test_dispatch_none_dormant

  !---------------------------------------------------------------------------
  ! FAO56_DATES tests
  !
  ! Setup: planting DOY=100, L_ini=20, L_dev=30, L_mid=40, L_late=25
  ! Total season = 115 days (DOY 100 to 214)
  ! Stage boundaries (days since planting):
  !   INI:  0–19   (DOY 100–119)
  !   DEV:  20–49  (DOY 120–149)
  !   MID:  50–89  (DOY 150–189)
  !   LATE: 90–114 (DOY 190–214)
  !   DORMANT: >= 115 (DOY >= 215)
  !---------------------------------------------------------------------------

  !> @brief Day 5 since planting → INI stage, stage_fraction ~0.25
  subroutine test_fao56_dates_ini_stage(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    call phenology_update_fao56_dates( &
      current_doy=105, &
      growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, &
      days_in_year=365, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_INI, &
               "DOY 105, planting 100: should be INI stage")
    if (allocated(error)) return
    call check(error, abs(stage_fraction - 0.25) < 0.01, &
               "5 days into 20-day INI: stage_fraction should be ~0.25")
    if (allocated(error)) return
    call check(error, it_is_growing_season .eqv. .true., &
               "INI stage should be growing season")
  end subroutine test_fao56_dates_ini_stage

  !> @brief Day 35 since planting → DEV stage midpoint, stage_fraction ~0.5
  subroutine test_fao56_dates_dev_stage_midpoint(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    call phenology_update_fao56_dates( &
      current_doy=135, &
      growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, &
      days_in_year=365, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_DEV, &
               "DOY 135, planting 100: should be DEV stage (35 days in)")
    if (allocated(error)) return
    call check(error, abs(stage_fraction - 0.5) < 0.02, &
               "15 days into 30-day DEV: stage_fraction should be ~0.5")
  end subroutine test_fao56_dates_dev_stage_midpoint

  !> @brief Day 60 since planting → MID stage
  subroutine test_fao56_dates_mid_stage(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    call phenology_update_fao56_dates( &
      current_doy=160, &
      growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, &
      days_in_year=365, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_MID, &
               "DOY 160, planting 100: should be MID stage (60 days in)")
    if (allocated(error)) return
    call check(error, abs(growth_fraction - 1.0) < 0.01, &
               "MID stage: growth_fraction should be 1.0")
  end subroutine test_fao56_dates_mid_stage

  !> @brief Day 100 since planting → LATE stage, stage_fraction ~0.4
  subroutine test_fao56_dates_late_stage(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    call phenology_update_fao56_dates( &
      current_doy=200, &
      growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, &
      days_in_year=365, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_LATE, &
               "DOY 200, planting 100: should be LATE stage (100 days in)")
    if (allocated(error)) return
    call check(error, abs(stage_fraction - 0.4) < 0.02, &
               "10 days into 25-day LATE: stage_fraction should be ~0.4")
  end subroutine test_fao56_dates_late_stage

  !> @brief Day 120 since planting → DORMANT (past end of season)
  subroutine test_fao56_dates_dormant_after(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    call phenology_update_fao56_dates( &
      current_doy=220, &
      growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, &
      days_in_year=365, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_DORMANT, &
               "DOY 220, 120 days past planting 100: should be DORMANT")
    if (allocated(error)) return
    call check(error, it_is_growing_season .eqv. .false., &
               "Past end of season: it_is_growing_season should be FALSE")
  end subroutine test_fao56_dates_dormant_after

  !> @brief Winter crop: planting DOY 300, total 120 days, wraps into next year.
  !!        DOY 30 = 95 days since planting → should be in MID stage.
  subroutine test_fao56_dates_winter_crop(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    ! Planting DOY 300, L_ini=20, L_dev=30, L_mid=40, L_late=30
    ! DOY 30 in next year = 30 + (365-300) = 95 days since planting
    ! INI: 0-19, DEV: 20-49, MID: 50-89, LATE: 90-119
    ! 95 days → LATE stage, stage_fraction = (95-90)/30 = 0.167
    call phenology_update_fao56_dates( &
      current_doy=30, &
      growing_season_start_doy=300, &
      l_ini=20, l_dev=30, l_mid=40, l_late=30, &
      days_in_year=365, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_LATE, &
               "Winter crop: DOY 30, planted 300, 95 days in: should be LATE")
    if (allocated(error)) return
    call check(error, it_is_growing_season .eqv. .true., &
               "Winter crop in LATE stage should be growing")
  end subroutine test_fao56_dates_winter_crop

  !---------------------------------------------------------------------------
  ! FAO56_GDD tests
  !
  ! Setup: growing_season_start_gdd=200, gdd_ini=100, gdd_dev=200,
  !        gdd_mid=400, gdd_late=150, killing_frost=28°F
  ! Cumulative GDD boundaries (since planting threshold):
  !   INI:  0–99
  !   DEV:  100–299
  !   MID:  300–699
  !   LATE: 700–849
  !   DORMANT: >= 850
  !---------------------------------------------------------------------------

  !> @brief GDD 250 (50 since planting threshold of 200) → INI stage
  subroutine test_fao56_gdd_ini_stage(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season
    logical(c_bool) :: frost_killed_season

    frost_killed_season = FALSE

    call phenology_update_fao56_gdd( &
      current_gdd=250.0_c_float, &
      mean_air_temperature=70.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      gdd_ini=100.0_c_float, gdd_dev=200.0_c_float, &
      gdd_mid=400.0_c_float, gdd_late=150.0_c_float, &
      frost_killed_season=frost_killed_season, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_INI, &
               "GDD 250, threshold 200: 50 GDD in → should be INI")
    if (allocated(error)) return
    call check(error, abs(stage_fraction - 0.5) < 0.01, &
               "50 of 100 GDD_ini: stage_fraction should be ~0.5")
  end subroutine test_fao56_gdd_ini_stage

  !> @brief GDD 600 (400 since planting threshold) → MID stage
  subroutine test_fao56_gdd_mid_stage(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season
    logical(c_bool) :: frost_killed_season

    frost_killed_season = FALSE

    call phenology_update_fao56_gdd( &
      current_gdd=600.0_c_float, &
      mean_air_temperature=80.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      gdd_ini=100.0_c_float, gdd_dev=200.0_c_float, &
      gdd_mid=400.0_c_float, gdd_late=150.0_c_float, &
      frost_killed_season=frost_killed_season, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_MID, &
               "GDD 600, threshold 200: 400 GDD in → should be MID")
    if (allocated(error)) return
    call check(error, abs(growth_fraction - 1.0) < 0.01, &
               "MID stage: growth_fraction should be 1.0")
  end subroutine test_fao56_gdd_mid_stage

  !> @brief Frost kills during growing → latches dormant, stays dormant on warm day
  subroutine test_fao56_gdd_frost_latch(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season
    logical(c_bool) :: frost_killed_season

    frost_killed_season = FALSE

    ! First call: frost kills (GDD=600 → would be MID, but temp=25 <= 28)
    call phenology_update_fao56_gdd( &
      current_gdd=600.0_c_float, &
      mean_air_temperature=25.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      gdd_ini=100.0_c_float, gdd_dev=200.0_c_float, &
      gdd_mid=400.0_c_float, gdd_late=150.0_c_float, &
      frost_killed_season=frost_killed_season, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_DORMANT, &
               "Frost kills: should be DORMANT")
    if (allocated(error)) return
    call check(error, frost_killed_season .eqv. .true., &
               "frost_killed_season should be TRUE")
    if (allocated(error)) return

    ! Second call: warm day after frost → should STAY dormant (latch)
    call phenology_update_fao56_gdd( &
      current_gdd=650.0_c_float, &
      mean_air_temperature=70.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      gdd_ini=100.0_c_float, gdd_dev=200.0_c_float, &
      gdd_mid=400.0_c_float, gdd_late=150.0_c_float, &
      frost_killed_season=frost_killed_season, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_DORMANT, &
               "After frost latch: warm day should still be DORMANT")
    if (allocated(error)) return
    call check(error, it_is_growing_season .eqv. .false., &
               "After frost latch: it_is_growing_season should be FALSE")
  end subroutine test_fao56_gdd_frost_latch

  !> @brief GDD past all stages → DORMANT naturally (no frost needed)
  subroutine test_fao56_gdd_dormant_after(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season
    logical(c_bool) :: frost_killed_season

    frost_killed_season = FALSE

    ! GDD 1100: 900 since threshold → past end_late (850)
    call phenology_update_fao56_gdd( &
      current_gdd=1100.0_c_float, &
      mean_air_temperature=70.0_c_float, &
      growing_season_start_gdd=200.0_c_float, &
      killing_frost_temperature=28.0_c_float, &
      gdd_ini=100.0_c_float, gdd_dev=200.0_c_float, &
      gdd_mid=400.0_c_float, gdd_late=150.0_c_float, &
      frost_killed_season=frost_killed_season, &
      growth_stage=growth_stage, &
      stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, &
      it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_DORMANT, &
               "GDD 1100, 900 past threshold: all stages done → DORMANT")
    if (allocated(error)) return
    call check(error, it_is_growing_season .eqv. .false., &
               "Past all stages: it_is_growing_season should be FALSE")
  end subroutine test_fao56_gdd_dormant_after

  !---------------------------------------------------------------------------
  ! GROWTH_FRACTION TRAJECTORY TESTS
  !
  ! These tests pin down the exact growth_fraction values at known positions
  ! within the growing season. This is the contract that downstream consumers
  ! (interception, etc.) depend on.
  !
  ! FAO56_DATES setup: planting DOY=100, L_ini=20, L_dev=30, L_mid=40, L_late=25
  !   Stage boundaries (days since planting):
  !     INI:  0–19    growth_fraction: 0.0 → 0.1
  !     DEV:  20–49   growth_fraction: 0.1 → 1.0
  !     MID:  50–89   growth_fraction: 1.0
  !     LATE: 90–114  growth_fraction: 1.0 → 0.2
  !     DORMANT: ≥115 growth_fraction: 0.0
  !
  ! FAO56_GDD setup: start_gdd=200, gdd_ini=100, gdd_dev=200, gdd_mid=400, gdd_late=150
  !   Stage boundaries (GDD since planting threshold):
  !     INI:  0–99    growth_fraction: 0.0 → 0.1
  !     DEV:  100–299 growth_fraction: 0.1 → 1.0
  !     MID:  300–699 growth_fraction: 1.0
  !     LATE: 700–849 growth_fraction: 1.0 → 0.2
  !     DORMANT: ≥850 growth_fraction: 0.0
  !---------------------------------------------------------------------------

  !> @brief DOY_BASED: growth_fraction is exactly 1.0 when growing (binary).
  subroutine test_gf_doy_is_binary_one(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_doy_based( &
      current_doy=150, growing_season_start_doy=100, growing_season_end_doy=280, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, abs(growth_fraction - 1.0) < 1.0e-6, &
               "DOY_BASED growing: growth_fraction must be exactly 1.0")
  end subroutine test_gf_doy_is_binary_one

  !> @brief DOY_BASED: growth_fraction is exactly 0.0 when dormant (binary).
  subroutine test_gf_doy_is_binary_zero(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season
    integer(c_int) :: growth_stage

    call phenology_update_doy_based( &
      current_doy=50, growing_season_start_doy=100, growing_season_end_doy=280, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, abs(growth_fraction) < 1.0e-6, &
               "DOY_BASED dormant: growth_fraction must be exactly 0.0")
  end subroutine test_gf_doy_is_binary_zero

  !> @brief GDD_THRESHOLD: growth_fraction is exactly 1.0 when growing (binary).
  subroutine test_gf_gdd_threshold_is_binary_one(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season, frost_killed_season
    integer(c_int) :: growth_stage

    frost_killed_season = FALSE
    call phenology_update_gdd_threshold( &
      current_gdd=500.0_c_float, mean_air_temperature=65.0_c_float, &
      growing_season_start_gdd=200.0_c_float, killing_frost_temperature=28.0_c_float, &
      it_is_growing_season_in=TRUE, frost_killed_season=frost_killed_season, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, abs(growth_fraction - 1.0) < 1.0e-6, &
               "GDD_THRESHOLD growing: growth_fraction must be exactly 1.0")
  end subroutine test_gf_gdd_threshold_is_binary_one

  !> @brief GDD_THRESHOLD: growth_fraction is exactly 0.0 when dormant (binary).
  subroutine test_gf_gdd_threshold_is_binary_zero(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: growth_fraction
    logical(c_bool) :: it_is_growing_season, frost_killed_season
    integer(c_int) :: growth_stage

    frost_killed_season = FALSE
    call phenology_update_gdd_threshold( &
      current_gdd=100.0_c_float, mean_air_temperature=50.0_c_float, &
      growing_season_start_gdd=200.0_c_float, killing_frost_temperature=28.0_c_float, &
      it_is_growing_season_in=FALSE, frost_killed_season=frost_killed_season, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season, &
      growth_stage=growth_stage)

    call check(error, abs(growth_fraction) < 1.0e-6, &
               "GDD_THRESHOLD dormant: growth_fraction must be exactly 0.0")
  end subroutine test_gf_gdd_threshold_is_binary_zero

  !> @brief FAO56_DATES: start of INI stage → growth_fraction = 0.0
  subroutine test_gf_dates_ini_start(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    ! Day 0 since planting (DOY=100, planting=100)
    call phenology_update_fao56_dates( &
      current_doy=100, growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, days_in_year=365, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, abs(growth_fraction) < 0.01, &
               "FAO56_DATES INI start (day 0): growth_fraction should be ~0.0")
  end subroutine test_gf_dates_ini_start

  !> @brief FAO56_DATES: end of INI stage → growth_fraction = 0.1
  subroutine test_gf_dates_ini_end(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    ! Day 19 since planting (last day of INI, stage_fraction = 19/20 = 0.95)
    ! growth_fraction = 0.95 * 0.1 = 0.095
    call phenology_update_fao56_dates( &
      current_doy=119, growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, days_in_year=365, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_INI, &
               "Day 19: should still be INI")
    if (allocated(error)) return
    call check(error, abs(growth_fraction - 0.095) < 0.01, &
               "FAO56_DATES end of INI (day 19): growth_fraction should be ~0.095")
  end subroutine test_gf_dates_ini_end

  !> @brief FAO56_DATES: start of DEV stage → growth_fraction = 0.1
  subroutine test_gf_dates_dev_start(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    ! Day 20 since planting (first day of DEV, stage_fraction = 0/30 = 0.0)
    ! growth_fraction = 0.1 + 0.0 * 0.9 = 0.1
    call phenology_update_fao56_dates( &
      current_doy=120, growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, days_in_year=365, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_DEV, &
               "Day 20: should be DEV")
    if (allocated(error)) return
    call check(error, abs(growth_fraction - 0.1) < 0.01, &
               "FAO56_DATES start of DEV (day 20): growth_fraction should be 0.1")
  end subroutine test_gf_dates_dev_start

  !> @brief FAO56_DATES: midpoint of DEV stage → growth_fraction = 0.55
  subroutine test_gf_dates_dev_midpoint(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    ! Day 35 since planting (15 days into 30-day DEV, stage_fraction = 0.5)
    ! growth_fraction = 0.1 + 0.5 * 0.9 = 0.55
    call phenology_update_fao56_dates( &
      current_doy=135, growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, days_in_year=365, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_DEV, &
               "Day 35: should be DEV")
    if (allocated(error)) return
    call check(error, abs(growth_fraction - 0.55) < 0.01, &
               "FAO56_DATES midpoint of DEV (day 35): growth_fraction should be 0.55")
  end subroutine test_gf_dates_dev_midpoint

  !> @brief FAO56_DATES: end of DEV / start of MID → growth_fraction = 1.0
  subroutine test_gf_dates_mid_start(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    ! Day 50 since planting (first day of MID, stage_fraction = 0/40 = 0.0)
    ! growth_fraction = 1.0
    call phenology_update_fao56_dates( &
      current_doy=150, growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, days_in_year=365, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_MID, &
               "Day 50: should be MID")
    if (allocated(error)) return
    call check(error, abs(growth_fraction - 1.0) < 0.01, &
               "FAO56_DATES start of MID (day 50): growth_fraction should be 1.0")
  end subroutine test_gf_dates_mid_start

  !> @brief FAO56_DATES: mid-season holds at growth_fraction = 1.0
  subroutine test_gf_dates_mid_holds(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    ! Day 70 since planting (20 days into 40-day MID)
    ! growth_fraction = 1.0 (constant through MID)
    call phenology_update_fao56_dates( &
      current_doy=170, growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, days_in_year=365, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, abs(growth_fraction - 1.0) < 0.01, &
               "FAO56_DATES middle of MID (day 70): growth_fraction should be 1.0")
  end subroutine test_gf_dates_mid_holds

  !> @brief FAO56_DATES: start of LATE → growth_fraction = 1.0
  subroutine test_gf_dates_late_start(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    ! Day 90 since planting (first day of LATE, stage_fraction = 0/25 = 0.0)
    ! growth_fraction = 1.0 (structure remains fully developed through LATE)
    call phenology_update_fao56_dates( &
      current_doy=190, growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, days_in_year=365, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_LATE, &
               "Day 90: should be LATE")
    if (allocated(error)) return
    call check(error, abs(growth_fraction - 1.0) < 0.01, &
               "FAO56_DATES start of LATE (day 90): growth_fraction should be 1.0")
  end subroutine test_gf_dates_late_start

  !> @brief FAO56_DATES: midpoint of LATE → growth_fraction = 1.0 (structure intact)
  subroutine test_gf_dates_late_midpoint(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    ! Day 102 since planting (12 days into 25-day LATE, stage_fraction = 12/25 = 0.48)
    ! growth_fraction = 1.0 (plant structure remains fully developed during senescence)
    call phenology_update_fao56_dates( &
      current_doy=202, growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, days_in_year=365, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_LATE, &
               "Day 102: should be LATE")
    if (allocated(error)) return
    call check(error, abs(growth_fraction - 1.0) < 0.01, &
               "FAO56_DATES midpoint of LATE (day 102): growth_fraction should be 1.0")
  end subroutine test_gf_dates_late_midpoint

  !> @brief FAO56_DATES: end of LATE → growth_fraction = 1.0 (structure intact until dormancy)
  subroutine test_gf_dates_late_end(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    ! Day 114 since planting (24 days into 25-day LATE, stage_fraction = 24/25 = 0.96)
    ! growth_fraction = 1.0 (structure fully intact, drops to 0 only at DORMANT)
    call phenology_update_fao56_dates( &
      current_doy=214, growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, days_in_year=365, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, growth_stage == GROWTH_STAGE_LATE, &
               "Day 114: should be LATE")
    if (allocated(error)) return
    call check(error, abs(growth_fraction - 1.0) < 0.01, &
               "FAO56_DATES end of LATE (day 114): growth_fraction should be 1.0")
  end subroutine test_gf_dates_late_end

  !> @brief FAO56_DATES: past season end → growth_fraction = 0.0 (dormant)
  subroutine test_gf_dates_dormant(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season

    ! Day 120 since planting → past total season length of 115
    call phenology_update_fao56_dates( &
      current_doy=220, growing_season_start_doy=100, &
      l_ini=20, l_dev=30, l_mid=40, l_late=25, days_in_year=365, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, abs(growth_fraction) < 1.0e-6, &
               "FAO56_DATES dormant (past season): growth_fraction must be 0.0")
  end subroutine test_gf_dates_dormant

  !> @brief FAO56_GDD: INI stage midpoint → growth_fraction = 0.05
  subroutine test_gf_gdd_ini_midpoint(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season, frost_killed_season

    frost_killed_season = FALSE

    ! GDD=250 → 50 since planting threshold (200), midpoint of 100-GDD INI
    ! stage_fraction = 50/100 = 0.5, growth_fraction = 0.5 * 0.1 = 0.05
    call phenology_update_fao56_gdd( &
      current_gdd=250.0_c_float, mean_air_temperature=70.0_c_float, &
      growing_season_start_gdd=200.0_c_float, killing_frost_temperature=28.0_c_float, &
      gdd_ini=100.0_c_float, gdd_dev=200.0_c_float, &
      gdd_mid=400.0_c_float, gdd_late=150.0_c_float, &
      frost_killed_season=frost_killed_season, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, abs(growth_fraction - 0.05) < 0.01, &
               "FAO56_GDD INI midpoint (50 of 100 GDD): growth_fraction should be 0.05")
  end subroutine test_gf_gdd_ini_midpoint

  !> @brief FAO56_GDD: DEV stage midpoint → growth_fraction = 0.55
  subroutine test_gf_gdd_dev_midpoint(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season, frost_killed_season

    frost_killed_season = FALSE

    ! GDD=400 → 200 since planting (200). INI ends at 100, so 100 into 200-GDD DEV.
    ! stage_fraction = 100/200 = 0.5, growth_fraction = 0.1 + 0.5 * 0.9 = 0.55
    call phenology_update_fao56_gdd( &
      current_gdd=400.0_c_float, mean_air_temperature=70.0_c_float, &
      growing_season_start_gdd=200.0_c_float, killing_frost_temperature=28.0_c_float, &
      gdd_ini=100.0_c_float, gdd_dev=200.0_c_float, &
      gdd_mid=400.0_c_float, gdd_late=150.0_c_float, &
      frost_killed_season=frost_killed_season, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, abs(growth_fraction - 0.55) < 0.01, &
               "FAO56_GDD DEV midpoint (100 of 200 GDD): growth_fraction should be 0.55")
  end subroutine test_gf_gdd_dev_midpoint

  !> @brief FAO56_GDD: MID stage → growth_fraction = 1.0
  subroutine test_gf_gdd_mid(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season, frost_killed_season

    frost_killed_season = FALSE

    ! GDD=700 → 500 since planting. INI=100, DEV=200 (ends at 300). 200 into 400-GDD MID.
    ! growth_fraction = 1.0
    call phenology_update_fao56_gdd( &
      current_gdd=700.0_c_float, mean_air_temperature=70.0_c_float, &
      growing_season_start_gdd=200.0_c_float, killing_frost_temperature=28.0_c_float, &
      gdd_ini=100.0_c_float, gdd_dev=200.0_c_float, &
      gdd_mid=400.0_c_float, gdd_late=150.0_c_float, &
      frost_killed_season=frost_killed_season, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, abs(growth_fraction - 1.0) < 0.01, &
               "FAO56_GDD MID stage: growth_fraction should be 1.0")
  end subroutine test_gf_gdd_mid

  !> @brief FAO56_GDD: LATE stage midpoint → growth_fraction = 1.0 (structure intact)
  subroutine test_gf_gdd_late_midpoint(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season, frost_killed_season

    frost_killed_season = FALSE

    ! GDD=975 → 775 since planting. end_mid=700, so 75 into 150-GDD LATE.
    ! stage_fraction = 75/150 = 0.5, growth_fraction = 1.0 (structure intact)
    call phenology_update_fao56_gdd( &
      current_gdd=975.0_c_float, mean_air_temperature=70.0_c_float, &
      growing_season_start_gdd=200.0_c_float, killing_frost_temperature=28.0_c_float, &
      gdd_ini=100.0_c_float, gdd_dev=200.0_c_float, &
      gdd_mid=400.0_c_float, gdd_late=150.0_c_float, &
      frost_killed_season=frost_killed_season, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, abs(growth_fraction - 1.0) < 0.01, &
               "FAO56_GDD LATE midpoint: growth_fraction should be 1.0 (structure intact)")
  end subroutine test_gf_gdd_late_midpoint

  !> @brief FAO56_GDD: past all stages → growth_fraction = 0.0
  subroutine test_gf_gdd_dormant_after(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: growth_stage
    real(c_float) :: stage_fraction, growth_fraction
    logical(c_bool) :: it_is_growing_season, frost_killed_season

    frost_killed_season = FALSE

    ! GDD=1100 → 900 since planting. end_late=850. Past all stages.
    call phenology_update_fao56_gdd( &
      current_gdd=1100.0_c_float, mean_air_temperature=70.0_c_float, &
      growing_season_start_gdd=200.0_c_float, killing_frost_temperature=28.0_c_float, &
      gdd_ini=100.0_c_float, gdd_dev=200.0_c_float, &
      gdd_mid=400.0_c_float, gdd_late=150.0_c_float, &
      frost_killed_season=frost_killed_season, &
      growth_stage=growth_stage, stage_fraction=stage_fraction, &
      growth_fraction=growth_fraction, it_is_growing_season=it_is_growing_season)

    call check(error, abs(growth_fraction) < 1.0e-6, &
               "FAO56_GDD past all stages: growth_fraction must be 0.0")
  end subroutine test_gf_gdd_dormant_after

end module test_phenology
