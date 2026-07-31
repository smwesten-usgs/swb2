module test_crop_coefficient_method_detection
  !! Unit tests for KCB_METHOD assignment logic in crop_coefficients__fao56.
  !!
  !! Verifies that the crop coefficient module correctly defers to the
  !! phenology module's PHENOLOGY_METHOD_INDEX and assigns KCB_METHOD based
  !! on available Kcb data. Also verifies that missing Kcb values produce
  !! a fatal error when the crop coefficient module is active.
  !!
  !! Uses a bespoke lookup table (Lookup__kcb_method_detection_test.txt)
  !! with 6 land uses covering all method combinations:
  !!   LU 12:  FAO56_GDD + staged Kcb         -> KCB_METHOD_GDD
  !!   LU 5:   FAO56_DATES + staged Kcb       -> KCB_METHOD_FAO56
  !!   LU 141: DOY_BASED + monthly Kcb        -> KCB_METHOD_MONTHLY_VALUES
  !!   LU 23:  DOY_BASED + staged Kcb         -> KCB_METHOD_FAO56
  !!   LU 131: PHENOLOGY_NONE + Kcb=0.0       -> KCB_METHOD_FAO56 (valid explicit zero)
  !!   LU 111: PHENOLOGY_NONE + missing Kcb   -> FATAL (expected)

  use iso_c_binding, only: c_int, c_float, c_bool
  use testdrive, only: check, error_type, new_unittest, unittest_type
  use constants_and_conversions, only: TRUE, FALSE
  use exceptions, only: HALT_UPON_FATAL_ERROR
  use crop_coefficients__FAO56, only: crop_coefficients_FAO56_initialize, &
       crop_coefficients_FAO56_deallocate,                                &
       KCB_METHOD, KCB_METHOD_GDD, KCB_METHOD_FAO56,                     &
       KCB_METHOD_MONTHLY_VALUES, KCB_METHOD_NONE
  use phenology, only: PHENOLOGY_METHOD_INDEX,                            &
       PHENOLOGY_FAO56_GDD, PHENOLOGY_FAO56_DATES, PHENOLOGY_DOY_BASED,  &
       PHENOLOGY_NONE
  use parameters, only: PARAMETERS_T
  use simulation_datetime, only: SIM_DT
  implicit none
  private

  public :: collect_crop_coefficient_method_detection

  ! Module-level local PARAMETERS_T instance (isolated from global PARAMS)
  type(PARAMETERS_T), save :: LOCAL_PARAMS
  logical, save            :: environment_ready = .false.

  ! Number of land uses in our test table
  integer(c_int), parameter :: NUM_TEST_LU = 6

contains

  !---------------------------------------------------------------------------
  !> @brief Register all crop coefficient method detection tests.
  !---------------------------------------------------------------------------
  subroutine collect_crop_coefficient_method_detection(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      ! --- KCB method assignment from phenology method + Kcb data ---
      new_unittest("kcb_method_gdd_from_fao56_gdd", test_kcb_method_gdd_from_fao56_gdd),        &
      new_unittest("kcb_method_fao56_from_dates", test_kcb_method_fao56_from_dates),             &
      new_unittest("kcb_method_monthly_from_doy", test_kcb_method_monthly_from_doy),             &
      new_unittest("kcb_method_fao56_from_doy_staged", test_kcb_method_fao56_from_doy_staged),   &
      new_unittest("kcb_method_fao56_from_none_explicit_zero", &
                   test_kcb_method_fao56_from_none_explicit_zero),                               &
      new_unittest("kcb_method_none_missing_kcb_is_fatal", &
                   test_kcb_method_none_missing_kcb_is_fatal),                                    &
      new_unittest("cleanup_crop_coeff_module_state", test_cleanup)                              &
    ]
  end subroutine collect_crop_coefficient_method_detection

  !---------------------------------------------------------------------------
  ! Helper: initialize the test environment with the bespoke lookup table.
  ! Directly populates PHENOLOGY_METHOD_INDEX (the phenology module's public
  ! array) to simulate what phenology_initialize would produce for our test
  ! table. This avoids calling phenology_initialize (which would conflict
  ! with earlier test suites that already initialized it).
  !
  ! Suppresses fatal errors so that the LU 111 row (missing Kcb) doesn't
  ! crash the test program — we verify the resulting KCB_METHOD instead.
  !---------------------------------------------------------------------------
  subroutine setup_kcb_method_detection_environment()

    if (environment_ready) return

    ! Set simulation start date
    call SIM_DT%start%setDateFormat("MM/DD/YYYY")
    call SIM_DT%start%parseDate("01/01/2002", &
         sFilename=trim(__FILE__), iLineNumber=__LINE__)

    ! Load the bespoke test table into a local PARAMETERS_T instance
    call LOCAL_PARAMS%add_file("../test_data/tables/Lookup__kcb_method_detection_test.txt")
    call LOCAL_PARAMS%munge_file()

    ! Directly populate PHENOLOGY_METHOD_INDEX to match what our test table
    ! would produce if phenology_initialize were called with it:
    !   Row 1 (LU 12):  FAO56_GDD      — has start_GDD + GDD_ini/dev/mid/late + killing_frost
    !   Row 2 (LU 5):   FAO56_DATES    — has start_date + L_ini/dev/mid/late
    !   Row 3 (LU 141): DOY_BASED      — has start_date + end_date only
    !   Row 4 (LU 23):  DOY_BASED      — has start_date + end_date only
    !   Row 5 (LU 131): PHENOLOGY_NONE — all phenology columns <NA>
    !   Row 6 (LU 111): PHENOLOGY_NONE — all phenology columns <NA>
    if (allocated(PHENOLOGY_METHOD_INDEX)) deallocate(PHENOLOGY_METHOD_INDEX)
    allocate(PHENOLOGY_METHOD_INDEX(NUM_TEST_LU))
    PHENOLOGY_METHOD_INDEX(1) = PHENOLOGY_FAO56_GDD
    PHENOLOGY_METHOD_INDEX(2) = PHENOLOGY_FAO56_DATES
    PHENOLOGY_METHOD_INDEX(3) = PHENOLOGY_DOY_BASED
    PHENOLOGY_METHOD_INDEX(4) = PHENOLOGY_DOY_BASED
    PHENOLOGY_METHOD_INDEX(5) = PHENOLOGY_NONE
    PHENOLOGY_METHOD_INDEX(6) = PHENOLOGY_NONE

    ! Suppress fatal errors so LU 111 (missing Kcb) doesn't crash the test
    HALT_UPON_FATAL_ERROR = FALSE

    ! Initialize crop coefficients — will attempt to fatal on LU 111
    call crop_coefficients_FAO56_initialize(LOCAL_PARAMS)

    ! Re-enable fatal errors
    HALT_UPON_FATAL_ERROR = TRUE

    environment_ready = .true.

  end subroutine setup_kcb_method_detection_environment

  !---------------------------------------------------------------------------
  ! KCB METHOD ASSIGNMENT TESTS
  ! Verify that crop_coefficients_FAO56_initialize correctly maps
  ! phenology method + Kcb data -> KCB_METHOD.
  !---------------------------------------------------------------------------

  !> @brief LU 12 (FAO56_GDD + staged Kcb) -> KCB_METHOD_GDD.
  subroutine test_kcb_method_gdd_from_fao56_gdd(error)
    type(error_type), allocatable, intent(out) :: error

    call setup_kcb_method_detection_environment()

    ! LU 12 is index 1
    call check(error, KCB_METHOD(1) == KCB_METHOD_GDD, &
               "LU 12 with GDD phenology and staged Kcb should get KCB_METHOD_GDD")
  end subroutine test_kcb_method_gdd_from_fao56_gdd

  !> @brief LU 5 (FAO56_DATES + staged Kcb) -> KCB_METHOD_FAO56.
  subroutine test_kcb_method_fao56_from_dates(error)
    type(error_type), allocatable, intent(out) :: error

    call setup_kcb_method_detection_environment()

    ! LU 5 is index 2
    call check(error, KCB_METHOD(2) == KCB_METHOD_FAO56, &
               "LU 5 with date phenology and staged Kcb should get KCB_METHOD_FAO56")
  end subroutine test_kcb_method_fao56_from_dates

  !> @brief LU 141 (DOY_BASED + monthly Kcb_Jan..Dec) -> KCB_METHOD_MONTHLY_VALUES.
  subroutine test_kcb_method_monthly_from_doy(error)
    type(error_type), allocatable, intent(out) :: error

    call setup_kcb_method_detection_environment()

    ! LU 141 is index 3
    call check(error, KCB_METHOD(3) == KCB_METHOD_MONTHLY_VALUES, &
               "LU 141 with DOY phenology and monthly Kcb should get KCB_METHOD_MONTHLY_VALUES")
  end subroutine test_kcb_method_monthly_from_doy

  !> @brief LU 23 (DOY_BASED + staged Kcb_ini/mid/end/min) -> KCB_METHOD_FAO56.
  subroutine test_kcb_method_fao56_from_doy_staged(error)
    type(error_type), allocatable, intent(out) :: error

    call setup_kcb_method_detection_environment()

    ! LU 23 is index 4
    call check(error, KCB_METHOD(4) == KCB_METHOD_FAO56, &
               "LU 23 with DOY phenology and staged Kcb should get KCB_METHOD_FAO56")
  end subroutine test_kcb_method_fao56_from_doy_staged

  !> @brief LU 131 (PHENOLOGY_NONE + explicit Kcb=0.0) -> KCB_METHOD_FAO56.
  !! User deliberately supplied Kcb=0.0; this is a valid conscious choice.
  subroutine test_kcb_method_fao56_from_none_explicit_zero(error)
    type(error_type), allocatable, intent(out) :: error

    call setup_kcb_method_detection_environment()

    ! LU 131 is index 5
    call check(error, KCB_METHOD(5) == KCB_METHOD_FAO56, &
               "LU 131 with PHENOLOGY_NONE and explicit Kcb=0.0 should get KCB_METHOD_FAO56")
  end subroutine test_kcb_method_fao56_from_none_explicit_zero

  !> @brief LU 111 (PHENOLOGY_NONE + missing Kcb) -> should remain KCB_METHOD_NONE (fatal was issued).
  !! The fatal error was suppressed for testing. KCB_METHOD should still be
  !! KCB_METHOD_NONE (the sentinel value) because the initialization loop
  !! called warn(lFatal=TRUE) and did not assign a valid method.
  subroutine test_kcb_method_none_missing_kcb_is_fatal(error)
    type(error_type), allocatable, intent(out) :: error

    call setup_kcb_method_detection_environment()

    ! LU 111 is index 6 — should still have the sentinel value
    call check(error, KCB_METHOD(6) == KCB_METHOD_NONE, &
               "LU 111 with PHENOLOGY_NONE and missing Kcb should remain KCB_METHOD_NONE "  &
               //"(fatal error was suppressed for testing)")
  end subroutine test_kcb_method_none_missing_kcb_is_fatal

  !---------------------------------------------------------------------------
  !> @brief Teardown: deallocate crop coefficient module state so subsequent
  !! test suites (e.g., fao56) can re-initialize cleanly.
  !---------------------------------------------------------------------------
  subroutine test_cleanup(error)
    type(error_type), allocatable, intent(out) :: error

    call crop_coefficients_FAO56_deallocate()

    ! Also clean up the PHENOLOGY_METHOD_INDEX we directly allocated
    if (allocated(PHENOLOGY_METHOD_INDEX)) deallocate(PHENOLOGY_METHOD_INDEX)

    ! Always passes — just ensures cleanup ran
    call check(error, .true., "crop coefficient module state cleaned up")
  end subroutine test_cleanup

end module test_crop_coefficient_method_detection
