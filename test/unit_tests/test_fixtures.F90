module test_fixtures
  !! Test environment setup routines — analogous to pytest fixtures.
  !!
  !! Provides tiered initialization matching SWB2's operational modes:
  !!   Tier 0: setup_common         — logging, OS detection, SIM_DT start date
  !!   Tier 1: setup_environment_doy_based   — + landuse lookup table
  !!   Tier 2: setup_environment_fao56_dates — + FAO-56 date-based parameters
  !!   Tier 3: setup_environment_fao56_gdd   — + GDD accumulation
  !!
  !! Each tier builds on the previous. All setups are idempotent (safe to
  !! call multiple times; duplicate entries in PARAMETERS_T are ignored
  !! on subsequent calls to munge_file).
  !!
  !! Data file paths assume tests run from test/unit_tests/.
  !! Meson workdir option can override this if needed.

  use iso_c_binding, only: c_int, c_float, c_double, c_bool
  use constants_and_conversions
  use logfiles, only: LOGS, LOG_DEBUG
  use parameters, only: PARAMETERS_T, PARAMS
  use simulation_datetime, only: SIM_DT
  use crop_coefficients__FAO56
  use growing_degree_day
  implicit none
  private

  ! --- Public interface: tiered setup routines ---
  public :: setup_common
  public :: setup_environment_doy_based
  public :: setup_environment_fao56_dates
  public :: setup_environment_fao56_gdd
  public :: reset_environment

  ! --- Shared state accessible by test modules ---
  public :: DEFAULT_IS_CELL_ACTIVE
  public :: DEFAULT_LANDUSE_INDEX

  logical(c_bool) :: DEFAULT_IS_CELL_ACTIVE(3,3) = .true._c_bool
  integer(c_int)  :: DEFAULT_LANDUSE_INDEX(9) = [1, 2, 3, 4, 5, 6, 7, 8, 9]

  ! --- Internal state tracking (idempotency flags) ---
  logical :: common_ready      = .false.
  logical :: doy_ready         = .false.
  logical :: fao56_dates_ready = .false.
  logical :: fao56_gdd_ready   = .false.

contains

  !---------------------------------------------------------------------------
  !> @brief Tier 0: Common foundation (logging, OS detection, simulation date).
  !!
  !! Called implicitly by all higher-level setups. Also useful for tests
  !! that only need logging (e.g., timer tests don't even need this).
  !---------------------------------------------------------------------------
  subroutine setup_common()

    if (common_ready) return

    ! Set path delimiter — forward slash works on all platforms for file I/O
    OS_NATIVE_PATH_DELIMITER = "/"

    call LOGS%initialize(iLogLevel=LOG_DEBUG)

    call SIM_DT%start%setDateFormat("MM/DD/YYYY")
    call SIM_DT%start%parseDate("01/01/2002", &
         sFilename=trim(__FILE__), iLineNumber=__LINE__)

    common_ready = .true.

  end subroutine setup_common

  !---------------------------------------------------------------------------
  !> @brief Tier 1: DOY_BASED environment — landuse lookup table only.
  !!
  !! Suitable for tests of: growing season DOY logic, basic interception,
  !! curve number selection, anything that just needs LU codes and basic
  !! table parameters.
  !---------------------------------------------------------------------------
  subroutine setup_environment_doy_based()

    if (doy_ready) return
    call setup_common()

    call PARAMS%add_file("../test_data/tables/Lookup__crop_coefficient_test.txt")
    call PARAMS%munge_file()

    doy_ready = .true.

  end subroutine setup_environment_doy_based

  !---------------------------------------------------------------------------
  !> @brief Tier 2: FAO56_DATES — adds date-based crop coefficient parameters.
  !!
  !! Loads FAO-56 example tables and initializes the crop coefficient module.
  !! Suitable for tests of: growth stage determination from calendar dates,
  !! Kcb interpolation along date-based curves, Kr/Ke/Ks calculations,
  !! equation 72 (Kcb_max).
  !---------------------------------------------------------------------------
  subroutine setup_environment_fao56_dates()

    if (fao56_dates_ready) return
    call setup_common()

    call PARAMS%add_file("../test_data/tables/Lookup__crop_coefficient_test.txt")
    call PARAMS%add_file("../test_data/tables/FAO56_Example_35.txt")
    call PARAMS%add_file("../test_data/tables/FAO56_equation_72_calcs.txt")
    call PARAMS%munge_file()

    ! Ensure date format is MM/DD/YYYY before FAO56 init (other tests may have
    ! changed the module-level sDATE_FORMAT via setDateFormat)
    call SIM_DT%start%setDateFormat("MM/DD/YYYY")

    call crop_coefficients_FAO56_initialize(PARAMS)

    doy_ready = .true.
    fao56_dates_ready = .true.

  end subroutine setup_environment_fao56_dates

  !---------------------------------------------------------------------------
  !> @brief Tier 3: FAO56_GDD — adds GDD accumulation on top of FAO-56 dates.
  !!
  !! Suitable for tests of: GDD accumulation logic, GDD-based growth stage
  !! transitions, GDD-threshold Kcb curve evaluation.
  !---------------------------------------------------------------------------
  subroutine setup_environment_fao56_gdd()

    if (fao56_gdd_ready) return
    call setup_environment_fao56_dates()

    call growing_degree_day_initialize(DEFAULT_IS_CELL_ACTIVE, DEFAULT_LANDUSE_INDEX)

    fao56_gdd_ready = .true.

  end subroutine setup_environment_fao56_gdd

  !---------------------------------------------------------------------------
  !> @brief Reset all initialization flags.
  !!
  !! Use sparingly — only when a test needs to verify initialization
  !! behavior itself (e.g., testing that a missing column produces a
  !! clear error message).
  !!
  !! Note: this resets the flags but does NOT deallocate module-level
  !! arrays in the SWB modules. For tests that truly need re-initialization,
  !! those modules would need their own cleanup routines (future work).
  !---------------------------------------------------------------------------
  subroutine reset_environment()

    common_ready      = .false.
    doy_ready         = .false.
    fao56_dates_ready = .false.
    fao56_gdd_ready   = .false.

  end subroutine reset_environment

end module test_fixtures
