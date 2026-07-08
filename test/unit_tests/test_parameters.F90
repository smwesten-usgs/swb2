module test_parameters
  !! Unit tests for the PARAMETERS_T input pipeline.
  !!
  !! Tests add_file, munge_file, and get_parameters (by key) for integer,
  !! float, and string list retrieval. Uses test data files in
  !! ../test_data/tables/.
  !!
  !! Requires Tier 1 (DOY_BASED) environment from test_fixtures at minimum
  !! for logging. Uses its own PARAMETERS_T instance to avoid polluting
  !! the shared TEST_PARAMS.

  use iso_c_binding, only: c_int, c_float, c_double, c_bool
  use testdrive, only: check, error_type, new_unittest, unittest_type, test_failed
  use parameters, only: PARAMETERS_T
  use fstring_list, only: FSTRING_LIST_T, NA_INT, NA_FLOAT
  use test_fixtures, only: setup_common
  implicit none
  private

  public :: collect_parameters

  type(PARAMETERS_T) :: LOCAL_PARAMS
  logical :: params_loaded = .false.

contains

  !---------------------------------------------------------------------------
  !> @brief Ensure test parameters are loaded (idempotent).
  !---------------------------------------------------------------------------
  subroutine ensure_params_loaded()
    if (params_loaded) return
    call setup_common()
    call LOCAL_PARAMS%add_file("../test_data/tables/Lookup__crop_coefficient_test.txt")
    call LOCAL_PARAMS%munge_file()
    params_loaded = .true.
  end subroutine ensure_params_loaded

  !---------------------------------------------------------------------------
  !> @brief Register all parameters tests.
  !---------------------------------------------------------------------------
  subroutine collect_parameters(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    call ensure_params_loaded()

    testsuite = [ &
      ! --- Basic file loading ---
      new_unittest("add_file_increments_count", test_add_file_increments_count), &
      ! --- Integer retrieval ---
      new_unittest("get_int_by_key", test_get_int_by_key), &
      new_unittest("get_int_correct_count", test_get_int_correct_count), &
      ! --- Float retrieval ---
      new_unittest("get_float_by_key", test_get_float_by_key), &
      new_unittest("get_float_values_reasonable", test_get_float_values_reasonable), &
      ! --- String list retrieval ---
      new_unittest("get_string_list_by_key", test_get_string_list_by_key), &
      ! --- Missing key behavior ---
      new_unittest("get_missing_key_nonfatal", test_get_missing_key_nonfatal), &
      ! --- Duplicate column verification ---
      new_unittest("duplicate_lu_code_matching", test_duplicate_lu_code_matching) &
    ]
  end subroutine collect_parameters

  !---------------------------------------------------------------------------
  ! BASIC FILE LOADING
  !---------------------------------------------------------------------------

  !> @brief add_file increments the internal file count.
  subroutine test_add_file_increments_count(error)
    type(error_type), allocatable, intent(out) :: error
    type(PARAMETERS_T) :: temp_params

    call temp_params%add_file("../test_data/tables/Lookup__crop_coefficient_test.txt")
    call check(error, temp_params%count == 1, "count should be 1 after one add_file")
    if (allocated(error)) return

    call temp_params%add_file("../test_data/tables/FAO56_Example_35.txt")
    call check(error, temp_params%count == 2, "count should be 2 after two add_file calls")
  end subroutine test_add_file_increments_count

  !---------------------------------------------------------------------------
  ! INTEGER RETRIEVAL
  !---------------------------------------------------------------------------

  !> @brief get_parameters retrieves integer values for a known key.
  subroutine test_get_int_by_key(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int), allocatable :: values(:)

    call LOCAL_PARAMS%get_parameters(values, sKey="LU_Code")

    call check(error, allocated(values), &
               "get_parameters should allocate values for 'LU_Code'")
    if (allocated(error)) return
    call check(error, size(values) > 0, &
               "LU_Code should have at least one value")
    if (allocated(error)) return
    ! First land use code should be a positive integer
    call check(error, values(1) > 0, &
               "first LU_Code should be > 0")
  end subroutine test_get_int_by_key

  !> @brief Integer array has expected number of entries (matching table rows).
  subroutine test_get_int_correct_count(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int), allocatable :: lu_codes(:)
    real(c_float), allocatable :: kcb_ini(:)

    call LOCAL_PARAMS%get_parameters(lu_codes, sKey="LU_Code")
    call LOCAL_PARAMS%get_parameters(kcb_ini, sKey="Kcb_ini")

    ! Both columns should have the same number of rows
    call check(error, size(lu_codes) == size(kcb_ini), &
               "LU_Code and Kcb_ini should have same number of entries")
  end subroutine test_get_int_correct_count

  !---------------------------------------------------------------------------
  ! FLOAT RETRIEVAL
  !---------------------------------------------------------------------------

  !> @brief get_parameters retrieves float values for a known key.
  subroutine test_get_float_by_key(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float), allocatable :: values(:)

    call LOCAL_PARAMS%get_parameters(values, sKey="Kcb_ini")

    call check(error, allocated(values), &
               "get_parameters should allocate values for 'Kcb_ini'")
    if (allocated(error)) return
    call check(error, size(values) > 0, &
               "Kcb_ini should have at least one value")
  end subroutine test_get_float_by_key

  !> @brief Retrieved Kcb values should be physically reasonable (0 to 2).
  subroutine test_get_float_values_reasonable(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float), allocatable :: values(:)
    integer(c_int) :: i, num_failures
    character(len=512) :: failure_msg
    character(len=80) :: line

    call LOCAL_PARAMS%get_parameters(values, sKey="Kcb_ini")

    num_failures = 0
    failure_msg = ""

    do i = 1, size(values)
      if (values(i) < 0.0 .or. values(i) > 2.0) then
        num_failures = num_failures + 1
        write(line, '("  Index ", i2, ": Kcb_ini=", f6.3, " out of range [0,2]", a)') &
              i, values(i), new_line('a')
        failure_msg = trim(failure_msg) // trim(line)
      end if
    end do

    if (num_failures > 0) call test_failed(error, trim(failure_msg))
  end subroutine test_get_float_values_reasonable

  !---------------------------------------------------------------------------
  ! STRING LIST RETRIEVAL
  !---------------------------------------------------------------------------

  !> @brief get_parameters retrieves a string list for a known key.
  subroutine test_get_string_list_by_key(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: values

    call LOCAL_PARAMS%get_parameters(values, sKey="LU_Code")

    call check(error, values%count > 0, &
               "string list retrieval for 'LU_Code' should have entries")
  end subroutine test_get_string_list_by_key

  !---------------------------------------------------------------------------
  ! MISSING KEY BEHAVIOR
  !---------------------------------------------------------------------------

  !> @brief Requesting a non-existent key with lFatal=FALSE should not crash.
  subroutine test_get_missing_key_nonfatal(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int), allocatable :: values(:)

    call LOCAL_PARAMS%get_parameters(values, sKey="NONEXISTENT_KEY_XYZ", lFatal=.false._c_bool)

    ! Should still allocate (with NA values) rather than crash
    call check(error, allocated(values), &
               "missing key with lFatal=FALSE should still allocate values array")
  end subroutine test_get_missing_key_nonfatal

  !---------------------------------------------------------------------------
  ! DUPLICATE COLUMN VERIFICATION
  !---------------------------------------------------------------------------

  !> @brief Loading a second file with matching LU_Code should succeed.
  subroutine test_duplicate_lu_code_matching(error)
    type(error_type), allocatable, intent(out) :: error
    type(PARAMETERS_T) :: test_params
    real(c_float), allocatable :: irr_rate(:)

    call setup_common()
    call test_params%add_file("../test_data/tables/Lookup__crop_coefficient_test.txt")
    call test_params%add_file("../test_data/tables/irrigation_test_matching.txt")
    call test_params%munge_file()

    ! If we get here without a fatal error, the LU_Code matched
    call test_params%get_parameters(irr_rate, sKey="Max_irrigation_rate")
    call check(error, allocated(irr_rate), &
               "should successfully load irrigation data when LU_Code matches")
    if (allocated(error)) return
    call check(error, size(irr_rate) == 9, &
               "irrigation table should have 9 entries")
  end subroutine test_duplicate_lu_code_matching

end module test_parameters
