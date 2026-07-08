module test_exceptions
  !! Unit tests for the exceptions module.
  !!
  !! Tests the index_values_valid bounds-checking function for integer,
  !! float, and double arrays.

  use iso_c_binding, only: c_int, c_float, c_double
  use testdrive, only: check, error_type, new_unittest, unittest_type
  use exceptions, only: index_values_valid
  implicit none
  private

  public :: collect_exceptions

contains

  !---------------------------------------------------------------------------
  !> @brief Register all exceptions tests into the test suite array.
  !---------------------------------------------------------------------------
  subroutine collect_exceptions(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      new_unittest("integer_valid_index", test_integer_valid_index), &
      new_unittest("integer_invalid_index", test_integer_invalid_index), &
      new_unittest("float_valid_index", test_float_valid_index), &
      new_unittest("float_invalid_index", test_float_invalid_index), &
      new_unittest("double_valid_index", test_double_valid_index), &
      new_unittest("double_invalid_index", test_double_invalid_index) &
    ]
  end subroutine collect_exceptions

  !---------------------------------------------------------------------------
  !> @brief Valid index within integer array bounds returns true.
  !---------------------------------------------------------------------------
  subroutine test_integer_valid_index(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: integer_values(615)

    call check(error, logical(index_values_valid(integer_values, 25)), &
               "index 25 should be valid for array of size 615")
  end subroutine test_integer_valid_index

  !---------------------------------------------------------------------------
  !> @brief Index beyond integer array bounds returns false.
  !---------------------------------------------------------------------------
  subroutine test_integer_invalid_index(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: integer_values(615)

    call check(error, .not. logical(index_values_valid(integer_values, 616)), &
               "index 616 should be invalid for array of size 615")
  end subroutine test_integer_invalid_index

  !---------------------------------------------------------------------------
  !> @brief Valid index within float array bounds returns true.
  !---------------------------------------------------------------------------
  subroutine test_float_valid_index(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: float_values(23)

    call check(error, logical(index_values_valid(float_values, 23)), &
               "index 23 should be valid for array of size 23")
  end subroutine test_float_valid_index

  !---------------------------------------------------------------------------
  !> @brief Index beyond float array bounds returns false.
  !---------------------------------------------------------------------------
  subroutine test_float_invalid_index(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: float_values(23)

    call check(error, .not. logical(index_values_valid(float_values, 24)), &
               "index 24 should be invalid for array of size 23")
  end subroutine test_float_invalid_index

  !---------------------------------------------------------------------------
  !> @brief Valid index within double array bounds returns true.
  !---------------------------------------------------------------------------
  subroutine test_double_valid_index(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: double_values(42)

    call check(error, logical(index_values_valid(double_values, 42)), &
               "index 42 should be valid for array of size 42")
  end subroutine test_double_valid_index

  !---------------------------------------------------------------------------
  !> @brief Index beyond double array bounds returns false.
  !---------------------------------------------------------------------------
  subroutine test_double_invalid_index(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_double) :: double_values(42)

    call check(error, .not. logical(index_values_valid(double_values, 43)), &
               "index 43 should be invalid for array of size 42")
  end subroutine test_double_invalid_index

end module test_exceptions
