module test_exceptions

  use exceptions
  use fruit
  use iso_c_binding, only :c_int, c_float, c_double, c_bool
  implicit none

contains

!-------------------------------------------------------------------------------

  subroutine test_check_array_bounds_1d

    integer (c_int) :: integer_values(615)
    real (c_float)  :: float_values(23)
    real (c_double) :: double_values(42)

    logical, parameter :: TRUE =.true.
    logical, parameter :: FALSE =.false.

    call assert_equals( TRUE, logical(index_values_valid(integer_values, 25)) )

  end subroutine test_check_array_bounds_1d

end module test_exceptions
