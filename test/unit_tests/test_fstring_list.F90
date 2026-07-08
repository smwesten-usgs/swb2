module test_fstring_list
  !! Unit tests for FSTRING_LIST_T (null-delimited string list).
  !!
  !! Tests append, count, get (by index), split/create_list,
  !! get_integer, get_float, missing value detection, and sort.
  !! No environment setup needed — FSTRING_LIST_T is self-contained.

  use iso_c_binding, only: c_int, c_float, c_double, c_bool
  use testdrive, only: check, error_type, new_unittest, unittest_type, test_failed
  use fstring_list, only: FSTRING_LIST_T, split, create_list, NA_INT, NA_FLOAT
  implicit none
  private

  public :: collect_fstring_list

contains

  !---------------------------------------------------------------------------
  !> @brief Register all fstring_list tests.
  !---------------------------------------------------------------------------
  subroutine collect_fstring_list(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      ! --- append and count ---
      new_unittest("append_single_item", test_append_single_item), &
      new_unittest("append_multiple_items", test_append_multiple_items), &
      new_unittest("append_empty_string", test_append_empty_string), &
      ! --- get (retrieve by index) ---
      new_unittest("get_first_item", test_get_first_item), &
      new_unittest("get_last_item", test_get_last_item), &
      new_unittest("get_middle_item", test_get_middle_item), &
      ! --- split / create_list ---
      new_unittest("split_comma_default", test_split_comma_default), &
      new_unittest("split_custom_delimiter", test_split_custom_delimiter), &
      new_unittest("split_no_delimiter_found", test_split_no_delimiter_found), &
      ! --- get_integer ---
      new_unittest("get_integer_simple", test_get_integer_simple), &
      new_unittest("get_integer_with_missing", test_get_integer_with_missing), &
      ! --- get_float ---
      new_unittest("get_float_simple", test_get_float_simple), &
      new_unittest("get_float_scientific", test_get_float_scientific), &
      ! --- missing value detection ---
      new_unittest("empty_entries_none", test_empty_entries_none), &
      new_unittest("empty_entries_present", test_empty_entries_present), &
      ! --- count_matching ---
      new_unittest("count_matching_found", test_count_matching_found), &
      new_unittest("count_matching_not_found", test_count_matching_not_found), &
      ! --- clear ---
      new_unittest("clear_resets_list", test_clear_resets_list), &
      ! --- sort ---
      new_unittest("sort_integer_ascending", test_sort_integer_ascending) &
    ]
  end subroutine collect_fstring_list

  !---------------------------------------------------------------------------
  ! APPEND AND COUNT TESTS
  !---------------------------------------------------------------------------

  !> @brief Appending one item gives count of 1.
  subroutine test_append_single_item(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list

    call list%append("hello")

    call check(error, list%count == 1, "count should be 1 after one append")
  end subroutine test_append_single_item

  !> @brief Appending three items gives count of 3.
  subroutine test_append_multiple_items(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list

    call list%append("one")
    call list%append("two")
    call list%append("three")

    call check(error, list%count == 3, "count should be 3 after three appends")
  end subroutine test_append_multiple_items

  !> @brief Appending an empty string increments count and missing_value_count.
  subroutine test_append_empty_string(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list

    call list%append("hello")
    call list%append("")
    call list%append("world")

    call check(error, list%count == 3, "count should be 3")
    if (allocated(error)) return
    call check(error, list%missing_value_count == 1, &
               "missing_value_count should be 1 after appending empty string")
  end subroutine test_append_empty_string

  !---------------------------------------------------------------------------
  ! GET (RETRIEVE BY INDEX) TESTS
  !---------------------------------------------------------------------------

  !> @brief get(1) returns the first appended item.
  subroutine test_get_first_item(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list
    character(len=:), allocatable :: result

    call list%append("alpha")
    call list%append("beta")
    call list%append("gamma")

    result = list%get(1)
    call check(error, trim(result) == "alpha", &
               "get(1) should return 'alpha', got '" // trim(result) // "'")
  end subroutine test_get_first_item

  !> @brief get(3) returns the last appended item.
  subroutine test_get_last_item(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list
    character(len=:), allocatable :: result

    call list%append("alpha")
    call list%append("beta")
    call list%append("gamma")

    result = list%get(3)
    call check(error, trim(result) == "gamma", &
               "get(3) should return 'gamma', got '" // trim(result) // "'")
  end subroutine test_get_last_item

  !> @brief get(2) returns the middle item.
  subroutine test_get_middle_item(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list
    character(len=:), allocatable :: result

    call list%append("alpha")
    call list%append("beta")
    call list%append("gamma")

    result = list%get(2)
    call check(error, trim(result) == "beta", &
               "get(2) should return 'beta', got '" // trim(result) // "'")
  end subroutine test_get_middle_item

  !---------------------------------------------------------------------------
  ! SPLIT / CREATE_LIST TESTS
  !---------------------------------------------------------------------------

  !> @brief split with default comma delimiter.
  subroutine test_split_comma_default(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list
    character(len=:), allocatable :: item

    list = split("one,two,three")

    call check(error, list%count == 3, "split('one,two,three') should give count=3")
    if (allocated(error)) return
    item = list%get(1)
    call check(error, trim(item) == "one", &
               "first item should be 'one', got '" // trim(item) // "'")
    if (allocated(error)) return
    item = list%get(3)
    call check(error, trim(item) == "three", &
               "third item should be 'three', got '" // trim(item) // "'")
  end subroutine test_split_comma_default

  !> @brief split with custom delimiter (tab).
  subroutine test_split_custom_delimiter(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list

    list = split("a|b|c|d", "|")

    call check(error, list%count == 4, "split('a|b|c|d', '|') should give count=4")
  end subroutine test_split_custom_delimiter

  !> @brief split with no delimiter present returns... what?
  subroutine test_split_no_delimiter_found(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list

    list = split("no_commas_here")

    ! When no delimiter is found, the list should have 0 items
    ! (based on the implementation: num_delimiters=0 → empty else branch)
    call check(error, list%count == 0, &
               "split with no delimiter found should give count=0")
  end subroutine test_split_no_delimiter_found

  !---------------------------------------------------------------------------
  ! GET_INTEGER TESTS
  !---------------------------------------------------------------------------

  !> @brief get_integer returns correct integer array.
  subroutine test_get_integer_simple(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list
    integer(c_int), allocatable :: values(:)

    call list%append("10")
    call list%append("20")
    call list%append("30")

    values = list%get_integer()

    call check(error, size(values) == 3, "get_integer should return 3 values")
    if (allocated(error)) return
    call check(error, values(1) == 10, "first value should be 10")
    if (allocated(error)) return
    call check(error, values(2) == 20, "second value should be 20")
    if (allocated(error)) return
    call check(error, values(3) == 30, "third value should be 30")
  end subroutine test_get_integer_simple

  !> @brief get_integer returns NA_INT for non-numeric entries.
  subroutine test_get_integer_with_missing(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list
    integer(c_int), allocatable :: values(:)

    call list%append("5")
    call list%append("abc")
    call list%append("15")

    values = list%get_integer()

    call check(error, values(1) == 5, "first value should be 5")
    if (allocated(error)) return
    call check(error, values(2) == NA_INT, "second value should be NA_INT for 'abc'")
    if (allocated(error)) return
    call check(error, values(3) == 15, "third value should be 15")
  end subroutine test_get_integer_with_missing

  !---------------------------------------------------------------------------
  ! GET_FLOAT TESTS
  !---------------------------------------------------------------------------

  !> @brief get_float returns correct float array.
  subroutine test_get_float_simple(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list
    real(c_float), allocatable :: values(:)

    call list%append("1.5")
    call list%append("2.7")
    call list%append("0.001")

    values = list%get_float()

    call check(error, size(values) == 3, "get_float should return 3 values")
    if (allocated(error)) return
    call check(error, abs(values(1) - 1.5) < 1.0e-5, "first value should be 1.5")
    if (allocated(error)) return
    call check(error, abs(values(2) - 2.7) < 1.0e-5, "second value should be 2.7")
    if (allocated(error)) return
    call check(error, abs(values(3) - 0.001) < 1.0e-5, "third value should be 0.001")
  end subroutine test_get_float_simple

  !> @brief get_float handles scientific notation (after our as_float fix).
  subroutine test_get_float_scientific(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list
    real(c_float), allocatable :: values(:)

    call list%append("1.5e-3")
    call list%append("2.0E2")

    values = list%get_float()

    call check(error, abs(values(1) - 1.5e-3) < 1.0e-7, &
               "get_float should parse '1.5e-3' correctly")
    if (allocated(error)) return
    call check(error, abs(values(2) - 200.0) < 1.0e-3, &
               "get_float should parse '2.0E2' correctly")
  end subroutine test_get_float_scientific

  !---------------------------------------------------------------------------
  ! MISSING VALUE DETECTION TESTS
  !---------------------------------------------------------------------------

  !> @brief No empty entries → empty_entries_present returns false.
  subroutine test_empty_entries_none(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list

    call list%append("one")
    call list%append("two")

    call check(error, .not. logical(list%empty_entries_present()), &
               "empty_entries_present should be false with no empty strings")
  end subroutine test_empty_entries_none

  !> @brief Empty entry present → empty_entries_present returns true.
  subroutine test_empty_entries_present(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list

    call list%append("one")
    call list%append("")
    call list%append("three")

    call check(error, logical(list%empty_entries_present()), &
               "empty_entries_present should be true when empty string appended")
  end subroutine test_empty_entries_present

  !---------------------------------------------------------------------------
  ! COUNT_MATCHING TESTS
  !---------------------------------------------------------------------------

  !> @brief count_matching finds the correct number of matching entries.
  subroutine test_count_matching_found(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list
    integer(c_int) :: nmatches

    call list%append("apple")
    call list%append("banana")
    call list%append("apple")
    call list%append("cherry")

    nmatches = list%count_matching("apple")

    call check(error, nmatches == 2, "count_matching('apple') should find 2")
  end subroutine test_count_matching_found

  !> @brief count_matching returns 0 when no match exists.
  subroutine test_count_matching_not_found(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list
    integer(c_int) :: nmatches

    call list%append("apple")
    call list%append("banana")

    nmatches = list%count_matching("grape")

    call check(error, nmatches == 0, "count_matching('grape') should find 0")
  end subroutine test_count_matching_not_found

  !---------------------------------------------------------------------------
  ! CLEAR TESTS
  !---------------------------------------------------------------------------

  !> @brief clear resets the list to empty state.
  subroutine test_clear_resets_list(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list

    call list%append("one")
    call list%append("two")
    call list%append("three")
    call list%clear()

    call check(error, list%count == 0, "count should be 0 after clear")
  end subroutine test_clear_resets_list

  !---------------------------------------------------------------------------
  ! SORT TESTS
  !---------------------------------------------------------------------------

  !> @brief sort_integer sorts numeric string entries in ascending order.
  subroutine test_sort_integer_ascending(error)
    type(error_type), allocatable, intent(out) :: error
    type(FSTRING_LIST_T) :: list
    integer(c_int), allocatable :: values(:)

    call list%append("30")
    call list%append("10")
    call list%append("20")

    call list%sort_integer()
    values = list%get_integer()

    call check(error, values(1) == 10, "after sort, first should be 10")
    if (allocated(error)) return
    call check(error, values(2) == 20, "after sort, second should be 20")
    if (allocated(error)) return
    call check(error, values(3) == 30, "after sort, third should be 30")
  end subroutine test_sort_integer_ascending

end module test_fstring_list
