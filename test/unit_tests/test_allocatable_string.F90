module test_allocatable_string
  !! Unit tests for fstring and fstring_list operations.
  !!
  !! Tests chomp (delimiter-based string splitting), replace, clean,
  !! and FSTRING_LIST_T append/count behavior.

  use iso_c_binding, only: c_int
  use testdrive, only: check, error_type, new_unittest, unittest_type
  use fstring_list, only: FSTRING_LIST_T
  use fstring, only: chomp, replace, clean
  implicit none
  private

  public :: collect_allocatable_string

contains

  !---------------------------------------------------------------------------
  !> @brief Register all allocatable string tests into the test suite array.
  !---------------------------------------------------------------------------
  subroutine collect_allocatable_string(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      new_unittest("chomp_comma_delimited", test_chomp_comma_delimited), &
      new_unittest("replace_characters", test_replace_characters), &
      new_unittest("fstring_list_append_count", test_fstring_list_append_count) &
    ]
  end subroutine collect_allocatable_string

  !---------------------------------------------------------------------------
  !> @brief chomp splits a comma-delimited string into the expected number
  !!        of substrings.
  !---------------------------------------------------------------------------
  subroutine test_chomp_comma_delimited(error)
    type(error_type), allocatable, intent(out) :: error

    integer(c_int), parameter :: MAX_STR_LEN = 8096
    character(len=MAX_STR_LEN) :: sBuf, sSubString
    integer(c_int) :: column_count

    sBuf = '"Begin at the beginning," the King said, very gravely, ' &
      //'"and go on till you come to the end: then stop."'

    column_count = 0
    do while (len_trim(sBuf) > 0)
      column_count = column_count + 1
      call chomp(sBuf, sSubString, ",")
    end do

    call check(error, column_count == 4, &
               "expected 4 comma-delimited columns, got " // char(ichar('0') + column_count))
  end subroutine test_chomp_comma_delimited

  !---------------------------------------------------------------------------
  !> @brief replace substitutes target characters correctly.
  !---------------------------------------------------------------------------
  subroutine test_replace_characters(error)
    type(error_type), allocatable, intent(out) :: error

    integer(c_int), parameter :: MAX_STR_LEN = 256
    character(len=MAX_STR_LEN) :: str

    str = "hello world"
    call replace(str, " ", "_")

    call check(error, trim(str) == "hello_world", &
               "replace(' ', '_') failed: got '" // trim(str) // "'")
  end subroutine test_replace_characters

  !---------------------------------------------------------------------------
  !> @brief Appending items to FSTRING_LIST_T tracks the correct count.
  !---------------------------------------------------------------------------
  subroutine test_fstring_list_append_count(error)
    type(error_type), allocatable, intent(out) :: error

    integer(c_int), parameter :: MAX_STR_LEN = 8096
    character(len=MAX_STR_LEN) :: sBuf, sSubString, sSubStringClean
    type(FSTRING_LIST_T) :: stList
    integer(c_int) :: column_count

    sBuf = '"Begin at the beginning," the King said, very gravely, ' &
      //'"and go on till you come to the end: then stop."'

    column_count = 0
    do while (len_trim(sBuf) > 0)
      column_count = column_count + 1
      call chomp(sBuf, sSubString, ",")
      call replace(sSubString, " ", "_")
      call replace(sSubString, ".", "_")
      sSubStringClean = trim(clean(sSubString, '"'))
      call stList%append(trim(adjustl(sSubStringClean)))
    end do

    call check(error, stList%count_entries() == 4, &
               "expected 4 items in list after append loop")
  end subroutine test_fstring_list_append_count

end module test_allocatable_string
