module test_allocatable_string
  !! Unit tests for fstring and fstring_list operations.
  !!
  !! Tests string manipulation (chomp, replace, clean), type conversions
  !! (as_integer, as_float, asCharacter), comparison operators
  !! (.strapprox., .containssimilar.), and FSTRING_LIST_T behavior.

  use iso_c_binding, only: c_int, c_float
  use testdrive, only: check, error_type, new_unittest, unittest_type, test_failed
  use fstring, only: chomp, replace, clean, as_integer, as_float, &
                     asCharacter, as_character, asUppercase, asLowercase, &
                     fieldCount, &
                     operator(.strapprox.), operator(.containssimilar.), &
                     operator(.strequal.), operator(.contains.)
  implicit none
  private

  public :: collect_allocatable_string

  ! Sentinel values matching fstring's internal NA_INT / NA_FLOAT
  integer(c_int), parameter :: NA_INT = -(huge(1_c_int) - 1_c_int)
  real(c_float), parameter  :: NA_FLOAT = -(huge(1.0_c_float) - 1.0_c_float)

contains

  !---------------------------------------------------------------------------
  !> @brief Register all fstring/fstring_list tests.
  !---------------------------------------------------------------------------
  subroutine collect_allocatable_string(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      ! --- chomp ---
      new_unittest("chomp_comma_delimited", test_chomp_comma_delimited), &
      new_unittest("chomp_space_delimited", test_chomp_space_delimited), &
      ! --- replace / clean ---
      new_unittest("replace_characters", test_replace_characters), &
      new_unittest("replace_multiple_occurrences", test_replace_multiple), &
      new_unittest("clean_strips_quotes", test_clean_strips_quotes), &
      ! --- case conversion ---
      new_unittest("uppercase_conversion", test_uppercase_conversion), &
      new_unittest("lowercase_conversion", test_lowercase_conversion), &
      ! --- string comparison operators ---
      new_unittest("strequal_case_sensitive", test_strequal_case_sensitive), &
      new_unittest("strapprox_case_insensitive", test_strapprox_case_insensitive), &
      new_unittest("contains_case_sensitive", test_contains_case_sensitive), &
      new_unittest("containssimilar_case_insensitive", test_containssimilar), &
      ! --- as_integer ---
      new_unittest("as_integer_simple", test_as_integer_simple), &
      new_unittest("as_integer_with_spaces", test_as_integer_with_spaces), &
      new_unittest("as_integer_from_float_string", test_as_integer_from_float), &
      new_unittest("as_integer_invalid_returns_NA", test_as_integer_invalid), &
      ! --- as_float ---
      new_unittest("as_float_simple", test_as_float_simple), &
      new_unittest("as_float_scientific", test_as_float_scientific), &
      new_unittest("as_float_invalid_returns_NA", test_as_float_invalid), &
      ! --- asCharacter ---
      new_unittest("asCharacter_integer", test_asCharacter_integer), &
      new_unittest("asCharacter_negative", test_asCharacter_negative), &
      new_unittest("asCharacter_float", test_asCharacter_float), &
      ! --- fieldCount ---
      new_unittest("fieldCount_comma", test_fieldCount_comma) &
    ]
  end subroutine collect_allocatable_string

  !---------------------------------------------------------------------------
  ! CHOMP TESTS
  !---------------------------------------------------------------------------

  !> @brief chomp splits a comma-delimited string into 4 substrings.
  subroutine test_chomp_comma_delimited(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=256) :: sBuf, sSubString
    integer(c_int) :: column_count

    sBuf = '"Begin at the beginning," the King said, very gravely, ' &
      //'"and go on till you come to the end: then stop."'

    column_count = 0
    do while (len_trim(sBuf) > 0)
      column_count = column_count + 1
      call chomp(sBuf, sSubString, ",")
    end do

    call check(error, column_count == 4, "expected 4 comma-delimited columns")
  end subroutine test_chomp_comma_delimited

  !> @brief chomp with space delimiter splits "hello world foo" into 3 parts.
  subroutine test_chomp_space_delimited(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=256) :: sBuf, sSubString
    integer(c_int) :: column_count

    sBuf = "hello world foo"

    column_count = 0
    do while (len_trim(sBuf) > 0)
      column_count = column_count + 1
      call chomp(sBuf, sSubString, " ")
    end do

    call check(error, column_count == 3, "expected 3 space-delimited columns")
  end subroutine test_chomp_space_delimited

  !---------------------------------------------------------------------------
  ! REPLACE / CLEAN TESTS
  !---------------------------------------------------------------------------

  !> @brief replace substitutes space with underscore.
  subroutine test_replace_characters(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=256) :: str

    str = "hello world"
    call replace(str, " ", "_")

    call check(error, trim(str) == "hello_world", &
               "replace(' ', '_') failed: got '" // trim(str) // "'")
  end subroutine test_replace_characters

  !> @brief replace handles multiple occurrences of the target character.
  subroutine test_replace_multiple(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=256) :: str

    str = "a.b.c.d"
    call replace(str, ".", "/")

    call check(error, trim(str) == "a/b/c/d", &
               "replace('.', '/') failed: got '" // trim(str) // "'")
  end subroutine test_replace_multiple

  !> @brief clean removes specified characters from a string.
  subroutine test_clean_strips_quotes(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=256) :: input_str
    character(len=:), allocatable :: result

    input_str = '"hello"'
    result = clean(input_str, '"')

    call check(error, result == "hello", &
               "clean should strip quotes, got '" // result // "'")
  end subroutine test_clean_strips_quotes

  !---------------------------------------------------------------------------
  ! CASE CONVERSION TESTS
  !---------------------------------------------------------------------------

  !> @brief asUppercase converts mixed case to all uppercase.
  subroutine test_uppercase_conversion(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: result

    result = asUppercase("Hello World 123!")

    call check(error, result == "HELLO WORLD 123!", &
               "asUppercase failed: got '" // result // "'")
  end subroutine test_uppercase_conversion

  !> @brief asLowercase converts mixed case to all lowercase.
  subroutine test_lowercase_conversion(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: result

    result = asLowercase("Hello World 123!")

    call check(error, result == "hello world 123!", &
               "asLowercase failed: got '" // result // "'")
  end subroutine test_lowercase_conversion

  !---------------------------------------------------------------------------
  ! STRING COMPARISON OPERATOR TESTS
  !---------------------------------------------------------------------------

  !> @brief .strequal. is case-sensitive: "Hello" /= "hello".
  subroutine test_strequal_case_sensitive(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, logical("Hello" .strequal. "Hello"), &
               ".strequal. should match identical strings")
    if (allocated(error)) return
    call check(error, .not. logical("Hello" .strequal. "hello"), &
               ".strequal. should NOT match different case")
  end subroutine test_strequal_case_sensitive

  !> @brief .strapprox. is case-insensitive: "GASH" == "gash" == "Gash".
  subroutine test_strapprox_case_insensitive(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, logical("GASH" .strapprox. "gash"), &
               ".strapprox. should match 'GASH' and 'gash'")
    if (allocated(error)) return
    call check(error, logical("Gash" .strapprox. "GASH"), &
               ".strapprox. should match 'Gash' and 'GASH'")
    if (allocated(error)) return
    call check(error, .not. logical("GASH" .strapprox. "BUCKET"), &
               ".strapprox. should NOT match different strings")
  end subroutine test_strapprox_case_insensitive

  !> @brief .contains. is case-sensitive substring check.
  subroutine test_contains_case_sensitive(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, logical("Hello World" .contains. "World"), &
               ".contains. should find 'World' in 'Hello World'")
    if (allocated(error)) return
    call check(error, .not. logical("Hello World" .contains. "world"), &
               ".contains. should NOT find 'world' (wrong case)")
  end subroutine test_contains_case_sensitive

  !> @brief .containssimilar. is case-insensitive substring check.
  subroutine test_containssimilar(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, logical("PHENOLOGY_METHOD" .containssimilar. "phenology"), &
               ".containssimilar. should find 'phenology' in 'PHENOLOGY_METHOD'")
    if (allocated(error)) return
    call check(error, logical("Windows_NT" .containssimilar. "windows"), &
               ".containssimilar. should find 'windows' in 'Windows_NT'")
    if (allocated(error)) return
    call check(error, .not. logical("Hello World" .containssimilar. "xyz"), &
               ".containssimilar. should NOT match absent substring")
  end subroutine test_containssimilar

  !---------------------------------------------------------------------------
  ! AS_INTEGER TESTS
  !---------------------------------------------------------------------------

  !> @brief as_integer parses "42" correctly.
  subroutine test_as_integer_simple(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, as_integer("42") == 42, &
               "as_integer('42') should return 42")
  end subroutine test_as_integer_simple

  !> @brief as_integer handles leading/trailing spaces.
  subroutine test_as_integer_with_spaces(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, as_integer("  123  ") == 123, &
               "as_integer('  123  ') should return 123")
  end subroutine test_as_integer_with_spaces

  !> @brief as_integer truncates a float string "3.7" to 3.
  subroutine test_as_integer_from_float(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, as_integer("3.7") == 3, &
               "as_integer('3.7') should truncate to 3")
  end subroutine test_as_integer_from_float

  !> @brief as_integer returns NA_INT for non-numeric input.
  subroutine test_as_integer_invalid(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, as_integer("abc") == NA_INT, &
               "as_integer('abc') should return NA_INT")
  end subroutine test_as_integer_invalid

  !---------------------------------------------------------------------------
  ! AS_FLOAT TESTS
  !---------------------------------------------------------------------------

  !> @brief as_float parses "3.14" correctly.
  subroutine test_as_float_simple(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, abs(as_float("3.14") - 3.14) < 1.0e-5, &
               "as_float('3.14') should return ~3.14")
  end subroutine test_as_float_simple

  !> @brief as_float parses scientific notation "1.5e-3".
  subroutine test_as_float_scientific(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, abs(as_float("1.5e-3") - 1.5e-3) < 1.0e-8, &
               "as_float('1.5e-3') should return ~0.0015")
  end subroutine test_as_float_scientific

  !> @brief as_float returns NA_FLOAT for non-numeric input.
  subroutine test_as_float_invalid(error)
    type(error_type), allocatable, intent(out) :: error

    call check(error, as_float("hello") == NA_FLOAT, &
               "as_float('hello') should return NA_FLOAT")
  end subroutine test_as_float_invalid

  !---------------------------------------------------------------------------
  ! ASCHARACTER TESTS
  !---------------------------------------------------------------------------

  !> @brief asCharacter converts integer 42 to "42".
  subroutine test_asCharacter_integer(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: result

    result = asCharacter(42_c_int)

    call check(error, result == "42", &
               "asCharacter(42) should yield '42', got '" // result // "'")
  end subroutine test_asCharacter_integer

  !> @brief asCharacter converts negative integer -7 to "-7".
  subroutine test_asCharacter_negative(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: result

    result = asCharacter(-7_c_int)

    call check(error, result == "-7", &
               "asCharacter(-7) should yield '-7', got '" // result // "'")
  end subroutine test_asCharacter_negative

  !> @brief asCharacter converts float 1.5 to a string containing "1.5".
  subroutine test_asCharacter_float(error)
    type(error_type), allocatable, intent(out) :: error
    character(len=:), allocatable :: result

    result = asCharacter(1.5_c_float)

    ! Float formatting may include trailing zeros; just check it starts right
    call check(error, result(1:3) == "1.5", &
               "asCharacter(1.5) should start with '1.5', got '" // result // "'")
  end subroutine test_asCharacter_float

  !---------------------------------------------------------------------------
  ! FIELDCOUNT TESTS
  !---------------------------------------------------------------------------

  !> @brief fieldCount counts comma-separated fields correctly.
  subroutine test_fieldCount_comma(error)
    type(error_type), allocatable, intent(out) :: error
    integer(c_int) :: nfields

    nfields = fieldCount("one,two,three,four", ",")

    call check(error, nfields == 4, &
               "fieldCount('one,two,three,four', ',') should be 4")
  end subroutine test_fieldCount_comma

end module test_allocatable_string
