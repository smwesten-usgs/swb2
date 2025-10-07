module fstring

  use iso_c_binding, only : c_int, c_long_long, c_float, c_double, c_bool,     &
                            c_short, c_null_char
  implicit none

  private

  public :: operator(+)
  interface operator(+)
    procedure :: concatenate_char_char_fn
    procedure :: concatenate_char_int_fn
    procedure :: concatenate_char_float_fn
    procedure :: concatenate_char_double_fn
  end interface operator(+)

  !   interface assignment(=)
  !     procedure :: integer_to_char_sub
  !     procedure :: float_to_char_sub
  !     procedure :: double_to_char_sub
  !   end interface assignment(=)

  public :: operator( .strequal. )
  interface operator( .strequal. )
    procedure :: is_char_equal_to_char_case_sensitive_fn
  end interface operator( .strequal. )

  public :: operator( .strapprox. )
  interface operator( .strapprox. )
    procedure :: is_char_equal_to_char_case_insensitive_fn
  end interface operator( .strapprox. )

  public :: operator( .contains. )
  interface operator( .contains. )
    procedure :: is_string2_present_in_string1_case_sensitive_fn
  end interface operator( .contains. )

  public :: operator( .containssimilar. )
  interface operator( .containssimilar. )
    procedure :: is_string2_present_in_string1_case_insensitive_fn
  end interface operator( .containssimilar. )

  public :: asCharacter
  interface asCharacter
    procedure :: short_to_char_fn
    procedure :: int_to_char_fn
    procedure :: long_long_to_char_fn
    procedure :: float_to_char_fn
    procedure :: double_to_char_fn
    procedure :: bool_to_char_fn
  end interface asCharacter


  public :: as_character
  interface as_character
    procedure :: short_to_char_fn
    procedure :: int_to_char_fn
    procedure :: long_long_to_char_fn
    procedure :: float_to_char_fn
    procedure :: double_to_char_fn
    procedure :: bool_to_char_fn
  end interface as_character

  public :: as_integer
  interface as_integer
    procedure :: string_to_integer_fn
  end interface as_integer

  public :: as_float
  interface as_float
    procedure :: string_to_float_fn
  end interface as_float

  public :: chomp
  interface chomp
    procedure :: split_and_return_text_sub
  end interface chomp

  public :: fieldCount
  interface fieldCount
    procedure :: count_number_of_fields_fn
  end interface fieldCount

  public :: clean
  interface clean
    procedure :: remove_multiple_characters_fn
  end interface clean

  public :: squote
  interface squote
    procedure :: squote_char_fn
  end interface squote

  public :: dquote
  interface dquote
    procedure :: dquote_char_fn
  end interface dquote

  public :: replace
  interface replace
    procedure :: replace_character_sub
  end interface replace

  public :: asUppercase
  interface asUppercase
    procedure :: char_to_uppercase_fn
  end interface asUppercase

  public :: asLowercase
  interface asLowercase
    procedure :: char_to_lowercase_fn
  end interface asLowercase

  public :: toUppercase
  interface toUppercase
    procedure :: char_to_uppercase_sub
  end interface toUppercase

  public :: toLowercase
  interface toLowercase
    procedure :: char_to_lowercase_sub
  end interface toLowercase

  public :: as_uppercase
  interface as_uppercase
    procedure :: char_to_uppercase_fn
  end interface as_uppercase

  public :: as_lowercase
  interface as_lowercase
    procedure :: char_to_lowercase_fn
  end interface as_lowercase

  public :: to_uppercase
  interface to_uppercase
    procedure :: char_to_uppercase_sub
  end interface to_uppercase

  public :: to_lowercase
  interface to_lowercase
    procedure :: char_to_lowercase_sub
  end interface to_lowercase

  public :: right
  interface right
    procedure :: return_right_part_of_string_fn
  end interface right

  public :: left
  interface left
    procedure :: return_left_part_of_string_fn
  end interface left

  interface strip_full_pathname
    procedure :: strip_full_pathname_fn
  end interface strip_full_pathname

  public :: f_to_c_str
  interface f_to_c_str
    procedure :: f_to_c_string_fn
  end interface f_to_c_str

  public :: c_to_f_str
  interface c_to_f_str
    procedure :: c_to_f_string_fn
  end interface c_to_f_str

  ! [ special ASCII characters ]
  public :: TAB, WHITESPACE, BACKSLASH, FORWARDSLASH, CARRIAGE_RETURN, COMMENT_CHARACTERS, &
            PUNCTUATION, DOUBLE_QUOTE
  character (len=1), parameter :: TAB = achar(9)
  character (len=2), parameter :: WHITESPACE = " "//achar(9)
  character (len=1), parameter :: BACKSLASH = achar(92)
  character (len=1), parameter :: FORWARDSLASH = achar(47)
  character (len=1), parameter :: CARRIAGE_RETURN = achar(13)
  character (len=3), parameter :: COMMENT_CHARACTERS = "#!%"
  character (len=1), parameter :: DOUBLE_QUOTE = achar(34)
  character (len=3), parameter :: PUNCTUATION = ",;:"

  private :: NA_INT, NA_FLOAT, NA_DOUBLE
  integer (c_int), parameter  :: NA_INT    = - (huge(1_c_int)-1_c_int)
  real (c_float), parameter   :: NA_FLOAT  = - (huge(1._c_float)-1._c_float)
  real (c_double), parameter  :: NA_DOUBLE = - (huge(1._c_double)-1._c_double)

contains

  ! remove file path from a filename
  function strip_full_pathname_fn( filename )   result(value)

    character (len=*), intent(in)   :: filename
    character (len=:), allocatable  :: value

    if (filename  .contains. "/") value = right( value, substring="/")
    if (filename  .contains. "\") value = right( value, substring="\")

  end function strip_full_pathname_fn  


  impure elemental function string_to_integer_fn(text)  result(value)

    character (len=*), intent(in) :: text
    integer (c_int) :: value

    ! [ LOCALS ]
    integer (c_int)                :: op_status
    character (len=:), allocatable :: temp_str
    real (c_float)                 :: float_value

    temp_str = keepnumeric(text)

    ! if the cleaned up string appears to be a real value,
    ! attempt to read as real and convert to int
    if ( scan(temp_str, ".") /= 0 ) then

      read(unit=temp_str, fmt=*, iostat=op_status) float_value
      if (op_status == 0)  value = int(float_value, c_int)

    else

      read(unit=temp_str, fmt=*, iostat=op_status) value

    endif

    if (op_status /= 0)  value = NA_INT

  end function string_to_integer_fn

!--------------------------------------------------------------------------------------------------

  impure elemental function string_to_float_fn(text)  result(value)

    character (len=*), intent(in) :: text
    real (c_float)                :: value

    ! [ LOCALS ]
    integer (c_int)                :: op_status
    character (len=:), allocatable :: temp_str

    temp_str = keepnumeric(text)

    read(unit=temp_str, fmt=*, iostat=op_status) value
    if (op_status /= 0)  value = NA_FLOAT

  end function string_to_float_fn

!--------------------------------------------------------------------------------------------------

  function return_left_part_of_string_fn( string, indx, substring )   result( left_part )

    character (len=*), intent(in)              :: string
    integer (c_int), intent(in), optional      :: indx
    character (len=*), intent(in), optional    :: substring
    character (len=:), allocatable             :: left_part

    ! [ LOCALS ]
    integer (c_int) :: position

    if ( present( indx ) ) then

      if ( ( indx > 0 ) .and. ( indx < len_trim( string ) ) ) then

        left_part = string( 1:indx )

      else

        left_part = "<NA>"

      endif

    elseif ( present( substring ) ) then

      position = index( string, substring )

      if ( position > 0 ) then

        left_part = string( 1:(position-1) )

      else

        left_part = "<NA>"

      endif

    else

      left_part = "<NA>"

    endif


  end function return_left_part_of_string_fn

!--------------------------------------------------------------------------------------------------

  function return_right_part_of_string_fn( string, indx, substring )   result( right_part )

    character (len=*), intent(in)              :: string
    integer (c_int), intent(in), optional      :: indx
    character (len=*), intent(in), optional    :: substring
    character (len=:), allocatable             :: right_part

    ! [ LOCALS ]
    integer (c_int) :: position

    if ( present( indx ) ) then

      if ( ( indx > 0 ) .and. ( indx < len_trim( string ) ) ) then

        right_part = string( (indx+1):len_trim(string) )

      else

        right_part = "<NA>"

      endif

    elseif ( present( substring ) ) then

      position = index( string, substring, back=.TRUE._c_bool )

      if ( position > 0 ) then

        right_part = string( (position+1):len_trim(string) )

      else

        right_part = "<NA>"

      endif

    else

      right_part = "<NA>"

    endif


  end function return_right_part_of_string_fn

  !--------------------------------------------------------------------------------------------------

  function is_string2_present_in_string1_case_insensitive_fn(sText1, sText2)   result(lBool)

    character (len=*), intent(in)      :: sText1
    character (len=*), intent(in)      :: sText2
    logical (c_bool)              :: lBool

    ! [ LOCALS ]
    character (len=len_trim(sText1))  :: sTemp1
    character (len=len_trim(sText2))  :: sTemp2

    lBool = .FALSE._c_bool

    sTemp1 = asUppercase(sText1)
    sTemp2 = asUppercase(sText2)

    if ( index(sTemp1, sTemp2) /= 0 ) lBool = .TRUE._c_bool

  end function is_string2_present_in_string1_case_insensitive_fn

  !--------------------------------------------------------------------------------------------------

  function is_string2_present_in_string1_case_sensitive_fn(sText1, sText2)   result(lBool)

    character (len=*), intent(in)      :: sText1
    character (len=*), intent(in)      :: sText2
    logical (c_bool)              :: lBool

    ! [ LOCALS ]
    character (len=len_trim(sText1))  :: sTemp1
    character (len=len_trim(sText2))  :: sTemp2

    lBool = .FALSE._c_bool

    sTemp1 = trim( sText1 )
    sTemp2 = trim( sText2 )

    if ( index(sTemp1, sTemp2) /= 0 ) lBool = .TRUE._c_bool

  end function is_string2_present_in_string1_case_sensitive_fn

  !--------------------------------------------------------------------------------------------------

   function is_char_equal_to_char_case_sensitive_fn(sText1, sText2)   result(lBool)

    character (len=*), intent(in)      :: sText1
    character (len=*), intent(in)      :: sText2
    logical (c_bool)              :: lBool

    ! [ LOCALS ]
    character (len=:), allocatable    :: sTemp1
    character (len=:), allocatable    :: sTemp2

    lBool = .FALSE._c_bool

    sTemp1 = trim( sText1 )
    sTemp2 = trim( sText2 )

    if (trim(adjustl( sTemp1 ) )  .eq. trim(adjustl( sTemp2) ) ) lBool = .TRUE._c_bool

  end function is_char_equal_to_char_case_sensitive_fn

  !--------------------------------------------------------------------------------------------------

   function is_char_equal_to_char_case_insensitive_fn(sText1, sText2)   result(lBool)

    character (len=*), intent(in)      :: sText1
    character (len=*), intent(in)      :: sText2
    logical (c_bool)              :: lBool

    ! [ LOCALS ]
    character (len=:), allocatable    :: sTemp1
    character (len=:), allocatable    :: sTemp2

    lBool = .FALSE._c_bool

    sTemp1 = asUppercase( sText1 )
    sTemp2 = asUppercase( sText2 )

    if (trim(adjustl( sTemp1 ) )  .eq. trim(adjustl( sTemp2) ) ) lBool = .TRUE._c_bool

  end function is_char_equal_to_char_case_insensitive_fn

  !--------------------------------------------------------------------------------------------------

  function concatenate_char_char_fn(sText1, sText2)   result(sText)

    character (len=*), intent(in)      :: sText1
    character (len=*), intent(in)      :: sText2
    character (len=:), allocatable     :: sText

    sText = sText1 // sText2

  end function concatenate_char_char_fn

  !--------------------------------------------------------------------------------------------------

  function concatenate_char_int_fn(sText1, iValue1)   result(sText)

    character (len=*), intent(in)        :: sText1
    integer (c_int), intent(in)     :: iValue1
    character (len=:), allocatable       :: sText

    sText = sText1 // asCharacter( iValue1 )

  end function concatenate_char_int_fn

  !--------------------------------------------------------------------------------------------------

  function concatenate_char_float_fn(sText1, fValue1)   result(sText)

    character (len=*), intent(in)        :: sText1
    real (c_float), intent(in)      :: fValue1
    character (len=:), allocatable       :: sText

    sText = sText1 // asCharacter( fValue1 )

  end function concatenate_char_float_fn

  !--------------------------------------------------------------------------------------------------

  function concatenate_char_double_fn(sText1, dValue1)   result(sText)

    character (len=*), intent(in)        :: sText1
    real (c_double), intent(in)     :: dValue1
    character (len=:), allocatable       :: sText

    sText = sText1 // asCharacter( dValue1 )

  end function concatenate_char_double_fn

  !--------------------------------------------------------------------------------------------------
  function short_to_char_fn(value, fmt_string)    result(text)
    integer (c_short), intent(in)            :: value
    character (len=*), intent(in), optional  :: fmt_string
    character (len=:), allocatable           :: text

    integer (c_int)      :: status
    character (len=32)   :: sbuf

    if ( present(fmt_string) ) then
      write(sbuf, fmt="("//trim(fmt_string)//")", iostat=status)  value
    else
      write(sbuf, fmt=*, iostat=status)  value
    endif

    if (status==0) then
      text = trim( adjustl(sbuf) )
    else
      text = "<NA>"
    endif

  end function short_to_char_fn

!--------------------------------------------------------------------------------------------------

  function int_to_char_fn(value, fmt_string)    result(text)
    integer (c_int), intent(in)              :: value
    character (len=*), intent(in), optional  :: fmt_string
    character (len=:), allocatable           :: text

    integer (c_int)      :: status
    character (len=32)   :: sbuf

    if ( present(fmt_string) ) then
      write(sbuf, fmt="("//trim(fmt_string)//")", iostat=status)  value
    else
      write(sbuf, fmt=*, iostat=status)  value
    endif

    if (status==0) then
      text = trim( adjustl(sbuf) )
    else
      text = "<NA>"
    endif

  end function int_to_char_fn

!--------------------------------------------------------------------------------------------------

  function long_long_to_char_fn(value, fmt_string)    result(text)
    integer (c_long_long), intent(in)           :: value
    character (len=*), intent(in), optional  :: fmt_string
    character (len=:), allocatable           :: text

    integer (c_int)      :: status
    character (len=32)   :: sbuf

    if ( present(fmt_string) ) then
      write(sbuf, fmt="("//trim(fmt_string)//")", iostat=status)  value
    else
      write(sbuf, fmt=*, iostat=status)  value
    endif

    if (status==0) then
      text = trim( adjustl(sbuf) )
    else
      text = "<NA>"
    endif

  end function long_long_to_char_fn

!--------------------------------------------------------------------------------------------------

  function float_to_char_fn(value, fmt_string)    result(text)
    real (c_float), intent(in)               :: value
    character (len=*), intent(in), optional  :: fmt_string
    character (len=:), allocatable           :: text

    integer (c_int)      :: status
    character (len=32)   :: sbuf

    if ( present(fmt_string) ) then
      write(sbuf, fmt="("//trim(fmt_string)//")", iostat=status)  value
    else
      write(sbuf, fmt=*, iostat=status)  value
    endif

    if (status==0) then
      text = trim( adjustl(sbuf) )
    else
      text = "<NA>"
    endif

  end function float_to_char_fn

!--------------------------------------------------------------------------------------------------

  function double_to_char_fn(value, fmt_string)    result(text)
    real (c_double), intent(in)              :: value
    character (len=*), intent(in), optional  :: fmt_string
    character (len=:), allocatable           :: text

    integer (c_int)      :: status
    character (len=32)   :: sbuf

    if ( present(fmt_string) ) then
      write(sbuf, fmt="("//trim(fmt_string)//")", iostat=status)  value
    else
      write(sbuf, fmt=*, iostat=status)  value
    endif

    if (status==0) then
      text = trim( adjustl(sbuf) )
    else
      text = "<NA>"
    endif

  end function double_to_char_fn

!--------------------------------------------------------------------------------------------------

  function bool_to_char_fn(value)    result(text)
    logical (c_bool), intent(in)    :: value
    character (len=:), allocatable  :: text

    integer (c_int)      :: status
    character (len=32)   :: sbuf

    if ( value ) then
      text = ".TRUE._c_bool"
    else
      text = "False"
    endif

  end function bool_to_char_fn

!--------------------------------------------------------------------------------------------------

   function squote_char_fn(sText1)    result(sText)

    character (len=*), intent(in)         :: sText1
    character (len=:), allocatable        :: sText

    sText = "'"//trim(sText1)//"'"

  end function squote_char_fn

!--------------------------------------------------------------------------------------------------

   function dquote_char_fn(sText1)    result(sText)

    character (len=*), intent(in)         :: sText1
    character (len=:), allocatable        :: sText

    sText = '"'//trim(sText1)//'"'

  end function dquote_char_fn

  !--------------------------------------------------------------------------------------------------

   function char_to_uppercase_fn ( s )                    result(sText)

    ! ARGUMENTS
    character (len=*), intent(in) :: s
    character(len=len(s))         :: sText

    ! LOCALS
    integer (c_int) :: i    ! do loop index

    ! CONSTANTS
    integer (c_int), parameter :: LOWER_TO_UPPER = -32
    integer (c_int), parameter :: ASCII_SMALL_A = ichar("a")
    integer (c_int), parameter :: ASCII_SMALL_Z = ichar("z")

    sText = s

    do i=1,len_trim(sText)
      if ( ichar(sText(i:i) ) >= ASCII_SMALL_A .and. ichar(sText(i:i)) <= ASCII_SMALL_Z ) then
        sText(i:i) = char( ichar( sText(i:i) ) + LOWER_TO_UPPER )
      end if
    end do

  end function char_to_uppercase_fn

  !--------------------------------------------------------------------------

   function char_to_lowercase_fn ( s )                               result(sText)

    ! ARGUMENTS
    character (len=*), intent(in) :: s
    character(len=len(s)) :: sText

    ! LOCALS
    integer (c_int) :: i    ! do loop index
    ! CONSTANTS
    integer (c_int), parameter :: UPPER_TO_LOWER = 32
    integer (c_int), parameter :: ASCII_A = ichar("A")
    integer (c_int), parameter :: ASCII_Z = ichar("Z")

    sText = s

    do i=1,len_trim(sText)
      if ( ichar(sText(i:i) ) >= ASCII_A .and. ichar(sText(i:i)) <= ASCII_Z ) then
        sText(i:i) = char( ichar( sText(i:i) ) + UPPER_TO_LOWER )
      end if
    end do

  end function char_to_lowercase_fn


   subroutine char_to_uppercase_sub ( s )

    ! ARGUMENTS
    character (len=*), intent(inout) :: s
    ! LOCALS
    integer (c_int) :: i    ! do loop index
    ! CONSTANTS
    integer (c_int), parameter :: LOWER_TO_UPPER = -32
    integer (c_int), parameter :: ASCII_SMALL_A = ichar("a")
    integer (c_int), parameter :: ASCII_SMALL_Z = ichar("z")

    do i=1,len_trim(s)
      if ( ichar(s(i:i) ) >= ASCII_SMALL_A .and. ichar(s(i:i)) <= ASCII_SMALL_Z ) then
        s(i:i) = char( ichar( s(i:i) ) + LOWER_TO_UPPER )
      end if
    end do

  end subroutine char_to_uppercase_sub


   subroutine char_to_lowercase_sub ( s )

    ! ARGUMENTS
    character (len=*), intent(inout) :: s
    ! LOCALS
    integer (c_int) :: i    ! do loop index
    ! CONSTANTS
    integer (c_int), parameter :: UPPER_TO_LOWER = 32
    integer (c_int), parameter :: ASCII_A = ichar("A")
    integer (c_int), parameter :: ASCII_Z = ichar("Z")

    ! UPPER_TO_LOWER = ichar( "a" ) - ichar( "A" )

    do i=1,len_trim( s )
      if ( ichar(s(i:i) ) >= ASCII_A .and. ichar(s(i:i)) <= ASCII_Z ) then
        s(i:i) = char( ichar( s(i:i) ) + UPPER_TO_LOWER )
      end if
    end do

  end subroutine char_to_lowercase_sub





  !--------------------------------------------------------------------------


  !> Strip offending characters from a text string.
  !!
  !! Remove unwanted characters from a text string. The target characters may optionally be supplied.
  !! @param[in] sTextIn
  impure function remove_multiple_characters_fn(sText1, sTargetCharacters)            result(sText)

    ! ARGUMENTS
    character (len=*), intent(inout)           :: sText1
    character (len=*), intent(in), optional    :: sTargetCharacters
    character (len=:), allocatable :: sText

    ! LOCALS
    character (len=512)            :: sBuf
    integer (c_int)           :: iR                 ! Index in sRecord
    integer (c_int)           :: iIndex1, iIndex2
    character (len=:), allocatable :: sTargetCharacters_l

    ! eliminate any leading spaces
    sText1 = adjustl(sText1)
    sBuf = ""
    iIndex2 = 0

    if (present(sTargetCharacters) ) then
      sTargetCharacters_l = sTargetCharacters
    else
      sTargetCharacters_l = ":/;,"
    endif

    do iIndex1 = 1,len_trim(sText1)

      iR = scan(sText1(iIndex1:iIndex1), sTargetCharacters_l)

      if(iR==0) then
        iIndex2 = iIndex2 + 1
        sBuf(iIndex2:iIndex2) = sText1(iIndex1:iIndex1)
      end if

    enddo

    sText = trim(sBuf)

  end function remove_multiple_characters_fn

  !--------------------------------------------------------------------------------------------------

  !> Strip repeated characters from string.
  !!
  !! Remove repeated characters from a string. By default the function looks for repeated spaces and eliminates them.
  !! @param[in] sTextIn
  function remove_repeats(sText1, sChar)            result(sText)

    ! ARGUMENTS
    character (len=*), intent(inout)           :: sText1
    character (len=*), intent(in), optional    :: sChar
    character (len=:), allocatable :: sText

    ! LOCALS
    character (len=256)            :: sBuf
    integer (c_int)           :: iR                 ! Index in sRecord
    integer (c_int)           :: iIndex1, iIndex2
    character (len=1)              :: sChar_l
    logical (c_bool)          :: lPreviouslyFound

    ! eliminate any leading spaces
    sText1 = adjustl(sText1)
    sBuf = ""
    iIndex2 = 0
    lPreviouslyFound = .FALSE._c_bool

    if (present(sChar) ) then
      sChar_l = sChar
    else
      sChar_l = " "
    endif

    do iIndex1 = 1,len_trim(sText1)

      iR = scan(sText1(iIndex1:iIndex1), sChar_l)

      if(iR==0) then
        ! sChar_l was not found
        iIndex2 = iIndex2 + 1
        sBuf(iIndex2:iIndex2) = sText1(iIndex1:iIndex1)
        lPreviouslyFound = .FALSE._c_bool

      elseif( lPreviouslyFound ) then
        ! sChar_l was found, and was also found in the position preceding this one

        ! No OP

      else
        ! sChar_l was found, but was *not* found in the preceding position

        iIndex2 = iIndex2 + 1
        sBuf(iIndex2:iIndex2) = sText1(iIndex1:iIndex1)
        lPreviouslyFound = .TRUE._c_bool

      end if

    enddo

    sText = trim(sBuf)

  end function remove_repeats

  !--------------------------------------------------------------------------------------------------

  function count_number_of_fields_fn( sText, sDelimiters )     result( iCount )

    character (len=*), intent(in)               :: sText
    character (len=*), intent(in), optional     :: sDelimiters
    integer (c_int)                        :: iCount

    ! [ LOCALS ]
    character (len=len(sText))      :: str
    character (len=len(sText))      :: substr
    character (len=:), allocatable  :: delimiter_chr_

    if ( present(sDelimiters) ) then
      delimiter_chr_=sDelimiters
    else
      delimiter_chr_ = WHITESPACE
    endif

    iCount = 0

    str = sText

    do
      call chomp(str=str, substr=substr, delimiter_chr=delimiter_chr_ )

      if ( len_trim( substr ) == 0 )  exit

      iCount = iCount + 1

    enddo

  end function count_number_of_fields_fn

  !--------------------------------------------------------------------------------------------------

  subroutine split_and_return_text_sub(str, substr, delimiter_chr, remove_extra_delimiters)

    character (len=*), intent(inout)                     :: str
    character (len=*), intent(out)                       :: substr
    character (len=*), intent(in), optional              :: delimiter_chr
    logical (c_bool), intent(in), optional               :: remove_extra_delimiters

    ! [ LOCALS ]
    character (len=:), allocatable :: delimiter_chr_
    logical (c_bool)     :: remove_extra_delimiters_
    integer (kind=c_int) :: iIndex
    integer (c_int)      :: n

    if ( present(remove_extra_delimiters)) then
      remove_extra_delimiters_ = remove_extra_delimiters
    else
      remove_extra_delimiters_ = .false._c_bool
    endif

    if ( present(delimiter_chr) ) then
      select case (delimiter_chr)
      case ("WHITESPACE")
        delimiter_chr_ = WHITESPACE
      case ("TAB", "TABS")
        delimiter_chr_ = TAB
      case ("COMMA", "CSV")
        delimiter_chr_ = ","
      case default
        delimiter_chr_ = delimiter_chr
      end select
    else
      delimiter_chr_ = WHITESPACE
    endif

    str = adjustl(str)

    iIndex = scan( string = str, set = delimiter_chr_ )

    if (iIndex == 0) then
      ! no delimiters found; return string as was supplied originally
      substr = str
      str = ""
    else
      ! delimiters were found; split and return the chunks of text
      substr = trim( str(1:iIndex-1) )
      str = trim( str(iIndex + 1: ) )
      ! inelegant, but something like this is needed to detect the presence of duplicate delimiters in cases where
      ! more than one delimiter in a row should just be ignored
      if (remove_extra_delimiters_) then
        do
          n = len_trim(str)
          if (n == 0 ) exit
          ! if we still have delimiters (whitespace, for example) in the first position, lop it off and try again
          if ( scan( string=str(1:1), set=delimiter_chr_) == 0) exit
          str = trim(str(2:n))
        enddo
      endif  
    endif

  end subroutine split_and_return_text_sub

  !--------------------------------------------------------------------------------------------------

  subroutine replace_character_sub(sText1, sFind, sReplace)

    character (len=*), intent(inout)         :: sText1
    character (len=1), intent(in)            :: sFind
    character (len=1), intent(in), optional  :: sReplace

    ! [ LOCALS ]
    integer (c_int)                  :: iIndex
    integer (c_int)                  :: iCount
    character (len=len_trim(sText1)) :: sText

    sText = ""

    if ( len(sText1) > 0 ) then

      iCount = 0
      do iIndex = 1, len_trim(sText1)
        if ( sText1(iIndex:iIndex) .ne. sFind) then
          iCount = iCount + 1
          sText(iCount:iCount) = sText1(iIndex:iIndex)
        else
          if (present(sReplace)) then
            iCount = iCount + 1
            sText(iCount:iCount) = sReplace
          endif
        endif
      enddo

    endif

    sText1 = trim(sText)

  end subroutine replace_character_sub

!--------------------------------------------------------------------------------------------------

  impure elemental function keepnumeric(text)            result(result_text)

    ! ARGUMENTS
    character (len=*), intent(in)         :: text
    character (len=len(text))             :: result_text

    character (len=:), allocatable     :: temp_str
    character (len=512)                :: temp_result

    integer (c_int)                :: n
    integer (c_int)                :: index1, index2
    character (len=:), allocatable :: target_characters

    ! target_character omits the period ("."): do not want a real value returned
    ! as a funky integer (e.g. string "3.141" returned as integer 3141 )
    target_characters = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM" &
      //"!@#$%^&*()_+-={}[]|\:;'<,>?/~`'"//DOUBLE_QUOTE

    temp_str = adjustl(text)
    temp_result = ""
    index2 = 0

    do index1 = 1,len_trim(text)
      n = scan(text(index1:index1), target_characters)
      if(n==0) then
        index2 = index2 + 1
        temp_result(index2:index2) = text(index1:index1)
      endif
    enddo

    result_text = trim(temp_result)

   end function keepnumeric

!--------------------------------------------------------------------------------------------------

  function c_to_f_string_fn(c_character_str)   result(f_character_str)

    character (len=*), intent(in)                    :: c_character_str
    character (len=:), allocatable                   :: f_character_str

    integer (c_int)   :: indx

    f_character_str = c_character_str

    do indx=1,len(c_character_str)
      if (c_character_str(indx:indx) == c_null_char) then
        f_character_str = c_character_str(1:indx-1)
        exit
      endif
    enddo

  end function c_to_f_string_fn

!--------------------------------------------------------------------------------------------------

  function f_to_c_string_fn(f_character_str)   result(c_character_str)

    character (len=*), intent(in)                    :: f_character_str
    character (len=:), allocatable                   :: c_character_str

    integer (c_int) :: str_len

    str_len = len_trim(f_character_str)

    if ( str_len == 0 ) then
      c_character_str = c_null_char
    elseif ( f_character_str(str_len:str_len) == c_null_char ) then
      ! already has a null character at end; do not append another
      c_character_str = trim(f_character_str)
    else
      ! last char is not null character; append c_null_char
      c_character_str = trim(f_character_str)//c_null_char
    endif

  end function f_to_c_string_fn


end module fstring
