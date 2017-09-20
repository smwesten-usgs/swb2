module strings

  use iso_c_binding, only : c_int, c_long_long, c_float, c_double, c_bool, c_short
  use constants_and_conversions
  use exceptions
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
    procedure :: is_string2_present_in_string1_case_insensitive_fn
  end interface operator( .contains. )

  public :: asCharacter
  interface asCharacter
    procedure :: short_to_char_fn
    procedure :: int_to_char_fn
    procedure :: long_long_to_char_fn
    procedure :: float_to_char_fn
    procedure :: double_to_char_fn
    procedure :: logical_to_char_fn
  end interface asCharacter

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

  public :: right
  interface right
    procedure :: return_right_part_of_string_fn
  end interface right

  public :: left
  interface left
    procedure :: return_left_part_of_string_fn
  end interface left

contains

  function return_left_part_of_string_fn( string, indx, substring )   result( left_part )

    character (len=*), intent(in)              :: string
    integer (kind=c_int), intent(in), optional :: indx
    character (len=*), intent(in), optional    :: substring
    character (len=:), allocatable             :: left_part

    ! [ LOCALS ]
    integer (kind=c_int) :: position

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
    integer (kind=c_int), intent(in), optional :: indx
    character (len=*), intent(in), optional    :: substring
    character (len=:), allocatable             :: right_part

    ! [ LOCALS ]
    integer (kind=c_int) :: position

    if ( present( indx ) ) then

      if ( ( indx > 0 ) .and. ( indx < len_trim( string ) ) ) then

        right_part = string( (indx+1):len_trim(string) )

      else

        right_part = "<NA>"

      endif

    elseif ( present( substring ) ) then

      position = index( string, substring, back=TRUE )

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
    logical (kind=c_bool)              :: lBool

    ! [ LOCALS ]
    character (len=len_trim(sText1))  :: sTemp1
    character (len=len_trim(sText2))  :: sTemp2

    lBool = lFALSE

    sTemp1 = asUppercase(sText1)
    sTemp2 = asUppercase(sText2)

    if ( index(sTemp1, sTemp2) /= 0 ) lBool = lTRUE

  end function is_string2_present_in_string1_case_insensitive_fn

  !--------------------------------------------------------------------------------------------------

  function is_string2_present_in_string1_case_sensitive_fn(sText1, sText2)   result(lBool)

    character (len=*), intent(in)      :: sText1
    character (len=*), intent(in)      :: sText2
    logical (kind=c_bool)              :: lBool

    ! [ LOCALS ]
    character (len=len_trim(sText1))  :: sTemp1
    character (len=len_trim(sText2))  :: sTemp2

    lBool = lFALSE

    sTemp1 = trim( sText1 )
    sTemp2 = trim( sText2 )

    if ( index(sTemp1, sTemp2) /= 0 ) lBool = lTRUE

  end function is_string2_present_in_string1_case_sensitive_fn

  !--------------------------------------------------------------------------------------------------

  elemental function is_char_equal_to_char_case_sensitive_fn(sText1, sText2)   result(lBool)

    character (len=*), intent(in)      :: sText1
    character (len=*), intent(in)      :: sText2
    logical (kind=c_bool)              :: lBool

    ! [ LOCALS ]
    character (len=:), allocatable    :: sTemp1
    character (len=:), allocatable    :: sTemp2

    lBool = lFALSE

    sTemp1 = trim( sText1 )
    sTemp2 = trim( sText2 )

    if (trim(adjustl( sTemp1 ) )  .eq. trim(adjustl( sTemp2) ) ) lBool = lTRUE

  end function is_char_equal_to_char_case_sensitive_fn

  !--------------------------------------------------------------------------------------------------

  elemental function is_char_equal_to_char_case_insensitive_fn(sText1, sText2)   result(lBool)

    character (len=*), intent(in)      :: sText1
    character (len=*), intent(in)      :: sText2
    logical (kind=c_bool)              :: lBool

    ! [ LOCALS ]
    character (len=:), allocatable    :: sTemp1
    character (len=:), allocatable    :: sTemp2

    lBool = lFALSE

    sTemp1 = asUppercase( sText1 )
    sTemp2 = asUppercase( sText2 )

    if (trim(adjustl( sTemp1 ) )  .eq. trim(adjustl( sTemp2) ) ) lBool = lTRUE

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
    integer (kind=c_int), intent(in)     :: iValue1
    character (len=:), allocatable       :: sText

    sText = sText1 // asCharacter( iValue1 )

  end function concatenate_char_int_fn

  !--------------------------------------------------------------------------------------------------

  function concatenate_char_float_fn(sText1, fValue1)   result(sText)

    character (len=*), intent(in)        :: sText1
    real (kind=c_float), intent(in)      :: fValue1
    character (len=:), allocatable       :: sText

    sText = sText1 // asCharacter( fValue1 )

  end function concatenate_char_float_fn

  !--------------------------------------------------------------------------------------------------

  function concatenate_char_double_fn(sText1, dValue1)   result(sText)

    character (len=*), intent(in)        :: sText1
    real (kind=c_double), intent(in)     :: dValue1
    character (len=:), allocatable       :: sText

    sText = sText1 // asCharacter( dValue1 )

  end function concatenate_char_double_fn

  !--------------------------------------------------------------------------------------------------

  elemental function short_to_char_fn(iValue)    result(sText)

    integer (kind=c_short), intent(in)  :: iValue
    character (len=:), allocatable    :: sText

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=32)   :: sBuf

    write(sBuf, fmt=*, iostat=iStat)  iValue

    if (iStat==0) then
      sText = trim( adjustl(sBuf) )
    else
      sText = "NA"
    endif

  end function short_to_char_fn

  !--------------------------------------------------------------------------------------------------

  elemental function int_to_char_fn(iValue)    result(sText)

    integer (kind=c_int), intent(in)  :: iValue
    character (len=:), allocatable    :: sText

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=32)   :: sBuf

    write(sBuf, fmt=*, iostat=iStat)  iValue

    if (iStat==0) then
      sText = trim( adjustl(sBuf) )
    else
      sText = "NA"
    endif

  end function int_to_char_fn

  !--------------------------------------------------------------------------------------------------

  elemental function long_long_to_char_fn(iValue)    result(sText)

    integer (kind=c_long_long), intent(in)  :: iValue
    character (len=:), allocatable          :: sText

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=32)   :: sBuf

    write(sBuf, fmt=*, iostat=iStat)  iValue

    if (iStat==0) then
      sText = trim( adjustl(sBuf) )
    else
      sText = "NA"
    endif

  end function long_long_to_char_fn

  !--------------------------------------------------------------------------------------------------

  elemental function float_to_char_fn(fValue, iFieldWidth, iNumdigits)    result(sText)

    real (kind=c_float), intent(in)             :: fValue
    integer (kind=c_int), intent(in), optional  :: iFieldWidth
    integer (kind=c_int), intent(in), optional  :: iNumdigits
    character (len=:), allocatable              :: sText

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=32)   :: sFmt
    character (len=32)   :: sBuf

    if ( present( iNumDigits) .and. present( iFieldWidth ) ) then
      write(sFmt, fmt="('(G',i0,'.',i0,')')") iFieldWidth, iNumdigits
    elseif (present(iNumDigits) ) then
      write(sFmt, fmt="('(G0.',i0,')')") iNumdigits
    elseif (present(iFieldWidth) ) then
      write(sFmt, fmt="('(G',i0,'.4)')") iNumdigits
    else
      sFmt = "(G0.4)"
    endif

    write(sBuf, fmt=trim(sFmt), iostat=iStat)  fValue

    if (iStat==0) then
      sText = trim( adjustl(sBuf) )
    else
      sText = "NA"
    endif

  end function float_to_char_fn

  !--------------------------------------------------------------------------------------------------

  elemental function double_to_char_fn(dValue, iNumdigits)    result(sText)

    real (kind=c_double), intent(in)             :: dValue
    integer (kind=c_int), intent(in), optional  :: iNumdigits
    character (len=:), allocatable              :: sText

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=:), allocatable :: sFmt
    character (len=32)   :: sBuf

    if (present(iNumDigits) ) then
      write(sFmt, fmt="('(G0.',i0,')')") iNumdigits
    else
      sFmt = "(G0.12)"
    endif

    write(sBuf, fmt=sFmt, iostat=iStat)  dValue

    if (iStat==0) then
      sText = trim( adjustl(sBuf) )
    else
      sText = "NA"
    endif

  end function double_to_char_fn

  !--------------------------------------------------------------------------------------------------

  elemental function logical_to_char_fn(lValue)    result(sText)

    logical (kind=c_bool), intent(in)    :: lValue
    character (len=:), allocatable       :: sText

    if (lValue) then
      sText = "TRUE"
    else
      sText = "FALSE"
    endif

  end function logical_to_char_fn

  !--------------------------------------------------------------------------------------------------

  elemental function squote_char_fn(sText1)    result(sText)

    character (len=*), intent(in)         :: sText1
    character (len=:), allocatable        :: sText

    sText = "'"//trim(sText1)//"'"

  end function squote_char_fn

  !--------------------------------------------------------------------------------------------------

  elemental function dquote_char_fn(sText1)    result(sText)

    character (len=*), intent(in)         :: sText1
    character (len=:), allocatable        :: sText

    sText = '"'//trim(sText1)//'"'

  end function dquote_char_fn

  !--------------------------------------------------------------------------------------------------

  elemental function char_to_uppercase_fn ( s )                    result(sText)

    ! ARGUMENTS
    character (len=*), intent(in) :: s
    character(len=len(s))         :: sText

    ! LOCALS
    integer (kind=c_int) :: i    ! do loop index

    ! CONSTANTS
    integer (kind=c_int), parameter :: LOWER_TO_UPPER = -32
    integer (kind=c_int), parameter :: ASCII_SMALL_A = ichar("a")
    integer (kind=c_int), parameter :: ASCII_SMALL_Z = ichar("z")

    sText = s

    do i=1,len_trim(sText)
      if ( ichar(sText(i:i) ) >= ASCII_SMALL_A .and. ichar(sText(i:i)) <= ASCII_SMALL_Z ) then
        sText(i:i) = char( ichar( sText(i:i) ) + LOWER_TO_UPPER )
      end if
    end do

  end function char_to_uppercase_fn

  !--------------------------------------------------------------------------

  elemental function char_to_lowercase_fn ( s )                               result(sText)

    ! ARGUMENTS
    character (len=*), intent(in) :: s
    character(len=len(s)) :: sText

    ! LOCALS
    integer (kind=c_int) :: i    ! do loop index
    ! CONSTANTS
    integer (kind=c_int), parameter :: UPPER_TO_LOWER = 32
    integer (kind=c_int), parameter :: ASCII_A = ichar("A")
    integer (kind=c_int), parameter :: ASCII_Z = ichar("Z")

    sText = s

    do i=1,len_trim(sText)
      if ( ichar(sText(i:i) ) >= ASCII_A .and. ichar(sText(i:i)) <= ASCII_Z ) then
        sText(i:i) = char( ichar( sText(i:i) ) + UPPER_TO_LOWER )
      end if
    end do

  end function char_to_lowercase_fn


  elemental subroutine char_to_uppercase_sub ( s )

    ! ARGUMENTS
    character (len=*), intent(inout) :: s
    ! LOCALS
    integer (kind=c_int) :: i    ! do loop index
    ! CONSTANTS
    integer (kind=c_int), parameter :: LOWER_TO_UPPER = -32
    integer (kind=c_int), parameter :: ASCII_SMALL_A = ichar("a")
    integer (kind=c_int), parameter :: ASCII_SMALL_Z = ichar("z")

    do i=1,len_trim(s)
      if ( ichar(s(i:i) ) >= ASCII_SMALL_A .and. ichar(s(i:i)) <= ASCII_SMALL_Z ) then
        s(i:i) = char( ichar( s(i:i) ) + LOWER_TO_UPPER )
      end if
    end do

  end subroutine char_to_uppercase_sub


  elemental subroutine char_to_lowercase_sub ( s )

    ! ARGUMENTS
    character (len=*), intent(inout) :: s
    ! LOCALS
    integer (kind=c_int) :: i    ! do loop index
    ! CONSTANTS
    integer (kind=c_int), parameter :: UPPER_TO_LOWER = 32
    integer (kind=c_int), parameter :: ASCII_A = ichar("A")
    integer (kind=c_int), parameter :: ASCII_Z = ichar("Z")

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
  function clean(sText1, sTargetCharacters)            result(sText)

    ! ARGUMENTS
    character (len=*), intent(inout)           :: sText1
    character (len=*), intent(in), optional    :: sTargetCharacters
    character (len=:), allocatable :: sText

    ! LOCALS
    character (len=512)            :: sBuf
    integer (kind=c_int)           :: iR                 ! Index in sRecord
    integer (kind=c_int)           :: iIndex1, iIndex2
    character (len=:), allocatable :: sTargetCharacters_

    ! eliminate any leading spaces
    sText1 = adjustl(sText1)
    sBuf = ""
    iIndex2 = 0

    if (present(sTargetCharacters) ) then
      sTargetCharacters_ = sTargetCharacters
    else
      sTargetCharacters_ = ":/;,"
    endif

    do iIndex1 = 1,len_trim(sText1)

      iR = scan(sText1(iIndex1:iIndex1), sTargetCharacters_)

      if(iR==0) then
        iIndex2 = iIndex2 + 1
        sBuf(iIndex2:iIndex2) = sText1(iIndex1:iIndex1)
      end if

    enddo

    sText = trim(sBuf)

  end function clean

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
    integer (kind=c_int)           :: iR                 ! Index in sRecord
    integer (kind=c_int)           :: iIndex1, iIndex2
    character (len=1)              :: sChar_
    logical (kind=c_bool)          :: lPreviouslyFound

    ! eliminate any leading spaces
    sText1 = adjustl(sText1)
    sBuf = ""
    iIndex2 = 0
    lPreviouslyFound = lFALSE

    if (present(sChar) ) then
      sChar_ = sChar
    else
      sChar_ = " "
    endif

    do iIndex1 = 1,len_trim(sText1)

      iR = scan(sText1(iIndex1:iIndex1), sChar_)

      if(iR==0) then
        ! sChar_ was not found
        iIndex2 = iIndex2 + 1
        sBuf(iIndex2:iIndex2) = sText1(iIndex1:iIndex1)
        lPreviouslyFound = lFALSE

      elseif( lPreviouslyFound ) then
        ! sChar_ was found, and was also found in the position preceding this one

        ! No OP

      else
        ! sChar_ was found, but was *not* found in the preceding position

        iIndex2 = iIndex2 + 1
        sBuf(iIndex2:iIndex2) = sText1(iIndex1:iIndex1)
        lPreviouslyFound = lTRUE

      end if

    enddo

    sText = trim(sBuf)

  end function remove_repeats

  !--------------------------------------------------------------------------------------------------

  function count_number_of_fields_fn( sText, sDelimiters )     result( iCount )

    character (len=*), intent(in)               :: sText
    character (len=*), intent(in), optional     :: sDelimiters
    integer (kind=c_int)                        :: iCount

    ! [ LOCALS ]
    character (len=len(sText))      :: sText1
    character (len=len(sText))      :: sText2
    character (len=:), allocatable  :: sDelimiters_

    if ( present(sDelimiters) ) then
      sDelimiters_ = sDelimiters_
    else
      sDelimiters_ = WHITESPACE
    endif

    iCount = 0

    sText1 = sText

    do
      call chomp(sText1=sText1, sText2=sText2, sDelimiters=sDelimiters_ )

      if ( len_trim( sText2 ) == 0 )  exit

      iCount = iCount + 1

    enddo

  end function count_number_of_fields_fn

  !--------------------------------------------------------------------------------------------------

  subroutine split_and_return_text_sub(sText1, sText2, sDelimiters)

    character (len=*), intent(inout)                     :: sText1
    character (len=*), intent(out)                       :: sText2
    character (len=*), intent(in), optional              :: sDelimiters

    ! [ LOCALS ]
    character (len=:), allocatable :: sDelimiters_
    integer (kind=c_int) :: iIndex

    if ( present(sDelimiters) ) then

      select case (sDelimiters)
      case ("WHITESPACE")
        sDelimiters_ = WHITESPACE
      case ("TAB", "TABS")
        sDelimiters_ = TAB
      case ("COMMA", "CSV")
        sDelimiters_ = ","
      case default
        sDelimiters_ = sDelimiters
      end select

    else

      sDelimiters_ = WHITESPACE

    endif

    sText1 = adjustl(sText1)

    iIndex = scan( string = sText1, set = sDelimiters_ )

    if (iIndex == 0) then
      ! no delimiters found; return string as was supplied originally
      sText2 = sText1
      sText1 = ""
    else
      ! delimiters were found; split and return the chunks of text
      sText2 = trim( sText1(1:iIndex-1) )
      sText1 = trim( adjustl( sText1(iIndex + 1:) ) )
    endif

  end subroutine split_and_return_text_sub

  !--------------------------------------------------------------------------------------------------

  subroutine replace_character_sub(sText1, sFind, sReplace)

    character (len=*), intent(inout)    :: sText1
    character (len=1), intent(in)       :: sFind
    character (len=1), intent(in)       :: sReplace

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    if ( len(sText1) > 0 ) then

      do iIndex = 1, len_trim(sText1)

        if ( sText1(iIndex:iIndex) .eq. sFind)    sText1(iIndex:iIndex) = sReplace

      enddo

    endif

  end subroutine replace_character_sub

  !--------------------------------------------------------------------------------------------------

  function remove_multiple_characters_fn(sText, sDelimiters)                result(sText1)

    ! ARGUMENTS
    character (len=*), intent(inout)           :: sText
    character (len=*), intent(in), optional    :: sDelimiters
    character (len=:), allocatable             :: sText1

    ! LOCALS
    character ( len=len_trim(sText) ) :: sTemp
    integer (kind=c_int) :: iR                 ! Index in sRecord
    integer (kind=c_int) :: i, j

    ! eliminate any leading spaces
    sText = adjustl(sText)
    sTemp = ""
    j = 0

    do i = 1,len_trim(sText)

      if(present(sDelimiters)) then
        iR = scan(sText(i:i), sDelimiters)
      else
        iR = scan(sText(i:i),":/;,")
      endif

      if(iR==0) then
        j = j + 1
        sTemp(j:j) = sText(i:i)
      end if

    enddo

    sText1 = trim(sTemp)

  end function remove_multiple_characters_fn


end module strings
