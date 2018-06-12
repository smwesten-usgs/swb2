module string_list

  use iso_c_binding, only             : c_int, c_float, c_bool, c_null_char
  use constants_and_conversions, only : asInt, asFloat, asLogical, lTRUE, lFALSE, TRUE, FALSE,   &
      WHITESPACE, PUNCTUATION
  use strings
  use logfiles, only                  : LOG_DEBUG
  use exceptions
  implicit none

  private

  public :: STRING_LIST_T, create_list

  public :: assignment(=)
  interface assignment(=)
    module procedure :: assign_string_list_to_string_list_sub
  end interface assignment(=)

  interface create_list
    procedure :: list_from_delimited_string_fn
  end interface create_list

  type STRING_LIST_ELEMENT_T

    character (len=:), allocatable               :: s
    type (STRING_LIST_ELEMENT_T), pointer        :: next => null()

  end type STRING_LIST_ELEMENT_T


  type STRING_LIST_T

    type (STRING_LIST_ELEMENT_T), pointer        :: first        => null()
    type (STRING_LIST_ELEMENT_T), pointer        :: last         => null()
    logical (kind=c_bool)                        :: autocleanup  = TRUE
    integer (kind=c_int)                         :: count        = 0
    logical (kind=c_bool)                        :: is_populated = FALSE

  contains

    procedure :: list_append_string_sub
    procedure :: list_append_int_sub
    procedure :: list_from_delimited_string_sub
    procedure :: list_get_value_at_index_fn
    procedure :: list_get_values_in_range_fn
    procedure :: list_replace_value_at_index_sub
    procedure :: list_print_sub
    procedure :: list_all_fn
    procedure :: list_all_delimited_fn
    procedure :: list_return_position_of_matching_string_fn
    procedure :: list_return_count_of_matching_string_fn
    procedure :: list_are_there_empty_entries_fn
    procedure :: list_is_string_in_list_fn
    procedure :: list_items_deallocate_all_sub
    procedure :: list_return_all_as_float_fn
    procedure :: list_return_all_as_int_fn
    procedure :: list_return_all_as_logical_fn
    procedure :: list_return_all_as_character_fn
    procedure :: list_subset_partial_matches_fn
    procedure :: list_set_auto_cleanup_sub
    final     :: list_finalize_sub

    generic :: append            => list_append_string_sub,                   &
                                    list_append_int_sub
    generic :: get               => list_get_value_at_index_fn,               &
                                    list_get_values_in_range_fn
    generic :: replace           => list_replace_value_at_index_sub
    generic :: create_list       => list_from_delimited_string_sub
    generic :: set_autocleanup   => list_set_auto_cleanup_sub
    generic :: print             => list_print_sub
    generic :: listall           => list_all_fn,                              &
                                    list_all_delimited_fn
    generic :: grep              => list_subset_partial_matches_fn
    generic :: which             => list_return_position_of_matching_string_fn
    generic :: countmatching     => list_return_count_of_matching_string_fn
    generic :: iselement         => list_is_string_in_list_fn
    generic :: empty_entries_present => list_are_there_empty_entries_fn
    generic :: clear             => list_items_deallocate_all_sub
    generic :: asFloat           => list_return_all_as_float_fn
    generic :: asInt             => list_return_all_as_int_fn
    generic :: asLogical         => list_return_all_as_logical_fn
    generic :: asCharacter       => list_return_all_as_character_fn

  end type STRING_LIST_T

contains

  subroutine list_append_int_sub( this, iValue )

    class (STRING_LIST_T), intent(inout)   :: this
    integer (kind=c_int), intent(in)       :: iValue

    call this%list_append_string_sub( asCharacter(iValue) )

  end subroutine list_append_int_sub

  !--------------------------------------------------------------------------------------------------

  subroutine list_set_auto_cleanup_sub( this, autocleanup )

    class (STRING_LIST_T), intent(inout)    :: this
    logical (kind=c_bool), intent(in)       :: autocleanup

    this%autocleanup = autocleanup

  end subroutine list_set_auto_cleanup_sub

  !--------------------------------------------------------------------------------------------------

  subroutine assign_string_list_to_string_list_sub(slList2, slList1)

    type (STRING_LIST_T), intent(out)   :: slList2
    type (STRING_LIST_T), intent(in)    :: slList1

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    if ( slList1%count > 0 ) then

      do iIndex=1, slList1%count

        call slList2%append( slList1%get(iIndex) )

      enddo

    endif

  end subroutine assign_string_list_to_string_list_sub

  !------------------------------------------------------------------------------

  subroutine list_append_string_sub( this, sText )

    class (STRING_LIST_T), intent(inout)   :: this
    character (len=*), intent(in) :: sText

    ! [ LOCALS ]
    type (STRING_LIST_ELEMENT_T), pointer   :: pNewElement
    integer (kind=c_int)                     :: iStat

    pNewElement => null()

    allocate(pNewElement, stat=iStat)
    call assert(iStat == 0, "There was a problem allocating memory for a new string list element", &
        __SRCNAME__, __LINE__)

    pNewElement%s    = trim( sText )
    pNewElement%next => null()

    if (associated( this%first ) ) then

      if (this%count == 0)  call die("Internal logic error: count should *not* be zero in this block", &
          __SRCNAME__, __LINE__)

      !      pOldLastElement => this%last
      !      pOldLastElement%next => pNewElement

      this%last%next => pNewElement
      this%last      => pNewElement
      this%last%next => null()

    else

      this%first => pNewElement
      this%last  => pNewElement

    endif

    this%count = this%count + 1
    this%is_populated = TRUE

  end subroutine list_append_string_sub

  !--------------------------------------------------------------------------------------------------

  subroutine list_replace_value_at_index_sub(this, iIndex, sText)

    class (STRING_LIST_T), intent(in)        :: this
    integer (kind=c_int), intent(in)         :: iIndex
    character (len=*), intent(in)            :: sText

    ! [ LOCALS ]
    integer (kind=c_int)                      :: iCount
    type (STRING_LIST_ELEMENT_T), pointer    :: current => null()

    iCount = 0

    current => this%first

    do while ( associated( current ) .and. iCount < this%count )

      iCount = iCount + 1

      if (iCount == iIndex)  exit

      current => current%next

    enddo

    if (associated(current) ) then
      current%s = trim( sText )
    else
      !      call warn(sMessage="Unable to find a pointer associated with index: "//asCharacter(iIndex),  &
      !                iLogLevel=LOG_DEBUG,                                                               &
      !                sModule=__SRCNAME__,                                                                  &
      !                iLine=__LINE__ )
    endif

  end subroutine list_replace_value_at_index_sub

  !--------------------------------------------------------------------------------------------------

  function list_get_value_at_index_fn(this, iIndex)   result(sText)

    class (STRING_LIST_T), intent(in)        :: this
    integer (kind=c_int), intent(in)         :: iIndex
    character (len=:), allocatable           :: sText

    ! [ LOCALS ]
    integer (kind=c_int)                      :: iCount
    type (STRING_LIST_ELEMENT_T), pointer    :: current => null()

    iCount = 1

    current => this%first

    do while ( associated( current ) .and. ( iCount <= this%count ) )

      if (iCount == iIndex)  exit
      current => current%next
      iCount = iCount + 1
    enddo

    sText = "<NA>"

    if (associated(current) ) then

      if ( allocated( current%s) )  sText = current%s

    endif

  end function list_get_value_at_index_fn

  !--------------------------------------------------------------------------------------------------

  !> Iterate over a range of indices; return a space-delimited string comprised of the values.
  function list_get_values_in_range_fn(this, iStartIndex, iEndIndex)   result(sText)

    class (STRING_LIST_T), intent(in)        :: this
    integer (kind=c_int), intent(in)         :: iStartIndex
    integer (kind=c_int), intent(in)         :: iEndIndex
    character (len=:), allocatable           :: sText

    ! [ LOCALS ]
    integer (kind=c_int)                      :: iCount
    type (STRING_LIST_ELEMENT_T), pointer    :: current => null()

    iCount = 0
    sText = " "

    current => this%first

    do while ( associated( current ) .and. iCount < this%count )

      iCount = iCount + 1

      if (iCount == iStartIndex ) then
        sText = current%s
      elseif (iCount > iStartIndex .and. iCount <= iEndIndex ) then
        sText = sText//" "//current%s
      endif

      current => current%next

    enddo

    if ( len_trim(sText) == 0 ) then
      sText = "<NA>"
      !      call warn("Unable to find a pointer associated with index range: " &
      !          //asCharacter(iStartIndex)//" to "//asCharacter(iEndIndex), &
      !          __SRCNAME__, __LINE__ )
    endif


  end function list_get_values_in_range_fn

  !--------------------------------------------------------------------------------------------------

  subroutine list_print_sub(this, iLU)

    use iso_fortran_env, only : OUTPUT_UNIT

    class (STRING_LIST_T), intent(in)     :: this
    integer (kind=c_int), optional        :: iLU

    ! [ LOCALS ]
    type (STRING_LIST_ELEMENT_T), pointer    :: current => null()
    integer (kind=c_int)                      :: iLU_l
    integer (kind=c_int)                      :: iCount

    if (present(iLU) ) then
      iLU_l = iLU
    else
      iLU_l = OUTPUT_UNIT
    endif

    current => this%first
    iCount = 0

    write(iLU_l, fmt="('|',a,t21,'|',a,t62,'|')") "Index","Value"
    write(iLU_l, fmt="('|',a,t21,'|',a,t62,'|')") repeat("-",18)//":", repeat("-",39)//":"

    do while ( associated( current ) .and. iCount < this%count )

      iCount = iCount + 1

      write(iLU_l, fmt="('|',a,t21,'|',a,t62,'|')") asCharacter(iCount), current%s

      current => current%next

    enddo

  end subroutine list_print_sub

  !--------------------------------------------------------------------------------------------------

  function list_all_fn(this)  result( sListValues )

    use iso_fortran_env, only : OUTPUT_UNIT

    class (STRING_LIST_T), intent(in)     :: this
    character (len=:), allocatable        :: sListValues

    ! [ LOCALS ]
    type (STRING_LIST_ELEMENT_T), pointer    :: current => null()
    integer (kind=c_int)                      :: iLU_l
    integer (kind=c_int)                      :: iCount
    character (len=2048)                      :: sBuf

    current => this%first
    iCount = 0

    sBuf = ""

    do while ( associated( current ) .and. iCount < this%count )

      iCount = iCount + 1

      sBuf = trim(sBuf)//" ("//asCharacter(iCount)//") "//current%s

      current => current%next

    enddo

    sListValues = adjustl( trim(sBuf) )

  end function list_all_fn

  !--------------------------------------------------------------------------------------------------

  function list_all_delimited_fn(this, delimiter)  result( sListValues )

    use iso_fortran_env, only : OUTPUT_UNIT

    class (STRING_LIST_T), intent(in)     :: this
    character (len=*), intent(in)         :: delimiter
    character (len=:), allocatable        :: sListValues

    ! [ LOCALS ]
    type (STRING_LIST_ELEMENT_T), pointer    :: current => null()
    integer (kind=c_int)                      :: iLU_l
    integer (kind=c_int)                      :: iCount
    character (len=2048)                      :: sBuf

    current => this%first
    iCount = 0

    sBuf = ""

    do while ( associated( current ) .and. iCount <= this%count )

      iCount = iCount + 1

      if ( iCount == 1 ) then

        sBuf = trim( current%s )

      elseif( iCount == this%count ) then

        sBuf = trim( sBuf )//trim( delimiter )//trim( current%s )//trim( delimiter )

      else

        sBuf = trim( sBuf )//trim( delimiter )//trim( current%s )

      endif

      current => current%next

    enddo

    sListValues = adjustl( trim(sBuf) )

  end function list_all_delimited_fn

  !--------------------------------------------------------------------------------------------------

  function list_return_all_as_float_fn(this)    result(rValues)

    class (STRING_LIST_T), intent(in)     :: this
    real (kind=c_float), allocatable      :: rValues(:)

    ! [ LOCALS ]
    type (STRING_LIST_ELEMENT_T), pointer    :: current => null()
    integer (kind=c_int)                      :: iStat
    integer (kind=c_int)                      :: iIndex

    allocate( rValues( 1:this%count ), stat=iStat )
    if (iStat /= 0)  call die("Failed to allocate memory for list conversion", __SRCNAME__, __LINE__)

    current => this%first
    iIndex = 0

    do while ( associated( current ) .and. iIndex < ubound(rValues,1) )

      iIndex = iIndex + 1
      rValues(iIndex) = asFloat( current%s )

      current => current%next

    enddo

  end function list_return_all_as_float_fn

  !--------------------------------------------------------------------------------------------------

  function list_return_all_as_character_fn(this, null_terminated )    result(sValues)

    class (STRING_LIST_T), intent(in)     :: this
    character (len=64), allocatable       :: sValues(:)
    logical (kind=c_bool), optional       :: null_terminated

    ! [ LOCALS ]
    type (STRING_LIST_ELEMENT_T), pointer    :: current => null()
    integer (kind=c_int)                      :: iStat
    integer (kind=c_int)                      :: iIndex
    logical (kind=c_bool)                     :: null_terminated_l

    if ( present( null_terminated) ) then
      null_terminated_l = null_terminated
    else
      null_terminated_l = FALSE
    endif

    allocate( sValues( 1:this%count ), stat=iStat )
    if (iStat /= 0)  call die("Failed to allocate memory for list conversion", __SRCNAME__, __LINE__)

    current => this%first
    iIndex = 0

    do while ( associated( current ) .and. iIndex < ubound(sValues,1) )

      iIndex = iIndex + 1

      if ( null_terminated_l ) then
        sValues(iIndex) = trim(current%s)//c_null_char
      else
        sValues(iIndex) = current%s
      endif

      current => current%next

    enddo

  end function list_return_all_as_character_fn

  !--------------------------------------------------------------------------------------------------

  function list_return_all_as_int_fn(this)    result(iValues)

    class (STRING_LIST_T), intent(in)     :: this
    integer (kind=c_int), allocatable     :: iValues(:)

    ! [ LOCALS ]
    type (STRING_LIST_ELEMENT_T), pointer    :: current => null()
    integer (kind=c_int)                      :: iStat
    integer (kind=c_int)                      :: iIndex

    allocate( iValues(this%count ), stat=iStat )
    if (iStat /= 0)  call die("Failed to allocate memory for list conversion", __SRCNAME__, __LINE__)

    current => this%first
    iIndex = 0

    do while ( associated( current ) .and. iIndex < ubound(iValues,1) )

      iIndex = iIndex + 1

      iValues(iIndex) = asInt( current%s )

      current => current%next

    enddo

  end function list_return_all_as_int_fn

  !--------------------------------------------------------------------------------------------------

  function list_return_all_as_logical_fn(this)    result(lValues)

    class (STRING_LIST_T), intent(in)     :: this
    logical (kind=c_bool), allocatable     :: lValues(:)

    ! [ LOCALS ]
    type (STRING_LIST_ELEMENT_T), pointer    :: current => null()
    integer (kind=c_int)                      :: iStat
    integer (kind=c_int)                      :: iIndex

    allocate( lValues( this%count ), stat=iStat )
    if (iStat /= 0)  call die("Failed to allocate memory for list conversion", __SRCNAME__, __LINE__)

    current => this%first
    iIndex = 0

    do while ( associated( current ) .and. iIndex < ubound(lValues,1) )

      iIndex = iIndex + 1

      lValues(iIndex) = asLogical( current%s )

      current => current%next

    enddo

  end function list_return_all_as_logical_fn

  !--------------------------------------------------------------------------------------------------

  function list_from_delimited_string_fn(sText1, delimiters)    result( newList )

    character (len=*), intent(in)           :: sText1
    character (len=*), intent(in), optional :: delimiters
    type (STRING_LIST_T)                    :: newList

    ! [ LOCALS ]
    character (len=:), allocatable :: sTempText
    character (len=:), allocatable :: sTempArg
    character (len=256)                :: delimiters_l

    if ( present( delimiters ) ) then
      delimiters_l = delimiters
    else
      delimiters_l = PUNCTUATION
    endif

    sTempText = sText1
    sTempArg = sText1

    do

      call chomp( sText1=sTempText, sText2=sTempArg, sDelimiters=delimiters_l )

      if (len_trim(sTempArg) > 0) then
        call newList%append( trim( adjustl( sTempArg ) ) )
      else
        exit
      endif

    end do

  end function list_from_delimited_string_fn

  !--------------------------------------------------------------------------------------------------
  !!
  !!
  !! @TODO: investigate why calling this routine leads to seg faults under
  !!        certain circumstances... may just be compiler issue?
  !!
  subroutine list_from_delimited_string_sub(this, sText, delimiters)

    class (STRING_LIST_T), intent(inout)    :: this
    character (len=*), intent(in)           :: sText
    character (len=*), intent(in), optional :: delimiters

    ! [ LOCALS ]
    character (len=:), allocatable    :: sTempText
    character (len=:), allocatable    :: sTempArg
    character (len=256)    :: delimiters_l

    if ( present( delimiters ) ) then
      delimiters_l = delimiters
    else
      delimiters_l = PUNCTUATION
    endif

    sTempText = sText
    sTempArg = ""

    call this%clear()

    do

      call chomp( sText1=sTempText, sText2=sTempArg, sDelimiters=trim(delimiters_l))

      if (len_trim(sTempArg ) > 0) then
        call this%append( sTempArg )
      else
        exit
      endif

    end do

  end subroutine list_from_delimited_string_sub

  !--------------------------------------------------------------------------------------------------

  function list_are_there_empty_entries_fn( this )  result( lResult )

    class (STRING_LIST_T), intent(in)       :: this
    logical (kind=c_bool)                   :: lResult

    ! [ LOCALS ]
    integer (kind=c_int)                        :: iIndex
    integer (kind=c_int)                        :: iStat
    integer (kind=c_int)                        :: iCount
    type (STRING_LIST_ELEMENT_T), pointer       :: current

    iCount = 0
    iIndex = 0
    lResult = FALSE

    current => this%first

    do while ( associated(current) .and. iCount < this%count )

      iIndex = iIndex + 1

      if ( len_trim( current%s ) == 0 ) then
        iCount = iCount + 1
      endif

      current => current%next

    enddo

    if ( iCount > 0 )  lResult = TRUE

  end function list_are_there_empty_entries_fn

!--------------------------------------------------------------------------------------------------

  function list_is_string_in_list_fn(this, sChar)  result( lResult )

    class (STRING_LIST_T), intent(in) :: this
    character (len=*), intent(in)     :: sChar
    logical (kind=c_bool)             :: lResult

    ! [ LOCALS ]
    integer (kind=c_int) :: iCount

    iCount = 0

    iCount = this%countmatching( sChar )

    if ( iCount > 0 ) then
      lResult = lTRUE
    else
      lResult = lFALSE
    endif

  end function list_is_string_in_list_fn

  !--------------------------------------------------------------------------------------------------

  function list_return_count_of_matching_string_fn(this, sChar) result(iCount)

    class (STRING_LIST_T), intent(in) :: this
    character (len=*), intent(in) :: sChar
    integer (kind=c_int) :: iCount

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iRetVal
    integer (kind=c_int), dimension(this%count) :: iTempResult
    type (STRING_LIST_ELEMENT_T), pointer :: current => null()

    iCount = 0
    iIndex = 0

    current => this%first

    do while ( associated(current) .and. iCount < this%count )

      iIndex = iIndex + 1

      if ( (current%s .strequal. sChar) .or. index(current%s, sChar) /=0 ) then
        iCount = iCount + 1
        iTempResult(iCount) = iIndex
      endif

      current => current%next

    enddo

  end function list_return_count_of_matching_string_fn

  !--------------------------------------------------------------------------------------------------

  function list_return_position_of_matching_string_fn(this, sChar)     result(iResult)

    class (STRING_LIST_T), intent(in)                    :: this
    character (len=*), intent(in)                        :: sChar
    integer (kind=c_int), dimension(:), allocatable      :: iResult

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iCount
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iRetVal
    integer (kind=c_int), dimension(this%count) :: iTempResult
    type (STRING_LIST_ELEMENT_T), pointer   :: current => null()

    iIndex = 0
    iCount = 0

    current => this%first

    do while ( associated(current) .and. iIndex < this%count )

      iIndex = iIndex + 1

      if ( (current%s .strequal. sChar) .or. index(current%s, sChar) /=0 ) then
        iCount = iCount + 1
        iTempResult(iCount) = iIndex
      endif

      current => current%next

    enddo

    if (iCount == 0) then

      allocate(iResult(1), stat=iStat)
      if (iStat /= 0)   call die("Problem allocating memory", __SRCNAME__, __LINE__)

      iResult(1) = -9999

    else

      allocate(iResult(iCount), stat=iStat)
      if (iStat /= 0)   call die("Problem allocating memory", __SRCNAME__, __LINE__)

      iResult(1:iCount) = iTempResult(1:iCount)

    endif

  end function list_return_position_of_matching_string_fn

  !--------------------------------------------------------------------------------------------------

  function list_subset_partial_matches_fn( this, sChar )     result(newList)

    class (STRING_LIST_T), intent(in)                    :: this
    character (len=*), intent(in)                        :: sChar
    type (STRING_LIST_T)                                 :: newList

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iRetVal
    integer (kind=c_int) :: iCount
    integer (kind=c_int), dimension(this%count) :: iTempResult
    type (STRING_LIST_ELEMENT_T), pointer   :: current => null()

    iCount = 0

    current => this%first

    do while ( associated(current) .and. iCount < this%count )

      if ( (current%s .strequal. sChar) .or. &
          (index(string=asUppercase(current%s), substring=asUppercase(sChar) ) > 0 ) ) then

        iCount = iCount + 1
        call newList%append(current%s)

      endif

      current => current%next

    enddo

  end function list_subset_partial_matches_fn

  !--------------------------------------------------------------------------------------------------

  subroutine list_finalize_sub(this)

    type (STRING_LIST_T) :: this

    if ( this%autocleanup )  call this%clear()

  end subroutine list_finalize_sub

  !--------------------------------------------------------------------------------------------------

  subroutine list_items_deallocate_all_sub(this)

    class (STRING_LIST_T), intent(inout) :: this

    ! [ LOCALS ]
    type (STRING_LIST_ELEMENT_T), pointer :: current
    type (STRING_LIST_ELEMENT_T), pointer :: toremove

    current => null()
    toremove => null()

    if ( associated( this%first ) ) then

      current => this%first

      do while ( associated( current ) )

        toremove => current

        current => current%next

        if ( allocated( toremove%s ) )    deallocate( toremove%s )

        if ( associated( toremove ) )  deallocate( toremove )

      enddo

    endif

    this%count = 0

    this%first => null()

    this%last => null()

  end subroutine list_items_deallocate_all_sub

end module string_list
