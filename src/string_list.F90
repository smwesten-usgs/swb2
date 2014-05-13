module string_list

  use iso_c_binding, only : c_int, c_float
  use constants_and_conversions, only : asInt, asFloat
  use strings
  use exceptions
  implicit none

  private

  public :: STRING_LIST_T

  type STRING_LIST_ELEMENT_T

    character (len=:), allocatable               :: s
    type (STRING_LIST_ELEMENT_T), pointer        :: next => null()

  end type STRING_LIST_ELEMENT_T


  type STRING_LIST_T

    type (STRING_LIST_ELEMENT_T), pointer        :: first   => null()
    type (STRING_LIST_ELEMENT_T), pointer        :: last    => null()
    integer (kind=c_int)                         :: count   = 0

  contains

    procedure :: list_append_string_sub
    procedure :: list_append_int_sub
    procedure :: list_get_value_at_index_fn
    procedure :: list_print_sub
    procedure :: list_return_position_of_matching_string_fn
    procedure :: list_return_count_of_matching_string_fn
    procedure :: list_items_deallocate_all_sub
    procedure :: list_return_all_as_float_fn
    procedure :: list_return_all_as_int_fn
    procedure :: list_subset_partial_matches_fn

    generic :: append => list_append_string_sub, &
                         list_append_int_sub
    generic :: get => list_get_value_at_index_fn
    generic :: print => list_print_sub
    generic :: grep => list_subset_partial_matches_fn
    generic :: which => list_return_position_of_matching_string_fn
    generic :: countmatching => list_return_count_of_matching_string_fn
    generic :: deallocate => list_items_deallocate_all_sub
    generic :: asFloat => list_return_all_as_float_fn
    generic :: asInt => list_return_all_as_int_fn

  end type STRING_LIST_T



contains

  
  subroutine list_append_int_sub( this, iValue )

    class (STRING_LIST_T), intent(inout)   :: this
    integer (kind=c_int), intent(in)       :: iValue

    call this%list_append_string_sub( asCharacter(iValue) )

  end subroutine list_append_int_sub



  subroutine list_append_string_sub( this, sText )

    class (STRING_LIST_T), intent(inout)   :: this
    character (len=*), intent(in)          :: sText

    ! [ LOCALS ] 
    class (STRING_LIST_ELEMENT_T), pointer   :: pNewElement => null()
    class (STRING_LIST_ELEMENT_T), pointer   :: pOldLastElement => null()
    integer (kind=c_int)                     :: iStat

    allocate(pNewElement, stat=iStat)  
    call assert(iStat == 0, "There was a problem allocating memory for a new string list element", &
      __FILE__, __LINE__)

    pNewElement%s    = trim(sText)
    pNewElement%next => null()

    if (associated( this%first ) ) then

      if (this%count == 0)  call die("Internal logic error: count should *not* be zero in this block", &
             __FILE__, __LINE__)

      pOldLastElement => this%last
      pOldLastElement%next => pNewElement
      this%last      => pNewElement
      this%last%next => null()

    else
    
      this%first => pNewElement
      this%last  => pNewElement

    endif    

    this%count = this%count + 1

  end subroutine list_append_string_sub

!--------------------------------------------------------------------------------------------------

  function list_get_value_at_index_fn(this, iIndex)   result(sText)

    class (STRING_LIST_T), intent(in)        :: this
    integer (kind=c_int), intent(in)         :: iIndex
    character (len=:), allocatable           :: sText

    ! [ LOCALS ]
    integer (kind=c_int)                      :: iCount
    class (STRING_LIST_ELEMENT_T), pointer    :: current => null()

    iCount = 0

    current => this%first

    if (iIndex > 0 .and. iIndex <= this%count .and. associated( current ) ) then

      do while ( associated( current ) .and. iCount < this%count )

        iCount = iCount + 1

        if (iCount == iIndex)  exit

        current => current%next

      enddo
    
      if (associated(current) ) then
        sText = current%s
      else
        sText = "<NA>"
        call warn("Internal logic error: should never be able to execute this part of block", &
          __FILE__, __LINE__ )
      endif  

    else

      sText = "<NA>"
      call warn("Internal logic error: Index out of bounds or unassociated pointer", &
          __FILE__, __LINE__ )

    endif  


  end function list_get_value_at_index_fn

!--------------------------------------------------------------------------------------------------

  subroutine list_print_sub(this, iLU)

    use iso_fortran_env, only : OUTPUT_UNIT

    class (STRING_LIST_T), intent(in)     :: this
    integer (kind=c_int), optional        :: iLU

    ! [ LOCALS ]
    class (STRING_LIST_ELEMENT_T), pointer    :: current => null()
    integer (kind=c_int)                      :: iLU_
    integer (kind=c_int)                      :: iCount

    if (present(iLU) ) then
      iLU_ = iLU
    else
      iLU_ = OUTPUT_UNIT
    endif
      
    current => this%first
    iCount = 0

    do while ( associated( current ) .and. iCount < this%count )

      iCount = iCount + 1

      write(iLU_, fmt="(a)") current%s

      current => current%next

    enddo

  end subroutine list_print_sub

!--------------------------------------------------------------------------------------------------

  function list_return_all_as_float_fn(this)    result(rValues)

    class (STRING_LIST_T), intent(in)     :: this
    real (kind=c_float), allocatable      :: rValues(:)

    ! [ LOCALS ]
    class (STRING_LIST_ELEMENT_T), pointer    :: current => null()
    integer (kind=c_int)                      :: iStat
    integer (kind=c_int)                      :: iIndex

    allocate( rValues( 1:this%count ), stat=iStat )
    if (iStat /= 0)  call die("Failed to allocate memory for list conversion", __FILE__, __LINE__)

    current => this%first
    iIndex = 0

    do while ( associated( current ) .and. iIndex < ubound(rValues,1) )

      iIndex = iIndex + 1
      rValues(iIndex) = asFloat( current%s )

      current => current%next

    enddo

  end function list_return_all_as_float_fn

!--------------------------------------------------------------------------------------------------

  function list_return_all_as_int_fn(this)    result(iValues)

    class (STRING_LIST_T), intent(in)     :: this
    integer (kind=c_int), allocatable     :: iValues(:)

    ! [ LOCALS ]
    class (STRING_LIST_ELEMENT_T), pointer    :: current => null()
    integer (kind=c_int)                      :: iStat
    integer (kind=c_int)                      :: iIndex

    allocate( iValues(this%count ), stat=iStat )
    if (iStat /= 0)  call die("Failed to allocate memory for list conversion", __FILE__, __LINE__)

    current => this%first
    iIndex = 0

    do while ( associated( current ) .and. iIndex < ubound(iValues,1) )

      iIndex = iIndex + 1

      iValues(iIndex) = asInt( current%s )
      
      current => current%next

    enddo

  end function list_return_all_as_int_fn

!--------------------------------------------------------------------------------------------------

  function break_string_into_list_fn(sText1)    result( newList )

    character (len=*), intent(inout)  :: sText1
    type (STRING_LIST_T)              :: newList

    ! [ LOCALS ]
    character ( len=len_trim(sText1) ) :: sTempText

    do

      call chomp( sText1=sText1, sText2=sTempText )

      if (len_trim(sTempText) > 0) then
        call newList%append( trim(sTempText ) )
      else
        exit
      endif

    end do

  end function break_string_into_list_fn

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
      if (iStat /= 0)   call die("Problem allocating memory", __FILE__, __LINE__)

      iResult(1) = -9999

    else
    
      allocate(iResult(iCount), stat=iStat)
      if (iStat /= 0)   call die("Problem allocating memory", __FILE__, __LINE__)

      iResult(1:iCount) = iTempResult(1:iCount)

    endif  
   
  end function list_return_position_of_matching_string_fn






  function list_subset_partial_matches_fn(this, sChar)     result(newList)

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
    
      if ( (current%s .strequal. sChar) .or. (current%s .contains. sChar) ) then

        iCount = iCount + 1
        call newList%append(current%s)

      endif

      current => current%next

    enddo  

  end function list_subset_partial_matches_fn



  subroutine list_items_deallocate_all_sub(this)

    class (STRING_LIST_T), intent(inout) :: this
    
    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iStat
    type (STRING_LIST_ELEMENT_T), pointer :: current => null()
    type (STRING_LIST_ELEMENT_T), pointer :: toremove => null()

    if ( associated(this%first) ) then

      current => this%first

      do while ( associated(current) )

        toremove => current
        current => current%next

        deallocate(toremove%s)

      enddo  

    endif


  end subroutine list_items_deallocate_all_sub


end module string_list