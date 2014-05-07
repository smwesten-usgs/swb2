module string_list

  use iso_c_binding, only : c_int
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

    generic :: append => list_append_string_sub, &
                         list_append_int_sub
    generic :: get => list_get_value_at_index_fn
    generic :: print => list_print_sub
    generic :: which => list_return_position_of_matching_string_fn
    generic :: countmatching => list_return_count_of_matching_string_fn
    generic :: deallocate => list_items_deallocate_all_sub

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
    integer (kind=c_int)                     :: iStat

    allocate(pNewElement, stat=iStat)  
    call assert(iStat == 0, "There was a problem allocating memory for a new string list element", &
      __FILE__, __LINE__)

    pNewElement%s    = sText
    pNewElement%next => null()

    if (associated( this%last ) ) then

      this%last%next => pNewElement
      this%last      => pNewElement

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

      do while ( associated( current%next) )

        iCount = iCount + 1

        if (iCount == iIndex)  exit

        current => current%next

      enddo
    
      sText = current%s

    else

      sText = "<NA>"

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

    if (present(iLU) ) then
      iLU_ = iLU
    else
      iLU_ = OUTPUT_UNIT
    endif
      
    current => this%first

    do while ( associated( current ) )

      write(iLU_, fmt="(a)") current%s

      current => current%next

    enddo

  end subroutine list_print_sub

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

    iCount = 0

    current => this%first

    do while ( associated(current) )
      iRetval = index(string = current%s, &
                      substring = sChar)

      if (iRetval /= 0)  then

        iCount = iCount + 1
        iTempResult(iCount) = iIndex

      endif

      current => current%next

    enddo  

    if (iCount == 0) then

      allocate(iResult(1), stat=iStat)
      iResult(1) = -9999

    else
    
      allocate(iResult(iCount), stat=iStat)
      iResult(1:iCount) = iTempResult(1:iCount)

    endif  

    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__)

  end function list_return_position_of_matching_string_fn



  function list_return_count_of_matching_string_fn(this, sChar)     result(iCount)

    class (STRING_LIST_T), intent(in)                    :: this
    character (len=*), intent(in)                        :: sChar
    integer (kind=c_int)                                 :: iCount

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iRetVal
    integer (kind=c_int), dimension(this%count) :: iTempResult
    type (STRING_LIST_ELEMENT_T), pointer   :: current => null()

    iCount = 0

    current => this%first

    do while ( associated(current) )
      iRetval = index(string = current%s, &
                      substring = sChar)

      if (iRetval /= 0)  then

        iCount = iCount + 1
        iTempResult(iCount) = iIndex

      endif

      current => current%next

    enddo  

  end function list_return_count_of_matching_string_fn








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