module string_list

  use iso_c_binding, only : c_int, c_float, c_double
  use constants_and_conversions, only : asInt, asFloat
  use strings
  use exceptions
  implicit none

  private

  public :: STRING_LIST_T

  public :: assignment(=)
  interface assignment(=)

    module procedure :: assign_string_list_to_string_list_sub,   &
                        assign_int_to_string_sub,                &
                        assign_float_to_string_sub,              &
                        assign_double_to_string_sub,             &
                        assign_char_to_string_sub,               &
                        assign_char_array_to_string_sub

  end interface assignment(=)
    

  type STRING_LIST_ELEMENT_T

    character (len=:), allocatable               :: s
    type (STRING_LIST_ELEMENT_T), pointer        :: next => null()

  end type STRING_LIST_ELEMENT_T


  type STRING_LIST_T

    type (STRING_LIST_ELEMENT_T), pointer        :: first      => null()
    type (STRING_LIST_ELEMENT_T), pointer        :: last       => null()
    type (STRING_LIST_ELEMENT_T), pointer        :: current    => null()      
    integer (kind=c_int)                         :: count      = 0

  contains

    procedure :: list_append_string_sub
    procedure :: list_append_int_sub
    procedure :: list_append_float_sub
    procedure :: list_append_double_sub
    procedure :: list_get_all_fn
    procedure :: list_get_first_value_fn
    procedure :: list_get_last_value_fn
    procedure :: list_get_next_value_fn    
    procedure :: list_get_value_at_index_fn
    procedure :: list_get_values_in_range_fn
    procedure :: list_print_sub
    procedure :: list_return_position_of_matching_string_fn
    procedure :: list_return_count_of_matching_string_fn
    procedure :: list_items_deallocate_all_sub
    procedure :: list_return_all_as_float_fn
    procedure :: list_return_all_as_int_fn
    procedure :: list_subset_partial_matches_fn

    generic :: append        => list_append_string_sub,      &
                                list_append_int_sub,         &
                                list_append_float_sub,       &
                                list_append_double_sub
    generic :: getFirst      => list_get_first_value_fn
    generic :: getLast       => list_get_last_value_fn
    generic :: getNext       => list_get_next_value_fn                            
    generic :: get           => list_get_value_at_index_fn,  &
                                list_get_values_in_range_fn 
    generic :: cat           => list_get_all_fn                
    generic :: print         => list_print_sub
    generic :: grep          => list_subset_partial_matches_fn
    generic :: which         => list_return_position_of_matching_string_fn
    generic :: countMatching => list_return_count_of_matching_string_fn
    generic :: deallocate    => list_items_deallocate_all_sub
    generic :: asFloat       => list_return_all_as_float_fn
    generic :: asInt         => list_return_all_as_int_fn

    final :: finalization_sub

  end type STRING_LIST_T

contains


  subroutine finalization_sub( string_list_type )

    type (STRING_LIST_T)   :: string_list_type

    call string_list_type%deallocate()

  end subroutine finalization_sub

!--------------------------------------------------------------------------------------------------
  
  subroutine list_append_int_sub( this, iValue )

    class (STRING_LIST_T), intent(inout)   :: this
    integer (kind=c_int), intent(in)       :: iValue

    call this%list_append_string_sub( asCharacter(iValue) )

  end subroutine list_append_int_sub

!--------------------------------------------------------------------------------------------------

  subroutine list_append_float_sub( this, fValue )

    class (STRING_LIST_T), intent(inout)   :: this
    real (kind=c_float), intent(in)        :: fValue

    call this%list_append_string_sub( asCharacter(fValue) )

  end subroutine list_append_float_sub

!--------------------------------------------------------------------------------------------------

  subroutine list_append_double_sub( this, dpValue )

    class (STRING_LIST_T), intent(inout)    :: this
    real (kind=c_double), intent(in)        :: dpValue

    call this%list_append_string_sub( asCharacter(dpValue) )

  end subroutine list_append_double_sub

!--------------------------------------------------------------------------------------------------
  
  subroutine assign_string_list_to_string_list_sub(slList2, slList1)

    type (STRING_LIST_T), intent(inout)   :: slList2
    type (STRING_LIST_T), intent(inout)   :: slList1

    ! [ LOCALS ]
    integer (kind=c_int)            :: iIndex
    character (len=:), allocatable  :: sText

    if ( slList1%count > 0 ) then

      call list_items_deallocate_all_sub( slList2 )

      do iIndex=1, slList1%count

        sText = slList1%get(iIndex)
        call slList2%append( sText )

      enddo
      
    endif     

  end subroutine assign_string_list_to_string_list_sub

!--------------------------------------------------------------------------------------------------

  subroutine assign_char_to_string_sub( slList, sText )

    type (STRING_LIST_T), intent(inout)    :: slList
    character (len=*), intent(in)          :: sText

    call slList%deallocate()

    if ( len_trim( sText ) > 0 )  call slList%append( sText )

  end subroutine assign_char_to_string_sub

!--------------------------------------------------------------------------------------------------

  subroutine assign_char_array_to_string_sub( slList, sText )

    type (STRING_LIST_T), intent(inout)    :: slList
    character (len=*), intent(in)          :: sText(:)

    ! [ LOCALS ]
    integer (kind=c_int)   :: iIndex

    call slList%deallocate()

    do iIndex = lbound(sText,1), ubound(sText, 1)   

      if ( len_trim( sText( iIndex ) ) > 0 )  call slList%append( sText( iIndex ) )

    enddo

  end subroutine assign_char_array_to_string_sub

!--------------------------------------------------------------------------------------------------

  subroutine assign_int_to_string_sub( slList, iValue )

    type (STRING_LIST_T), intent(inout)    :: slList
    integer (kind=c_int), intent(in)       :: iValue

    call slList%deallocate()
    call slList%append( iValue )

  end subroutine assign_int_to_string_sub

!--------------------------------------------------------------------------------------------------

  subroutine assign_float_to_string_sub( slList, fValue )

    type (STRING_LIST_T), intent(inout)    :: slList
    real (kind=c_float), intent(in)        :: fValue

    call slList%deallocate()
    call slList%append( fValue )

  end subroutine assign_float_to_string_sub

!--------------------------------------------------------------------------------------------------

  subroutine assign_double_to_string_sub( slList, dValue )

    type (STRING_LIST_T), intent(inout)    :: slList
    real (kind=c_double), intent(in)        :: dValue

    call slList%deallocate()
    call slList%append( dValue )

  end subroutine assign_double_to_string_sub

!--------------------------------------------------------------------------------------------------

  subroutine list_append_string_sub( this, sText )

    class (STRING_LIST_T), intent(inout)   :: this
    character (len=*), intent(in)          :: sText

    ! [ LOCALS ] 
    class (STRING_LIST_ELEMENT_T), pointer   :: pNewElement     => null()
    class (STRING_LIST_ELEMENT_T), pointer   :: current         => null()    
    integer (kind=c_int)                     :: iStat

    allocate(pNewElement, stat=iStat)  
    call assert(iStat == 0, "There was a problem allocating memory for a new string list element", &
      __FILE__, __LINE__)

    pNewElement%s    = trim(sText)
    pNewElement%next => null()

    if ( associated( this%first ) ) then

      if (this%count == 0)  call die("Internal logic error: count should *not* be zero in this block", &
             __FILE__, __LINE__)

      this%last%next       => pNewElement
      this%last            => pNewElement
      this%current         => pNewElement
      this%last%next       => null()

    else
    
      this%first   => pNewElement
      this%last    => pNewElement
      this%current => pNewElement

    endif    

    this%count = this%count + 1

  end subroutine list_append_string_sub

!--------------------------------------------------------------------------------------------------

  function list_get_first_value_fn(this)    result( sText )

    class (STRING_LIST_T), intent(inout)     :: this
    character (len=:), allocatable           :: sText

    if ( associated( this%first ) ) then
      sText = this%first%s
      this%current => this%first
    else
      sText = ""
    endif

  end function list_get_first_value_fn

!--------------------------------------------------------------------------------------------------

  function list_get_last_value_fn(this)    result( sText )

    class (STRING_LIST_T), intent(inout)     :: this
    character (len=:), allocatable           :: sText

    if ( associated( this%last ) ) then
      sText = this%last%s
      this%current => this%last
    else
      sText = ""
    endif

  end function list_get_last_value_fn

!--------------------------------------------------------------------------------------------------

  function list_get_next_value_fn(this)    result( sText )

    class (STRING_LIST_T), intent(inout)     :: this
    character (len=:), allocatable           :: sText

    if ( associated( this%current ) )    this%current => this%current%next

    if (associated( this%current) ) then
      sText = this%current%s
    else
      sText = ""
    endif

  end function list_get_next_value_fn

!--------------------------------------------------------------------------------------------------

  function list_get_value_at_index_fn(this, iIndex)   result( sText )

    class (STRING_LIST_T), intent(inout)     :: this
    integer (kind=c_int), intent(in)         :: iIndex
    character (len=:), allocatable           :: sText

    ! [ LOCALS ]
    integer (kind=c_int)                      :: iCount
    type (STRING_LIST_ELEMENT_T), pointer     :: current => null()

    iCount = 0

    current => this%first

    do while ( associated( current ) .and. iCount < this%count )

      iCount = iCount + 1

      if (iCount == iIndex)  exit

      current => current%next

    enddo
    
    if (associated(current) ) then
      sText = current%s
      this%current => current
    else
      sText = "<NA>"
      call warn("Unable to find a pointer associated with index: "//asCharacter(iIndex), &
        __FILE__, __LINE__ )
    endif  
   
  end function list_get_value_at_index_fn

!--------------------------------------------------------------------------------------------------

  !> Iterate over a range of indices; return a delimited string comprised of the values.

  function list_get_values_in_range_fn(this, iStartIndex, iEndIndex, sDelimiter)   result(sText)

    class (STRING_LIST_T), intent(inout)               :: this
    integer (kind=c_int), intent(in)                   :: iStartIndex
    integer (kind=c_int), intent(in)                   :: iEndIndex
    character (len=*), intent(in), optional            :: sDelimiter
    character (len=:), allocatable                     :: sText

    ! [ LOCALS ]
    integer (kind=c_int)                      :: iIndex
    character (len=:), allocatable            :: sDelimiter_
    character (len=1024)                      :: sTempText
    class (STRING_LIST_ELEMENT_T), pointer    :: current => null()

    if ( present( sDelimiter ) ) then
      sDelimiter_ = sDelimiter
    else
      sDelimiter_ = "   "
    endif

    sText = ""

    do iIndex = iStartIndex, iEndIndex
      sTempText = sTempText//trim( this%get( iIndex ) )//sDelimiter_
    enddo
 
    sText = trim( adjustl( sTempText ) )       

  end function list_get_values_in_range_fn

!--------------------------------------------------------------------------------------------------

  !> Iterate over all indices; return a delimited string comprised of the values.
  
  function list_get_all_fn( this, sDelimiter )   result( sText )

    class (STRING_LIST_T), intent(inout)               :: this
    character (len=*), intent(in), optional            :: sDelimiter
    character (len=:), allocatable                     :: sText

    ! [ LOCALS ]
    integer (kind=c_int)                      :: iIndex
    integer (kind=c_int)                      :: iStartIndex
    integer (kind=c_int)                      :: iEndIndex    
    character (len=:), allocatable            :: sDelimiter_
    character (len=1024)                      :: sTempText
    class (STRING_LIST_ELEMENT_T), pointer    :: current => null()


    iStartIndex = 1
    iEndIndex = this%count

    if ( present( sDelimiter ) ) then
      sDelimiter_ = sDelimiter
    else
      sDelimiter_ = "   "
    endif

    do iIndex = iStartIndex, iEndIndex
      sTempText = sTempText//trim( this%get( iIndex ) )//sDelimiter_
    enddo
 
    sText = trim( adjustl( sTempText ) )       

  end function list_get_all_fn

!--------------------------------------------------------------------------------------------------

  subroutine list_print_sub( this, iLU )

    use iso_fortran_env, only : OUTPUT_UNIT

    class (STRING_LIST_T), intent(inout)  :: this
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

      write(iLU_, fmt="(t10,a,t20,a)") "["//asCharacter(iCount)//"] ", current%s

      current => current%next

    enddo

  end subroutine list_print_sub

!--------------------------------------------------------------------------------------------------

  function list_return_all_as_float_fn( this )    result( rValues )

    class (STRING_LIST_T), intent(inout)  :: this
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

  function list_return_all_as_int_fn( this )    result( iValues )

    class (STRING_LIST_T), intent(inout)  :: this
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

  function break_string_into_list_fn( sText, sDelimiters )    result( newList )

    character (len=*), intent(inout)         :: sText
    character (len=*), intent(in), optional  :: sDelimiters
    type (STRING_LIST_T)                     :: newList

    ! [ LOCALS ]
    character ( len=len_trim(sText) ) :: sTempText

    if ( present( sDelimiters) ) then
      
      do

        call chomp( sText1=sText, sText2=sTempText, sDelimiters=sDelimiters )

        if (len_trim(sTempText) > 0) then
          call newList%append( trim(sTempText ) )
        else
          exit
        endif

      end do

    else

      do

        call chomp( sText1=sText, sText2=sTempText )

        if (len_trim(sTempText) > 0) then
          call newList%append( trim(sTempText ) )
        else
          exit
        endif

      end do
    
    endif

  end function break_string_into_list_fn

!--------------------------------------------------------------------------------------------------

  function list_return_count_of_matching_string_fn(this, sChar) result(iCount)

    class (STRING_LIST_T), intent(inout) :: this
    character (len=*), intent(in)        :: sChar
    integer (kind=c_int)                 :: iCount

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

    class (STRING_LIST_T), intent(inout)                 :: this
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

      iResult(1) = 0

    else
    
      allocate(iResult(iCount), stat=iStat)
      if (iStat /= 0)   call die("Problem allocating memory", __FILE__, __LINE__)

      iResult(1:iCount) = iTempResult(1:iCount)

    endif  
   
  end function list_return_position_of_matching_string_fn

!--------------------------------------------------------------------------------------------------

  function list_subset_partial_matches_fn(this, sChar)     result(newList)

    class (STRING_LIST_T), intent(inout)                 :: this
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

  subroutine list_items_deallocate_all_sub(this)

    class (STRING_LIST_T), intent(inout) :: this
    
    ! [ LOCALS ]
    type (STRING_LIST_ELEMENT_T), pointer :: current => null()
    type (STRING_LIST_ELEMENT_T), pointer :: elementtoremove => null()

    if ( associated(this%first) ) then

      current => this%first

      do while ( associated(current) )

        elementtoremove => current
        current => current%next

        elementtoremove%s = ""
        deallocate(elementtoremove)

      enddo  

    endif

    this%count = 0
    this%first => null()
    this%last => null()
    this%current => null()

  end subroutine list_items_deallocate_all_sub


end module string_list