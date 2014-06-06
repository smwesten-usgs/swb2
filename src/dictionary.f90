module dictionary

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use constants_and_conversions, only : iTINYVAL, fTINYVAL, lTRUE, lFALSE
  use logfiles
  use strings
  use string_list
  use exceptions
  implicit none

  private

  type, public :: DICT_ENTRY_T

    character (len=:), allocatable            :: key
    character (len=:), allocatable            :: secondary_key
    type (STRING_LIST_T)                      :: sl
    type (DICT_ENTRY_T), pointer              :: previous   => null()
    type (DICT_ENTRY_T), pointer              :: next       => null()

  contains

    procedure, private   :: add_key_sub
    generic              :: add_key => add_key_sub

    procedure, private   :: add_string_sub
    generic              :: add_string => add_string_sub

  end type DICT_ENTRY_T 


  type, public :: DICT_T

    type (DICT_ENTRY_T), pointer   :: first   => null()
    type (DICT_ENTRY_T), pointer   :: last    => null()
    integer (kind=c_int)           :: count   = 0

  contains
  
    procedure, private   :: get_entry_by_key_fn 
    generic              :: get_entry => get_entry_by_key_fn

    procedure, private   :: add_entry_to_dict_sub
    generic              :: add_entry => add_entry_to_dict_sub

    procedure, private   :: delete_entry_by_key_sub
    generic              :: delete_entry => delete_entry_by_key_sub

    procedure, private   :: print_all_dictionary_entries_sub
    generic              :: print_all => print_all_dictionary_entries_sub

    procedure, private   :: grep_dictionary_key_names_fn
    generic              :: grep_keys => grep_dictionary_key_names_fn

    procedure, private   :: get_values_as_int_sub
    procedure, private   :: get_values_as_float_sub
    procedure, private   :: get_values_as_string_list_sub
    procedure, private   :: get_values_as_int_given_list_of_keys_sub
    procedure, private   :: get_values_as_float_given_list_of_keys_sub
    generic              :: get_values => get_values_as_int_sub,                       &
                                          get_values_as_float_sub,                     &
                                          get_values_as_string_list_sub,               &
                                          get_values_as_int_given_list_of_keys_sub,    &
                                          get_values_as_float_given_list_of_keys_sub

  end type DICT_T

contains

 !!
 !! This section contains methods bound to the DICT_ENTRY_T class 
 !!

 subroutine add_key_sub(this, sKey)

    class (DICT_ENTRY_T)  :: this
    character (len=*), intent(in)   :: sKey

     this%key = sKey

  end subroutine add_key_sub  

!--------------------------------------------------------------------------------------------------

  subroutine add_string_sub(this, sValue)

    class (DICT_ENTRY_T)  :: this
    character (len=*), intent(in)   :: sValue

    call this%sl%append(sValue)

  end subroutine 

!--------------------------------------------------------------------------------------------------

  !! 
  !! The methods in the section below are bound to the DICT_T type, which is essentially a 
  !! linked list of DICT_ENTRY_T objects 
  !!

  function get_entry_by_key_fn(this, sKey)   result( pDict )

    class (DICT_T)                :: this
    character (len=*), intent(in) :: sKey
    type (DICT_ENTRY_T), pointer  :: pDict
    
    pDict => this%first

    do while ( associated( pDict ) )

      if ( pDict%key .strequal. sKey )  exit
      
      pDict => pDict%next

    enddo

    if (.not. associated( pDict ) )  &
      call warn( sMessage="Failed to find a dictionary entry with a key value of "//dquote(sKey), &
        iLogLevel=LOG_DEBUG )

  end function get_entry_by_key_fn
 



 function grep_dictionary_key_names_fn(this, sKey)   result( slString )

    class (DICT_T)                :: this
    character (len=*), intent(in) :: sKey
    type (STRING_LIST_T)          :: slString

    ! [ LOCALS ]
    type (DICT_ENTRY_T), pointer     :: current
    integer (kind=c_int)             :: iIndex
    
    current => this%first

    do while ( associated( current ) )

      iIndex = index(string=current%key, substring=sKey)

      if ( iIndex > 0 )  call slString%append(current%key)
      
      current => current%next

    enddo

    if ( slString%count == 0 )  &
      call warn(sMessage="Failed to find a dictionary entry that contains the value of " , &     ! //dquote(sKey), &
        sModule=__FILE__, iLine=__LINE__, iLogLevel=LOG_DEBUG, lEcho=lFalse )

  end function grep_dictionary_key_names_fn
 


!--------------------------------------------------------------------------------------------------

  subroutine add_entry_to_dict_sub(this, dict_entry)

    class (DICT_T)                :: this
    type (DICT_ENTRY_T), pointer  :: dict_entry  

    if ( associated(dict_entry) ) then

      this%count = this%count + 1

      if ( associated( this%last) ) then

      ! dictionary has at least one entry

        dict_entry%previous  => this%last
        dict_entry%next      => null()
        this%last%next       => dict_entry
        this%last            => dict_entry

      else  ! this is the first dictionary entry

        this%first => dict_entry
        this%last  => dict_entry
        dict_entry%previous => null()
        dict_entry%next     => null()

      endif

    else

      call warn( "Internal programming error: dictionary entry is null",   &
          __FILE__, __LINE__ )

    endif  

  end subroutine add_entry_to_dict_sub  

!--------------------------------------------------------------------------------------------------

  subroutine delete_entry_by_key_sub(this, sKey)

    class (DICT_T)                  :: this
    character (len=*), intent(in)   :: sKey

    ! [ LOCALS ]
    type (DICT_ENTRY_T), pointer   :: pTarget
    type (DICT_ENTRY_T), pointer   :: pTemp

    pTarget => this%get_entry(sKey)

    if ( associated( pTarget ) ) then

      ! first remove object from linked list
      pTemp => pTarget%previous
      pTemp%next => pTarget%next
      pTarget%next%previous => pTemp

      call pTarget%sl%deallocate()

    endif  


  end subroutine delete_entry_by_key_sub


  subroutine get_values_as_int_sub(this, sKey, iValues)

    class (DICT_T)                                  :: this
    character (len=*), intent(in)                   :: sKey
    integer (kind=c_int), allocatable, intent(out)  :: iValues(:)

    ! [ LOCALS ]
    type (DICT_ENTRY_T), pointer   :: pTarget
    integer (kind=c_int)           :: iStat
    
    pTarget => this%get_entry(sKey)

    if ( associated( pTarget ) ) then

      iValues = pTarget%sl%asInt()

    else

      allocate(iValues(1), stat=iStat)
      call assert(iStat == 0, "Failed to allocate memory to iValues array", &
        __FILE__, __LINE__)

      iValues = iTINYVAL

    endif  


  end subroutine get_values_as_int_sub






 subroutine get_values_as_string_list_sub(this, sKey, slString)

    class (DICT_T)                                  :: this
    character (len=*), intent(in)                   :: sKey
    type (STRING_LIST_T), intent(out)               :: slString

    ! [ LOCALS ]
    type (DICT_ENTRY_T), pointer   :: pTarget
    integer (kind=c_int)           :: iStat
    
    pTarget => this%get_entry(sKey)

    if ( associated( pTarget ) ) then

      slString = pTarget%sl

    else

      call slString%append("<NA>")
      call warn(sMessage="Failed to find a dictionary entry that contains the value of "//dquote(sKey), &
        sModule=__FILE__, iLine=__LINE__, iLogLevel=LOG_DEBUG, lEcho=lFALSE)


    endif  


  end subroutine get_values_as_string_list_sub









  !> Search through keys for a match; return integer values.
  !!
  !! THis routine allows for multiple header values to be supplied 
  !! in the search for the appropriate column.
  !! @param[in]  this  Object of DICT_T class.
  !! @param[in]  slKeys String list containing one or more possible key values to
  !!                    search for.
  !! @param[out] iValues Integer vector of values associated with one of the provided keys.
  subroutine get_values_as_int_given_list_of_keys_sub(this, slKeys, iValues)

    class (DICT_T)                                  :: this
    type (STRING_LIST_T), intent(in)                :: slKeys
    integer (kind=c_int), allocatable, intent(inout)  :: iValues(:)

    ! [ LOCALS ]
    type (DICT_ENTRY_T), pointer   :: pTarget
    integer (kind=c_int)           :: iStat
    integer (kind=c_int)           :: iCount
    
    iCount = 0

    do while ( iCount < slKeys%count )

      iCount = iCount + 1
     
      pTarget => this%get_entry(slKeys%get(iCount) )

      if ( associated( pTarget ) ) exit

    enddo

    if ( associated( pTarget ) ) then

      iValues = pTarget%sl%asInt()

    else

      allocate(iValues(1), stat=iStat)
      call assert(iStat == 0, "Failed to allocate memory to iValues array", &
        __FILE__, __LINE__)

      iValues = iTINYVAL

    endif  


  end subroutine get_values_as_int_given_list_of_keys_sub

  !> Search through keys for a match; return float values.
  !!
  !! THis routine allows for multiple header values to be supplied 
  !! in the search for the appropriate column.
  !! @param[in]  this  Object of DICT_T class.
  !! @param[in]  slKeys String list containing one or more possible key values to
  !!                    search for.
  !! @param[out] fValues Float vector of values associated with one of the provided keys.
  subroutine get_values_as_float_given_list_of_keys_sub(this, slKeys, fValues)

    class (DICT_T)                                  :: this
    type (STRING_LIST_T), intent(in)                :: slKeys
    real (kind=c_float), allocatable, intent(inout)   :: fValues(:)

    ! [ LOCALS ]
    type (DICT_ENTRY_T), pointer   :: pTarget
    integer (kind=c_int)           :: iStat
    integer (kind=c_int)           :: iCount
    
    iCount = 0

    do while ( iCount < slKeys%count )

      iCount = iCount + 1
     
      pTarget => this%get_entry(slKeys%get(iCount) )

      if ( associated( pTarget ) ) exit

    enddo

    if ( associated( pTarget ) ) then

      fValues = pTarget%sl%asFloat()

    else

      allocate(fValues(1), stat=iStat)
      call assert(iStat == 0, "Failed to allocate memory to iValues array", &
        __FILE__, __LINE__)

      fValues = fTINYVAL

    endif  


  end subroutine get_values_as_float_given_list_of_keys_sub


  subroutine get_values_as_float_sub(this, sKey, fValues)

    class (DICT_T)                                    :: this
    character (len=*), intent(in)                     :: sKey
    real (kind=c_float), allocatable, intent(inout)   :: fValues(:)

    ! [ LOCALS ]
    type (DICT_ENTRY_T), pointer   :: pTarget
    integer (kind=c_int)           :: iStat
    
    pTarget => this%get_entry(sKey)

    if ( associated( pTarget ) ) then

      fValues = pTarget%sl%asFloat()

    else

      allocate(fValues(1), stat=iStat)
      call assert(iStat == 0, "Failed to allocate memory to iValues array", &
        __FILE__, __LINE__)
      fValues = fTINYVAL

    endif  

  end subroutine get_values_as_float_sub





  subroutine print_all_dictionary_entries_sub(this)

    class (DICT_T)    :: this

    ! [ LOCALS ]
    type (DICT_ENTRY_T), pointer   :: current
    character (len=512)            :: sTempBuf
    integer (kind=c_int)           :: iCount

    current => this%first
    iCount = 0
   
    do while ( associated( current ) )

      iCount = iCount + 1
      sTempBuf = current%key

      call LOGS%write( asCharacter(iCount)//")  KEY: "//dquote(sTempBuf), iLogLevel=LOG_DEBUG, lEcho=lFALSE, iTab=2 )
      call LOGS%write( " --ENTRIES--:", iTab=5 )
      call current%sl%print( iLU=LOGS%iUnitNum( LOG_DEBUG ) )

      current => current%next

    enddo  

  end subroutine print_all_dictionary_entries_sub
 
end module dictionary