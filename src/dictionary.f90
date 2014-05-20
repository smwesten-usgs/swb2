module dictionary

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use strings
  use string_list
  implicit none

  private

  type, public :: DICT_ENTRY_T

    character (len=:), allocatable            :: key
    type (STRING_LIST_T)                      :: sl
    type (DICT_ENTRY_T), pointer              :: previous   => null()
    type (DICT_ENTRY_T), pointer              :: next       => null()

  contains

    procedure, private   :: add_key_sub
    generic              :: addkey => add_key_sub

    procedure, private   :: add_string_sub
    generic              :: addstring => add_string_sub

  end type DICT_ENTRY_T 


  type, public :: DICT_T

    type (DICT_ENTRY_T), pointer   :: first   => null()
    type (DICT_ENTRY_T), pointer   :: last    => null()
    integer (kind=c_int)           :: count   = 0

  contains
  
    procedure, private   :: get_entry_by_key_fn 
    generic              :: get => get_entry_by_key_fn

    procedure, private   :: add_entry_to_dict_sub
    generic              :: add => add_entry_to_dict_sub

    procedure, private   :: delete_entry_by_key_sub
    generic              :: delete => delete_entry_by_key_sub

    procedure, private   :: print_all_dictionary_entries_sub
    generic              :: print => print_all_dictionary_entries_sub

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

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    
    pDict => this%first

    do while ( associated( pDict ) )

      if ( pDict%key .strequal. sKey )  exit
      
      pDict => pDict%next

    enddo

  end function get_entry_by_key_fn
  
!--------------------------------------------------------------------------------------------------

  subroutine add_entry_to_dict_sub(this, dict_entry)

    class (DICT_T)                :: this
    type (DICT_ENTRY_T), pointer  :: dict_entry  

    if ( associated(dict_entry) ) then

      if ( associated( this%last) ) then

      ! dictionary has at least one entry

        dict_entry%previous  => this%last
        dict_entry%next      => null()
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


  subroutine delete_entry_by_key_sub(this, sKey)

    class (DICT_T)                  :: this
    character (len=*), intent(in)   :: sKey

    ! [ LOCALS ]
    type (DICT_ENTRY_T), pointer   :: pTarget
    type (DICT_ENTRY_T), pointer   :: pTemp

    pTarget => this%get(sKey)

    if ( associated( pTarget ) ) then

      ! first remove object from linked list
      pTemp => pTarget%previous
      pTemp%next => pTarget%next
      pTarget%next%previous => pTemp

      call pTarget%sl%deallocate()

    endif  


  end subroutine delete_entry_by_key_sub



  subroutine print_all_dictionary_entries_sub(this)

    class (DICT_T)    :: this



  end subroutine print_all_dictionary_entries_sub
 
end module dictionary