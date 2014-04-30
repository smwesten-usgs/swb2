module data_catalog

  use iso_c_binding, only : c_int, c_float, c_bool, c_double
  implicit none

  private

  public :: DATA_CATALOG_T

  type DATA_CATALOG_T
    type (DATA_CATALOG_ENTRY_T), pointer            :: first => null()
    type (DATA_CATALOG_ENTRY_T), pointer            :: last  => null()
    integer (kind=c_int)                       :: count = 0
  contains

    procedure :: catalog_add_entry_sub
    procedure :: catalog_delete_entry_sub
    procedure :: catalog_find_key_fn
    procedure :: catalog_print_sub

    generic :: add => catalog_add_entry_sub
    generic :: delete => catalog_delete_entry_sub
    generic :: find => catalog_find_key_fn
    generic :: print => catalog_print_sub

  end type DATA_CATALOG_T

  type (DATA_CATALOG_T) :: DAT


contains

!--------------------------------------------------------------------------------------------------

  subroutine catalog_add_entry_sub( this, key, data )

    class (DATA_CATALOG_T)                :: this
    character (len=*), intent(in)         :: key
    type (DATA_CATALOG_ENTRY_T), pointer  :: data

    if (this%count == 0 ) then

      if (associated (data) ) then
        this%last  => null()
        this%first => data
        data%next      => null()
        data%previous  => null() 
        count = 1

    else if( associated(data) ) then

      call data%setkey( key )
      this%last%next   => data
      data%previous        => this%last
      this%last        => data
      data%next            => null()

      count = count + 1

    endif  

  end subroutine catalog_add_entry_sub

!--------------------------------------------------------------------------------------------------

  subroutine catalog_delete_entry_sub( this, key )

    class (DATA_CATALOG_T)           :: this
    character (len=*), intent(in)    :: key

    type (DATA_CATALOG_ENTRY_T), pointer :: current => null()
    type (DATA_CATALOG_ENTRY_T), pointer :: previous => null()
    type (DATA_CATALOG_ENTRY_T), pointer :: next => null()


    if ( associated (this) ) then

      current => catalog_find_key(this, key)

      if ( associated (current) ) then

        previous => current%previous
        next => current%next

        previous%next => next
        next%previous => previous

        deallocate( current )
        current => null()

        count = count - 1 

      endif

    endif 

  subroutine catalog_delete_entry_sub

!--------------------------------------------------------------------------------------------------

  function catalog_find_key_fn( this, key)   result( data )

    class (DATA_CATALOG_T)                :: this
    character (len=*), intent(in)         :: key
    type (DATA_CATALOG_ENTRY_T), pointer  :: data => null()

    if ( associated( this%first ) ) then

      data => this%first

      do while ( associated(data) )

        if ( data%sKeyword .eq. key ) exit
        data => data%next

      enddo

    endif

  end function catalog_find_key_fn

!--------------------------------------------------------------------------------------------------

  subroutine catalog_print_sub(this)

    class (DATA_CATALOG_T)           :: this

      
    type (DATA_CATALOG_ENTRY_T), pointer :: current

    if (associated( this%first ) ) then

      current => this%first

      do while ( associated(current) )
  
        call current%print()

        current => current%next

      enddo  

    endif  

  end subroutine catalog_print_sub


!--------------------------------------------------------------------------------------------------


!--------------------------------------------------------------------------------------------------



end module data_catalog