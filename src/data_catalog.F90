module data_catalog

  use iso_c_binding, only : c_int, c_float, c_bool, c_double
  use data_catalog_entry
  implicit none

  private

  public :: DATA_CATALOG_T

  type DATA_CATALOG_T
    type (DATA_CATALOG_ENTRY_T), pointer            :: first => null()
    type (DATA_CATALOG_ENTRY_T), pointer            :: last  => null()
    integer (kind=c_int)                            :: count = 0
  contains

    procedure :: catalog_add_entry_sub
    procedure :: catalog_delete_entry_sub
    procedure :: catalog_find_key_fn
    procedure :: catalog_get_entry_at_index_fn
    procedure :: catalog_print_sub
    procedure :: catalog_set_all_PROJ4_string_sub
    procedure :: catalog_set_all_start_year_sub
    procedure :: catalog_set_all_end_year_sub

    generic :: add => catalog_add_entry_sub
    generic :: delete => catalog_delete_entry_sub
    generic :: find => catalog_find_key_fn
    generic :: get => catalog_get_entry_at_index_fn
    generic :: print => catalog_print_sub
    generic :: set_PROJ4 => catalog_set_all_PROJ4_string_sub
    generic :: set_EndYear => catalog_set_all_end_year_sub
    generic :: set_StartYear => catalog_set_all_start_year_sub
 
  end type DATA_CATALOG_T

  public :: DAT

  ! DAT is a global to hold data catalog entries
  type (DATA_CATALOG_T) :: DAT

contains

!--------------------------------------------------------------------------------------------------

  subroutine catalog_set_all_start_year_sub( this, iStartYear )


    class (DATA_CATALOG_T)              :: this
    integer (kind=c_int), intent(in)    :: iStartYear
  
    ! [ LOCALS ]
    type (DATA_CATALOG_ENTRY_T), pointer :: current

    if (associated( this%first ) ) then

      current => this%first

      do while ( associated(current) )

        current%iStartYear = iStartYear

        current => current%next

      enddo

    endif

  end subroutine catalog_set_all_start_year_sub

!--------------------------------------------------------------------------------------------------

  subroutine catalog_set_all_end_year_sub( this, iEndYear )


    class (DATA_CATALOG_T)              :: this
    integer (kind=c_int), intent(in)    :: iEndYear
  
    ! [ LOCALS ]
    type (DATA_CATALOG_ENTRY_T), pointer :: current

    if (associated( this%first ) ) then

      current => this%first

      do while ( associated(current) )

        current%iEndYear = iEndYear

        current => current%next

      enddo

    endif

  end subroutine catalog_set_all_end_year_sub

!--------------------------------------------------------------------------------------------------

  subroutine catalog_set_all_PROJ4_string_sub( this, PROJ4_string )


    class (DATA_CATALOG_T)           :: this
    character (len=*), intent(in)    :: PROJ4_string
  
    ! [ LOCALS ]
    type (DATA_CATALOG_ENTRY_T), pointer :: current

    if (associated( this%first ) ) then

      current => this%first

      do while ( associated(current) )

        call current%set_PROJ4( PROJ4_string )

        current => current%next

      enddo

    endif



  end subroutine catalog_set_all_PROJ4_string_sub

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
        this%count = 1

      endif

    else if( associated(data) ) then

      call data%setkey( key )
      this%last%next   => data
      data%previous        => this%last
      this%last        => data
      data%next            => null()

      this%count = this%count + 1

    endif



  end subroutine catalog_add_entry_sub

!--------------------------------------------------------------------------------------------------

  subroutine catalog_delete_entry_sub( this, key )

    class (DATA_CATALOG_T)           :: this
    character (len=*), intent(in)    :: key

    ! [ LOCALS ]
    type (DATA_CATALOG_ENTRY_T), pointer :: current => null()
    type (DATA_CATALOG_ENTRY_T), pointer :: previous => null()
    type (DATA_CATALOG_ENTRY_T), pointer :: next => null()


    current => catalog_find_key_fn(this, key)

    if ( associated (current) ) then

      previous => current%previous
      next => current%next

      previous%next => next
      next%previous => previous

      deallocate( current )
      current => null()

      this%count = this%count - 1

    endif

  end subroutine catalog_delete_entry_sub

!--------------------------------------------------------------------------------------------------

  function catalog_get_entry_at_index_fn(this, index)    result(data)

    class (DATA_CATALOG_T)                :: this
    integer (kind=c_int), intent(in)      :: index
    type (DATA_CATALOG_ENTRY_T), pointer  :: data

    ! [ LOCALS ]
    integer (kind=c_int) :: iCount

    data => null()
    iCount = 0

    if ( associated( this%first ) ) then

      data => this%first

      do while ( associated(data) )

        iCount = iCount + 1

        if ( iCount == index ) exit
        data => data%next

      enddo

    endif

  end function catalog_get_entry_at_index_fn

!--------------------------------------------------------------------------------------------------

  function catalog_find_key_fn( this, key)   result( data )

    class (DATA_CATALOG_T)                :: this
    character (len=*), intent(in)         :: key
    type (DATA_CATALOG_ENTRY_T), pointer  :: data

    data => null()

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

        call current%dump_data_structure()

        current => current%next

      enddo

    endif

  end subroutine catalog_print_sub


!--------------------------------------------------------------------------------------------------


!--------------------------------------------------------------------------------------------------



end module data_catalog
