module data_catalog

  use iso_c_binding, only : c_int, c_float, c_bool, c_double
  implicit none

  type CATALOG_T
    type (CATALOG_ENTRY_T), pointer            :: first => null()
    type (CATALOG_ENTRY_T), pointer            :: last  => null()
    integer (kind=c_int)                       :: count = 0
  end type CATALOG_T

contains

  subroutine catalog_create_sub( key, data, pCatalog )

    character (len=*), intent(in)                 :: key
    type (CATALOG_ENTRY_T), pointer               :: data
    type (CATALOG_T), intent(inout), pointer      :: pCatalog

    call assert( .not. associated(pCatalog), &
      "Internal programming error -- pointer to the data catalog is already allocated", __FILE__, __LINE__ )

    allocate(pCatalog)

    call data%setkey( key )
    pCatalog%first => data
    pCatalog%last  => null()
    data%next      => null()

    count = 1

  end subroutine catalog_create_sub

!--------------------------------------------------------------------------------------------------

  subroutine catalog_add_entry_sub( pCatalog, key, data )

    type (CATALOG_T), pointer        :: pCatalog
    character (len=*), intent(in)    :: key
    type (CATALOG_ENTRY_T), pointer  :: data

    if ( associated(data) ) then

      call data%setkey( key )
      pCatalog%last%next => data
      pCatalog%last      => data
      data%next          => null()

    endif  

  end subroutine catalog_add_entry_sub

!--------------------------------------------------------------------------------------------------

  subroutine catalog_delete_entry_sub( pCatalog, key )

  subroutine catalog_delete_entry_sub

!--------------------------------------------------------------------------------------------------

  function catalog_find_key_fn( pCatalog, key)   result(pData)

  end function catalog_find_key_fn

!--------------------------------------------------------------------------------------------------

  subroutine catalog_print_sub(pCatalog)

      type (T_DICT_ENTRY), pointer :: pDict

      type (T_DICT_ENTRY), pointer :: pCurrent

      if (associated( pDict) ) then

        pCurrent => pDict

        do while ( associated(pCurrent) )
    
          call pCurrent%data%print(pCurrent%key)

          pCurrent => pCurrent%next

        enddo  

      endif  

    end subroutine dict_print


!--------------------------------------------------------------------------------------------------


!--------------------------------------------------------------------------------------------------



end module data_catalog