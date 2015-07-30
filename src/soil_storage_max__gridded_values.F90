!> @file
!>  Contains a single module, soil_storage_max__gridded_values, which
!>  populates soil_storage_max by reading maximum soil storage directly from a gridded input.


!>  Populate maximum soil storage by reading in 
!>  the maximum soil storage from a gridded data source.

module soil_storage_max__gridded_values

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry

  implicit none

  private

  public :: soil_storage_max_gridded_values_initialize

  type (DATA_CATALOG_ENTRY_T), pointer :: pSOIL_STORAGE_MAX_GRID

contains
    
  subroutine soil_storage_max_gridded_values_initialize( lActive, fSoil_Storage_Max )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)
    real (kind=c_float), intent(inout)    :: fSoil_Storage_Max(:)

    ! locate the data structure associated with the gridded rainfall zone entries
    pSOIL_STORAGE_MAX_GRID => DAT%find("SOIL_STORAGE_MAX")
    if ( .not. associated(pSOIL_STORAGE_MAX_GRID) ) &
        call warn("A SOIL_STORAGE_MAX grid must be supplied in order to make use of this option.", &
           __FILE__, __LINE__)

    call pSOIL_STORAGE_MAX_GRID%getvalues( )

    fSoil_Storage_Max = pack( pSOIL_STORAGE_MAX_GRID%pGrdBase%rData, lActive )
    
  end subroutine soil_storage_max_gridded_values_initialize

end module soil_storage_max__gridded_values
