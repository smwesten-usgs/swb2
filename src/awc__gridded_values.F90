!> @file
!>  Contains a single module, awc_gridded_values, which
!>  populates awc by reading available water content directly from a gridded input.


!>  Populate available water content by reading in
!>  the available water content from a gridded data source.

module awc__gridded_values

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use exceptions

  implicit none

  private

  public :: awc_gridded_values_initialize, awc_gridded_values_read, pAWC_GRID

  type (DATA_CATALOG_ENTRY_T), pointer :: pAWC_GRID

contains

  subroutine awc_gridded_values_read( )

    ! locate the data structure associated with the gridded available water content entries
    pAWC_GRID => DAT%find("AVAILABLE_WATER_CONTENT")
    if ( .not. associated(pAWC_GRID) ) then
        call warn("An AVAILABLE_WATER_CONTENT grid must be supplied in order to make use of this option.", &
           __FILE__, __LINE__)

    else

      call pAWC_GRID%getvalues( )

    end if

  end subroutine awc_gridded_values_read

!--------------------------------------------------------------------------------------------------

  subroutine awc_gridded_values_initialize( lActive, fAWC )

    logical (c_bool), intent(in)     :: lActive(:,:)
    real (c_float), intent(inout)    :: fAWC(:)

    if ( associated( pAWC_GRID ) ) then

      fAWC = pack( pAWC_GRID%pGrdBase%rData, lActive )

    else

      fAWC = -9999.0

    endif

  end subroutine awc_gridded_values_initialize

end module awc__gridded_values
