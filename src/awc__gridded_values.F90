!> @file
!>  Contains a single module, et_gridded_values, which
!>  populates the reference et by applying the monthly value obtained from a reference grid.


!>  Populate potential evapotranspiration by substituting in 
!>  the daily average ET from a gridded data source.

module awc__gridded_values

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry

  implicit none

  private

  public :: awc_gridded_values_initialize, awc_gridded_values_read, pAWC_GRID

  type (DATA_CATALOG_ENTRY_T), pointer :: pAWC_GRID

contains

  subroutine awc_gridded_values_read( )

    ! locate the data structure associated with the gridded rainfall zone entries
    pAWC_GRID => DAT%find("AVAILABLE_WATER_CONTENT")
    if ( .not. associated(pAWC_GRID) ) &
        call warn("An AVAILABLE_WATER_CONTENT grid must be supplied in order to make use of this option.", &
           __FILE__, __LINE__)

    call pAWC_GRID%getvalues( )
    
  end subroutine awc_gridded_values_read

!--------------------------------------------------------------------------------------------------

  subroutine awc_gridded_values_initialize( lActive, fAWC )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)
    real (kind=c_float), intent(inout)    :: fAWC(:)

    fAWC = pack( pAWC_GRID%pGrdBase%rData, lActive )
    
  end subroutine awc_gridded_values_initialize

end module awc__gridded_values
