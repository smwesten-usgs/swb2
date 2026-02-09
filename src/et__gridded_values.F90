!> @file
!>  Contains a single module, et_gridded_values, which
!>  populates the reference et by applying the monthly value obtained from a reference grid.


!>  Populate potential evapotranspiration by substituting in
!>  the daily average ET from a gridded data source.

module et__gridded_values

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use exceptions
  use simulation_datetime
  use fstring
  use fstring_list

  implicit none

  private

  public :: et_gridded_values_initialize, et_gridded_values_calculate, pET_GRID

  type (DATA_CATALOG_ENTRY_T), pointer :: pET_GRID

contains

  subroutine et_gridded_values_initialize( lActive )

    logical (c_bool), intent(in)     :: lActive(:,:)

    ! [ LOCALS ]
    integer (c_int)                 :: iStat
    type (FSTRING_LIST_T)                 :: slString
    integer (c_int)                 :: iIndex


    ! locate the data structure associated with the gridded rainfall zone entries
    pET_GRID => DAT%find("POTENTIAL_ET")
    if ( .not. associated(pET_GRID) ) then
      pET_GRID => DAT%find("REFERENCE_ET0")
      if (.not. associated(pET_GRID) ) &
        call die("A POTENTIAL_ET or REFERENCE_ET0 grid must be supplied in order to make"   &
                 //" use of this option.", __FILE__, __LINE__)

    endif

  end subroutine et_gridded_values_initialize

!--------------------------------------------------------------------------------------------------

  subroutine et_gridded_values_calculate()

    associate ( dt => SIM_DT%curr )

      call pET_GRID%getvalues( dt )

    end associate

  end subroutine et_gridded_values_calculate

end module et__gridded_values
