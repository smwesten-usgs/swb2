!> @file
!>  Contains a single module, et_monthly_grid, which
!>  populates the reference et by applying the monthly value obtained from a reference grid.


!>  Populate potential evapotranspiration by substituting in 
!>  the daily average ET from a monthly grid.

module et__monthly_grid

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use simulation_datetime
  use strings
  use string_list

  implicit none

  private

  public :: et_monthly_grid_initialize, et_monthly_grid_calculate, pMONTHLY_ET_GRID

  type (DATA_CATALOG_ENTRY_T), pointer :: pMONTHLY_ET_GRID

contains

  subroutine et_monthly_grid_initialize( lActive )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    type (STRING_LIST_T)                 :: slString
    integer (kind=c_int)                 :: iIndex 


    ! locate the data structure associated with the gridded rainfall zone entries
    pMONTHLY_ET_GRID => DAT%find("POTENTIAL_ET")
    if ( .not. associated(pMONTHLY_ET_GRID) ) &
        call die("A POTENTIAL_ET grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

  end subroutine et_monthly_grid_initialize

!--------------------------------------------------------------------------------------------------

  subroutine et_monthly_grid_calculate()

    ! [ LOCALS ] 
    integer (kind=c_int) :: iJulianDay
    integer (kind=c_int) :: iMonth
    integer (kind=c_int) :: iDay
    integer (kind=c_int) :: iYear
    integer (kind=c_int) :: iDaysInMonth

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear
      iDaysInMonth = SIM_DT%iDaysInMonth
  
      call pMONTHLY_ET_GRID%getvalues( iMonth, iDay, iYear, iJulianDay )

    end associate

  end subroutine et_monthly_grid_calculate

end module et__monthly_grid
