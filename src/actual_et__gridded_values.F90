!> @file
!>  Contains a single module, actual_et__gridded_values, which
!>  populates the reference et by applying the monthly value obtained from a reference grid.


!>  Populate actual evapotranspiration by substituting in
!>  the daily average actual ET from a gridded data source.

module actual_et__gridded_values

  use iso_c_binding, only             : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use datetime, only                  : DATETIME_T
  use exceptions
  use simulation_datetime, only       : SIM_DT
  use fstring
  use fstring_list

  implicit none

  private

  public :: actual_et_gridded_values_initialize,                               &
            actual_et_gridded_values_calculate,                                &
            ACTUAL_ET

  type (DATA_CATALOG_ENTRY_T), pointer :: pAET_GRID

  real ( c_float), allocatable    :: ACTUAL_ET(:)

  type ( DATETIME_T )                  :: DATE_OF_LAST_RETRIEVAL

contains

  subroutine actual_et_gridded_values_initialize( is_cell_active )

    logical (c_bool), intent(in)     :: is_cell_active(:,:)

    ! [ LOCALS ]
    integer (c_int)                 :: iStat
    type (FSTRING_LIST_T)                 :: slString
    integer (c_int)                 :: iIndex

    ! locate the data structure associated with the gridded actual_et entries
    pAET_GRID => DAT%find("ACTUAL_ET")
    if (.not. associated(pAET_GRID) ) &
      call die("A set of ACTUAL_ET grids must be supplied in order to make"   &
               //" use of this option.", __FILE__, __LINE__)

    allocate( ACTUAL_ET( count( is_cell_active ) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory for ACTUAL_ET vector.",  &
      __FILE__, __LINE__)

  end subroutine actual_et_gridded_values_initialize

!--------------------------------------------------------------------------------------------------

  subroutine actual_et_gridded_values_calculate( is_cell_active )

    logical (c_bool), intent(in)     :: is_cell_active(:,:)

    if ( .not. DATE_OF_LAST_RETRIEVAL == SIM_DT%curr ) then

      call pAET_GRID%getvalues( SIM_DT%curr )
      DATE_OF_LAST_RETRIEVAL = SIM_DT%curr
      ACTUAL_ET = pack( pAET_GRID%pGrdBase%rData, is_cell_active)

    endif

  end subroutine actual_et_gridded_values_calculate

end module actual_et__gridded_values
