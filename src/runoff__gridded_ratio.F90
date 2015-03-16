!> @file
!! Contains the module \ref runoff__gridded_ratio.

!>
!!  Module \ref runoff__gridded_ratio
!!  provides support for estimating fog drip given a gridded map
!!  of RUNOFF_ZONE, and a table containing monthly
!!  fog factors. 
module runoff__gridded_ratio

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_bool
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use dictionary
  use file_operations
  use netcdf4_support
  use simulation_datetime
  use strings
  use string_list

  implicit none

  private

  public :: runoff_gridded_ratio_initialize, runoff_gridded_ratio_calculate

  type (DATA_CATALOG_ENTRY_T), pointer :: pRUNOFF_ZONE
  integer (kind=c_int), allocatable    :: RUNOFF_ZONE(:)

  type (DATA_CATALOG_ENTRY_T), pointer :: pRUNOFF_RATIOS
  integer (kind=c_int), allocatable    :: RUNOFF_RATIOS(:)

contains

  !> Initialize the infiltration grid. 
  !!
  !! Read in a runoff zone grid.
  !!
  subroutine runoff_gridded_ratio_initialize( lActive )

    logical (kind=c_bool), intent(in)   :: lActive(:,:)

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    ! locate the data structure associated with the gridded fog ratio entries
    pRUNOFF_ZONE => DAT%find("RUNOFF_ZONE")
    if ( .not. associated(pRUNOFF_ZONE) ) &
        call die("A RUNOFF_ZONE grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    call pRUNOFF_ZONE%getvalues( )

    allocate ( RUNOFF_ZONE( count( lActive ) ), stat=iStat )
    call assert(iStat==0, "Failed to allocate memory for the RUNOFF_ZONE variable", __FILE__, __LINE__)

    RUNOFF_ZONE = pack( pRUNOFF_ZONE%pGrdBase%iData, lActive )

    allocate ( RUNOFF_RATIOS( count( lActive ) ), stat=iStat)
    call assert(iStat==0, "Failed to allocate memory for the RUNOFF_RATIOS variable", __FILE__, __LINE__)

  end subroutine runoff_gridded_ratio_initialize

!--------------------------------------------------------------------------------------------------

  subroutine runoff_gridded_ratio_calculate( fRainfall, fRunoff, lActive )

    real (kind=c_float), intent(inout)        :: fRainfall(:)
    real (kind=c_float), intent(inout)        :: fRunoff(:)
    logical (kind=c_bool), intent(in)      :: lActive(:,:)

    ! [ LOCALS ] 
    integer (kind=c_int) :: iJulianDay
    integer (kind=c_int) :: iMonth
    integer (kind=c_int) :: iDay
    integer (kind=c_int) :: iYear
    integer (kind=c_int) :: iDaysInMonth
    integer (kind=c_int) :: iNumDaysFromOrigin
    integer (kind=c_int) :: iIndex
    real (kind=c_float)  :: fFactor

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear
      iDaysInMonth = SIM_DT%iDaysInMonth
      iNumDaysFromOrigin = SIM_DT%iNumDaysFromOrigin

      if ( .not. associated(pRUNOFF_RATIOS) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

      if ( .not. allocated(pRUNOFF_RATIOS%pGrdBase%rData) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of unallocated variable", __FILE__, __LINE__)

      call pRUNOFF_RATIOS%getvalues( iMonth, iDay, iYear, iJulianDay )

    end associate

    RUNOFF_RATIOS = pack( pRUNOFF_RATIOS%pGrdBase%rData, lActive ) 

  end subroutine runoff_gridded_ratio_calculate

end module runoff__gridded_ratio
