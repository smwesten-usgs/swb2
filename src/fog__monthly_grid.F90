module fog__monthly_grid

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use simulation_datetime
  use strings
  use string_list

  implicit none

  private

  public :: fog_monthly_grid_initialize, fog_monthly_grid_calculate, pFOG_ZONE

  type (DATA_CATALOG_ENTRY_T), pointer :: pFOG_ZONE
  integer (kind=c_int), allocatable    :: FOG_ZONE_ID(:)                 

contains

  subroutine fog_monthly_grid_initialize( lActive )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    type (STRING_LIST_T)                 :: slString
    integer (kind=c_int)                 :: iIndex 


    ! locate the data structure associated with the gridded rainfall zone entries
    pFOG_ZONE => DAT%find("FOG_ZONE")
    if ( .not. associated(pFOG_ZONE) ) &
        call die("A FOG_ZONE grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    allocate( FOG_ZONE_ID( count (lActive ) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    FOG_ZONE_ID = pack( pFOG_ZONE%pGrdBase%iData, lActive )

    ! look up the name of the fragments file in the control file dictionary
!    call CF_DICT%get_values( sKey="FRAGMENTS_DAILY_FILE", slString=slString )

!    call read_daily_fragments( slString%get(1) )


  end subroutine fog_monthly_grid_initialize

!--------------------------------------------------------------------------------------------------

  subroutine fog_monthly_grid_calculate()

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
  
!      call pMONTHLY_FOG_FRACTION%getvalues( iMonth, iDay, iYear, iJulianDay )

    end associate

  end subroutine fog_monthly_grid_calculate

end module fog__monthly_grid
