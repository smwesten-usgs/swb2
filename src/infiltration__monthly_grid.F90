!> @file
!! Contains the module \ref infiltration__monthly_grid.

!>
!!  Module \ref infiltration__monthly_grid
!!  provides support for estimating fog drip given a gridded map
!!  of RUNOFF_ZONE, and a table containing monthly
!!  fog factors. 
module infiltration__monthly_grid

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

  public :: infiltration_monthly_grid_initialize, infiltration_monthly_grid_calculate

  type (DATA_CATALOG_ENTRY_T), pointer :: pRUNOFF_ZONE
  integer (kind=c_int), allocatable    :: RUNOFF_ZONE(:)

contains

  !> Initialize the infiltration grid. 
  !!
  !! Read in a runoff zone grid.
  !!
  subroutine infiltration_monthly_grid_initialize( lActive )

    logical (kind=c_logical), intent(in)   :: lActive(:,:)

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    ! locate the data structure associated with the gridded fog ratio entries
    pRUNOFF_ZONE => DAT%find("RUNOFF_ZONE")
    if ( .not. associated(pRUNOFF_ZONE) ) &
        call die("A RUNOFF_ZONE grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    call pRUNOFF_ZONE%getvalues( )

    allocate RUNOFF_ZONE( lActive, stat=iStat )

    RUNOFF_ZONE = pack( pRUNOFF_ZONE%iData, lActive )

  end subroutine infiltration_monthly_grid_initialize

!--------------------------------------------------------------------------------------------------

  elemental function infiltration_monthly_grid_calculate( fRainfall )   result( fRunoff )

    real (kind=c_float), intent(inout)        :: fRainfall(:)

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

      if ( .not. associated(pFOG_RATIO) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

      if ( .not. allocated(pFOG_RATIO%pGrdBase%rData) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of unallocated variable", __FILE__, __LINE__)

      call pFOG_RATIO%getvalues( iMonth, iDay, iYear, iJulianDay )

     
    end associate

  end subroutine infiltration_monthly_grid_calculate

!--------------------------------------------------------------------------------------------------

  subroutine read_monthly_runoff_ratios( sFilename )

    character (len=*), intent(in)    :: sFilename

    ! [ LOCALS ]
    character (len=512)   :: sRecord, sSubstring
    integer (kind=c_int)  :: iStat
    integer (kind=c_int)  :: iCount
    integer (kind=c_int)  :: iIndex
    integer (kind=c_int)  :: iNumLines  
    type (ASCII_FILE_T)   :: RUNOFF_RATIO_FILE

    call RUNOFF_RATIO_FILE%open( sFilename = sFilename, &
                  sCommentChars = "#%!", &
                  sDelimiters = "WHITESPACE", &
                  lHasHeader = .false._c_bool )

    iNumLines = RUNOFF_RATIO_FILE%numLines()

    allocate(  RUNOFF_RATIOS( iNumLines ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory for runoff ratio table", &
      __FILE__, __LINE__ )

    iCount = 0

    do 

      ! read in next line of file
      sRecord = RUNOFF_RATIO_FILE%readLine()

      if ( RUNOFF_RATIO_FILE%isEOF() ) exit 

      iCount = iCount + 1

      ! read in date
      call chomp(sRecord, sSubstring, RUNOFF_RATIO_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 ) &
      call die( "Missing date in the monthly runoff ratio file", &
        __FILE__, __LINE__, "Problem occured on line number "//asCharacter(RUNOFF_RATIO_FILE%currentLineNum() ) &
        //" of file "//dquote(sFilename) )

      RUNOFF_RATIOS(iCount)%iMonth = asInt(sSubString)

      ! read in rain gage zone
      call chomp(sRecord, sSubstring, RUNOFF_RATIO_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 ) &
      call die( "Missing rain gage zone number in the daily fragments file", &
        __FILE__, __LINE__, "Problem occured on line number "//asCharacter(RUNOFF_RATIO_FILE%currentLineNum() ) &
        //" of file "//dquote(sFilename) )

      RUNOFF_RATIOS(iCount)%iRainGageZone = asInt(sSubString)

      ! read in fragment set number for this zone
      call chomp(sRecord, sSubstring, RUNOFF_RATIO_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 ) &
      call die( "Missing fragment set number in the daily fragments file", &
        __FILE__, __LINE__, "Problem occured on line number "//asCharacter(RUNOFF_RATIO_FILE%currentLineNum() ) &
        //" of file "//dquote(sFilename) )

      RUNOFF_RATIOS(iCount)%iFragmentSet = asInt(sSubString)
      
      do iIndex = 1, 31

        ! read in fragment for given day of month
        call chomp(sRecord, sSubstring, RUNOFF_RATIO_FILE%sDelimiters )

        if ( len_trim(sSubstring) == 0 ) &
        call die( "Missing fragment value in the daily fragments file", &
          __FILE__, __LINE__, "Problem occured on line number "//asCharacter(RUNOFF_RATIO_FILE%currentLineNum() ) &
          //" of file "//dquote(sFilename) )

        RUNOFF_RATIOS(iCount)%fFragmentValue(iIndex) = asFloat( sSubstring )

      enddo
      
    enddo    

    call LOGS%write("Maximum runoff zone number: "//asCharacter(maxval(RUNOFF_RATIOS%iRainGageZone)), &
      iTab=31, iLinesAfter=1, iLogLevel=LOG_ALL)

  end subroutine read_monthly_runoff_ratios

end module infiltration__monthly_grid
