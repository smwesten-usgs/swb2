!> @file
!! Contains the module \ref runoff__gridded_values.

!>
!!  Module \ref runoff__gridded_values
!!  provides support for estimating fog drip given a gridded map
!!  of RUNOFF_ZONE, and a table containing monthly
!!  fog factors. 
module runoff__gridded_values

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_bool
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use datetime
  use dictionary
  use file_operations
  use simulation_datetime
  use strings
  use string_list

  implicit none

  private

  public :: runoff_gridded_values_initialize, runoff_gridded_values_update_ratios

  real (kind=c_float), allocatable           :: RUNOFF_TABLE_VALUES(:,:)
  type ( DATETIME_T ), allocatable           :: RUNOFF_TABLE_DATES(:)

  type (DATA_CATALOG_ENTRY_T), pointer       :: pRUNOFF_ZONE
  integer (kind=c_int), allocatable          :: RUNOFF_ZONE(:)
 
  real (kind=c_float), allocatable, public   :: RUNOFF_RATIOS(:)

 contains

  !> Initialize the infiltration grid. 
  !!
  !! Read in a runoff zone grid.
  !!
  subroutine runoff_gridded_values_initialize( lActive )

    logical (kind=c_bool), intent(in)   :: lActive(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)      :: iStat
    type (STRING_LIST_T)      :: slString

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


    ! look up the name of the fragments file in the control file dictionary
    call CF_DICT%get_values( sKey="RUNOFF_RATIO_MONTHLY_FILE", slString=slString )

    ! use the first entry in the string list slString as the filename to open for
    ! use with the daily fragments routine
    call read_runoff_ratio_table( slString%get(1) )

  end subroutine runoff_gridded_values_initialize

!--------------------------------------------------------------------------------------------------

  subroutine read_runoff_ratio_table( sFilename )

    character (len=*), intent(in)    :: sFilename

    ! [ LOCALS ]
    character (len=65536)   :: sRecord, sSubstring
    integer (kind=c_int)    :: iStat
    integer (kind=c_int)    :: iLineNum
    integer (kind=c_int)    :: iFieldNum
    integer (kind=c_int)    :: iIndex
    integer (kind=c_int)    :: iNumLines  
    integer (kind=c_int)    :: iNumFields
    type (ASCII_FILE_T)     :: RUNOFF_RATIO_FILE    

    call RUNOFF_RATIO_FILE%open( sFilename = sFilename, &
                  sCommentChars = "#%!", &
                  sDelimiters = "WHITESPACE", &
                  lHasHeader = .false._c_bool )

    iNumLines = RUNOFF_RATIO_FILE%numLines()

    ! read in next line of file
    sRecord = RUNOFF_RATIO_FILE%readLine()

    iNumFields = fieldCount( sRecord )

    allocate(  RUNOFF_TABLE_VALUES( iNumLines, iNumFields -1 ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory for runoff ratio table values", &
      __FILE__, __LINE__ )

    allocate(  RUNOFF_TABLE_DATES( iNumLines ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory for runoff ratio table date values vector", &
      __FILE__, __LINE__ )


    iLineNum = 0
    iFieldNum = 0

    do 

      ! read in next line of file
      sRecord = RUNOFF_RATIO_FILE%readLine()

      if ( RUNOFF_RATIO_FILE%isEOF() ) exit 

      iLineNum = iLineNum + 1
      iFieldNum = 0

      ! read in date
      call chomp(sRecord, sSubstring, RUNOFF_RATIO_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 ) &
      call die( "Missing date in the monthly runoff ratio file", &
        __FILE__, __LINE__, "Problem occured on line number "//asCharacter(RUNOFF_RATIO_FILE%currentLineNum() ) &
        //" of file "//dquote(sFilename) )

      call RUNOFF_TABLE_DATES( iLineNum )%parseDate( sSubString ) 

       do iIndex = 2, iNumFields

!         ! read in fragment for given day of month
         call chomp(sRecord, sSubstring, RUNOFF_RATIO_FILE%sDelimiters )

         if ( len_trim(sSubstring) == 0 ) &
           call die( "Missing or corrupt value in the runoff ratio file", &
             __FILE__, __LINE__, "Problem occured on line number "//asCharacter(RUNOFF_RATIO_FILE%currentLineNum() ) &
             //" of file "//dquote(sFilename) )

         RUNOFF_TABLE_VALUES(iLineNum, iIndex - 1 ) = asFloat( sSubstring )

      enddo
      
    enddo    


  end subroutine read_runoff_ratio_table

!--------------------------------------------------------------------------------------------------

  subroutine runoff_gridded_values_update_ratios( )

    ! [ LOCALS ] 
    integer (kind=c_int)  :: iJulianDay
    integer (kind=c_int)  :: iMonth
    integer (kind=c_int)  :: iDay
    integer (kind=c_int)  :: iYear
    integer (kind=c_int)  :: iDaysInMonth
    integer (kind=c_int)  :: iNumDaysFromOrigin
    integer (kind=c_int)  :: iLineNum
    integer (kind=c_int)  :: iFieldNum
    real (kind=c_float)   :: fFactor
    logical (kind=c_bool) :: lMatch
    integer (kind=c_int)  :: iCount

    lMatch = lFALSE
    iCount = 0

    do iLineNum = lbound( RUNOFF_TABLE_DATES, 1), ubound( RUNOFF_TABLE_DATES, 1) - 1

      if (     ( RUNOFF_TABLE_DATES( iLineNum ) <= SIM_DT%curr )   &
        .and.  ( RUNOFF_TABLE_DATES( iLineNum + 1 ) > SIM_DT%curr ) )  then
          
          lMatch = lTRUE
          exit
      endif    

    enddo  

    if ( .not. lMatch )   &
      call die( "Failed to find an appropriate date value in the RUNOFF_RATIO file.", &
        __FILE__, __LINE__ )

    do iFieldNum = lbound(RUNOFF_TABLE_VALUES, 2), ubound(RUNOFF_TABLE_VALUES, 2)

      iCount = iCount + count( RUNOFF_ZONE == iFieldNum )

      where ( RUNOFF_ZONE == iFieldNum )

        RUNOFF_RATIOS = RUNOFF_TABLE_VALUES( iLineNum, iFieldNum )

      end where
      
    enddo    

  end subroutine runoff_gridded_values_update_ratios

end module runoff__gridded_values
