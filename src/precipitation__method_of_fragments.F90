module precipitation__method_of_fragments

  use iso_c_binding
  use data_catalog
  use data_catalog_entry
  use dictionary
  use exceptions
  use file_operations
  use parameters
  use strings
  use string_list
  use simulation_datetime
  implicit none

  private

  integer (kind=c_int), allocatable, public :: RAIN_GAGE_ID(:)
  real (kind=c_float), allocatable, public  :: FRAGMENT_VALUE(:)

  type, public :: FRAGMENTS_T
    integer (kind=c_int) :: iMonth
    integer (kind=c_int) :: iRainGageZone
    integer (kind=c_int) :: iFragmentSet
    real (kind=c_float)  :: fFragmentValue(31)
  end type FRAGMENTS_T

  type, public :: PTR_FRAGMENTS_T
    type (FRAGMENTS_T), pointer  :: pFragment => null()
  end type PTR_FRAGMENTS_T

  type, public :: FRAGMENTS_SET_T
    integer (kind=c_int) :: iRainGageZone
    integer (kind=c_int) :: iNumberOfFragments(12)
    integer (kind=c_int) :: iStartRecord(12)
  end type FRAGMENTS_SET_T

  
  type (FRAGMENTS_T), allocatable, target, public            :: FRAGMENTS(:)

  type (PTR_FRAGMENTS_T), allocatable                :: CURRENT_FRAGMENTS(:)
  
  type (FRAGMENTS_SET_T), allocatable, public        :: FRAGMENTS_SETS(:)

  public :: initialize_precipitation_method_of_fragments, read_daily_fragments
  public :: calculate_precipitation_method_of_fragments

contains

  ! current concept: use normal template mechanism to bring in month-year data grids 
  !                      (i.e. PRCP object contains MONTHLY sums for given year)
  ! 
  !  read fragments in as a table
  !  randomly choose a fragment
  !  calculate daily precip by multiplying daily fragment value by month-year value 

  subroutine initialize_precipitation_method_of_fragments( lActive )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    type (DATA_CATALOG_ENTRY_T), pointer :: pRAINFALL_ZONE
    type (STRING_LIST_T)                 :: slString
    integer (kind=c_int)                 :: iIndex
    integer (kind=c_int)                 :: iMaxRainZones
    integer (kind=c_int)                 :: iRainGageZone    
    integer (kind=c_int)                 :: iFragmentChunk
    integer (kind=c_int)                 :: iMonth
    integer (kind=c_int)                 :: iCount
    character (len=10)   :: sBuf0
    character (len=10)   :: sBuf1
    character (len=12)   :: sBuf2
    character (len=10)   :: sBuf3
    character (len=52)   :: sBuf4


    ! locate the data structure associated with the gridded rainfall zone entries
    pRAINFALL_ZONE => DAT%find("RAINFALL_ZONE")
    if ( .not. associated(pRAINFALL_ZONE) ) &
        call die("A RAINFALL_ZONE grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    allocate( RAIN_GAGE_ID( count(lActive) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( FRAGMENT_VALUE( count(lActive) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    call pRAINFALL_ZONE%getvalues()

    RAIN_GAGE_ID = pack( pRAINFALL_ZONE%pGrdBase%iData, lActive )

    ! look up the name of the fragments file in the control file dictionary
    call CF_DICT%get_values( sKey="FRAGMENTS_DAILY_FILE", slString=slString )

    call read_daily_fragments( slString%get(1) )

    !> Now the fragments file is in memory. Now create an ancillary data structure
    !> to keep track of which records correspond to various rain zones

    iMaxRainZones = maxval(FRAGMENTS%iRainGageZone)

    allocate ( FRAGMENTS_SETS( iMaxRainZones ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate (CURRENT_FRAGMENTS( iMaxRainZones ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    iCount = 0 

    ! now iterate through *all* fragments, keeping track of the starting record for each new rainfall gage #
    do iIndex = 1, ubound( FRAGMENTS, 1 ) - 1
 
      iRainGageZone = FRAGMENTS(iIndex)%iRainGageZone
      iMonth = FRAGMENTS(iIndex)%iMonth

      iCount = iCount + 1

      if (FRAGMENTS(iIndex + 1)%iFragmentSet == 1) then 
        FRAGMENTS_SETS( iRainGageZone )%iNumberOfFragments(iMonth) = iCount
        iCount = 0
      elseif ( FRAGMENTS(iIndex)%iFragmentSet == 1) then
        FRAGMENTS_SETS( iRainGageZone )%iRainGageZone = iRainGageZone
        FRAGMENTS_SETS( iRainGageZone )%iStartRecord(iMonth) = iIndex
      endif  

    enddo  

    ! This needs to be in place to fill in the data value for the last record.
    FRAGMENTS_SETS( iRainGageZone )%iNumberOfFragments(iMonth) = iCount


    call LOGS%write("### Summary of fragment sets in memory ###", &
       iLogLevel=LOG_ALL, iLinesBefore=1, iLinesAfter=1 )
    call LOGS%write("gage number | month      | start index  | num records ")
    call LOGS%write("----------- | ---------- | ------------ | ------------")
    do iIndex=1, ubound( FRAGMENTS_SETS, 1)
      do iMonth=1,12
        write (sBuf0, fmt="(i10)") iIndex
        write (sBuf1, fmt="(i10)") iMonth
        write (sBuf2, fmt="(i12)") FRAGMENTS_SETS(iIndex)%iStartRecord(iMonth)
        write (sBuf3, fmt="(i10)") FRAGMENTS_SETS(iIndex)%iNumberOfFragments(iMonth)
        write (sBuf4, fmt="(a10,'  | ', a10,' | ', a12,' | ',a10)") adjustl(sBuf0), adjustl(sBuf1), adjustl(sBuf2), adjustl(sBuf3)
        call LOGS%write( sBuf4 )
      enddo  
    end do

  end subroutine initialize_precipitation_method_of_fragments

!--------------------------------------------------------------------------------------------------

  subroutine read_daily_fragments( sFilename )

    character (len=*), intent(in)    :: sFilename

    ! [ LOCALS ]
    character (len=512)   :: sRecord, sSubstring
    integer (kind=c_int)  :: iStat
    integer (kind=c_int)  :: iCount
    integer (kind=c_int)  :: iIndex
    integer (kind=c_int)  :: iNumLines  
    type (ASCII_FILE_T)   :: FRAGMENTS_FILE


    call FRAGMENTS_FILE%open( sFilename = sFilename, &
                  sCommentChars = "#%!", &
                  sDelimiters = "WHITESPACE", &
                  lHasHeader = .false._c_bool )

    iNumLines = FRAGMENTS_FILE%numLines()

    allocate(  FRAGMENTS( iNumLines ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory for fragments table", &
      __FILE__, __LINE__ )

    iCount = 0

    do 

      ! read in next line of file
      sRecord = FRAGMENTS_FILE%readLine()

      if ( FRAGMENTS_FILE%isEOF() ) exit 

      iCount = iCount + 1

      ! read in month number
      call chomp(sRecord, sSubstring, FRAGMENTS_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 ) &
      call die( "Missing month number in the daily fragments file", &
        __FILE__, __LINE__, "Problem occured on line number "//asCharacter(FRAGMENTS_FILE%currentLineNum() ) &
        //" of file "//dquote(sFilename) )

      FRAGMENTS(iCount)%iMonth = asInt(sSubString)

      ! read in rain gage zone
      call chomp(sRecord, sSubstring, FRAGMENTS_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 ) &
      call die( "Missing rain gage zone number in the daily fragments file", &
        __FILE__, __LINE__, "Problem occured on line number "//asCharacter(FRAGMENTS_FILE%currentLineNum() ) &
        //" of file "//dquote(sFilename) )

      FRAGMENTS(iCount)%iRainGageZone = asInt(sSubString)

      ! read in fragment set number for this zone
      call chomp(sRecord, sSubstring, FRAGMENTS_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 ) &
      call die( "Missing fragment set number in the daily fragments file", &
        __FILE__, __LINE__, "Problem occured on line number "//asCharacter(FRAGMENTS_FILE%currentLineNum() ) &
        //" of file "//dquote(sFilename) )

      FRAGMENTS(iCount)%iFragmentSet = asInt(sSubString)
      
      do iIndex = 1, 31

        ! read in fragment for given day of month
        call chomp(sRecord, sSubstring, FRAGMENTS_FILE%sDelimiters )

        if ( len_trim(sSubstring) == 0 ) &
        call die( "Missing fragment value in the daily fragments file", &
          __FILE__, __LINE__, "Problem occured on line number "//asCharacter(FRAGMENTS_FILE%currentLineNum() ) &
          //" of file "//dquote(sFilename) )

        FRAGMENTS(iCount)%fFragmentValue(iIndex) = asFloat( sSubstring )

      enddo
      
    enddo    

    call LOGS%write("Maximum rain gage zone number: "//asCharacter(maxval(FRAGMENTS%iRainGageZone)), &
      iTab=31, iLinesAfter=1, iLogLevel=LOG_ALL)

  end subroutine read_daily_fragments

!--------------------------------------------------------------------------------------------------
  
  subroutine shuffle_fragments()

    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iMaxRainZones
    integer (kind=c_int) :: iMonth
    integer (kind=c_int) :: iDay
    real (kind=c_float), allocatable  :: fRandomNumbers(:)

    integer (kind=c_int) :: iNumberOfFragments
    integer (kind=c_int)  :: iStartRecord
    integer (kind=c_int) :: iEndRecord
    integer (kind=c_int) :: iTargetRecord
    integer (kind=c_int) :: iStat

    iMaxRainZones = maxval(FRAGMENTS%iRainGageZone)
    iMonth = SIM_DT%curr%iMonth
    iDay = SIM_DT%curr%iDay

    allocate (fRandomNumbers(iMaxRainZones), stat=iStat)
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__)

    call random_number( fRandomNumbers )

    do iIndex = 1, iMaxRainZones
 
      iStartRecord = FRAGMENTS_SETS( iIndex )%iStartRecord(iMonth)   
      iNumberOfFragments = FRAGMENTS_SETS(iIndex)%iNumberOfFragments(iMonth)
      iEndRecord = iStartRecord + iNumberOfFragments - 1
      iTargetRecord = iStartRecord + fRandomNumbers(iIndex) * real( iNumberOfFragments - 1)

      call assert (iTargetRecord <= iEndRecord, "Miscalculation in target record: calculated record is past the end", &
        __FILE__, __LINE__ )

      print *, "TargetRecord: ", iTargetRecord

      CURRENT_FRAGMENTS(iIndex)%pFragment => FRAGMENTS( iTargetRecord )

      write(*,fmt="(i5,a,i4,i5,i5,31f8.3)") iIndex,") ", FRAGMENTS( iTargetRecord)%iRainGageZone, FRAGMENTS( iTargetRecord)%iMonth, &
         FRAGMENTS( iTargetRecord)%iFragmentSet, FRAGMENTS( iTargetRecord)%fFragmentValue

      where ( RAIN_GAGE_ID == iIndex )

        FRAGMENT_VALUE = FRAGMENTS( iTargetRecord )%fFragmentValue( iDay )

      endwhere  

    enddo  

    


  end subroutine shuffle_fragments



  subroutine calculate_precipitation_method_of_fragments()

    integer (kind=c_int)              :: iIndex
    integer (kind=c_int)              :: iMaxRainZones
    real (kind=c_float), allocatable  :: fRandomNumbers(:)
    integer (kind=c_int)              :: iStat
    logical (kind=c_bool), save       :: lFirstCall = lTRUE

    integer (kind=c_int) :: iMonth
    integer (kind=c_int) :: iDay

    iMonth = SIM_DT%curr%iMonth
    iDay = SIM_DT%curr%iDay

    iMaxRainZones = maxval(FRAGMENTS%iRainGageZone)

    if ( iDay == 1 .or. lFirstCall ) then
      call shuffle_fragments()
      lFirstCall = lFALSE
    endif  

  end subroutine calculate_precipitation_method_of_fragments

!--------------------------------------------------------------------------------------------------

end module precipitation__method_of_fragments