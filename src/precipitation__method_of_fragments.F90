module precipitation__method_of_fragments

  use iso_c_binding
  use data_catalog_entry
  use exceptions
  use file_operations
  use strings
  implicit none

  private

  integer (kind=c_int), allocatable, public :: RAIN_GAGE_ID(:)

  type, public :: FRAGMENTS_T
    integer (kind=c_int) :: iMonth
    integer (kind=c_int) :: iRainGageZone
    integer (kind=c_int) :: iFragmentSet
    real (kind=c_float)  :: fFragmentValue(31)
  end type FRAGMENTS_T
  
  type (FRAGMENTS_T), allocatable, public            :: FRAGMENTS(:)
  type (FRAGMENTS_T), pointer, public                :: pFRAGMENT => null()

  public :: initialize_precipitation_method_of_fragments, read_daily_fragments

contains

  ! current concept: use normal template mechanism to bring in month-year data grids 
  !                      (i.e. PRCP object contains MONTHLY sums for given year)
  ! 
  !  read fragments in as a table
  !  randomly choose a fragment
  !  multiply monthly grid by normalized annual sum (e.g. 1.1 in a wet year, 0.9 in a dry year)
  !  calculate daily precip by multiplying daily fragment value by monthly sum 

  subroutine initialize_precipitation_method_of_fragments(iNumActiveCells )

    integer (kind=c_int), intent(in)  :: iNumActiveCells

    ! [ LOCALS ]
    integer (kind=c_int)        :: iStat

    allocate( RAIN_GAGE_ID( iNumActiveCells ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

!     !> Determine how many landuse codes are present
!     call PARAMS%get_values( sKey="LU_Code", iValues=iLanduseCodes )
!     iNumberOfLanduses = count( iLanduseCodes > 0 )
    
!     call PARAMS%get_values( sKey="Interception_Growing" , fValues=fInterceptionValue_GrowingSeason )
!     call PARAMS%get_values( sKey="Interception_Nongrowing", fValues=fInterceptionValue_DormantSeason )

!     lAreLengthsEqual = ( ( ubound(fInterceptionValue_GrowingSeason,1) == ubound(iLanduseCodes,1) )  &
!                   .and. ( ubound(fInterceptionValue_DormantSeason,1) == ubound(iLanduseCodes,1) )    )

!     if ( .not. lAreLengthsEqual )     &
!       call warn( sMessage="The number of landuses does not match the number of interception values.",   &
!         sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

  end subroutine initialize_precipitation_method_of_fragments



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

  
!   subroutine apply_daily_fragments()

!   end subroutine apply_daily_fragments



end module precipitation__method_of_fragments