!> @file
!! Contains the module @ref precipitation__method_of_fragments.

!>
!!  Module @ref precipitation__method_of_fragments
!!  provides support for creating synthetic daily precipitation
!!  given grids of monthly sum precipitation and a "fragments" file.
!!  The fragments file is generated from observations at discrete locations,
!!  the values of which range from 0 to 1, and the sum of which is 1.
!!  The fragment value is simply the daily observed precipitation value divided
!!  by the monthly sum of all observed precipitation values for that station.
!!
!!  In addition, this routine accepts another set of rainfall adjustment grids,
!!  needed in order to ensure the resulting precipitation totals fall in line with
!!  other published values, in the development case, the Rainfall Atlas of Hawaii.

module precipitation__method_of_fragments

  use iso_c_binding
  use constants_and_conversions, only  : asInt, asFloat, TRUE, FALSE, RANDOM_START
  use data_catalog
  use data_catalog_entry
  use dictionary
  use exceptions
  use file_operations
  use kiss_random_number_generator
  use logfiles, only            : LOGS, LOG_ALL, LOG_DEBUG
  use parameters
  use fstring
  use fstring_list
  use simulation_datetime
  use grid
  implicit none

  private

  public :: precipitation_method_of_fragments_initialize
  public :: read_daily_fragments
  public :: precipitation_method_of_fragments_calculate

  !> Module variable that holds the rainfall gage (zone) number
  integer (c_int), allocatable, public :: RAIN_GAGE_ID(:)

  !> Module variable that holds the current day's rainfall fragment value
  real (c_float), allocatable, public  :: FRAGMENT_VALUE(:)

  !> Module variable indicating which "simulation number" is active
  !! Only has meaning if the rainfall fragments are being applied via a predetermined
  !! sequence file
  integer (c_int), public :: SIMULATION_NUMBER = 1

  !> Module variable that holds the rainfall adjustment factor
  real (c_float), allocatable, public  :: RAINFALL_ADJUST_FACTOR(:)

  !> Module variable that holds a sequence of random numbers associated with the selection
  !! of the fragment set to use
  real (c_double), allocatable :: RANDOM_VALUES(:,:)

  !> Module level variable used to create subsets of the FRAGMENT_SEQUENCES file
  logical (c_bool), allocatable :: SEQUENCE_SELECTION(:)

  !> Module variable detemining whether fragment sequences are chosen at random or
  !! selected from an external file
  logical (c_bool) :: RANDOM_FRAGMENT_SEQUENCES = .true._c_bool

  !> Data structure that holds a single line of data from the input rainfall fragments file.
  type, public :: FRAGMENTS_T
    integer (c_int) :: iMonth
    integer (c_int) :: iRainGageZone
    integer (c_int) :: iFragmentSet
    real (c_float)  :: fFragmentValue(31)
  end type FRAGMENTS_T

  !> Pointer to a rainfall fragments data structure.
  type, public :: PTR_FRAGMENTS_T
    type (FRAGMENTS_T), pointer  :: pFragment => null()
  end type PTR_FRAGMENTS_T

  !> Data structure to hold the current active rainfall fragments for
  !! a particular rain gage zone.
  type, public :: FRAGMENTS_SET_T
    integer (c_int) :: iRainGageZone
    integer (c_int) :: iNumberOfFragments(12)
    integer (c_int) :: iStartRecord(12)
  end type FRAGMENTS_SET_T

  !> Array of all fragments read in from the rainfall fragments file.
  type (FRAGMENTS_T), allocatable, target, public       :: FRAGMENTS(:)

  !> Subset of rainfall fragments file pointing to the currently active fragments.
  type (PTR_FRAGMENTS_T), allocatable                   :: CURRENT_FRAGMENTS(:,:)

  !> Array of fragments sets; fragments sets include indices to the start record
  !! associated with the fragment for each month; FRAGMENTS_SETS will have a
  !! number of elements equal to the number of rainfall gages in the model domain
  type (FRAGMENTS_SET_T), allocatable, public           :: FRAGMENTS_SETS(:)

  !> Data structure to hold static (pre-calculated) fragment selection numbers
  type, public :: FRAGMENTS_SEQUENCE_T
    integer (c_int) :: sim_number
    integer (c_int) :: sim_month
    integer (c_int) :: sim_rainfall_zone
    integer (c_int) :: sim_year
    real (c_float)  :: sim_random_number
    integer (c_int) :: sim_selected_set
  end type FRAGMENTS_SEQUENCE_T

  !> Pointer to all or some of the FRAGMENTS_SEQUENCE array
  type ( FRAGMENTS_SEQUENCE_T ), pointer :: pFRAGMENTS_SEQUENCE

  !> Array of fragment sequence sets
  type (FRAGMENTS_SEQUENCE_T), allocatable, public  :: FRAGMENTS_SEQUENCE(:)

  type (DATA_CATALOG_ENTRY_T), pointer :: pRAINFALL_ADJUST_FACTOR

  integer (c_int) :: LU_FRAGMENTS_ECHO

contains

  !> Initialize method of fragments.
  !!
  !! This routine accesses the "RAINFALL_ZONE" gridded data object and
  !! calls the routine to read in the rainfall fragments file. Values of RAINFALL_ZONE are stored
  !! in a module variable @ref RAIN_GAGE_ID for future reference.
  !!
  !! @params[in]   lActive   2-D boolean array defining active and inactive cells

  subroutine precipitation_method_of_fragments_initialize( lActive )

    logical (c_bool), intent(in)     :: lActive(:,:)

    ! [ LOCALS ]
    integer (c_int)                 :: iStat
    type (DATA_CATALOG_ENTRY_T), pointer :: pRAINFALL_ZONE
    type (FSTRING_LIST_T)                 :: slString
    integer (c_int)                 :: iMaxRainZones
    integer (c_int), allocatable    :: iSimulationNumbers(:)
    character (len=256)                  :: error_str

    ! look up the simulation number associated with the desired fragment sequence set
    call CF_DICT%get_values( sKey="FRAGMENTS_SEQUENCE_SIMULATION_NUMBER", iValues=iSimulationNumbers )
    if ( iSimulationNumbers(1) > 0 )  SIMULATION_NUMBER = iSimulationNumbers(1)

    ! locate the data structure associated with the gridded rainfall zone entries
    pRAINFALL_ZONE => DAT%find("RAINFALL_ZONE")
    if ( .not. associated(pRAINFALL_ZONE) ) &
        call die("A RAINFALL_ZONE grid must be supplied in order to make use of this option.",    &
          __SRCNAME__, __LINE__)

    allocate( RAIN_GAGE_ID( count(lActive) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    call pRAINFALL_ZONE%getvalues()

    ! map the 2D array of RAINFALL_ZONE values to the vector of active cells
    RAIN_GAGE_ID = pack( pRAINFALL_ZONE%pGrdBase%iData, lActive )

    allocate( RAINFALL_ADJUST_FACTOR( count(lActive) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( FRAGMENT_VALUE( count(lActive) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __SRCNAME__, __LINE__ )


    ! look up the name of the fragments file in the control file dictionary
    call CF_DICT%get_values( sKey="FRAGMENTS_DAILY_FILE", slString=slString )

    ! use the first entry in the string list slString as the filename to open for
    ! use with the daily fragments routine


    call read_daily_fragments( slString%get(1) )
    call slString%clear()

    ! look up the name of the fragments SEQUENCE file in the control file dictionary
    call CF_DICT%get_values( sKey="FRAGMENTS_SEQUENCE_FILE", slString=slString )

    if ( .not. ( slString%get(1) .strequal. "<NA>" ) )  then
      call read_fragments_sequence( slString%get(1) )
      RANDOM_FRAGMENT_SEQUENCES = .false._c_bool
      allocate ( SEQUENCE_SELECTION( count(FRAGMENTS_SEQUENCE%sim_month > 0) ), stat=iStat )
      call assert( iStat == 0, "Problem allocating memory", __SRCNAME__, __LINE__ )
    endif

    !> Now the fragments file is in memory. Create an ancillary data structure
    !> to keep track of which records correspond to various rain zones

    iMaxRainZones = maxval(FRAGMENTS%iRainGageZone)

    allocate ( FRAGMENTS_SETS( iMaxRainZones ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    if ( .not. allocated(CURRENT_FRAGMENTS) ) then
      allocate (CURRENT_FRAGMENTS( iMaxRainZones, 1 ), stat=iStat, errmsg=error_str )
      call assert( iStat == 0, "Problem allocating memory, stat="//asCharacter(iStat)  &
        //"; msg: "//trim(error_str), __SRCNAME__, __LINE__ )
    endif

    if ( .not. allocated( RANDOM_VALUES) ) then
      allocate (RANDOM_VALUES( iMaxRainZones, 1 ), stat=iStat )
      call assert( iStat == 0, "Problem allocating memory, stat="//asCharacter(iStat)  &
        //"; msg: "//trim(error_str), __SRCNAME__, __LINE__ )
    endif

    if ( RANDOM_FRAGMENT_SEQUENCES ) call initialize_kiss_rng( RANDOM_START )

    call process_fragment_sets()

    open( newunit=LU_FRAGMENTS_ECHO, file="Fragments_as_implemented_by_SWB.csv")
    write( LU_FRAGMENTS_ECHO, fmt="(a, 30('fragment, '),'fragment')")          &
        "Simulation_Number, Month, Rain_Zone, Year, Random_Number, Fragment_Set,"

  end subroutine precipitation_method_of_fragments_initialize

  !--------------------------------------------------------------------------------------------------

  subroutine read_daily_fragments( sFilename )

    character (len=*), intent(in)    :: sFilename

    ! [ LOCALS ]
    character (len=512)   :: sRecord, sSubstring
    integer (c_int)  :: iStat
    integer (c_int)  :: iCount
    integer (c_int)  :: iIndex
    integer (c_int)  :: last_zone
    integer (c_int)  :: last_fragment
    integer (c_int)  :: last_month
    integer (c_int)  :: iNumLines
    real (c_float)   :: fTempValue
    type (ASCII_FILE_T)   :: FRAGMENTS_FILE


    call FRAGMENTS_FILE%open( sFilename = sFilename,         &
                              sCommentChars = "#%!",         &
                              sDelimiters = "WHITESPACE",    &
                              lHasHeader = .false._c_bool )

    iNumLines = FRAGMENTS_FILE%numLines()

    allocate(  FRAGMENTS( iNumLines ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory for fragments table", __SRCNAME__, __LINE__ )

    iCount = 0
    last_zone = 1
    last_fragment = 0
    last_month = 1

    do

      ! read in next line of file
      sRecord = FRAGMENTS_FILE%readLine()

      if ( FRAGMENTS_FILE%isEOF() ) exit

      iCount = iCount + 1

      ! read in month number
      call chomp(sRecord, sSubstring, FRAGMENTS_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 )                                    &
        call die( "Missing month number in the daily fragments file",     &
          __SRCNAME__, __LINE__, "Problem occured on line number "        &
          //asCharacter(FRAGMENTS_FILE%currentLineNum() )                 &
          //" of file "//dquote(sFilename) )

      FRAGMENTS(iCount)%iMonth = asInt(sSubString)

      ! reset the counter tracking the previous fragment ID
      if ( FRAGMENTS(iCount)%iMonth == (last_month + 1) ) then
        last_fragment = 0
        last_zone = 0
      endif

      if ( FRAGMENTS(iCount)%iMonth < last_month )                             &
        call die( "Out-of-order month value in the daily fragments file",      &
          __SRCNAME__, __LINE__, "Problem occured on line number "             &
          //asCharacter(FRAGMENTS_FILE%currentLineNum() )                      &
          //" of file "//dquote(sFilename) )

      last_month = FRAGMENTS(iCount)%iMonth

      ! read in rain gage zone
      call chomp(sRecord, sSubstring, FRAGMENTS_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 )                                         &
        call die( "Missing rain gage zone number in the daily fragments file", &
          __SRCNAME__, __LINE__, "Problem occured on line number "             &
          //asCharacter(FRAGMENTS_FILE%currentLineNum() )                      &
          //" of file "//dquote(sFilename) )

      FRAGMENTS(iCount)%iRainGageZone = asInt(sSubString)

      if ( FRAGMENTS(iCount)%iRainGageZone < last_zone )                            &
        call die( "Rain gage zone number out of order in the daily fragments file", &
          __SRCNAME__, __LINE__, "Problem occured on line number "                  &
          //asCharacter(FRAGMENTS_FILE%currentLineNum() )                           &
          //" of file "//dquote(sFilename) )

      ! reset the counter tracking the previous fragment ID
      if ( FRAGMENTS(iCount)%iRainGageZone == (last_zone + 1) )                &
        last_fragment = 0

      last_zone = FRAGMENTS(iCount)%iRainGageZone

      ! read in fragment set number for this zone
      call chomp(sRecord, sSubstring, FRAGMENTS_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 )                                         &
        call die( "Missing fragment set number in the daily fragments file",   &
          __SRCNAME__, __LINE__, "Problem occured on line number "             &
          //asCharacter(FRAGMENTS_FILE%currentLineNum() )                      &
          //" of file "//dquote(sFilename) )

      FRAGMENTS(iCount)%iFragmentSet = asInt(sSubString)

      if ( FRAGMENTS(iCount)%iFragmentSet /= (last_fragment + 1) )                           &
        call die( "Missing or out-of-order fragment value in the daily fragments file",      &
          __SRCNAME__, __LINE__, "Problem occured on line number "                           &
          //asCharacter(FRAGMENTS_FILE%currentLineNum() )                                    &
          //" of file "//dquote(sFilename) )

      last_fragment = FRAGMENTS(iCount)%iFragmentSet

      ! read in fragments for each day of the current month
      do iIndex = 1, 31

        ! read in fragment for given day of month
        call chomp(sRecord, sSubstring, FRAGMENTS_FILE%sDelimiters )

        if ( len_trim(sSubstring) == 0 )                                       &
          call die( "Missing fragment value in the daily fragments file",      &
            __SRCNAME__, __LINE__, "Problem occured on line number "           &
            //asCharacter(FRAGMENTS_FILE%currentLineNum() )                    &
            //" of file "//dquote(sFilename) )

        fTempValue = asFloat( sSubstring )

        ! This substitution is needed to prevent "-9999" or "9999" values embedded in the fragments file
        ! from creeping into calculations. In the event of a "9999", the appropriate substitution is
        ! zero, since the previous fragments for the month to this point should already sum to 1.0
        if ( ( fTempValue < 0.0_c_float ) .or. ( fTempValue > 1.0_c_float ) ) then
          FRAGMENTS(iCount)%fFragmentValue(iIndex) = 0.0_c_float
        else
          FRAGMENTS(iCount)%fFragmentValue(iIndex) = fTempValue
        endif

      enddo

      if ( FRAGMENTS(iCount)%iMonth == 2 )  call normalize_february_fragment_sequence( iCount )

    enddo

    call LOGS%write("Maximum rain gage zone number: "//asCharacter(maxval(FRAGMENTS%iRainGageZone)), &
      iTab=31, iLinesAfter=1, iLogLevel=LOG_ALL)

  end subroutine read_daily_fragments

!--------------------------------------------------------------------------------------------------

  !> after fragments file has been read in, iterate over a set of rainfall fragments
  ! and keep track of the index values that correspond with changes
  ! in month and rain gage numbers
  subroutine process_fragment_sets()

    integer (c_int)   :: iCount
    integer (c_int)   :: iIndex
    integer (c_int)   :: iRainGageZone
    integer (c_int)   :: iPreviousRainGageZone
    integer (c_int)   :: iFragmentChunk
    integer (c_int)   :: iMonth
    integer (c_int)   :: iPreviousMonth
    character (len=10)     :: sBuf0
    character (len=10)     :: sBuf1
    character (len=12)     :: sBuf2
    character (len=10)     :: sBuf3
    character (len=52)     :: sBuf4

    ! this counter is used to accumulate the number of fragments associated with the
    ! current raingage zone/month combination
    iCount = 1

    iRainGageZone = FRAGMENTS( lbound( FRAGMENTS, 1) )%iRainGageZone
    iPreviousRainGageZone = iRainGageZone
    iPreviousMonth = FRAGMENTS( lbound( FRAGMENTS, 1) )%iMonth

    ! at this point, iRainGageZone should be 1, and iPreviousMonth should be 1,
    ! assuming that the fragments were sorted properly upon input

    ! populate the first record of FRAGMENT_SETS
    FRAGMENTS_SETS( iRainGageZone )%iRainGageZone = iRainGageZone
    FRAGMENTS_SETS( iRainGageZone )%iStartRecord(iPreviousMonth) = lbound( FRAGMENTS, 1)

    ! now iterate through *all* fragments, keeping track of the starting record for each new rainfall gage
    ! zone number
    do iIndex = lbound( FRAGMENTS, 1) + 1, ubound( FRAGMENTS, 1 )

      iRainGageZone = FRAGMENTS(iIndex)%iRainGageZone
      iMonth = FRAGMENTS(iIndex)%iMonth

      if ( iRainGageZone /= iPreviousRainGageZone ) then
        ! the previous record was the last one associated with the previous
        ! rainfall gage zone; do not count the current record as part of the
        ! collection of records associated with previous zone

        FRAGMENTS_SETS( iPreviousRainGageZone )%iNumberOfFragments(iPreviousMonth) = iCount


        FRAGMENTS_SETS( iRainGageZone )%iRainGageZone = iRainGageZone
        FRAGMENTS_SETS( iRainGageZone )%iStartRecord(iMonth) = iIndex
        ! ! need to handle the last fragment set as a special case
        ! FRAGMENTS_SETS( iRainGageZone )%iNumberOfFragments(iMonth) = iCount
        iCount = 1

      else
        iCount = iCount + 1
      endif

      iPreviousMonth = iMonth
      iPreviousRainGageZone = iRainGageZone

    enddo

    ! need to handle the last month of the last fragment set as a special case
    FRAGMENTS_SETS( iRainGageZone )%iNumberOfFragments(iMonth) = iCount

    call LOGS%write("### Summary of fragment sets in memory ###", &
       iLogLevel=LOG_ALL, iLinesBefore=1, iLinesAfter=1, lEcho=FALSE )
    call LOGS%write("gage number | month      | start index  | num records ")
    call LOGS%write("----------- | ---------- | ------------ | ------------")
    do iIndex=1, ubound( FRAGMENTS_SETS, 1)
      do iMonth=1,12
        write (sBuf0, fmt="(i10)") iIndex
        write (sBuf1, fmt="(i10)") iMonth
        write (sBuf2, fmt="(i12)") FRAGMENTS_SETS(iIndex)%iStartRecord(iMonth)
        write (sBuf3, fmt="(i10)") FRAGMENTS_SETS(iIndex)%iNumberOfFragments(iMonth)
        write (sBuf4, fmt="(a10,'  | ', a10,' | ', a12,' | ',a10)") adjustl(sBuf0),     &
               adjustl(sBuf1), adjustl(sBuf2), adjustl(sBuf3)
        call LOGS%write( sBuf4 )
      enddo
    end do

  end subroutine process_fragment_sets

!--------------------------------------------------------------------------------------------------

  !> eliminate rainfall on the 29th day of February; bump up all other values to ensure sum = 1
  subroutine normalize_february_fragment_sequence( iCount )

    integer (c_int), intent(in)   :: iCount

    ! [ LOCALS ]
    real (c_float) :: sum_fragments

    ! we only want to correct the fragment if it was actually generated during a
    ! leap year
    if (FRAGMENTS(iCount)%fFragmentValue(29) > 0.0_c_float) then
      sum_fragments = sum( FRAGMENTS(iCount)%fFragmentValue(1:28) )

      ! bump up all February fragments so that their 28-day sum is 1
      FRAGMENTS(iCount)%fFragmentValue(1:28) = FRAGMENTS(iCount)%fFragmentValue(1:28)   &
                                                 / sum_fragments
      ! zero out remaining values
      FRAGMENTS(iCount)%fFragmentValue(29:31) = 0.0_c_float
    endif

  end subroutine normalize_february_fragment_sequence

!--------------------------------------------------------------------------------------------------

  ! in order to compare Hawaii Water Budget results to SWB results,
  ! it is necessary to force SWB to use the same sequence of fragments
  ! that was used in the HWB simulations
  subroutine read_fragments_sequence( sFilename )

    character (len=*), intent(in)    :: sFilename

    ! [ LOCALS ]
    character (len=512)   :: sRecord, sSubstring
    integer (c_int)  :: iStat
    integer (c_int)  :: iCount
    integer (c_int)  :: iIndex
    integer (c_int)  :: iNumLines
    type (ASCII_FILE_T)   :: SEQUENCE_FILE
    character (len=10)     :: sBuf0
    character (len=10)     :: sBuf1
    character (len=12)     :: sBuf2
    character (len=10)     :: sBuf3
    character (len=10)     :: sBuf4
    character (len=256)    :: sBuf5
    character (len=256)    :: error_str
    type (FSTRING_LIST_T)   :: slHeader
    integer (c_int)   :: max_rain_gage_number
    integer (c_int)   :: max_simulation_number


    call SEQUENCE_FILE%open( sFilename = sFilename,         &
                             sCommentChars = "#%!",         &
                             sDelimiters = "WHITESPACE",    &
                             lHasHeader = .true._c_bool )

    slHeader = SEQUENCE_FILE%readHeader()

    iNumLines = SEQUENCE_FILE%numLines()

    allocate(  FRAGMENTS_SEQUENCE( iNumLines ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory for fragments sequence table",    &
      __SRCNAME__, __LINE__ )

    iCount = 0

    do

      ! read in next line of file
      sRecord = SEQUENCE_FILE%readLine()

      if ( SEQUENCE_FILE%isEOF() ) exit

      iCount = iCount + 1

      ! read in simulation number
      call chomp(sRecord, sSubstring, SEQUENCE_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 )                                              &
          call die( "Missing simulation number in the fragments sequence file",     &
            __SRCNAME__, __LINE__, "Problem occured on line number "                &
            //asCharacter(SEQUENCE_FILE%currentLineNum() )                          &
            //" of file "//dquote(sFilename) )

      FRAGMENTS_SEQUENCE(iCount)%sim_number = asInt(sSubString)

      ! read in month
      call chomp(sRecord, sSubstring, SEQUENCE_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 )                                              &
        call die( "Missing month number in the fragments sequence file",            &
          __SRCNAME__, __LINE__, "Problem occured on line number "                  &
          //asCharacter(SEQUENCE_FILE%currentLineNum() )                            &
          //" of file "//dquote(sFilename) )

      FRAGMENTS_SEQUENCE(iCount)%sim_month = asInt(sSubString)

      ! read in rainfall zone
      call chomp(sRecord, sSubstring, SEQUENCE_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 )                                              &
        call die( "Missing rainfall zone number in the fragments sequence file",    &
          __SRCNAME__, __LINE__, "Problem occured on line number "                  &
          //asCharacter(SEQUENCE_FILE%currentLineNum() )                            &
          //" of file "//dquote(sFilename) )

      FRAGMENTS_SEQUENCE(iCount)%sim_rainfall_zone = asInt(sSubString)


      ! read in sim_year
      call chomp(sRecord, sSubstring, SEQUENCE_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 )                                              &
        call die( "Missing year number in the fragments sequence file",             &
          __SRCNAME__, __LINE__, "Problem occured on line number "                  &
          //asCharacter(SEQUENCE_FILE%currentLineNum() )                            &
          //" of file "//dquote(sFilename) )

      FRAGMENTS_SEQUENCE(iCount)%sim_year = asInt(sSubString)

      ! read in sim_random_number
      call chomp(sRecord, sSubstring, SEQUENCE_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 )                                                   &
        call die( "Missing simulation random number in the fragments sequence file",     &
          __SRCNAME__, __LINE__, "Problem occured on line number "                       &
          //asCharacter(SEQUENCE_FILE%currentLineNum() )                                 &
          //" of file "//dquote(sFilename) )

      FRAGMENTS_SEQUENCE(iCount)%sim_random_number = asFloat(sSubString)

      ! read in simulation selected set (fragment set selected by HWB)
      call chomp(sRecord, sSubstring, SEQUENCE_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 )                                                   &
        call die( "Missing selected fragment set number in the fragments sequence file", &
          __SRCNAME__, __LINE__, "Problem occured on line number "                       &
          //asCharacter(SEQUENCE_FILE%currentLineNum() )                                 &
          //" of file "//dquote(sFilename) )

      FRAGMENTS_SEQUENCE(iCount)%sim_selected_set = asInt(sSubString)

    enddo

    max_rain_gage_number = maxval(FRAGMENTS_SEQUENCE(:)%sim_rainfall_zone,1)
    max_simulation_number = maxval(FRAGMENTS_SEQUENCE(:)%sim_number,1)

    ! idea here is that if we are reading in a sequence file, there is a good chance
    ! that the users is running multiple simulations; this will allow for a
    ! separate pointer to be established for each rain gage/simulation number combination
    ! as well as a means to keep the random value sequences separate by simulation

    if ( allocated(CURRENT_FRAGMENTS))  deallocate(CURRENT_FRAGMENTS, stat=iStat, &
      errmsg=error_str)
    call assert( iStat == 0, "Problem deallocating memory, stat="//asCharacter(iStat)  &
      //"; msg: "//trim(error_str), __SRCNAME__, __LINE__ )

    allocate(CURRENT_FRAGMENTS(max_rain_gage_number, max_simulation_number), stat=iStat, &
      errmsg=error_str)
    call assert( iStat == 0, "Problem allocating memory, stat="//asCharacter(iStat)  &
      //"; msg: "//trim(error_str), __SRCNAME__, __LINE__ )


    if ( allocated(RANDOM_VALUES))  deallocate(RANDOM_VALUES, stat=iStat, errmsg=error_str)
    call assert( iStat == 0, "Problem deallocating memory, stat="//asCharacter(iStat)  &
      //"; msg: "//trim(error_str), __SRCNAME__, __LINE__ )

    allocate(RANDOM_VALUES(max_rain_gage_number, max_simulation_number), stat=iStat, &
      errmsg=error_str )
    call assert( iStat == 0, "Problem allocating memory, stat="//asCharacter(iStat)  &
      //"; msg: "//trim(error_str), __SRCNAME__, __LINE__ )

    call LOGS%write("### Summary of fragment sequence sets in memory ###", &
       iLogLevel=LOG_DEBUG, iLinesBefore=1, iLinesAfter=1, lEcho=FALSE )
    call LOGS%write("sim number | rainfall zone   | month  | year   | selected set ")
    call LOGS%write("----------- | ---------- | ------------ | ------------|------------")
    do iIndex=1, ubound( FRAGMENTS_SEQUENCE, 1)
      write (sBuf0, fmt="(i10)") FRAGMENTS_SEQUENCE( iIndex )%sim_number
      write (sBuf1, fmt="(i10)") FRAGMENTS_SEQUENCE( iIndex )%sim_rainfall_zone
      write (sBuf2, fmt="(i12)") FRAGMENTS_SEQUENCE( iIndex )%sim_month
      write (sBuf3, fmt="(i10)") FRAGMENTS_SEQUENCE( iIndex )%sim_year
      write (sBuf4, fmt="(i10)") FRAGMENTS_SEQUENCE( iIndex )%sim_selected_set

      write (sBuf5, fmt="(a,'  | ', a,'  |  ', a,'  |  ',a,'  |  ',a)")                        &
        adjustl(sBuf0), adjustl(sBuf1), adjustl(sBuf2), adjustl(sBuf3), adjustl(sBuf4)
      call LOGS%write( trim( sBuf5 ) )
    end do

  end subroutine read_fragments_sequence

!--------------------------------------------------------------------------------------------------

  !> Update rainfall fragments on daily basis.
  !!
  !! If called when lShuffle is TRUE:
  !! 1) update random values
  !! 2) random values are used to select the next active fragment set
  !!    for the current RainGageZone
  !!
  !! *Each* time the routine is called, the appropriate fragment is
  !! selected from the current active fragment set and is assigned
  !! to all cells that share a common RainGageZone

  subroutine update_fragments( lShuffle )

    logical (c_bool), intent(in) :: lShuffle

    ! [ LOCALS ]
    integer (c_int) :: rain_zone
    integer (c_int) :: iMaxRainZones
    integer (c_int) :: iMonth
    integer (c_int) :: iDay
    integer (c_int) :: iYearOfSimulation

    integer (c_int) :: iNumberOfFragments
    integer (c_int)  :: iStartRecord
    integer (c_int) :: iEndRecord
    integer (c_int) :: iTargetRecord
    integer (c_int) :: iStat
    integer (c_int) :: iUBOUND_FRAGMENTS
    integer (c_int) :: iUBOUND_CURRENT_FRAGMENTS
    character (len=512)  :: sBuf


    iMaxRainZones = maxval(FRAGMENTS%iRainGageZone)
    iMonth = SIM_DT%curr%iMonth
    iDay = SIM_DT%curr%iDay
    iYearOfSimulation=SIM_DT%iYearOfSimulation

    ! equal the number of fragments are in memory
    iUBOUND_FRAGMENTS = ubound( FRAGMENTS, 1)

    ! should equal the number of rainfall gages in model domain
    iUBOUND_CURRENT_FRAGMENTS = ubound( CURRENT_FRAGMENTS, 1)

    ! if by chance a mismatch in shape-to-grid results in an active cell with *NO* valid
    ! rain gage ID, we need to set the entire array to zero to quash any spurious values getting in
    FRAGMENT_VALUE = 0.0_c_float

    do rain_zone = 1, iMaxRainZones

      if ( lShuffle ) then
        ! find next fragment *record*

        ! update the module variable RANDOM_VALUES
        call update_random_values()

        iStartRecord = FRAGMENTS_SETS( rain_zone )%iStartRecord(iMonth)
        iNumberOfFragments = FRAGMENTS_SETS(rain_zone)%iNumberOfFragments(iMonth)
        iEndRecord = iStartRecord + iNumberOfFragments - 1
        iTargetRecord = iStartRecord                                           &
                           + int(RANDOM_VALUES(rain_zone, SIMULATION_NUMBER)   &
                           * real( iNumberOfFragments ))

        if ( ( rain_zone > iUBOUND_CURRENT_FRAGMENTS ) .or. ( iTargetRecord > iUBOUND_FRAGMENTS )   &
            .or. ( rain_zone < 1 ) .or. ( iTargetRecord < 1) ) then
          call LOGS%write("Error detected in method of fragments routine; dump of current"       &
                          //" variables follows:", iLinesBefore=1)
          call LOGS%write("rain_zone         : "//asCharacter(rain_zone), iTab=3 )
          call LOGS%write("simulation_number : "//asCharacter(rain_zone), iTab=3 )
          call LOGS%write("iStartRecord      : "//asCharacter(iStartRecord), iTab=3 )
          call LOGS%write("iNumberOfFragments: "//asCharacter(iNumberOfFragments), iTab=3 )
          call LOGS%write("iEndRecord        : "//asCharacter(iEndRecord), iTab=3 )
          call LOGS%write("iTargetRecord     : "//asCharacter(iTargetRecord), iTab=3 )
          call LOGS%write("ubound(CURRENT_FRAGMENTS, 1): "//asCharacter(iUBOUND_CURRENT_FRAGMENTS), &
                          iTab=3 )
          call LOGS%write("ubound(FRAGMENTS, 1): "//asCharacter(iUBOUND_FRAGMENTS), iTab=3 )
          call LOGS%write("RANDOM_VALUES(rain_zone,SIMULATION_NUMBER): "       &
            //asCharacter(RANDOM_VALUES(rain_zone,SIMULATION_NUMBER)), iTab=3 )
          call die( "Miscalculation in target record: calculated record index is out of bounds", &
            __SRCNAME__, __LINE__ )
        endif

        ! reassign fragment pointer for this rain zone to the newly selected record
        CURRENT_FRAGMENTS(rain_zone, SIMULATION_NUMBER)%pFragment => FRAGMENTS( iTargetRecord )

         write(LU_FRAGMENTS_ECHO,fmt="(4(i5,','),f10.6,',',i5,',',30(f8.3,','),f8.3)")   &
                     SIMULATION_NUMBER,                                                  &
                     FRAGMENTS( iTargetRecord)%iMonth,                                   &
                     FRAGMENTS( iTargetRecord)%iRainGageZone,                            &
                     iYearOfSimulation,                                                  &
                     RANDOM_VALUES(rain_zone, SIMULATION_NUMBER),                        &
                     FRAGMENTS( iTargetRecord)%iFragmentSet,                             &
                     FRAGMENTS( iTargetRecord)%fFragmentValue

        ! call LOGS%write( trim(sBuf), iLogLevel=LOG_DEBUG, lEcho=FALSE )

      endif

      if ( ( CURRENT_FRAGMENTS( rain_zone, SIMULATION_NUMBER )%pFragment%fFragmentValue( iDay ) < 0.0 ) &
         .or. ( CURRENT_FRAGMENTS( rain_zone, SIMULATION_NUMBER )%pFragment%fFragmentValue( iDay ) > 1.0 ) ) then

        call LOGS%write("Error detected in method of fragments routine; dump of current variables"  &
                        //" follows:", iLinesBefore=1, iLogLevel=LOG_ALL )
        call LOGS%write("rain_zone:"//asCharacter(rain_zone), iTab=3 )
        call LOGS%write("iDay: "//asCharacter(iDay), iTab=3 )
        call LOGS%write("iRainGageZone: "//asCharacter(FRAGMENTS( iTargetRecord)%iRainGageZone), iTab=3 )
        call LOGS%write("iFragmentSet: "//asCharacter(FRAGMENTS( iTargetRecord)%iFragmentSet), iTab=3 )
        call LOGS%write("fFragmentValue: "//asCharacter(FRAGMENTS( iTargetRecord)%fFragmentValue(iDay) ), iTab=3 )

      endif

      ! call LOGS%write("frag: "//asCharacter(rain_zone)//"  day: "//asCharacter(iDay) &
      !    //"  value: "//asCharacter( CURRENT_FRAGMENTS( rain_zone )%pFragment%fFragmentValue( iDay ) ), &
      !    lEcho=FALSE )

      ! now place current days' fragment value into the matching cells
      where ( RAIN_GAGE_ID == rain_zone )

        FRAGMENT_VALUE = CURRENT_FRAGMENTS( rain_zone, SIMULATION_NUMBER )%pFragment%fFragmentValue( iDay )

      endwhere

    enddo

  end subroutine update_fragments

!--------------------------------------------------------------------------------------------------

  subroutine update_random_values()

    ! [ LOCALS ]
    integer (c_int) :: iIndex, iIndex2
    logical (c_bool) :: lSequenceSelection

    if ( RANDOM_FRAGMENT_SEQUENCES ) then

      do iIndex2=1,size(RANDOM_VALUES,1)

      !call random_number( RANDOM_VALUES )
        RANDOM_VALUES(iIndex2, SIMULATION_NUMBER) = kiss64_uniform_rng()

      enddo

    else

      RANDOM_VALUES(:,SIMULATION_NUMBER) = -9999999.9

      do iIndex=1, size(FRAGMENTS_SEQUENCE%sim_month, 1)

        lSequenceSelection =       ( FRAGMENTS_SEQUENCE(iIndex)%sim_month == SIM_DT%curr%iMonth )            &
                             .and. ( FRAGMENTS_SEQUENCE(iIndex)%sim_year == SIM_DT%iYearOfSimulation )       &
                             .and. ( FRAGMENTS_SEQUENCE(iIndex)%sim_number == SIMULATION_NUMBER )

        if ( .not. lSequenceSelection ) cycle

        do iIndex2=1,size(RANDOM_VALUES,1)

          if ( FRAGMENTS_SEQUENCE( iIndex )%sim_rainfall_zone == iIndex2 ) then

            RANDOM_VALUES( iIndex2, SIMULATION_NUMBER ) = FRAGMENTS_SEQUENCE( iIndex )%sim_random_number
            exit

          endif

        enddo

      enddo

    endif

    if (any( RANDOM_VALUES(:, SIMULATION_NUMBER) < 0.0 ) ) then

      call LOGS%write("Error detected in method of fragments routine - random values " &
        //"not found in sequence file for rainfall zone(s):", iLinesBefore=1)
      do iIndex=1,size(RANDOM_VALUES, 1)
        if ( RANDOM_VALUES(iIndex, SIMULATION_NUMBER) < 0.0 )  &
          call LOGS%write("simulation number, rainfall zone: "                 &
            //trim(asCharacter(SIMULATION_NUMBER))//", "//trim(asCharacter(iIndex)), iTab=3 )
      enddo

    endif

  end subroutine update_random_values

!--------------------------------------------------------------------------------------------------

  subroutine precipitation_method_of_fragments_calculate( lActive )

    logical (c_bool), intent(in)     :: lActive(:,:)

    ! [ LOCALS ]
    integer (c_int)              :: iIndex
    integer (c_int)              :: iMaxRainZones
    integer (c_int)              :: iStat
    logical (c_bool), save       :: lFirstCall = TRUE

    type (DATA_CATALOG_ENTRY_T), pointer :: pRAINFALL_ADJUST_FACTOR


    !! if it is the first day of the month, update the rainfall adjustment factor grid
    !! and update the fragments
    if ( SIM_DT%curr%iDay == 1 .or. lFirstCall ) then

      ! locate the data structure associated with the gridded rainfall adjustment factor
      pRAINFALL_ADJUST_FACTOR => DAT%find("RAINFALL_ADJUST_FACTOR")

      if ( .not. associated(pRAINFALL_ADJUST_FACTOR) ) &
          call die("A RAINFALL_ADJUST_FACTOR grid must be supplied in order to make use"     &
                   //" of this option.", __SRCNAME__, __LINE__)

      call pRAINFALL_ADJUST_FACTOR%getvalues( SIM_DT%curr )

      ! map the 2D array of RAINFALL_ADJUST_FACTOR values to the vector of active cells
      RAINFALL_ADJUST_FACTOR = pack( pRAINFALL_ADJUST_FACTOR%pGrdBase%rData, lActive )

      call update_fragments( lShuffle = TRUE)
      lFirstCall = FALSE

    else

      call update_fragments( lShuffle = FALSE )

    endif

  end subroutine precipitation_method_of_fragments_calculate

!--------------------------------------------------------------------------------------------------

end module precipitation__method_of_fragments
