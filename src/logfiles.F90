module logfiles

  use version_control, only  : SWB_VERSION, GIT_COMMIT_HASH_STRING,           &
                               GIT_BRANCH_STRING, COMPILE_DATE, COMPILE_TIME, &
                               COMPILATION_TIMESTAMP
  use iso_c_binding, only : c_bool, c_int
  use iso_fortran_env, only : OUTPUT_UNIT
  implicit none

  private

  public :: LOGFILE_T

  enum, bind(c)
    enumerator :: DT_YEAR = 1, DT_MONTH = 2, DT_DAY = 3, DT_DIFF_FM_UTC = 4,          &
                  DT_HOUR = 5, DT_MINUTES = 6, DT_SECONDS = 7, DT_MILLISECONDS = 8
  end enum

  public :: LOG_NONE, LOG_GENERAL, LOG_DEBUG, LOG_ALL

  enum, bind(c)
    enumerator :: LOG_NONE = 0, LOG_GENERAL = 1, LOG_DEBUG = 2, LOG_ALL = 3
  end enum

  type LOGFILE_T
    character (len=:), allocatable  :: sFilePrefix
    character (len=64)              :: sFilename(2)
    logical (kind=c_bool)           :: lIsOpen(2)       = .false._c_bool
    integer (kind=c_int)            :: iUnitNum(2)
    integer (kind=c_int)            :: iStat(2)
    integer (kind=c_int)            :: iLogLevel        = LOG_GENERAL

  contains

    procedure, private :: initialize_logfiles_sub
    generic            :: initialize => initialize_logfiles_sub

    procedure, private :: write_to_logfiles_sub
    generic            :: write => write_to_logfiles_sub

    procedure, private :: set_log_level_sub
    generic            :: set_loglevel => set_log_level_sub

    procedure, private :: set_screen_echo_sub
    generic            :: set_echo => set_screen_echo_sub

    procedure, private :: open_files_write_access_sub
    generic            :: open => open_files_write_access_sub

    procedure, private :: close_files_sub
    generic            :: close => close_files_sub

    procedure, private :: make_prefix_sub
    generic            :: make_prefix => make_prefix_sub

    procedure, private :: set_output_directory_name_sub
    generic            :: set_output_directory => set_output_directory_name_sub

  end type LOGFILE_T

  type (LOGFILE_T), public :: LOGS

  integer (kind=c_int)               :: CURRENT_LOG_LEVEL     = LOG_GENERAL
  logical (kind=c_bool)              :: CURRENT_LOG_ECHO      = .false._c_bool
  character (len=64)                 :: OUTPUT_DIRECTORY_NAME = ""
  logical (kind=c_bool), parameter   :: TRUE = .true._c_bool

contains

  subroutine set_output_directory_name_sub( this, sDirName )

    class (LOGFILE_T)                           :: this
    character (len=*), intent(in)               :: sDirName

    OUTPUT_DIRECTORY_NAME = trim(sDirName)

  end subroutine set_output_directory_name_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_log_level_sub( this, iLogLevel )

    class (LOGFILE_T)                           :: this
    integer (kind=c_int), intent(in)            :: iLogLevel

    CURRENT_LOG_LEVEL = iLogLevel

  end subroutine set_log_level_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_screen_echo_sub( this, lEcho )

    class (LOGFILE_T)                           :: this
    logical (kind=c_bool), intent(in)           :: lEcho

    CURRENT_LOG_ECHO = lEcho

  end subroutine set_screen_echo_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_logfiles_sub(this, iLogLevel, sFilePrefix, lWrite_SWB_Info )

    class (LOGFILE_T)                            :: this
    integer (kind=c_int), intent(in), optional   :: iLogLevel
    character (len=*), intent(in), optional      :: sFilePrefix
    logical (kind=c_bool), intent(in), optional  :: lWrite_SWB_Info


    ! [ LOCALS ]
    integer (kind=c_int)    :: iLogLevel_l
    logical (kind=c_bool)   :: lWrite_SWB_Info_l

    if ( present( lWrite_SWB_Info) ) then
      lWrite_SWB_Info_l = lWrite_SWB_Info
    else
      lWrite_SWB_Info_l = .true._c_bool
    endif

    if (present(iLogLevel) ) then
      iLogLevel_l = iLogLevel
    else
      iLogLevel_l = LOG_GENERAL
    endif

    if ( present( sFilePrefix) ) then
      this%sFilePrefix = sFilePrefix
    else
      call this%make_prefix()
    endif

    this%iLogLevel = iLogLevel_l
    call this%open( lWrite_SWB_Info=lWrite_SWB_Info_l )

  end subroutine initialize_logfiles_sub

!--------------------------------------------------------------------------------------------------

  subroutine open_files_write_access_sub(this, lWrite_SWB_Info )

    class (LOGFILE_T), intent(inout)             :: this
    logical (kind=c_bool), intent(in), optional  :: lWrite_SWB_Info

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    character (len=:), allocatable   :: sFilename
    character (len=:), allocatable   :: sDatetime
    character (len=12)               :: sDescriptor(2) = [ "         ", "_DEBUG   " ]
    logical (kind=c_bool)            :: lWrite_SWB_Info_l

    if ( present( lWrite_SWB_Info) ) then
      lWrite_SWB_Info_l = lWrite_SWB_Info
    else
      lWrite_SWB_Info_l = .true._c_bool
    endif

    if ( this%iLogLevel /= LOG_NONE ) then

      do iIndex = 1, min(this%iLogLevel, 2)

        sFilename = trim(OUTPUT_DIRECTORY_NAME)//trim(this%sFilePrefix)//trim(sDescriptor(iIndex))//".md"

        if (.not. this%lIsOpen(iIndex) ) then

          open(newunit=this%iUnitNum(iIndex), file=sFilename, iostat=this%iStat(iIndex), action='WRITE', encoding='UTF-8')
          if (this%iStat(iIndex) /= 0) then
            write(unit=OUTPUT_UNIT, fmt="(a)") "Failed to open logfile "//'"'//trim(sFilename)//'".'
            stop
          endif

          if (this%iStat(iIndex) == 0) this%lIsOpen(iIndex) = .true._c_bool

          sDatetime = make_timestamp()

          if ( lWrite_SWB_Info_l ) then

            write(this%iUnitNum(iIndex), fmt="(a)") "# USGS Soil Water Balance Code run log #"
            write(this%iUnitNum(iIndex), fmt="(a,/)") repeat("-", 80)
            write(this%iUnitNum(iIndex), fmt="(a,/)") "## Model run started on "//sDatetime//" ##"
            write(this%iUnitNum(iIndex), fmt="(a)") "## SWB version "//SWB_VERSION//" compiled on "  &
              //COMPILATION_TIMESTAMP//" ##"
            write(this%iUnitNum(iIndex), fmt="(a,/)") "Git branch and commit hash: " &
              //trim( GIT_BRANCH_STRING)//"  "//trim(GIT_COMMIT_HASH_STRING)

          endif

        else

          stop ( "Failed to open file logfile." )

        endif

      enddo

    endif


  end subroutine open_files_write_access_sub

!--------------------------------------------------------------------------------------------------

  subroutine close_files_sub(this)

    class (LOGFILE_T)    :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    do iIndex = 1,2

      if (this%lIsOpen(iIndex) ) then

        flush ( unit=this%iUnitNum(iIndex) )
        close ( unit=this%iUnitNum(iIndex) )

      endif

    enddo

  end subroutine close_files_sub

!--------------------------------------------------------------------------------------------------

  subroutine make_prefix_sub(this)

    class (LOGFILE_T)     :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iValues(8)
    character (len=2)    :: sHour
    character (len=2)    :: sMinutes
    character (len=2)    :: sSeconds

    character (len=2)    :: sDay
    character (len=2)    :: sMonth
    character (len=4)    :: sYear

    call DATE_AND_TIME( VALUES = iValues )

    write(sHour, fmt="(i0.2)") iValues( DT_HOUR )
    write(sMinutes, fmt="(i0.2)") iValues( DT_MINUTES )
    write(sSeconds, fmt="(i0.2)") iValues( DT_SECONDS )
    write(sMonth, fmt="(i0.2)") iValues( DT_MONTH )
    write(sDay, fmt="(i0.2)") iValues( DT_DAY )
    write(sYear, fmt="(i0.4)") iValues( DT_YEAR )

    this%sFilePrefix = "SWB_LOGFILE__"//sYear//sMonth//sDay//"_"  &
                                                   //sHour//sMinutes//sSeconds


  end subroutine make_prefix_sub

!--------------------------------------------------------------------------------------------------

  subroutine write_to_logfiles_sub(this, sMessage, iTab, iLinesBefore, iLinesAfter, iLogLevel, lEcho )

    class (LOGFILE_T)                            :: this
    character (len=*), intent(in)                :: sMessage
    integer (kind=c_int), intent(in), optional   :: iTab
    integer (kind=c_int), intent(in), optional   :: iLinesBefore
    integer (kind=c_int), intent(in), optional   :: iLinesAfter
    integer (kind=c_int), intent(in), optional   :: iLogLevel
    logical (kind=c_bool), intent(in), optional  :: lEcho

    ! [ LOCALS ]
    integer (kind=c_int)  :: iTab_l
    integer (kind=c_int)  :: iLinesBefore_l
    integer (kind=c_int)  :: iLinesAfter_l

    if (present(iLogLevel) )   call this%set_loglevel( iLogLevel )
    if (present(lEcho) )       call this%set_echo( lEcho )

    if (present(iTab) ) then
      iTab_l = iTab
    else
      iTab_l = 1
    endif

    if (present(iLinesBefore) ) then
      iLinesBefore_l = iLinesBefore
    else
      iLinesBefore_l = 0
    endif

    if (present(iLinesAfter) ) then
      iLinesAfter_l = iLinesAfter
    else
      iLinesAfter_l = 0
    endif

    if ( CURRENT_LOG_ECHO ) then

      call writeMultiLine(sMessageText=sMessage, iLU=OUTPUT_UNIT, &
        iTab=iTab_l, iLinesBefore=iLinesBefore_l, iLinesAfter=iLinesAfter_l )

    endif

    select case ( CURRENT_LOG_LEVEL )

      case ( LOG_GENERAL )

        if ( this%iLogLevel >= LOG_GENERAL ) &
          call writeMultiLine(sMessageText=sMessage, iLU=this%iUnitNum( LOG_GENERAL ), &
            iTab=iTab_l, iLinesBefore=iLinesBefore_l, iLinesAfter=iLinesAfter_l )

      case ( LOG_DEBUG )

        if ( this%iLogLevel >= LOG_DEBUG ) &
          call writeMultiLine(sMessageText=sMessage, iLU=this%iUnitNum( LOG_DEBUG ), &
            iTab=iTab_l, iLinesBefore=iLinesBefore_l, iLinesAfter=iLinesAfter_l )

      case ( LOG_ALL )

        if ( this%iLogLevel >= LOG_GENERAL ) &
          call writeMultiLine(sMessageText=sMessage, iLU=this%iUnitNum( LOG_GENERAL ), &
            iTab=iTab_l, iLinesBefore=iLinesBefore_l, iLinesAfter=iLinesAfter_l )

        if ( this%iLogLevel >= LOG_DEBUG ) &
          call writeMultiLine(sMessageText=sMessage, iLU=this%iUnitNum( LOG_DEBUG ), &
            iTab=iTab_l, iLinesBefore=iLinesBefore_l, iLinesAfter=iLinesAfter_l )

      case default

    end select

  end subroutine write_to_logfiles_sub

!--------------------------------------------------------------------------------------------------

  !> Write multiple lines of output to Fortran logical unit
  !> @details Writes one or more lines of an input text string to a Fortran
  !> logical unit number. To output multiple lines, insert a tilde (~) at
  !> each point in the text string where a carriage return is desired.
  !> @param[in] sMessageText Character string that contains the message to be written.
  !> @param[in] iLU Integer value of the Fortran logical unit number to write to.
  subroutine writeMultiLine(sMessageText, iLU, iTab, iLinesBefore, iLinesAfter)

    ! [ ARGUMENTS ]
    character (len=*), intent(in)     :: sMessageText
    integer (kind=c_int), intent(in)  :: iLU
    integer (kind=c_int), intent(in)  :: iTab
    integer (kind=c_int), intent(in)  :: iLinesBefore
    integer (kind=c_int), intent(in)  :: iLinesAfter


    ! [ LOCALS ]
    character (len=len(sMessageText) ) :: sRecord
    character (len=256) :: sItem
    logical (kind=c_bool) :: lFileOpen
    character (len=12) :: sFmt
    integer (kind=c_int) :: iIndex

    inquire (unit=iLU, opened=lFileOpen)

    sRecord = trim(sMessageText)

    if (lFileOpen) then

      if ( iLinesBefore > 0 ) then
        do iIndex=1, iLinesBefore
          write(UNIT=iLU, FMT="(a,2x)" ) ""
        enddo
      endif

      do

        ! break up string with '~' as delimiter
        call split(sRecord, sItem)
        write(sFmt, fmt="('(t',i0,' ,a)')") iTab
        if(len_trim(sItem) == 0) exit
        write(UNIT=iLU,FMT=trim(sFmt) ) trim(sItem)//"  "
      enddo

      if ( iLinesAfter > 0 ) then
        do iIndex=1, iLinesAfter
          write(UNIT=iLU,FMT="(a,2x)" ) ""
        enddo
      endif

      flush(iLU)

    endif

  end subroutine writeMultiLine

!--------------------------------------------------------------------------------------------------

  subroutine split(sText1, sText2)

    character (len=*), intent(inout)                     :: sText1
    character (len=*), intent(inout)                     :: sText2

    ! [ LOCALS ]
    character (len=1)    :: sDelimiter
    integer (kind=c_int) :: iIndex

    sDelimiter = "~"

    sText1 = adjustl(sText1)

    iIndex = scan( string = sText1, set = sDelimiter )

    if (iIndex == 0) then
      ! no delimiter found; return string as was supplied originally
      sText2 = sText1
      sText1 = ""
    else
      ! delimiters were found; split and return the chunks of text
      sText2 = trim( sText1(1:iIndex-1) )
      sText1 = trim( sText1(iIndex + 1:) )
    endif

  end subroutine split

!--------------------------------------------------------------------------------------------------

  function dquote(sText1)    result(sText)

    character (len=*), intent(in)         :: sText1
    character (len=len_trim(sText1)+2)    :: sText

    sText = '"'//trim(sText1)//'"'

  end function dquote

!--------------------------------------------------------------------------------------------------

  function make_timestamp()    result(sDatetime)

    character (len=:), allocatable   :: sDatetime

    ! [ LOCALS ]
    integer (kind=c_int) :: iValues(8)
    character (len=2)    :: sHour
    character (len=2)    :: sMinutes
    character (len=2)    :: sSeconds

    character (len=2)    :: sDay
    character (len=2)    :: sMonth
    character (len=4)    :: sYear
    character (len=9)    :: sMonthName

    character (len=9), parameter :: MONTHS(12) = &
      ["January  ", "February ", "March    ", "April    ", "May      ", "June     ", "July     ", &
       "August   ", "September", "October  ", "November ", "December "]

    call DATE_AND_TIME( VALUES = iValues )

    write(sHour, fmt="(i0.2)") iValues( DT_HOUR )
    write(sMinutes, fmt="(i0.2)") iValues( DT_MINUTES )
    write(sSeconds, fmt="(i0.2)") iValues( DT_SECONDS )
    write(sMonth, fmt="(i0.2)") iValues( DT_MONTH )
    write(sDay, fmt="(i0.2)") iValues( DT_DAY )
    write(sYear, fmt="(i0.4)") iValues( DT_YEAR )

    sMonthName = MONTHS( iValues( DT_MONTH ) )

    sDatetime = trim(sMonthName)//" "//sDay//" "//sYear//" "//sHour//":"//sMinutes//":"//sSeconds

  end function make_timestamp


end module logfiles
