module logfiles

  use iso_c_binding, only : c_bool, c_int
  use iso_fortran_env, only : OUTPUT_UNIT
  implicit none

  private

  public :: LOGFILE_T

  enum, bind(c)
    enumerator :: DT_YEAR = 1, DT_MONTH = 2, DT_DAY = 3, DT_DIFF_FM_UTC = 4,          &
                  DT_HOUR = 5, DT_MINUTES = 6, DT_SECONDS = 7, DT_MILLISECONDS = 8
  end enum

  enum, bind(c)
    enumerator :: LOG_NONE = 0, LOG_GENERAL = 1, LOG_DETAILED = 2, LOG_ALL = 3
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

    procedure, private :: write_to_logfiles_and_screen_sub
    generic            :: echo => write_to_logfiles_sub

    procedure, private :: open_files_write_access_sub
    generic            :: open => open_files_write_access_sub

    procedure, private :: close_files_sub
    generic            :: close => close_files_sub

    procedure, private :: make_prefix_sub
    generic            :: make_prefix => make_prefix_sub

  end type LOGFILE_T

  type (LOGFILE_T), public :: LOGS

contains

  subroutine initialize_logfiles_sub(this)

    class (LOGFILE_T)  :: this


  end subroutine initialize_logfiles_sub


  subroutine open_files_write_access_sub(this)

    class (LOGFILE_T), intent(inout) :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    character (len=:), allocatable   :: sFilename
    character (len=12)               :: sDescriptor(2) = [ "GENERAL ", "DETAILED" ]

    if ( this%iLogLevel /= LOG_NONE ) then

      do iIndex = 1, min(this%iLogLevel, 2)

        sFilename = trim(this%sFilePrefix)//"_"//trim(sDescriptor(iIndex))//".txt"

        if (.not. this%lIsOpen(iIndex) ) then

          open(newunit=this%iUnitNum(iIndex), file=sFilename, iostat=this%iStat(iIndex), action='WRITE')
          call assert(this%iStat(iIndex) == 0, "Failed to open file.", __FILE__, __LINE__)

          if (this%iStat(iIndex) == 0) this%lIsOpen(iIndex) = .true._c_bool
   
        else

          stop( "Failed to open file logfile." )

        endif

      enddo
      
    endif
        

  end subroutine open_files_write_access_sub


  subroutine close_files_sub(this)

    class (LOGFILE_T)    :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    do iIndex = 1,2

      if (this%lIsOpen(iIndex) )  close( unit=this%iUnitNum(iIndex) )

    enddo

  end subroutine close_files_sub


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

    this%sFilePrefix = "SWB_LOGFILE__"//sYear//"_"//sMonth//"_"//sDay//"__"  &
                                                   //sHour//"-"//sMinutes//"-"//sSeconds


  end subroutine make_prefix_sub


  subroutine write_to_logfiles_sub(this, sMessage, iLogLevel )

    class (LOGFILE_T)                            :: this
    character (len=*), intent(in)                :: sMessage
    integer (kind=c_int), intent(in), optional   :: iLogLevel

    ! [ LOCALS ]
    integer (kind=c_int)  :: iLogLevel_

    ! if no loglevel value supplied by user, assume that only a general level of logging is desired.
    if ( present(iLogLevel) ) then
      iLogLevel_ = min( iLogLevel, this%iLogLevel )
    else
      iLogLevel_ = min( 1, this%iLogLevel )
    endif

    select case (iLogLevel_)

      case ( LOG_GENERAL )

        call writeMultiLine(sMessageText=sMessage, iLU=this%iUnitNum( LOG_GENERAL ) )

      case ( LOG_DETAILED )

        call writeMultiLine(sMessageText=sMessage, iLU=this%iUnitNum( LOG_DETAILED ) )

      case ( LOG_ALL )

        call writeMultiLine(sMessageText=sMessage, iLU=this%iUnitNum( LOG_GENERAL ) )
        call writeMultiLine(sMessageText=sMessage, iLU=this%iUnitNum( LOG_DETAILED ) )

      case default

    end select  

  end subroutine write_to_logfiles_sub

  !> Write multiple lines of output to Fortran logical unit
  !> @details Writes one or more lines of an input text string to a Fortran
  !> logical unit number. To output multiple lines, insert a tilde (~) at
  !> each point in the text string where a carriage return is desired.
  !> @param[in] sMessageText Character string that contains the message to be written.
  !> @param[in] iLU Integer value of the Fortran logical unit number to write to.
  subroutine writeMultiLine(sMessageText, iLU)

    ! [ ARGUMENTS ]
    character (len=*) :: sMessageText
    integer (kind=c_int) :: iLU

    ! [ LOCALS ]
    character (len=len(sMessageText) ) :: sRecord
    character (len=256) :: sItem
    logical (kind=c_bool) :: lFileOpen

    inquire (unit=iLU, opened=lFileOpen)

    sRecord = trim(sMessageText)

    if (lFileOpen) then

      do

        ! break up string with '~' as delimiter
        call split(sRecord, sItem)
        if(len_trim(sItem) == 0) exit
        write(UNIT=iLU,FMT="(a)") trim(sItem)
      enddo

    endif

  end subroutine writeMultiLine


!> echo to screen AND write to logfile
  subroutine write_to_logfiles_and_screen_sub(this, sMessage)

    class (LOGFILE_T)                    :: this
    character(len=*), intent(in)         :: sMessage

    call writeMultiLine(sMessage, OUTPUT_UNIT ) 
    call this%write( sMessage ) 

  end subroutine write_to_logfiles_and_screen_sub


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


  function dquote(sText1)    result(sText)

    character (len=*), intent(in)         :: sText1
    character (len=len_trim(sText1)+2)    :: sText

    sText = '"'//trim(sText1)//'"'

  end function dquote

end module logfiles