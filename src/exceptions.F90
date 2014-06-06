module exceptions

  use iso_c_binding
  use iso_fortran_env, only : OUTPUT_UNIT
  use logfiles, only : LOGS, LOG_GENERAL, LOG_ALL
  implicit none

  private

  public :: assert, die, warn, check_warnings

  interface  assert
     module procedure :: assert_4bit
     module procedure :: assert_1bit
  end interface assert

  integer (kind=c_int), public :: NUMBER_OF_FATAL_WARNINGS = 0

contains

   subroutine die(sMessage, sModule, iLine, sHints)

    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in), optional     :: sModule
    integer (kind=c_int), intent(in), optional  :: iLine 
    character (len=*), intent(in), optional     :: sHints
!    integer (kind=c_int), intent(in), optional  :: iLU
    
    ! [ LOCALS ]
    character (len=6) :: sLineNum

    call LOGS%set_loglevel( LOG_ALL )
    call LOGS%set_echo( .true._c_bool )

    call LOGS%write( "** ERROR -- PROGRAM EXECUTION HALTED **", iLinesBefore=1, iLinesAfter=1 )
    call LOGS%write( "error condition:  "//trim(sMessage), iTab=12 )

    if (present(sModule))  &
      call LOGS%write( "module:  "//trim(sModule), iTab=21 )

    if (present(iLine)) then
      write(sLineNum, fmt="(i0)") iLine
      call LOGS%write( "line number:  "//trim(sLineNum), iTab=16)
    endif  

    if (present(sHints)) &
      call LOGS%write( "   ==> "//trim(sHints), iTab=9, iLinesBefore=1)

    call LOGS%write("", iLinesAfter=1)

    stop
 
  end subroutine die

  subroutine check_warnings()

    ! [ LOCALS ]
    character (len=6) :: sNumWarnings

    if ( NUMBER_OF_FATAL_WARNINGS == 1 ) then

      write(unit=sNumWarnings, fmt="(i0)") NUMBER_OF_FATAL_WARNINGS
      call die( sMessage=trim(adjustl(sNumWarnings))//" warning was issued and will "  &
        //"cause serious problems later in the run.", &
        sHints="Please check the logfile, address the warnings, and try again.")
   
    elseif ( NUMBER_OF_FATAL_WARNINGS > 1 ) then
   
      write(unit=sNumWarnings, fmt="(i0)") NUMBER_OF_FATAL_WARNINGS
      call die( sMessage=trim(adjustl(sNumWarnings))//" warnings were issued and will "  &
        //"cause serious problems later in the run.", &
        sHints="Please check the logfile, address the warnings, and try again.")
   
    endif  


  end subroutine check_warnings  

   subroutine warn(sMessage, sModule, iLine, sHints, lFatal, iLogLevel, lEcho)

    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in), optional     :: sModule
    integer (kind=c_int), intent(in), optional  :: iLine 
    character (len=*), intent(in), optional     :: sHints
    logical (kind=c_bool), intent(in), optional :: lFatal
    integer (kind=c_int), intent(in), optional  :: iLogLevel
    logical (kind=c_bool), intent(in), optional :: lEcho
!    integer (kind=c_int), intent(in), optional  :: iLU
    
    ! [ LOCALS ]
    character (len=32)   :: sBuf

    if ( present( iLogLevel ) )   call LOGS%set_loglevel( iLogLevel )
    if ( present( lEcho ) )       call LOGS%set_echo( lEcho )

    call LOGS%write("** WARNING **", iLinesBefore=1, iTab=2)

    if (present(lFatal)) then
      if (lFatal) then
        NUMBER_OF_FATAL_WARNINGS = NUMBER_OF_FATAL_WARNINGS + 1
        call LOGS%write("fatal error:  "//trim(sMessage), iTab=16 )
      endif
    else
      call LOGS%write("possible error:  "//trim(sMessage), iTab=12 )
    endif  

    if (present(sModule))  &
      call LOGS%write("module:  "//trim(sModule), iTab=20 )

    if (present(iLine)) then
      write(sBuf, fmt="(i0)") iLine
      call LOGS%write("line no:  "//trim(sBuf), iTab=19 )
    endif  

    if (present(sHints)) &
      call LOGS%write("   ==> "//trim(sHints), iTab=9, iLinesBefore=1 )

    call LOGS%write("", iLinesAfter=1)  

  end subroutine warn



  subroutine assert_1bit(lCondition, sMessage, sModule, iLine)

    logical (kind=c_bool), intent(in)           :: lCondition
    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in), optional     :: sModule
    integer (kind=c_int), intent(in), optional  :: iLine 

    if (.not. lCondition) then

      if ( present(sModule) .and. present(iLine) ) then
        call die( sMessage=sMessage, sModule=sModule, iLine=iLine )
      elseif ( present(sModule) ) then
        call die( sMessage=sMessage, sModule=sModule )  
      elseif ( present(iLine) ) then
        call die( sMessage=sMessage, iLine=iLine )
      else
        call die( sMessage=sMessage )  
      endif

    endif      

  end subroutine assert_1bit


  subroutine assert_4bit(lCondition, sMessage, sModule, iLine)

    logical (kind=4), intent(in)                :: lCondition
    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in), optional     :: sModule
    integer (kind=c_int), intent(in), optional  :: iLine 


    if (.not. lCondition) then

      if ( present(sModule) .and. present(iLine) ) then
        call die( sMessage=sMessage, sModule=sModule, iLine=iLine )
      elseif ( present(sModule) ) then
        call die( sMessage=sMessage, sModule=sModule )  
      elseif ( present(iLine) ) then
        call die( sMessage=sMessage, iLine=iLine )
      else
        call die( sMessage=sMessage )  
      endif

    endif      

  end subroutine assert_4bit


end module exceptions