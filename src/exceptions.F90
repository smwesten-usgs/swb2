module exceptions

  use iso_c_binding
  use iso_fortran_env, only : OUTPUT_UNIT
  use types_new
  use logfiles, only : LOGS
  implicit none

  private

  public :: assert, die, warn, check_warnings

  interface  assert
     module procedure :: assert_4bit
     module procedure :: assert_1bit
  end interface assert

  integer (kind=c_int) :: NUMBER_OF_FATAL_WARNINGS = 0

contains

   subroutine die(sMessage, sModule, iLine, sHints)

    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in), optional     :: sModule
    integer (kind=c_int), intent(in), optional  :: iLine 
    character (len=*), intent(in), optional     :: sHints
!    integer (kind=c_int), intent(in), optional  :: iLU
    
    ! [ LOCALS ]
    character (len=6) :: sLineNum

    call LOGS%echo( " " )
    call LOGS%echo( "** ERROR -- PROGRAM EXECUTION HALTED **" )
    call LOGS%echo( " " )
    call LOGS%echo( "         error condition:  "//trim(sMessage) )

    if (present(sModule))  &
      call LOGS%echo( "                  module:  "//trim(sModule) )

    if (present(iLine)) then
      write(sLineNum, fmt="(i0)") iLine
      call LOGS%echo( "               line number:  "//trim(sLineNum) )
    endif  

    if (present(sHints)) &
      call LOGS%echo( "   ==> "//trim(sHints) )

    call LOGS%echo(" ")

    stop
 
  end subroutine die

  subroutine check_warnings()

    ! [ LOCALS ]
    character (len=6) :: sNumWarnings

    if ( NUMBER_OF_FATAL_WARNINGS > 0 ) then
      write(unit=sNumWarnings, fmt="(i0)") NUMBER_OF_FATAL_WARNINGS
      call die( sMessage=trim(adjustl(sNumWarnings))//" warnings were issued and will "  &
        //"cause serious problems later in the run.", &
        sHints="Please address the warnings and try again.")
    endif  

  end subroutine check_warnings  

   subroutine warn(sMessage, sModule, iLine, sHints, lFatal)

    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in), optional     :: sModule
    integer (kind=c_int), intent(in), optional  :: iLine 
    character (len=*), intent(in), optional     :: sHints
    logical (kind=c_bool), intent(in), optional :: lFatal
!    integer (kind=c_int), intent(in), optional  :: iLU
    
    ! [ LOCALS ]
    integer (kind=c_int) :: iLU
    character (len=32)   :: sBuf

    iLU = OUTPUT_UNIT
    
    call LOGS%echo(" "); call LOGS%echo(" ")
    call LOGS%echo("  ** WARNING **")
    call LOGS%echo("      possible error:  "//trim(sMessage) )

    if (present(sModule))  &
      call LOGS%echo("              module:  "//trim(sModule) )

    if (present(iLine)) then
      write(sBuf, fmt="(i0)") iLine
      call LOGS%echo("             line no:  "//trim(sBuf) )
    endif  

    if (present(sHints)) &
      call LOGS%echo("   ==> "//trim(sHints) )

      call LOGS%echo(" ")

    if (present(lFatal)) then
      if (lFatal)  NUMBER_OF_FATAL_WARNINGS = NUMBER_OF_FATAL_WARNINGS + 1
    endif  
 
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