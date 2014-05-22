module exceptions

  use iso_c_binding
  use iso_fortran_env, only : OUTPUT_UNIT
  use types_new
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
    integer (kind=c_int) :: iLU

    iLU = OUTPUT_UNIT

    write (unit=iLU, fmt="(/,/,a)") "** ERROR -- PROGRAM EXECUTION HALTED **"
    write (unit=iLU, fmt="(/,t3,a19,t22,a)") "error condition:  ", trim(sMessage)

    if (present(sModule))  &
      write (unit=iLU, fmt="(t3,a19,t22,a)") "module:  ", trim(sModule)

    if (present(iLine))  &
      write (unit=iLU, fmt="(t3,a19,t22,i0)") "line no:  ", iLine

    if (present(sHints)) &
      write (unit=iLU, fmt="(/,t2,'==>  ',a)") sHints

    write (unit=iLU, fmt="(/)")

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

    iLU = OUTPUT_UNIT

    write (unit=iLU, fmt="(/,/,a)") "** WARNING **"
    write (unit=iLU, fmt="(/,t3,a19,t22,a)") "possible error:  ", trim(sMessage)

    if (present(sModule))  &
      write (unit=iLU, fmt="(t3,a19,t22,a)")     "module:  ", trim(sModule)

    if (present(iLine))  &
      write (unit=iLU, fmt="(t3,a19,t22,i0)")  "line no:  ", iLine

    if (present(sHints)) &
      write (unit=iLU, fmt="(/,t3,'==>  ',a)") sHints

    write (unit=iLU, fmt="(/)")

    if (present(lFatal)) then
      if (lFatal)  NUMBER_OF_FATAL_WARNINGS = NUMBER_OF_FATAL_WARNINGS + 1
    endif  
 
  end subroutine warn



  subroutine assert_1bit(lCondition, sMessage, sModule, iLine)

    logical (kind=c_bool), intent(in)           :: lCondition
    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in)               :: sModule
    integer (kind=c_int), intent(in)            :: iLine 

    if (.not. lCondition) then

      call die(sMessage, sModule, iLine)

    endif      

  end subroutine assert_1bit


  subroutine assert_4bit(lCondition, sMessage, sModule, iLine)

    logical (kind=4), intent(in)                :: lCondition
    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in)               :: sModule
    integer (kind=c_int), intent(in)            :: iLine 


    if (.not. lCondition) then

      call die(sMessage, sModule, iLine)

    endif      

  end subroutine assert_4bit


end module exceptions