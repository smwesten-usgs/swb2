module exceptions

  use iso_c_binding
  use iso_fortran_env, only : OUTPUT_UNIT
  use types
  implicit none

  private

  public :: assert, die, warn

  interface  assert
     module procedure :: assert_4bit
     module procedure :: assert_1bit
  end interface assert



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
    write (unit=iLU, fmt="(/,t5,a,t23,a)") "error condition: ", trim(sMessage)

    if (present(sModule))  &
      write (unit=iLU, fmt="(t5,a,t23,a)")     "module: ", trim(sModule)

    if (present(iLine))  &
      write (unit=iLU, fmt="(t5,a,t23,i0)")  "line no: ", iLine

    if (present(sHints)) &
      write (unit=iLU, fmt="(/,t5,'==> ',a)") sHints

    write (unit=iLU, fmt="(/)")

    stop
 
  end subroutine die


   subroutine warn(sMessage, sModule, iLine, sHints)

    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in), optional     :: sModule
    integer (kind=c_int), intent(in), optional  :: iLine 
    character (len=*), intent(in), optional     :: sHints
!    integer (kind=c_int), intent(in), optional  :: iLU
    
    ! [ LOCALS ]
    integer (kind=c_int) :: iLU

    iLU = OUTPUT_UNIT

    write (unit=iLU, fmt="(/,/,a)") "** WARNING **"
    write (unit=iLU, fmt="(/,t5,a,t23,a)") "possible error: ", trim(sMessage)

    if (present(sModule))  &
      write (unit=iLU, fmt="(t5,a,t23,a)")     "module: ", trim(sModule)

    if (present(iLine))  &
      write (unit=iLU, fmt="(t5,a,t23,i0)")  "line no: ", iLine

    if (present(sHints)) &
      write (unit=iLU, fmt="(/,t5,'==> ',a)") sHints

    write (unit=iLU, fmt="(/)")
 
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