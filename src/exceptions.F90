module exceptions

  use iso_c_binding
  use iso_fortran_env, only : OUTPUT_UNIT
  use logfiles, only : LOGS, LOG_GENERAL, LOG_ALL
  implicit none

  private

  public :: assert, die, warn, check_for_fatal_warnings, index_values_valid

  logical (c_bool), parameter :: TRUE = .true._c_bool
  logical (c_bool), parameter :: FALSE = .false._c_bool

  interface  assert
     module procedure :: assert_4bit
     module procedure :: assert_1bit
  end interface assert

  interface index_values_valid
    module procedure :: bounds_check_integer_1d
    module procedure :: bounds_check_float_1d
    module procedure :: bounds_check_double_1d
    module procedure :: bounds_check_integer_2d
    module procedure :: bounds_check_float_2d
    module procedure :: bounds_check_double_2d
  end interface index_values_valid

  integer (c_int), public     :: NUMBER_OF_FATAL_WARNINGS = 0
  integer (c_int), parameter  :: MAX_FATAL_WARNINGS = 50
  character (len=256)         :: WARNING_TEXT( MAX_FATAL_WARNINGS )
  logical (c_bool), public    :: HALT_UPON_FATAL_ERROR = TRUE

contains

   function bounds_check_integer_1d( array_vals, array_index )   result(bounds_ok)

     integer (c_int), intent(in)  :: array_vals(:)
     integer (c_int), intent(in)  :: array_index
     logical (c_bool)             :: bounds_ok

     bounds_ok = FALSE

     if (      lbound(array_vals,1) <= array_index                             &
         .and. ubound(array_vals,1) >= array_index ) bounds_ok = TRUE

   end function bounds_check_integer_1d

!------------------------------------------------------------------------------------------------

   function bounds_check_float_1d( array_vals, array_index )   result(bounds_ok)

     real (c_float), intent(in)   :: array_vals(:)
     integer (c_int), intent(in)  :: array_index
     logical (c_bool)             :: bounds_ok

     bounds_ok = FALSE

     if (      lbound(array_vals,1) <= array_index                             &
         .and. ubound(array_vals,1) >= array_index ) bounds_ok = TRUE

   end function bounds_check_float_1d

!------------------------------------------------------------------------------------------------

   function bounds_check_double_1d( array_vals, array_index )   result(bounds_ok)

     real (c_double), intent(in)  :: array_vals(:)
     integer (c_int), intent(in)  :: array_index
     logical (c_bool)             :: bounds_ok

     bounds_ok = FALSE

     if (      lbound(array_vals,1) <= array_index                             &
         .and. ubound(array_vals,1) >= array_index ) bounds_ok = TRUE

   end function bounds_check_double_1d

!------------------------------------------------------------------------------------------------

   function bounds_check_integer_2d( array_vals, array_index1,                 &
                                                 array_index2 )  result(bounds_ok)

     integer (c_int), intent(in)  :: array_vals(:,:)
     integer (c_int), intent(in)  :: array_index1
     integer (c_int), intent(in)  :: array_index2
     logical (c_bool)             :: bounds_ok

     bounds_ok = FALSE

     if (      lbound(array_vals,1) <= array_index1                            &
         .and. ubound(array_vals,1) >= array_index1                            &
         .and. lbound(array_vals,2) <= array_index2                            &
         .and. ubound(array_vals,2) >= array_index2 )  bounds_ok = TRUE

   end function bounds_check_integer_2d

!------------------------------------------------------------------------------------------------

   function bounds_check_float_2d( array_vals, array_index1,                 &
                                               array_index2 )  result(bounds_ok)

     real (c_float), intent(in)   :: array_vals(:,:)
     integer (c_int), intent(in)  :: array_index1
     integer (c_int), intent(in)  :: array_index2
     logical (c_bool)             :: bounds_ok

     bounds_ok = FALSE

     if (      lbound(array_vals,1) <= array_index1                            &
         .and. ubound(array_vals,1) >= array_index1                            &
         .and. lbound(array_vals,2) <= array_index2                            &
         .and. ubound(array_vals,2) >= array_index2 )  bounds_ok = TRUE

   end function bounds_check_float_2d

!------------------------------------------------------------------------------------------------

   function bounds_check_double_2d( array_vals, array_index1,                 &
                                                array_index2 )  result(bounds_ok)

     real (c_double), intent(in)  :: array_vals(:,:)
     integer (c_int), intent(in)  :: array_index1
     integer (c_int), intent(in)  :: array_index2
     logical (c_bool)             :: bounds_ok

     bounds_ok = FALSE

     if (      lbound(array_vals,1) <= array_index1                            &
         .and. ubound(array_vals,1) >= array_index1                            &
         .and. lbound(array_vals,2) <= array_index2                            &
         .and. ubound(array_vals,2) >= array_index2 )  bounds_ok = TRUE

   end function bounds_check_double_2d

!------------------------------------------------------------------------------------------------

   subroutine die(sMessage, sModule, iLine, sHints, sCalledBy, iCalledByLine )

    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in), optional     :: sModule
    integer (c_int), intent(in), optional       :: iLine
    character (len=*), intent(in), optional     :: sHints
    character (len=*), intent(in), optional     :: sCalledBy
    integer (c_int), intent(in), optional       :: iCalledByLine

!    integer (c_int), intent(in), optional  :: iLU

    ! [ LOCALS ]
    character (len=6) :: sLineNum

    call LOGS%set_loglevel( LOG_ALL )

    if (HALT_UPON_FATAL_ERROR)   call LOGS%set_echo( .true._c_bool )

    call LOGS%write( "error condition:  "//trim(sMessage), iTab=12, iLinesBefore=1 )

    if ( present( sCalledBy ) )  &
      call LOGS%write( "called by:  "//trim(sCalledBy), iTab=18 )

    if (present(iCalledByLine)) then
      write(sLineNum, fmt="(i0)") iCalledByLine
      call LOGS%write( "line number:  "//trim(sLineNum), iTab=16 )
    endif

    if (present(sModule))  &
      call LOGS%write( "module:  "//trim(sModule), iTab=21 )

    if (present(iLine)) then
      write(sLineNum, fmt="(i0)") iLine
      call LOGS%write( "line number:  "//trim(sLineNum), iTab=16 )
    endif

    if (present(sHints)) then
      if ( len_trim(sHints) > 0 )  &
        call LOGS%write( "==> "//trim(sHints), iTab=12 )
    endif

    call LOGS%write("", iLinesAfter=1)

    if (HALT_UPON_FATAL_ERROR) then
      call LOGS%write( "** ERROR -- PROGRAM EXECUTION HALTED **", iLinesBefore=1, iLinesAfter=1 )
      stop
    endif

  end subroutine die

!------------------------------------------------------------------------------------------------

  subroutine check_for_fatal_warnings()

    ! [ LOCALS ]
    character (len=6)     :: sNumWarnings
    character (len=6)     :: sMaxWarnings
    character (len=6)     :: sIndex
    integer (c_int)  :: iIndex
    character (len=10)    :: sBigS
    character (len=1)     :: sLittleS

    if ( NUMBER_OF_FATAL_WARNINGS >= 1 ) then

      if ( NUMBER_OF_FATAL_WARNINGS > 1 ) then
        sBigS = "S WERE"
        sLittleS = "s"
      else
        sBigS = " WAS"
        sLittleS = ""
      endif

      call LOGS%set_loglevel( LOG_ALL )
      call LOGS%set_echo( .true._c_bool )

      write(unit=sNumWarnings, fmt="(i0)") NUMBER_OF_FATAL_WARNINGS
      call LOGS%write( "** "//trim(adjustl(sNumWarnings))//" FATAL WARNING"//trim(sBigS)     &
                       //" DETECTED IN INPUT **", iLinesBefore=1, iLinesAfter=1 )

      call LOGS%write( "# Summary of fatal warning"//trim(sLittleS)//" #" )
      call LOGS%write( "-------------------------------", iLinesAfter=1 )

      do iIndex = 1, NUMBER_OF_FATAL_WARNINGS
        if (iIndex <= MAX_FATAL_WARNINGS ) then
          write(unit=sIndex, fmt="(i0)") iIndex
          call LOGS%write( trim(adjustl(sIndex))//": "//trim(WARNING_TEXT(iIndex)), iLinesBefore=1)
        endif
      enddo

      if ( NUMBER_OF_FATAL_WARNINGS >= MAX_FATAL_WARNINGS ) then
        write(unit=sMaxWarnings, fmt="(i0)") MAX_FATAL_WARNINGS
        call LOGS%write( "*There were more than "//trim(adjustl(sMaxWarnings))//" fatal warnings. " &
          //" Only a partial list of warnings is shown above.*", iLinesBefore=1, iLinesAfter=1, iTab=2 )
      endif

      call die( sMessage="Fatal warning"//trim(sLittleS)//" associated with input.", &
                sHints="Address the problem"//trim(sLittleS)//" listed above and try again." )

    endif

  end subroutine check_for_fatal_warnings

!------------------------------------------------------------------------------------------------

  subroutine warn(sMessage, sModule, iLine, sHints, lFatal, iLogLevel, lEcho)

    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in), optional     :: sModule
    integer (c_int), intent(in), optional  :: iLine
    character (len=*), intent(in), optional     :: sHints
    logical (c_bool), intent(in), optional :: lFatal
    integer (c_int), intent(in), optional  :: iLogLevel
    logical (c_bool), intent(in), optional :: lEcho
!    integer (c_int), intent(in), optional  :: iLU

    ! [ LOCALS ]
    character (len=32)   :: sBuf

    if ( present( iLogLevel ) )   call LOGS%set_loglevel( iLogLevel )
    if ( present( lEcho ) )       call LOGS%set_echo( lEcho )

    if (present(lFatal)) then
      if (lFatal) then
        NUMBER_OF_FATAL_WARNINGS = NUMBER_OF_FATAL_WARNINGS + 1
        call LOGS%write(" ** WARNING fatal error: **", iTab=6, iLinesBefore=1)
        call LOGS%write( trim(sMessage), iTab=16 )

        if (NUMBER_OF_FATAL_WARNINGS <= ubound( WARNING_TEXT,1 ) )     &
          WARNING_TEXT( NUMBER_OF_FATAL_WARNINGS ) = trim(sMessage)
      else
        call LOGS%write(" ** WARNING **", iTab=10, iLinesBefore=1)
        call LOGS%write( trim(sMessage), iTab=16 )
      endif
    else
      call LOGS%write(" ** WARNING **", iTab=10, iLinesBefore=1)
      call LOGS%write( trim(sMessage), iTab=16 )
    endif

    if (present(sModule))  &
      call LOGS%write("module:  "//trim(sModule), iTab=18 )

    if (present(iLine)) then
      write(sBuf, fmt="(i0)") iLine
      call LOGS%write("line no:  "//trim(sBuf), iTab=18 )
    endif

    if (present(sHints)) then
      if ( len_trim(sHints) > 0 )  &
        call LOGS%write("   ==> "//trim(sHints), iTab=9, iLinesBefore=1 )
    endif

    call LOGS%write("", iLinesAfter=1)

  end subroutine warn

!------------------------------------------------------------------------------------------------

  subroutine assert_1bit(lCondition, sMessage, sModule, iLine, sCalledBy, iCalledByLine, sHints )

    logical (c_bool), intent(in)                :: lCondition
    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in), optional     :: sHints
    character (len=*), intent(in), optional     :: sCalledBy
    integer (c_int), intent(in), optional       :: iCalledByLine
    character (len=*), intent(in), optional     :: sModule
    integer (c_int), intent(in), optional       :: iLine

    character (len=256) :: sHints_l

    if (.not. lCondition) then

      if ( present( sHints ) ) then
        sHints_l = trim( sHints )
      else
        sHints_l = ""
      endif

      if (present( sCalledBy ) .and. present( iCalledByLine ) &
        .and. present(sModule) .and. present(iLine) ) then
        call die( sMessage=sMessage, sCalledBy=sCalledBy, iCalledByLine=iCalledByLine, &
          sModule=sModule, iLine=iLine, sHints=sHints_l )
      elseif ( present(sModule) .and. present(iLine) ) then
        call die( sMessage=sMessage, sModule=sModule, iLine=iLine, sHints=sHints_l )
      elseif ( present(sModule) ) then
        call die( sMessage=sMessage, sModule=sModule, sHints=sHints_l )
      else
        call die( sMessage=sMessage, sHints=sHints_l )
      endif

    endif

  end subroutine assert_1bit

!------------------------------------------------------------------------------------------------

subroutine assert_4bit(lCondition, sMessage, sModule, iLine, sCalledBy, iCalledByLine, sHints )

  logical (4), intent(in)                     :: lCondition
  character (len=*), intent(in)               :: sMessage
  character (len=*), intent(in), optional     :: sHints
  character (len=*), intent(in), optional     :: sCalledBy
  integer (c_int), intent(in), optional       :: iCalledByLine
  character (len=*), intent(in), optional     :: sModule
  integer (c_int), intent(in), optional       :: iLine

  character (len=256) :: sHints_l

  if (.not. lCondition) then

    if ( present( sHints ) ) then
      sHints_l = trim( sHints )
    else
      sHints_l = ""
    endif

    if (present( sCalledBy ) .and. present( iCalledByLine ) &
      .and. present(sModule) .and. present(iLine) ) then
      call die( sMessage=sMessage, sCalledBy=sCalledBy, iCalledByLine=iCalledByLine, &
        sModule=sModule, iLine=iLine, sHints=sHints_l )
    elseif ( present(sModule) .and. present(iLine) ) then
      call die( sMessage=sMessage, sModule=sModule, iLine=iLine, sHints=sHints_l )
    elseif ( present(sModule) ) then
      call die( sMessage=sMessage, sModule=sModule, sHints=sHints_l )
    else
      call die( sMessage=sMessage, sHints=sHints_l )
    endif

  endif

end subroutine assert_4bit

!------------------------------------------------------------------------------------------------

end module exceptions
