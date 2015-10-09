!> @file
!>  Main program which references all other modules; execution begins here.


!>  Main program which references all other modules; execution begins here.
!>
!> Accepts command-line arguments and makes a single call
!> to the control_setModelOptions routine in module \ref control.
program swb_launch

  use iso_c_binding, only    : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use strings
  use dictionary
  use logfiles, only         : LOGS, LOG_DEBUG
  use file_utilities, only   : mkdir, get_libc_errno, get_libc_err_string,   &
                                c_get_libc_errno
  use model_initialize, only : initialize_all, read_control_file, write_control_file
  use version_control, only  : SWB_VERSION, GIT_COMMIT_HASH_STRING, &
                               GIT_BRANCH_STRING, COMPILE_DATE, COMPILE_TIME
  use string_list, only      : STRING_LIST_T
  use iso_fortran_env

  implicit none

  type (STRING_LIST_T)             :: slControlFiles

  character (len=256)              :: sBuf
  real (kind=c_float), allocatable :: fValues(:)
  character (len=64)               :: sOutputDirectoryName = ""
  integer (kind=c_int)             :: iNumArgs
  character (len=1024)             :: sCompilerFlags       = ""
  character (len=256)              :: sCompilerVersion     = ""
  character (len=256)              :: sVersionString       = ""
  character (len=256)              :: sGitHashString       = ""
  integer (kind=c_int)             :: iCount
  integer (kind=c_int)             :: iIndex, iIndex1, iIndex2
  integer (kind=c_int)             :: iLen
  real (kind=c_float)              :: divisions_x, divisions_y
  real (kind=c_float)              :: fCellSize
  character (len=64)               :: filename
  integer (kind=c_int)             :: loc_err
  character(len=:), allocatable    :: err_string

  real (kind=c_float)              :: fUR_x, fUR_y, fLL_x, fLL_y
  real (kind=c_float)              :: fLL_x1, fLL_y1

  integer (kind=c_int)             :: iNX, iNY, iNX1, iNY1

  iNumArgs = COMMAND_ARGUMENT_COUNT()

  sVersionString = "  Soil Water Balance Code version "//trim( SWB_VERSION )    &
      //" -- compiled on: "//trim(COMPILE_DATE)//" "//trim(COMPILE_TIME)

  sGitHashString = "    [ Git branch and commit hash: "//trim( GIT_BRANCH_STRING )    &
     //", "//trim( GIT_COMMIT_HASH_STRING )//" ]"

  iCount = max( len_trim( sVersionString ), len_trim( sGitHashString ) )

  write(unit=*, fmt="(/,a)") repeat("-",iCount + 2)  
  write(UNIT=*,FMT="(a)") trim( sVersionString )
  write(UNIT=*,FMT="(a)") trim( sGitHashString )  
  write(unit=*, fmt="(a,/)") repeat("-",iCount + 2)  

  if(iNumArgs == 0 ) then

#ifdef __GFORTRAN__
    sCompilerFlags = COMPILER_OPTIONS()
    sCompilerVersion = COMPILER_VERSION()
    write(UNIT=*,FMT="(a,/)") "Compiled with: gfortran ("//TRIM(sCompilerVersion)//")"
    write(UNIT=*,FMT="(a)") "Compiler flags:"
    write(UNIT=*,FMT="(a)") "-------------------------------"
    write(UNIT=*,FMT="(a,/)") TRIM(sCompilerFlags)
#endif

#ifdef __INTEL_COMPILER
    write(UNIT=*,FMT="(a)") "Compiled with: Intel Fortran version " &
      //TRIM(int2char(__INTEL_COMPILER))
      write(UNIT=*,FMT="(a,/)") "Compiler build date:"//TRIM(int2char(__INTEL_COMPILER_BUILD_DATE))
#endif

#ifdef __G95__
    write(UNIT=*,FMT="(a,/)") "Compiled with: G95 minor version " &
      //TRIM(int2char(__G95_MINOR__))
#endif

    write(UNIT=*,FMT="(/,/,a,/)")    "Usage: swb2 [control file name]"

    stop

  end if

  iCount = 0

  do iIndex=1, iNumArgs

    call GET_COMMAND_ARGUMENT( iIndex, sBuf )
    
    print *, iIndex, squote( sBuf )

    if ( scan( sBuf, ".") > 0 ) then

      call slControlFiles%append( sBuf )

    else

      iCount = iCount + 1
      if ( iCount == 1 ) then
        divisions_x = asFloat(sBuf)
      else
        divisions_y = asFloat(sBuf)
      endif    

    endif  

  enddo 
 
  ! open and initialize logfiles
  call LOGS%initialize( iLogLevel = LOG_DEBUG )

  do iIndex=1, slControlFiles%count

    ! read control file
    call read_control_file( slControlFiles%get( iIndex ) )

  enddo  

  call slControlFiles%clear()

  call CF_DICT%get_values("GRID", fValues)

  iNX = fValues(1)
  iNY = fValues(2)
  fLL_x = fValues(3)
  fLL_y = fValues(4)
  fCellSize = fValues( count( fValues > 0.0 ) )
  fUR_x = iNX * fCellSize + fLL_x
  fUR_y = iNY * fCellSize + fLL_y

  iNX1 = iNX / divisions_x
  iNY1 = iNY / divisions_y

  iCount = 0

  do iIndex1=1, int(divisions_x)
    do iIndex2=1, int(divisions_y)
      iCount = iCount + 1
      fLL_x1 = fLL_x + ( iIndex1 - 1 ) * iNX1 * fCellSize
      fLL_y1 = fLL_y + ( iIndex2 - 1 ) * iNY1 * fCellSize

      filename = "test_control_file_"//asCharacter(iCount)//".ctl"

      call write_control_file( filename,                                            &
        "GRID "//asCharacter(iNX1)//" "//asCharacter(iNY1)//" "                     &
        //asCharacter( fLL_x1, 23, 9 )//" "//asCharacter(fLL_y1, 23, 9)             &
        //" "//asCharacter( fCellsize, 14, 4 ) )

      call mkdir( "tmp"//asCharacter( iCount ), loc_err )

      ! suppress error messages due to directory that already exists
      if ( loc_err /= 0 .and. loc_err /= 17 ) then
        call get_libc_err_string( err_string, c_get_libc_errno() )
        write(*, fmt="(a, i0)") "call to 'mkdir' failed. err=", loc_err
        write(*, fmt="(a,i0)") "libc errno=",get_libc_errno()
        write(*, fmt="(a)") "libc error msg: "//trim(err_string)
      endif  


      call execute_command_line( "swb2 "//trim(filename)//" "//"tmp"//asCharacter( iCount )  &
        //" > cmdlog"//asCharacter( iCount)//".md", wait=.false. )

    enddo
  enddo    
   
  call LOGS%close()

end program swb_launch
