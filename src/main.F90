!> @file
!>  Main program which references all other modules; execution begins here.


!>  Main program which references all other modules; execution begins here.
!>
!> Accepts command-line arguments and makes a single call
!> to the control_setModelOptions routine in module \ref control.
program main

  use iso_c_binding, only    : c_short, c_int, c_float, c_double
  use logfiles, only         : LOGS, LOG_DEBUG
  use model_initialize, only : initialize_all, read_control_file
  use model_domain, only     : MODEL
  use model_iterate, only    : iterate_over_simulation_days
  use version_control, only  : SWB_VERSION, GIT_COMMIT_HASH_STRING, &
                               GIT_BRANCH_STRING, COMPILE_DATE, COMPILE_TIME
  use string_list, only      : STRING_LIST_T
  use iso_fortran_env

  implicit none

  type (STRING_LIST_T)           :: slControlFiles

  character (len=256)            :: sBuf
  character (len=64)             :: sOutputDirectoryName = ""
  integer (kind=c_int)           :: iNumArgs
  character (len=1024)           :: sCompilerFlags       = ""
  character (len=256)            :: sCompilerVersion     = ""
  character (len=256)            :: sVersionString       = ""
  character (len=256)            :: sGitHashString       = ""
  integer (kind=c_int)           :: iCount
  integer (kind=c_int)           :: iIndex
  integer (kind=c_int)           :: iLen

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

    write(UNIT=*,FMT="(/,/,a,/)")    "Usage: swb2 [control file name] [output directory name (optional)]"

    stop

  end if

  do iIndex=1, iNumArgs

    call GET_COMMAND_ARGUMENT( iIndex, sBuf )

    if ( scan( sBuf, ".") > 0 ) then

      call slControlFiles%append( sBuf )

    else  

      sOutputDirectoryName = sBuf
      iLen = len_trim( sOutputDirectoryName )

      if ( .not. sOutputDirectoryName(iLen:iLen) .eq. "/" )  &
        sOutputDirectoryName = trim(sOutputDirectoryName)//"/"

      call LOGS%set_output_directory( sOutputDirectoryName )

    endif  

  enddo 
 
  ! open and initialize logfiles
  call LOGS%initialize( iLogLevel = LOG_DEBUG )

  do iIndex=1, slControlFiles%count

    ! read control file
    call read_control_file( slControlFiles%get( iIndex ) )

  enddo  

  call slControlFiles%clear()

  call initialize_all( sOutputDirectoryName )

  call iterate_over_simulation_days( MODEL )
   
  call LOGS%close()

end program main
