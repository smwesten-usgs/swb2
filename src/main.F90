!> @file
!>  Main program which references all other modules; execution begins here.


!>  Main program which references all other modules; execution begins here.
!>
!> Accepts command-line arguments and makes a single call
!> to the control_setModelOptions routine in module \ref control.
program main

  use iso_c_binding, only   : c_short, c_int, c_float, c_double
  use logfiles, only        : LOGS, LOG_DEBUG, SWB_VERSION
  use loop_initialize, only : initialize_options, check_for_fatal_warnings, read_control_file
  use model_domain, only    : MODEL
  use iso_fortran_env

  implicit none

  character (len=256)            :: sControlFile
  integer (kind=c_int)           :: iNumArgs
  character (len=1024)           :: sCompilerFlags
  character (len=256)            :: sCompilerVersion
  character (len=256)            :: sVersionString 

  iNumArgs = COMMAND_ARGUMENT_COUNT()

  sVersionString = "  Soil Water Balance Code version "//trim( SWB_VERSION )    &
      //" -- compiled on: "//trim(__DATE__)//" "//trim(__TIME__)

  if(iNumArgs/=1) then

    write(UNIT=*,FMT="(/,a,/)") trim( sVersionString )

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

    write(UNIT=*,FMT="(/,/,a,/)")    "Usage: swb [control file name]"

    stop

  end if

  call GET_COMMAND_ARGUMENT(1,sControlFile)

  write(unit=*, fmt="(/,a)") repeat("-",len_trim(sVersionString)+2)  
  write(UNIT=*,FMT="(a)") trim( sVersionString )
  write(unit=*, fmt="(a,/)") repeat("-",len_trim(sVersionString)+2)  

  ! open and initialize logfiles
  call LOGS%initialize( iLogLevel = LOG_DEBUG )

  ! read control file
  call read_control_file( sControlFile )

  call initialize_options()
  
  call check_for_fatal_warnings()

  call MODEL%iterate_over_simulation_days()
   




  call LOGS%close()




end program main
