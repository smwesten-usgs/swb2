!> @file
!>  Main program which references all other modules; execution begins here.


!>  Main program which references all other modules; execution begins here.
!>
!> Accepts command-line arguments and makes a single call
!> to the control_setModelOptions routine in module \ref control.
program main

  use iso_c_binding, only             : c_short, c_int, c_float, c_double, c_bool, c_long
  use constants_and_conversions, only : OS_NATIVE_PATH_DELIMITER, RANDOM_START, asInt
  use disclaimers, only               : log_provisional_disclaimer
  use logfiles, only                  : LOGS, LOG_DEBUG
  use model_initialize, only          : initialize_all, read_control_file
  use model_domain, only              : MODEL
  use model_iterate, only             : iterate_over_simulation_days

  use model_iterate_multiple_simulations, only  : iterate_over_multiple_simulation_days

  use strings, only                   : operator(.containssimilar.),            &
                                          asCharacter
  use version_control, only           : SWB_VERSION, GIT_COMMIT_HASH_STRING,    &
                                        GIT_BRANCH_STRING, COMPILE_DATE,        &
                                        COMPILE_TIME, SYSTEM_NAME
  use string_list, only               : STRING_LIST_T
  use timer, only                     : TIMER_T
  use iso_fortran_env

  implicit none

  type (STRING_LIST_T)           :: slControlFiles

  character (len=256)            :: sBuf
  character (len=256)            :: sOutputPrefixName
  character (len=256)            :: sOutputDirectoryName
  character (len=256)            :: sDataDirectoryName
  character (len=256)            :: sWeatherDataDirectoryName
  integer (kind=c_int)           :: iNumArgs
  character (len=1024)           :: sCompilerOptions
  character (len=256)            :: sCompilerVersion
  character (len=256)            :: sCompilerName
  character (len=256)            :: sVersionString
  character (len=256)            :: sCompilationDateString
  character (len=256)            :: sCompilationSystemString
  character (len=256)            :: sGitHashString
  integer (kind=c_int)           :: iCount
  integer (kind=c_int)           :: iIndex
  integer (kind=c_int)           :: iLen
  integer (kind=c_int)           :: number_of_simulations

  type (TIMER_T)                 :: runtimer

  sOutputPrefixName         = ""
  sOutputDirectoryName      = ""
  sDataDirectoryName        = ""
  sWeatherDataDirectoryName = ""
  number_of_simulations = 1

  call runtimer%start()

  iNumArgs = COMMAND_ARGUMENT_COUNT()

  sVersionString = trim( SWB_VERSION )
  sCompilationDateString   = trim(COMPILE_DATE)//" "//trim(COMPILE_TIME)
  sCompilationSystemString = trim(SYSTEM_NAME)
  sCompilerOptions = COMPILER_OPTIONS()
  sCompilerVersion = COMPILER_VERSION()

  if (     (SYSTEM_NAME .containssimilar. "Windows")                           &
      .or. (SYSTEM_NAME .containssimilar. "Mingw") ) then
    OS_NATIVE_PATH_DELIMITER = "\"
  else
    OS_NATIVE_PATH_DELIMITER = "/"
  endif

  sGitHashString = trim( adjustl(GIT_BRANCH_STRING ) )//", "//trim( GIT_COMMIT_HASH_STRING )

#ifdef __GFORTRAN__
  sCompilerName = "gfortran"
#endif

#ifdef __INTEL_COMPILER
  ! populate with empty string, since for Intel the COMPILER_VERSION string contains
  ! the compiler name already
  sCompilerName = ""
!  write(UNIT=*,FMT="(a,/)") "compiler build date:"//TRIM(asCharacter(__INTEL_COMPILER_BUILD_DATE))
#endif

  iCount = max( len_trim( sVersionString ), len_trim( sGitHashString ) )
  iCount = max( iCount, len_trim( sCompilationSystemString) )
  iCount = max( iCount, len_trim( sCompilerName) + len_trim( sCompilerVersion ) )
  iCount = iCount + 24

  write(unit=*, fmt="(/,a)") repeat("-",iCount + 4)
  write(UNIT=*,FMT="(2x,a,/)") "USGS Soil-Water-Balance Code version "//trim( sVersionString )
  write(UNIT=*,FMT="(a24,a)") "compiled on : ", trim( sCompilationDateString )
  write(UNIT=*,FMT="(a24,a)") "compiled with : ", trim( sCompilerName )//", "//trim(sCompilerVersion)
  write(UNIT=*,FMT="(a24,a)") "compiled for : ",trim( sCompilationSystemString )
  write(UNIT=*,FMT="(a24,a)") "git hash and branch : ", trim( sGitHashString )
  write(unit=*, fmt="(a,/)") repeat("-",iCount + 4)

  if(iNumArgs == 0 ) then

      write(UNIT=*,FMT="(//,a,/,/,5(a,/))")  "Usage: swb2 [ options ] control_file_name ",                   &
               "[ --output_prefix= ]     :: text to use as a prefix for output filenames",                   &
               "[ --output_dir= ]        :: directory to place output in (may be relative or absolute)",     &
               "[ --data_dir= ]          :: directory to search for input grids or lookup tables",           &
               "[ --weather_data_dir= ]  :: directory to search for weather data grids"
!               "[ --random_start= ]      :: advance random number generator to this position in the series", &
!               "[ --number_of_sims= ]    :: number of simulations to run when 'method of fragments' is used"
    stop

  end if

  do iIndex=1, iNumArgs

    call GET_COMMAND_ARGUMENT( iIndex, sBuf )

    if ( sBuf(1:13) .eq. "--output_dir=" ) then

      sOutputDirectoryName = trim( sBuf(14:) )
      iLen = len_trim( sOutputDirectoryName )

      ! if there is no trailing "/", append one so we can form (more) fully
      ! qualified filenames later
      if ( .not. sOutputDirectoryName(iLen:iLen) .eq. OS_NATIVE_PATH_DELIMITER )  &
        sOutputDirectoryName = trim(sOutputDirectoryName)//OS_NATIVE_PATH_DELIMITER

      call LOGS%set_output_directory( sOutputDirectoryName )

    elseif( sBuf(1:15) .eq."--random_start=" ) then

      RANDOM_START = int( asInt( sBuf(16:) ), kind=c_long )

    elseif( sBuf(1:17) .eq."--number_of_sims=" ) then

      number_of_simulations = asInt( sBuf(18:) )

    elseif ( sBuf(1:16) .eq. "--output_prefix=" ) then

      sOutputPrefixName = trim( sBuf(17:) )
      iLen = len_trim( sOutputPrefixName )

    elseif ( sBuf(1:11) .eq. "--data_dir=" ) then

      sDataDirectoryName = sBuf(12:)
      iLen = len_trim( sDataDirectoryName )

      ! if there is no trailing "/", append one so we can form (more) fully
      ! qualified filenames later
      if ( .not. sDataDirectoryName(iLen:iLen) .eq. OS_NATIVE_PATH_DELIMITER )  &
        sDataDirectoryName = trim(sDataDirectoryName)//OS_NATIVE_PATH_DELIMITER

    elseif ( sBuf(1:19) .eq. "--weather_data_dir=" ) then

      sWeatherDataDirectoryName = sBuf(20:)
      iLen = len_trim( sWeatherDataDirectoryName )

      ! if there is no trailing "/", append one so we can form (more) fully
      ! qualified filenames later
      if ( .not. sWeatherDataDirectoryName(iLen:iLen) .eq. OS_NATIVE_PATH_DELIMITER )  &
        sWeatherDataDirectoryName = trim(sWeatherDataDirectoryName)//OS_NATIVE_PATH_DELIMITER

    else

      ! no match on the command-line argument flags; this must be a control file
      call slControlFiles%append( trim( sBuf ) )

    endif

  enddo

  ! open and initialize logfiles
  call LOGS%initialize( iLogLevel = LOG_DEBUG )

  call log_provisional_disclaimer()

  call LOGS%write( sMessage='Base data directory name set to:',lEcho=.TRUE._c_bool )
  call LOGS%write( sMessage='"'//trim( sDataDirectoryName )//'"', iTab=4,lEcho=.TRUE._c_bool )
  call LOGS%write( sMessage='Weather data directory (precip, tmin, tmax grids) name set to: ',lEcho=.TRUE._c_bool )
  call LOGS%write( '"'//trim( sWeatherDataDirectoryName )//'"',iTab=4,lEcho=.TRUE._c_bool )

  call LOGS%write( sMessage='Output file prefix set to:',lEcho=.TRUE._c_bool )
  call LOGS%write( sMessage='"'//trim( sOutputPrefixName )//'"', iTab=4, lEcho=.TRUE._c_bool )

  if (RANDOM_START >0)   &
    call LOGS%write(sMessage="Pseudo-random numbers will be pulled from index number "   &
           //trim(asCharacter(RANDOM_START))//" in the generated series.",               &
           lEcho=.true._c_bool )

  if ( number_of_simulations > 1)  &

    call LOGS%write(sMessage="Running multiple simulations; assuming method of fragments "   &
         //"was selected as the precipitation method.",                                      &
         lEcho=.true._c_bool )


  do iIndex=1, slControlFiles%count

    ! read control file
    call read_control_file( slControlFiles%get( iIndex ) )

  enddo

  call slControlFiles%clear()

  call initialize_all( sOutputPrefixName, sOutputDirectoryName,                &
                       sDataDirectoryName, sWeatherDataDirectoryName )

  call runtimer%stop()
  call runtimer%calc_time_values("split")
  call LOGS%write(sMessage="Time spent initializing simulation: "              &
     //trim( runtimer%get_pretty()), lEcho=.true._c_bool,                      &
     iLinesBefore=1, iLinesAfter=1 )

  call runtimer%start()

  if ( number_of_simulations > 1) then

    call iterate_over_multiple_simulation_days(MODEL, number_of_simulations)

  else

    call iterate_over_simulation_days( MODEL )

  endif

  call runtimer%stop()
  call runtimer%calc_time_values("split")
  call LOGS%write(sMessage="Time spent running simulation: "                   &
     //trim( runtimer%get_pretty()), lEcho=.true._c_bool,                      &
     iLinesBefore=1)
  call runtimer%calc_time_values("elapsed")
  call LOGS%write(sMessage="Total runtime                : "                   &
    //trim( runtimer%get_pretty()), lEcho=.true._c_bool,                       &
    iLinesAfter=1 )
  call LOGS%close()

end program main
