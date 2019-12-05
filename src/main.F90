!> @file
!>  Main program which references all other modules; execution begins here.


!>  Main program which references all other modules; execution begins here.
!>
!> Accepts command-line arguments and makes a single call
!> to the control_setModelOptions routine in module \ref control.
program main

  use iso_c_binding, only             : c_short, c_int, c_float, c_double, c_bool, c_long
  use constants_and_conversions, only : OS_NATIVE_PATH_DELIMITER, RANDOM_START, asInt
  use disclaimers, only               : log_provisional_disclaimer,                          &
                                        write_provisional_disclaimer
  use logfiles, only                  : LOGS, LOG_DEBUG
  use model_initialize, only          : initialize_all, read_control_file
  use model_domain, only              : MODEL
  use model_iterate, only             : iterate_over_simulation_days

  use model_iterate_multiple_simulations, only  : iterate_over_multiple_simulation_days

  use fstring, only                   : operator(.containssimilar.),            &
                                          asCharacter, right
  use version_control, only           : SWB_VERSION, GIT_COMMIT_HASH_STRING,    &
                                        GIT_BRANCH_STRING, COMPILE_DATE,        &
                                        COMPILE_TIME, SYSTEM_NAME
  use fstring_list, only              : FSTRING_LIST_T
  use timer, only                     : TIMER_T
  use iso_fortran_env

  implicit none

  type (FSTRING_LIST_T)           :: slControlFiles

  character (len=256)            :: sBuf
  character (len=256)            :: sOutputPrefixName
  character (len=256)            :: sOutputDirectoryName
  character (len=256)            :: sLogfileDirectoryName
  character (len=256)            :: sDataDirectoryName
  character (len=256)            :: sLookupTableDirectoryName
  character (len=256)            :: sWeatherDataDirectoryName
  integer (c_int)                :: iNumArgs
  character (len=1024)           :: sCompilerOptions
  character (len=256)            :: sCompilerVersion
  character (len=:), allocatable :: sCompilerName
  character (len=256)            :: sVersionString
  character (len=256)            :: sCompilationDateString
  character (len=256)            :: sCompilationSystemString
  character (len=256)            :: sExecutableDescription
  character (len=256)            :: sGitHashString
  integer (c_int)                :: iCount
  integer (c_int)                :: iIndex
  integer (c_int)                :: iLen
  integer (c_int)                :: number_of_simulations

  type (TIMER_T)                 :: runtimer

  sOutputPrefixName         = ""
  sOutputDirectoryName      = ""
  sDataDirectoryName        = ""
  sLogfileDirectoryName     = ""
  sLookupTableDirectoryName = ""
  sWeatherDataDirectoryName = ""
  number_of_simulations = 1

  call runtimer%start()

  iNumArgs = COMMAND_ARGUMENT_COUNT()

  sVersionString = trim( SWB_VERSION )
  sCompilationDateString   = trim(COMPILE_DATE)//" "//trim(COMPILE_TIME)
  sCompilationSystemString = trim(SYSTEM_NAME)
  sCompilerOptions = COMPILER_OPTIONS()
  sCompilerVersion = COMPILER_VERSION()
  sExecutableDescription = "USGS Soil-Water-Balance Code version "//trim( sVersionString )

  if (     (SYSTEM_NAME .containssimilar. "Windows")                           &
      .or. (SYSTEM_NAME .containssimilar. "Mingw")                             &
      .or. (sCompilerVersion .containssimilar. "Windows")) then
    OS_NATIVE_PATH_DELIMITER = "\"
  else
    OS_NATIVE_PATH_DELIMITER = "/"
  endif

  sGitHashString = trim( adjustl(GIT_BRANCH_STRING ) )//", "//trim( GIT_COMMIT_HASH_STRING )

#ifdef __GFORTRAN__
  sCompilerName = "gfortran" // trim(sCompilerVersion)
#endif

#ifdef __INTEL_COMPILER
  ! populate with empty string, since for Intel the COMPILER_VERSION string contains
  ! the compiler name already
  sCompilerName = "Intel Fortran,"//right(string=sCompilerVersion, substring=",")
  sCompilerVersion = ""
!  write(UNIT=*,FMT="(a,/)") "compiler build date:"//TRIM(asCharacter(__INTEL_COMPILER_BUILD_DATE))
#endif

  iCount = max( len_trim( sVersionString ), len_trim( sGitHashString ) )
  iCount = max( iCount, len_trim( sCompilationSystemString) )
  iCount = max( iCount, len_trim( sCompilerName) + len_trim( sCompilerVersion ) )
  iCount = iCount + 24
  iCount = max( iCount, len_trim( sExecutableDescription ) )

  write(unit=*, fmt="(/,a)") repeat("-",iCount + 4)
  write(UNIT=*,FMT="(2x,a,/)") trim(sExecutableDescription)
  write(UNIT=*,FMT="(a24,a)") "compiled on : ", trim( sCompilationDateString )
  write(UNIT=*,FMT="(a24,a)") "compiled with : ", trim( sCompilerName )
  write(UNIT=*,FMT="(a24,a)") "compiled for : ",trim( sCompilationSystemString )
  write(UNIT=*,FMT="(a24,a)") "git hash and branch : ", trim( sGitHashString )
  write(unit=*, fmt="(a,/)") repeat("-",iCount + 4)

  if(iNumArgs == 0 ) then

  write(UNIT=*,FMT="(a)")       " Compiler options :  "
  write(UNIT=*,FMT="(a,/,a,/)") "==================== ", trim( sCompilerOptions )

    call write_provisional_disclaimer()

    write(UNIT=*,FMT="(//,a,/,/,6(a,/))")  "Usage: swb2 [ options ] control_file_name ",                   &
             "[ --output_prefix= ]     :: text to use as a prefix for output filenames",                   &
             "[ --output_dir= ]        :: directory to place output in (may be relative or absolute)",     &
             "[ --lookup_dir= ]        :: directory to search for lookup tables",                          &
             "[ --logfile_dir= ]       :: directory to write logfiles to",                                 &
             "[ --data_dir= ]          :: directory to search for input data grids",                       &
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

    elseif( sBuf(1:15) .eq."--random_start=" ) then

      RANDOM_START = int( asInt( sBuf(16:) ), c_long )

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

    elseif ( sBuf(1:14) .eq. "--logfile_dir=" ) then
      sLogfileDirectoryName = sBuf(15:)
      iLen = len_trim( sLogfileDirectoryName )
    
      ! if there is no trailing "/", append one so we can form (more) fully
      ! qualified filenames later
      if ( .not. sLogfileDirectoryName(iLen:iLen) .eq. OS_NATIVE_PATH_DELIMITER )  &
      sLogfileDirectoryName = trim(sLogfileDirectoryName)//OS_NATIVE_PATH_DELIMITER
    
    elseif ( sBuf(1:13) .eq. "--lookup_dir=" ) then

      sLookupTableDirectoryName = sBuf(14:)
      iLen = len_trim( sLookupTableDirectoryName )
    
      ! if there is no trailing "/", append one so we can form (more) fully
      ! qualified filenames later
      if ( .not. sLookupTableDirectoryName(iLen:iLen) .eq. OS_NATIVE_PATH_DELIMITER )  &
      sLookupTableDirectoryName = trim(sLookupTableDirectoryName)//OS_NATIVE_PATH_DELIMITER

    elseif ( sBuf(1:19) .eq. "--lookup_table_dir=" ) then

      sLookupTableDirectoryName = sBuf(20:)
      iLen = len_trim( sLookupTableDirectoryName )
      
      ! if there is no trailing "/", append one so we can form (more) fully
      ! qualified filenames later
      if ( .not. sLookupTableDirectoryName(iLen:iLen) .eq. OS_NATIVE_PATH_DELIMITER )  &
      sLookupTableDirectoryName = trim(sLookupTableDirectoryName)//OS_NATIVE_PATH_DELIMITER

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

  ! allow for modification of the location in which logfiles will be written
  if ( len_trim( sLogfileDirectoryName ) /= 0 ) then
    call LOGS%set_output_directory( trim( sLogfileDirectoryName ) )
  elseif ( len_trim( sOutputDirectoryName ) /= 0 ) then
    call LOGS%set_output_directory( trim( sOutputDirectoryName ) )
  endif  

  call LOGS%initialize( iLogLevel = LOG_DEBUG )

  call log_provisional_disclaimer()

  call LOGS%write( sMessage='Grid data directory name set to:',lEcho=.TRUE._c_bool )
  call LOGS%write( sMessage='"'//trim( sDataDirectoryName )//'"', iTab=4,lEcho=.TRUE._c_bool )
  call LOGS%write( sMessage='Lookup table directory name set to:',lEcho=.TRUE._c_bool )
  call LOGS%write( sMessage='"'//trim( sLookupTableDirectoryName )//'"', iTab=4,lEcho=.TRUE._c_bool )
  call LOGS%write( sMessage='Weather data directory (precip, tmin, tmax grids) name set to: ',lEcho=.TRUE._c_bool )
  call LOGS%write( '"'//trim( sWeatherDataDirectoryName )//'"',iTab=4,lEcho=.TRUE._c_bool )

  call LOGS%write( sMessage='Output will be written to: ',lEcho=.TRUE._c_bool )
  call LOGS%write( '"'//trim( sOutputDirectoryName )//'"',iTab=4,lEcho=.TRUE._c_bool )
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

  ! idea is that there may be multiple control files fed to SWB; all of the files
  ! will be munged and the entries added to the CF_DICT dictionary
  do iIndex=1, slControlFiles%count
    ! read control file
    call read_control_file( slControlFiles%get( iIndex ) )
  enddo

  call slControlFiles%clear()

  call initialize_all( sOutputPrefixName, sOutputDirectoryName,                &
                       sDataDirectoryName, sLookupTableDirectoryName,          &
                       sWeatherDataDirectoryName )

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
