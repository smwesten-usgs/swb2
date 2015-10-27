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
                                c_get_libc_errno, remove, execv
  use model_initialize, only : initialize_all, read_control_file, write_control_file
  use version_control, only  : SWB_VERSION, GIT_COMMIT_HASH_STRING, &
                               GIT_BRANCH_STRING, COMPILE_DATE, COMPILE_TIME
  use string_list, only      : STRING_LIST_T
  use omp_lib
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
  integer (kind=c_int)             :: loc_err, c_error_number
  character(len=:), allocatable    :: err_string

  real (kind=c_float)              :: fUR_x, fUR_y, fLL_x, fLL_y
  real (kind=c_float)              :: fLL_x1, fLL_y1

  integer (kind=c_int)             :: iNX, iNY, iNX1, iNY1
  integer (kind=c_int)             :: status
  integer (kind=c_int)             :: number_at_100
  integer (kind=c_int)             :: number_uninitialized
  integer (kind=c_int)             :: simulation_instance
  integer (kind=c_int)             :: number_of_subsets
  character(len=64), allocatable    :: argv(:)
  integer (kind=c_int), allocatable :: indexXY(:,:)

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

    write(UNIT=*,FMT="(/,/,a,/)")    "Usage: swb_launch [control file name] [num divisions x] [num divisions y]"
    write(UNIT=*,FMT="(a)")          "swb_launch takes a swb control file and two divisors to break a swb model domain"
    write(UNIT=*,FMT="(a,/)")        "into equal-sized subproblems, and then launches each as a seperate process."
    write(UNIT=*,FMT="(a)")          "For example, issuing the command 'swb_launch some_file.ctl 2 2' will result in the"
    write(UNIT=*,FMT="(a)")          "model domain being broken into four pieces and run as four seperate processes."

    stop

  end if

  iCount = 0
  number_at_100 = 0
  number_uninitialized = 0

 
  ! open and initialize logfiles
  call LOGS%initialize( iLogLevel = LOG_DEBUG,  sFilePrefix="SWB_LAUNCH_", lWrite_SWB_Info=.false._c_bool )


  do iIndex=1, iNumArgs

    call GET_COMMAND_ARGUMENT( iIndex, sBuf )
    
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

  number_of_subsets = int(divisions_x) * int(divisions_y)
  allocate( indexXY( 2, number_of_subsets ) )

  iCount = 0
  ! split up model domain over the desired spatial domain
  do iIndex1=int(divisions_y), 1, -1
    do iIndex2=1, int(divisions_x)

      iCount = iCount + 1
      indexXY( 2, iCount ) = iIndex1
      indexXY( 1, iCount ) = iIndex2
      print *, iCount, iIndex1, iIndex2
    enddo
  enddo  



!$omp parallel
!$omp do private( simulation_instance, iIndex1, fLL_x1, fLL_y1, filename, loc_err )

  do iIndex1 = 1, ubound(indexXY,2)

    !! !$omp task private( simulation_instance, fLL_x1, fLL_y1, filename, loc_err ) firstprivate( iIndex1 )

    simulation_instance = int(divisions_x) * ( int(divisions_y) - indexXY( 2, iIndex1 ) ) + indexXY( 1, iIndex1 )


    print*, "thread, index, sim instance, indexXY: ",omp_get_thread_num(), iIndex1, simulation_instance, indexXY(1, iIndex1), indexXY(2, iIndex1) 

    fLL_x1 = fLL_x + ( indexXY( 1, iIndex1) - 1 ) * iNX1 * fCellSize
    fLL_y1 = fLL_y + ( indexXY( 2, iIndex1 ) - 1 ) * iNY1 * fCellSize

    filename = "test_control_file_"//asCharacter(simulation_instance)//".ctl"

    call sleep( simulation_instance * 2 )

    call write_control_file( filename,                                            &
      "GRID "//asCharacter(iNX1)//" "//asCharacter(iNY1)//" "                     &
      //asCharacter( fLL_x1, 23, 9 )//" "//asCharacter(fLL_y1, 23, 9)             &
      //" "//asCharacter( fCellsize, 14, 4 ) )

    call mkdir( "tmp"//asCharacter( simulation_instance ), loc_err )

    ! suppress error messages due to directory that already exists
    if ( loc_err /= 0 ) then
      c_error_number = get_libc_errno()
      if ( c_error_number /= 0 .and. c_error_number /= 17 ) then
        call get_libc_err_string( err_string, c_error_number )
        write(*, fmt="(a, i0)") "call to 'mkdir' failed. err=", loc_err
        write(*, fmt="(a,i0)") "libc errno=", c_error_number
        write(*, fmt="(a)") "libc error msg: "//trim(err_string)
      endif  

    endif  

    call remove( "tmp"//asCharacter( simulation_instance )//"/run_progress.txt", loc_err )

    ! suppress error messages due to directory that already exists
    if ( loc_err /= 0 ) then
      c_error_number = get_libc_errno()
      if ( c_error_number /= 0 .and. c_error_number /= 2 ) then
        call get_libc_err_string( err_string, c_error_number )
        write(*, fmt="(a, i0)") "call to 'remove' failed. err=", loc_err
        write(*, fmt="(a,i0)") "libc errno=", c_error_number
        write(*, fmt="(a)") "libc error msg: "//trim(err_string)
      endif  
    endif  

    write(*,fmt="(a)") "Starting SWB in directory /tmp"//asCharacter( simulation_instance )   &
      //", thread number "//asCharacter( omp_get_thread_num() )//" of "//asCharacter( omp_get_max_threads() )

!      write(*,fmt="(a)") "Starting SWB in directory /tmp"//asCharacter( simulation_instance )

    call execute_command_line( "swb2 "//trim(filename)//" "//"tmp"//asCharacter( simulation_instance )  &
      //" > cmdlog"//asCharacter( simulation_instance )//".md", wait=.false. )

     ! argv(1) = "swb2"
     ! argv(2) = trim(filename)
     ! argv(3) = "tmp"//asCharacter( simulation_instance )
     ! argv(4) = " cat > nul"

!      call execv( "swb2",[ argv ], loc_err )

      ! if ( loc_err /= 0 ) then
      !   c_error_number = get_libc_errno()
      !   if ( c_error_number /= 0 ) then
      !     call get_libc_err_string( err_string, c_error_number )
      !     write(*, fmt="(a, i0)") "call to 'execv' failed. err=", loc_err
      !     write(*, fmt="(a,i0)") "libc errno=", c_error_number
      !     write(*, fmt="(a)") "libc error msg: "//trim(err_string)
      !   endif  
      ! endif  

!      call system( "swb2 "//trim(filename)//" "//"tmp"//asCharacter( iCount )  &
!        //" > cmdlog"//asCharacter( iCount)//".md" )

    !! !$omp end task

  enddo    
 !   #!$omp end single

!$omp end do nowait

!$omp end parallel


  do

    do iIndex=1, iCount
      filename = "tmp"//asCharacter(iCount)//"/run_progress.txt"
      open(unit=12, file=filename, iostat=status, action='READ')
      if ( status == 0 ) then
        read (12, fmt="(a)", iostat=status) sBuf
        !if ( status /= 0 ) cycle
        if ( scan( sBuf, "Done") > 0 ) number_at_100 = number_at_100 + 1
        close(unit=12, iostat=status )
        write(*,fmt="(a)" ) "Instance "//asCharacter(iIndex)//": "//trim(sBuf)
      else
        write(*,fmt="(a)" ) "Instance "//asCharacter(iIndex)//": initializing"
      endif
    enddo

    write(*,fmt="(/)" ) 

    if ( number_at_100 == iCount ) exit

    call sleep(10)

  enddo
   
  call LOGS%close()

end program swb_launch
