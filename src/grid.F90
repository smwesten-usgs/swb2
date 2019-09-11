!> @file
!>  Contains a single module, grid, which
!>  provides support for gridded ASCII data file and data structure operations
!> @ingroup grid

!>  Provides support for input and output of gridded ASCII data,
!> as well as for creation and destruction of grid data structures (defined types).
module grid

  use iso_c_binding
  use constants_and_conversions
  use exceptions
  use fstring
  use logfiles

  implicit none

  private

  integer (c_int), public, parameter :: GRID_NODATA_INT  = -9999_c_int
  real (c_float), public, parameter  :: GRID_NODATA_REAL = -9999._c_float
  real (c_double), public, parameter  :: GRID_NODATA_DOUBLE = -9999._c_double

  ! @TODO: why redefine these when types have been defined in 'constants_and_conversions'?
  integer (c_int), public, parameter :: GRID_DATATYPE_INT = DATATYPE_INT
  integer (c_int), public, parameter :: GRID_DATATYPE_REAL = DATATYPE_REAL
  integer (c_int), public, parameter :: GRID_DATATYPE_DOUBLE = DATATYPE_DOUBLE

  integer (c_int), public, parameter :: OUTPUT_SURFER = 0
  integer (c_int), public, parameter :: OUTPUT_ARC = 1

  integer (c_int), public, parameter :: GRID_ACTIVE_CELL = 1
  integer(c_int), parameter :: NC_FILL_INT     = GRID_NODATA_INT
  real(c_float),  parameter :: NC_FILL_FLOAT   = GRID_NODATA_REAL
  real(c_double),  parameter :: NC_FILL_DOUBLE   = GRID_NODATA_DOUBLE

  !> interface to C code that provides a simplified entry point to PROJ4
  !> capabilities: it has been modified so that all C pointers are kept within the
  !> C code; no pointers are returned to fortran
  public :: pj_init_and_transform
  interface
    function pj_init_and_transform(from_projection, to_projection, caller_name,                             &
                                   caller_linenum, point_count, x, y) bind(c,name='pj_init_and_transform')
    import
    character(c_char) :: from_projection(*)
    character(c_char) :: to_projection(*)
    character(c_char) :: caller_name(*)
    integer(c_int),value  :: caller_linenum
    integer(c_long),value :: point_count
    real(c_double) :: x(*)
    real(c_double) :: y(*)
    integer(c_int) :: pj_init_and_transform
    end function pj_init_and_transform
  end interface


  type, public :: GENERAL_GRID_T
    integer (c_int)            :: iNX                   ! Number of cells in the x-direction
    integer (c_int)            :: iNY                   ! Number of cells in the y-direction
    integer (c_int)            :: iNumGridCells         ! Total number of grid cells
    integer (c_int)            :: iDataType             ! Data type contained in the grid (integer, real, SWB cell)
    character (len=:), allocatable  :: sProj4_string         ! proj4 string defining coordinate system of grid
    character (len=:), allocatable  :: sFilename             ! original file name that the data was read from
    real (c_double)            :: rGridCellSize         ! size of one side of a grid cell
    integer (c_int)            :: iLengthUnits= -99999  ! length units code
    real (c_double)            :: rX0, rX1              ! World-coordinate range in X
    real (c_double)            :: rY0, rY1              ! World-coordinate range in Y

    integer (c_int), dimension(:,:), allocatable :: iData ! Integer data
    real (c_float), dimension(:,:), allocatable  :: rData    ! Real data
    real (c_float), dimension(:,:), allocatable  :: fData    ! Float data
    real (c_double), dimension(:,:), allocatable :: dpData   ! Douple-precision data
    real (c_double), dimension(:,:), allocatable :: rX    ! x coordinate associated with data
    real (c_double), dimension(:,:), allocatable :: rY    ! y coordinate associated with data
    integer (c_int), dimension(:,:), allocatable :: iMask ! Mask for processing results

    integer (c_int)                    :: iNoDataValue = NC_FILL_INT
    real (c_float)                     :: rNoDataValue = NC_FILL_FLOAT
    real (c_double)                    :: dpNoDataValue = NC_FILL_DOUBLE
!      type (T_CELL), dimension(:,:), pointer :: Cells        ! T_CELL objects
  end type GENERAL_GRID_T


  type, public :: GRID_BOUNDS_T
    real (c_double) :: rXll, rYll
    real (c_double) :: rXul, rYul
    real (c_double) :: rXlr, rYlr
    real (c_double) :: rXur, rYur
    character (len=256) :: sPROJ4_string
  end type GRID_BOUNDS_T


  type ERROR_MESSAGE_T
    character(len=40) :: cErrorMessageText
    integer           :: iErrorNumber
  end type ERROR_MESSAGE_T

  ! error messages adapted from file "pj_strerrno.c" in PROJ.4.8.0
  type(ERROR_MESSAGE_T), dimension(49) :: tErrorMessage = (/ &
    ERROR_MESSAGE_T("no arguments in initialization list          ",  -1), &
    ERROR_MESSAGE_T("no options found in 'init' file              ",  -2), &
    ERROR_MESSAGE_T("no colon in init= string                     ",  -3), &
    ERROR_MESSAGE_T("projection not named                         ",  -4), &
    ERROR_MESSAGE_T("unknown projection id                        ",  -5), &
    ERROR_MESSAGE_T("effective eccentricity = 1.                  ",  -6), &
    ERROR_MESSAGE_T("unknown unit conversion id                   ",  -7), &
    ERROR_MESSAGE_T("invalid boolean param argument               ",  -8), &
    ERROR_MESSAGE_T("unknown elliptical parameter name            ",  -9), &
    ERROR_MESSAGE_T("reciprocal flattening (1/f) = 0              ", -10), &
    ERROR_MESSAGE_T("|radius reference latitude| > 90             ", -11), &
    ERROR_MESSAGE_T("squared eccentricity < 0                     ", -12), &
    ERROR_MESSAGE_T("major axis or radius = 0 or not given        ", -13), &
    ERROR_MESSAGE_T("latitude or longitude exceeded limits        ", -14), &
    ERROR_MESSAGE_T("invalid x or y                               ", -15), &
    ERROR_MESSAGE_T("improperly formed DMS value                  ", -16), &
    ERROR_MESSAGE_T("non-convergent inverse meridional dist       ", -17), &
    ERROR_MESSAGE_T("non-convergent inverse phi2                  ", -18), &
    ERROR_MESSAGE_T("acos/asin: |arg| >1.+1e-14                   ", -19), &
    ERROR_MESSAGE_T("tolerance condition error                    ", -20), &
    ERROR_MESSAGE_T("conic lat_1 = -lat_2                         ", -21), &
    ERROR_MESSAGE_T("lat_1 >= 90                                  ", -22), &
    ERROR_MESSAGE_T("lat_1 = 0                                    ", -23), &
    ERROR_MESSAGE_T("lat_ts >= 90                                 ", -24), &
    ERROR_MESSAGE_T("no distance between control points           ", -25), &
    ERROR_MESSAGE_T("projection not selected to be rotated        ", -26), &
    ERROR_MESSAGE_T("W <= 0 or M <= 0                             ", -27), &
    ERROR_MESSAGE_T("lsat not in 1-5 range                        ", -28), &
    ERROR_MESSAGE_T("path not in range                            ", -29), &
    ERROR_MESSAGE_T("h <= 0                                       ", -30), &
    ERROR_MESSAGE_T("k <= 0                                       ", -31), &
    ERROR_MESSAGE_T("lat_0 = 0 or 90 or alpha = 90                ", -32), &
    ERROR_MESSAGE_T("lat_1=lat_2 or lat_1=0 or lat_2=90           ", -33), &
    ERROR_MESSAGE_T("elliptical usage required                    ", -34), &
    ERROR_MESSAGE_T("invalid UTM zone number                      ", -35), &
    ERROR_MESSAGE_T("arg(s) out of range for Tcheby eval          ", -36), &
    ERROR_MESSAGE_T("failed to find projection to be rotated      ", -37), &
    ERROR_MESSAGE_T("failed to load datum shift file              ", -38), &
    ERROR_MESSAGE_T("both n ), & m must be spec'd and > 0         ", -39), &
    ERROR_MESSAGE_T("n <= 0, n > 1 or not specified               ", -40), &
    ERROR_MESSAGE_T("lat_1 or lat_2 not specified                 ", -41), &
    ERROR_MESSAGE_T("|lat_1| == |lat_2|                           ", -42), &
    ERROR_MESSAGE_T("lat_0 is pi/2 from mean lat                  ", -43), &
    ERROR_MESSAGE_T("unparseable coordinate system definition     ", -44), &
    ERROR_MESSAGE_T("geocentric transformation missing z or ellps ", -45), &
    ERROR_MESSAGE_T("unknown prime meridian conversion id         ", -46), &
    ERROR_MESSAGE_T("illegal axis orientation combination         ", -47), &
    ERROR_MESSAGE_T("point not within available datum shift grids ", -48), &
    ERROR_MESSAGE_T("invalid sweep axis, choose x or y            ", -49) /)

  public :: grid_gridToGrid_int, grid_gridToGrid_sgl, grid_Create, grid_Destroy, grid_Read, &
     grid_CheckForPROJ4Error, grid_CompletelyCover,                                         &
     grid_ReadExisting, grid_DumpGridExtent,                                                &
     grid_Conform, grid_Transform, grid_Interpolate, grid_PopulateXY,                       &
     grid_GetGridX, grid_GetGridY,                                                          &
     grid_set_nodata_value,                                                                 &
     grid_CreateComplete, grid_CreateSimple, grid_WriteGrid,                                &
     grid_WriteArcGrid, grid_WriteSurferGrid, grid_set_output_directory_name,               &
     grid_GetGridRowNum, grid_GetGridColNum

  interface grid_Create
    module procedure grid_CreateSimple
    module procedure grid_CreateComplete
  end interface grid_Create

  ! global parameters for use with the majority filter
  integer (c_int), parameter :: FOUR_CELLS = 1
  integer (c_int), parameter :: EIGHT_CELLS = 2

  integer (c_int), parameter, private :: COLUMN = 1
  integer (c_int), parameter, private :: ROW = 2

  !> @todo change these global (module) variables to local variables
  integer (c_int) :: LU_TEMP, LU_GRID

  character (len=64) :: OUTPUT_GRID_DIRECTORY_NAME = ""

contains


  subroutine grid_set_output_directory_name( sDirName )

    character (len=*), intent(in)               :: sDirName

    OUTPUT_GRID_DIRECTORY_NAME = trim(sDirName)

    call LOGS%write("ASCII grids will be written to subdirectory "             &
      //dquote( OUTPUT_GRID_DIRECTORY_NAME ), iLogLevel=LOG_ALL,                    &
      iLinesBefore=1, lEcho=TRUE )

  end subroutine grid_set_output_directory_name


!--------------------------------------------------------------------------

!> Creates a grid of a specified type.
!>
!>  Creates a grid pointer object and allocates memory for the data
!>  associated with the grid (REAL, INTEGER, or T_CELL).
!
!> @param iNX Number of grid cells in the x direction
!> @param iNY Number of grid cells in the y direction
!> @param rX0 X coordinate for the lower left corner of the grid
!> @param rY0 Y coordinate for the lower left corner of the grid
!> @param rX1 X coordinate for the upper right corner of the grid
!> @param rY1 Y coordinate for the upper right corner of the grid
!> @param iDataType Integer value corresponding to the type of data contained in the grid
!>
!> @return pGrd Pointer to a grid object
function grid_CreateComplete ( iNX, iNY, rX0, rY0, rX1, rY1, iDataType ) result ( pGrd )

  ! ARGUMENTS
  integer (c_int), intent(in) :: iNX, iNY        ! Grid dimensions
  real (c_double), intent(in) :: rX0, rY0          ! Lower-left corner (world coords)
  real (c_double), intent(in) :: rX1, rY1          ! Upper-right corner (world coords)
  integer (c_int), intent(in) :: iDataType       ! Data type (DATATYPE_INT, etc.)
  ! RETURN VALUE
  type (GENERAL_GRID_T), pointer :: pGrd
  ! LOCALS
  integer (c_int) :: iStat

  allocate ( pGrd, stat=iStat )
  call assert ( iStat == 0, &
     "Could not allocate pointer to T_GRID object", __SRCNAME__,__LINE__ )

  if (iNX <= 0 .or. iNY <= 0) then
    call LOGS%write("Illegal grid dimensions: ")
    call LOGS%write("iNX: "//asCharacter(iNX) )
    call LOGS%write("iNY: "//asCharacter(iNY) )
    call LOGS%write("rX0: "//asCharacter(rX0) )
    call LOGS%write("rY0: "//asCharacter(rY0) )
    call LOGS%write("rX1: "//asCharacter(rX1) )
    call LOGS%write("rY1: "//asCharacter(rY1) )
    call assert ( FALSE, &
       "INTERNAL PROGRAMMING ERROR? - Illegal grid dimensions specified", __SRCNAME__,__LINE__)
  endif

  select case (iDataType)
      case ( GRID_DATATYPE_INT )

          allocate ( pGrd%iData( iNX, iNY ), stat=iStat )
          call assert (iStat == 0, &
             "Could not allocate integer data", &
              __SRCNAME__,__LINE__)
          pGrd%iData = 0

      case ( GRID_DATATYPE_REAL )
          allocate ( pGrd%rData( iNX, iNY ), stat=iStat )
          call assert (iStat == 0, &
             "Could not allocate real data", &
              __SRCNAME__,__LINE__)
          pGrd%rData = rZERO

      case ( GRID_DATATYPE_DOUBLE )
          allocate ( pGrd%dpData( iNX, iNY ), stat=iStat )
          call assert (iStat == 0, &
             "Could not allocate double-precision data", &
              __SRCNAME__,__LINE__)
          pGrd%dpData = 0.0_c_double

      case default
          call assert ( FALSE, 'Internal error -- illegal grid data type' )
  end select

  pGrd%iDataType = iDataType
  pGrd%iNX = iNX
  pGrd%iNY = iNY
  pGrd%rX0 = rX0
  pGrd%rX1 = rX1
  pGrd%rY0 = rY0
  pGrd%rY1 = rY1
  pGrd%rGridCellSize = (pGrd%rX1 - pGrd%rX0) / real(pGrd%iNX, c_float)
  pGrd%iNumGridCells = iNX * iNY

  allocate(pGrd%iMask(iNX, iNY))
  pGrd%iMask = GRID_ACTIVE_CELL

end function grid_CreateComplete

function grid_CreateSimple ( iNX, iNY, rX0, rY0, rGridCellSize, iDataType ) result ( pGrd )
  !! Creates a new iNX-by-iNY T_GRID of data type iDataType, over the extent
  !! (rX0,rY0)-(rX1,rY1), and returns a pointer.
  ! ARGUMENTS
  integer (c_int), intent(in) :: iNX, iNY        ! Grid dimensions
  real (c_double), intent(in) :: rX0, rY0          ! Lower-left corner (world coords)
  real (c_double), intent(in) :: rGridCellSize
  integer (c_int), intent(in) :: iDataType       ! Data type (DATATYPE_INT, etc.)

  ! RETURN VALUE
  type (GENERAL_GRID_T), pointer :: pGrd
  ! LOCALS
  integer (c_int) :: iStat

  allocate ( pGrd, stat=iStat )
  call assert ( iStat == 0, &
     "Could not allocate pointer to T_GRID object", __SRCNAME__,__LINE__ )
  call assert ( iNX > 0 .and. iNY > 0, &
     "Illegal grid dimensions specified: NX="//asCharacter(iNX)      &
     //"  NY="//asCharacter(iNY), __SRCNAME__,__LINE__)

  select case (iDataType)
      case ( GRID_DATATYPE_INT )

          allocate ( pGrd%iData( iNX, iNY ), stat=iStat )
          call assert (iStat == 0, &
             "Could not allocate integer data", &
              __SRCNAME__,__LINE__)
          pGrd%iData = pGrd%iNoDataValue

      case ( GRID_DATATYPE_REAL )
          allocate ( pGrd%rData( iNX, iNY ), stat=iStat )
          call assert (iStat == 0, &
             "Could not allocate real data", &
              __SRCNAME__,__LINE__)
          pGrd%rData = pGrd%rNoDataValue

      case ( GRID_DATATYPE_DOUBLE )
          allocate ( pGrd%dpData( iNX, iNY ), stat=iStat )
          call assert (iStat == 0, &
             "Could not allocate double-precision data", &
              __SRCNAME__,__LINE__)
          pGrd%dpData = pGrd%dpNoDataValue

      case default
          call assert ( FALSE, 'Internal error -- illegal grid data type' )
  end select

  pGrd%iDataType = iDataType
  pGrd%iNX = iNX
  pGrd%iNY = iNY
  pGrd%rX0 = rX0
  pGrd%rX1 = rX0 + real(iNX, c_double) * rGridCellSize
  pGrd%rY0 = rY0
  pGrd%rY1 = rY0 + real(iNY, c_double) * rGridCellSize
  pGrd%rGridCellSize = rGridCellSize
  pGrd%iNumGridCells = iNX * iNY

  allocate(pGrd%iMask(iNX, iNY))
  pGrd%iMask = GRID_ACTIVE_CELL

end function grid_CreateSimple

!!***

!--------------------------------------------------------------------------
!!****f* grid/grid_Destroy
! NAME
!   grid_Destroy - Destroys a grid of a specified type.
!
! SYNOPSIS
!   Destroys a grid pointer object and deallocates memory for the data
!   associated with the grid (REAL, INTEGER, or T_CELL).
!
! INPUTS
!   pGrd - Pointer to a grid object
!
! OUTPUTS
!   None
!
! NOTES
!   Code refers to parameters that are set within types.f95.
!
! SOURCE

subroutine grid_Destroy ( pGrd )
  !! Destroys the data in the GENERAL_GRID_T pGrd
  ! ARGUMENTS
  type ( GENERAL_GRID_T ), pointer :: pGrd
  ! LOCALS
  integer (c_int) :: iStat

  if(associated(pGrd) )then

    if ( pGrd%iDataType == GRID_DATATYPE_INT ) then
      deallocate ( pGrd%iData, stat=iStat )
      call assert ( iStat == 0, "Failed to deallocate integer grid" )
    else if ( pGrd%iDataType == GRID_DATATYPE_REAL ) then
      deallocate ( pGrd%rData, stat=iStat )
      call assert ( iStat == 0, "Failed to deallocate real grid" )
    else if ( pGrd%iDataType == GRID_DATATYPE_DOUBLE ) then
      deallocate ( pGrd%dpData, stat=iStat )
      call assert ( iStat == 0, "Failed to deallocate double-precision grid" )

!     else if ( pGrd%iDataType == GRID_DATATYPE_CELL_GRID ) then
!       deallocate ( pGrd%Cells, stat=iStat )
!       call assert ( iStat == 0, "Failed to deallocate cell grid" )
!       else if ( pGrd%iDataType == GRID_DATATYPE_ALL ) then
!       deallocate ( pGrd%iData, stat=iStat )
!       call assert ( iStat == 0, "Failed to deallocate integer grid" )
!       deallocate ( pGrd%rData, stat=iStat )
!       call assert ( iStat == 0, "Failed to deallocate real grid" )
!       deallocate ( pGrd%Cells, stat=iStat )
!       call assert ( iStat == 0, "Failed to deallocate cell grid" )
    else
      call assert ( FALSE, "Internal error -- unknown grid type", &
        __SRCNAME__, __LINE__)
    end if

    if( allocated(pGrd%rX) ) then
      deallocate( pGrd%rX, stat=iStat)
      call assert ( iStat == 0, "Failed to deallocate X-coordinate data structure associated with grid", &
        __SRCNAME__, __LINE__ )
    endif

    if( allocated(pGrd%rY) ) then
      deallocate( pGrd%rY, stat=iStat)
      call assert ( iStat == 0, "Failed to deallocate Y-coordinate data structure associated with grid", &
        __SRCNAME__, __LINE__ )
    endif

  endif

  pGrd => null()

end subroutine grid_Destroy
!!***

!--------------------------------------------------------------------------
!!****f* grid/grid_Read
! NAME
!   grid_Read - Reads a grid of a specified type.
!
! SYNOPSIS
!   Reads a grid of the
! INPUTS
!   sFileName - Character string containing the name of the file to be read
!   sFileType - Character string indicating the type of file to be read
!   iDataType - Integer value corresponding to the type of data contained
!     in the input data file
!
! OUTPUTS
!   pGrd - Pointer to a grid object
!
! NOTES
!
! Current legal file types are
!    "ARC_GRID"            ESRI ARC Grid (ASCII)
!    "SURFER"              Golden Software SURFER grid (ASCII)
!
! Valid data types are (see module 'types'):
!    DATATYPE_INT
!    DATATYPE_REAL
!
! SOURCE

function grid_Read ( sFilename, sFileType, iDataType ) result ( pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFilename          ! Name of the grid input file
  character (len=*), intent(in) :: sFileType          ! File type (see above)
  integer (c_int), intent(in) :: iDataType       ! T_GRID_INT or T_GRID_REAL
  ! RETURN VALUE
  type (GENERAL_GRID_T), pointer :: pGrd

  if ( trim(sFileType) == "ARC_GRID" ) then
      pGrd => grid_ReadArcGrid_fn( sFileName, iDataType )
  else if ( trim(sFileType) == "SURFER" ) then
      pGrd => grid_ReadSurferGrid_fn( sFileName, iDataType )
  else
      call assert( FALSE, "Illegal grid file type requested" )
  end if

  pGrd%sFilename = trim(sFilename)

end function grid_Read

!--------------------------------------------------------------------------

subroutine grid_ReadExisting ( sFileName, sFileType, pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFilename          ! Name of the grid input file
  character (len=*), intent(in) :: sFileType          ! File type (see above)
  type (GENERAL_GRID_T), pointer :: pGrd

  if ( trim(sFileType) == "ARC_GRID" ) then
      call grid_ReadArcGrid_sub( sFilename, pGrd )
  else if ( trim(sFileType) == "SURFER" ) then
      call grid_ReadSurferGrid_sub( sFilename, pGrd )
  else
      call assert( FALSE, "Illegal grid file type requested" )
  end if

  pGrd%sFilename = trim(sFilename)

end subroutine grid_ReadExisting

!!***
!--------------------------------------------------------------------------
!!****f* grid/grid_ReadArcGrid_fn
! NAME
!   grid_ReadArcGrid_fn - Reads an ARC ASCII grid of a specified type.
!
! SYNOPSIS
!   Reads an ARC ASCII grid of specified data type
!
! INPUTS
!   sFileName - Character string containing the name of the file to be read
!   iDataType - Integer value corresponding to the type of data contained
!     in the input data file
!
! OUTPUTS
!   pGrd - Pointer to a grid object
!
! NOTES
!
! Valid data types are (see module 'types'):
!    DATATYPE_INT
!    DATATYPE_REAL
!
! SOURCE

function grid_ReadArcGrid_fn ( sFileName, iDataType ) result ( pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  integer (c_int), intent(in) :: iDataType       ! T_GRID_INT or T_GRID_REAL
  ! RETURN VALUE
  type (GENERAL_GRID_T), pointer :: pGrd
  ! LOCALS
  character (len=256) :: sInputRecord                 ! Text record for input
  character (len=256) :: sDirective                   ! Directive for input
  character (len=256) :: sArgument                    ! Argument for keyword directives
  character (len=256) :: sNoDataValue                 ! String to hold nodata value
  character (len=8192) :: sBuf
  integer (c_int) :: iStat                       ! For "iostat="
  integer (c_int) :: iNX,iNY                     ! Grid dimensions
  integer (c_int) :: iHdrRecs                    ! Number of records in header
  integer (c_int) :: iCol,iRow,k                 ! Loop indices for grid reading
  real (c_double) :: rX0,rX1                        ! Limits in X
  real (c_double) :: rY0,rY1                        ! Limits in Y
  real (c_double) :: rCellSize                      ! Cell size
  integer (c_int) :: iCount,iCumlCount
  logical (c_bool) :: lXLLCenter, lYLLCenter  ! Flags XLLCENTER / XLLCORNER
  logical (c_bool) :: lFileExists
  logical (c_bool) :: lIsOpen

  ! Pre-scan for the number of header records and read the header
  inquire(file=trim(sFileName), EXIST=lFileExists)
  call assert( lFileExists, "The Arc ASCII grid file "//dquote(sFilename)// &
    " could not be found.",__SRCNAME__,__LINE__)

  inquire(unit=LU_GRID, OPENED=lIsOpen )
  if (lIsOpen )  close( unit=LU_GRID )

  open ( newunit=LU_GRID, iostat=iStat, file=trim(sFileName) )
  call assert( iStat == 0, &
    "Could not open input file " // trim(sFileName) )

  iHdrRecs = 0
  lXLLCenter = FALSE
  lYLLCenter = FALSE
  sNoDataValue = ""

  do
      read ( unit=LU_GRID, fmt="(a256)", iostat=iStat ) sInputRecord
      call assert ( iStat == 0, &
      "Could not read input record - file:"//trim(sFileName) )
      call Chomp ( sInputRecord, sDirective )
      call toUppercase ( sDirective )
      call Chomp ( sInputRecord, sArgument )
      if ( sDirective == "NCOLS" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) iNX
          call assert ( iStat == 0, "Could not read NCOLS" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "NROWS" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) iNY
          call assert ( iStat == 0, "Could not read NROWS" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "XLLCENTER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rX0
          call assert ( iStat == 0, "Could not read XLLCENTER" )
          lXLLCenter = TRUE
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "YLLCENTER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rY0
          call assert ( iStat == 0, "Could not read YLLCENTER" )
          lXLLCenter = TRUE
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "XLLCORNER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rX0
          call assert ( iStat == 0, "Could not read XLLCORNER" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "YLLCORNER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rY0
          call assert ( iStat == 0, "Could not read YLLCORNER" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "CELLSIZE" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rCellSize
          call assert ( iStat == 0, "Could not read CELLSIZE" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "NODATA_VALUE" ) then
          sNoDataValue = trim(sArgument)
          iHdrRecs = iHdrRecs + 1
      else
          ! Found the data -- construct the grid
          if ( lXLLCenter ) rX0 = rX0 - 0.5_c_float*rCellSize
          if ( lYLLCenter ) rY0 = rY0 - 0.5_c_float*rCellSize
          rX1 = rX0 + real(iNX, c_double) * rCellSize
          rY1 = rY0 + real(iNY, c_double) * rCellSize

          pGrd => grid_Create ( iNX, iNY, rX0, rY0, rX1, rY1, iDataType )

          pGrd%rGridCellSize = rCellSize
          ! Go back to the top, skip the header...
          rewind ( unit=LU_GRID, iostat=iStat )
          call assert ( iStat == 0, "Failed to rewind grid file" )
          do iCol=1,iHdrRecs
              read ( unit=LU_GRID, fmt="(a256)", iostat=iStat ) sInputRecord
              call assert ( iStat == 0, &
                "Could not read input record - file: "//trim(sFileName))
          end do
          ! ... and read the data.
          select case ( iDataType )

              case ( DATATYPE_INT )

                do iRow=1,pGrd%iNY
                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%iData(:,iRow)
                  call assert ( iStat == 0, &
                    "Failed to read integer grid data - file: " &
                    //trim(sFileName)//"  row num: "//TRIM( asCharacter(iRow)), &
                   __SRCNAME__,__LINE__ )
                end do
                if(len_trim(sNoDataValue) > 0) then
                  read(unit=sNoDataValue, fmt=*, iostat=iStat) pGrd%iNoDataValue
                  call assert ( iStat == 0, &
                    "Failed to read NODATA value in grid data - file: " &
                    //dquote(sFileName), __SRCNAME__,__LINE__ )
                endif

              case ( DATATYPE_REAL )

                do iRow=1,pGrd%iNY
                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%rData(:,iRow)
                  call assert ( iStat == 0, &
                    "Failed to read real grid data - file: " &
                    //trim(sFileName)//"  row num: "//TRIM( asCharacter(iRow)), &
                   __SRCNAME__,__LINE__ )
                end do
                if(len_trim(sNoDataValue) > 0) then
                  read(unit=sNoDataValue, fmt=*, iostat=iStat) pGrd%rNoDataValue
                  call assert ( iStat == 0, &
                    "Failed to read NODATA value in grid data - file: " &
                    //trim(sFileName), __SRCNAME__,__LINE__ )
                endif

              case ( DATATYPE_DOUBLE )

                do iRow=1,pGrd%iNY
                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%dpData(:,iRow)
                  call assert ( iStat == 0, &
                    "Failed to read double-precision grid data - file: "        &
                    //trim(sFileName)//"  row num: "//TRIM( asCharacter(iRow)), &
                   __SRCNAME__,__LINE__ )
                end do
                if(len_trim(sNoDataValue) > 0) then
                  read(unit=sNoDataValue, fmt=*, iostat=iStat) pGrd%dpNoDataValue
                  call assert ( iStat == 0, &
                    "Failed to read NODATA value in grid data - file: " &
                    //trim(sFileName), __SRCNAME__,__LINE__ )
                endif

              case default

                  call assert ( FALSE, &
                    "Internal error -- illegal ARC GRID data type", &
                    __SRCNAME__,__LINE__)

          end select
          exit
      end if
  end do

  if ( iDataType == DATATYPE_INT ) call grid_checkIntegerGridValues(pGrd, sFilename)

  open ( unit=LU_GRID, iostat=iStat )
  call assert ( iStat == 0, "Failed to close grid file" )

end function grid_ReadArcGrid_fn

!--------------------------------------------------------------------------

subroutine grid_ReadArcGrid_sub ( sFileName, pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  type (GENERAL_GRID_T), pointer :: pGrd

  ! LOCALS
  character (len=256) :: sInputRecord                 ! Text record for input
  character (len=256) :: sDirective                   ! Directive for input
  character (len=256) :: sArgument                    ! Argument for keyword directives
  character (len=256) :: sNoDataValue                 ! String to hold nodata value
  character (len=8192) :: sBuf
  character (len=256) :: sTemp
  integer (c_int) :: iStat                       ! For "iostat="
  integer (c_int) :: iNX,iNY                     ! Grid dimensions
  integer (c_int) :: iHdrRecs                    ! Number of records in header
  integer (c_int) :: iCol,iRow,k                 ! Loop indices for grid reading
  real (c_double) :: rX0,rX1                        ! Limits in X
  real (c_double) :: rY0,rY1                        ! Limits in Y
  real (c_double) :: rCellSize                      ! Cell size
  integer (c_int) :: iCount,iCumlCount
  logical (c_bool) :: lXLLCenter, lYLLCenter  ! Flags XLLCENTER / XLLCORNER
  logical (c_bool) :: lFileExists
  logical (c_bool) :: lIsOpen

  ! Pre-scan for the number of header records and read the header

  inquire(file=trim(sFileName), EXIST=lFileExists, OPENED=lIsOpen )
  call assert( lFileExists, "The Arc ASCII grid file "//dquote(sFilename)// &
    " could not be found.",__SRCNAME__,__LINE__)

  inquire(unit=LU_GRID, OPENED=lIsOpen )
  if (lIsOpen )  close( unit=LU_GRID )

  open ( newunit=LU_GRID, iostat=iStat, file=trim(sFileName) )
  call assert( iStat == 0, &
    "Could not open input file " // trim(sFileName) )

  iHdrRecs = 0
  lXLLCenter = FALSE
  lYLLCenter = FALSE
  sNoDataValue = ""

  do
      read ( unit=LU_GRID, fmt="(a256)", iostat=iStat ) sInputRecord
      call assert ( iStat == 0, &
      "Could not read input record - file:"//trim(sFileName) )
      call Chomp ( sInputRecord, sDirective )
      call toUppercase ( sDirective )
      call Chomp ( sInputRecord, sArgument )
      if ( sDirective == "NCOLS" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) iNX
          call assert ( iStat == 0, "Could not read NCOLS" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "NROWS" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) iNY
          call assert ( iStat == 0, "Could not read NROWS" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "XLLCENTER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rX0
          call assert ( iStat == 0, "Could not read XLLCENTER" )
          lXLLCenter = TRUE
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "YLLCENTER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rY0
          call assert ( iStat == 0, "Could not read YLLCENTER" )
          lXLLCenter = TRUE
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "XLLCORNER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rX0
          call assert ( iStat == 0, "Could not read XLLCORNER" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "YLLCORNER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rY0
          call assert ( iStat == 0, "Could not read YLLCORNER" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "CELLSIZE" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rCellSize
          call assert ( iStat == 0, "Could not read CELLSIZE" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "NODATA_VALUE" ) then
          sNoDataValue = trim(sArgument)
          iHdrRecs = iHdrRecs + 1
      else
          ! Found the data -- construct the grid

          ! Go back to the top, skip the header...
          rewind ( unit=LU_GRID, iostat=iStat )
          call assert ( iStat == 0, "Failed to rewind grid file" )
          do iCol=1,iHdrRecs
              read ( unit=LU_GRID, fmt="(a256)", iostat=iStat ) sInputRecord
              call assert ( iStat == 0, &
                "Could not read input record - file: "//trim(sFileName))
          end do
          ! ... and read the data.
          select case ( pGrd%iDataType )

              case ( DATATYPE_INT )
                do iRow=1,pGrd%iNY
                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%iData(:,iRow)
                  call assert ( iStat == 0, &
                    "Failed to read integer grid data - file: " &
                    //trim(sFileName)//"  row num: "//TRIM( asCharacter(iRow)), &
                   __SRCNAME__,__LINE__ )
                end do
                if(len_trim(sNoDataValue) > 0) then
                  read(unit=sNoDataValue, fmt=*, iostat=iStat) pGrd%iNoDataValue
                  call assert ( iStat == 0, &
                    "Failed to read NODATA value in grid data - file: " &
                    //trim(sFileName), __SRCNAME__,__LINE__ )
                endif

              case ( DATATYPE_REAL )

                pGrd%rData = 3.141592654

                do iRow=1,pGrd%iNY

                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%rData(:,iRow)

                  if(iStat /= 0) then
                    do iCol=1,pGrd%iNX
                      print *, iCol, ": ",pGrd%rData(iCol,iRow)
                    enddo
                  endif

                  call assert ( iStat == 0, &
                    "Failed to read real grid data - file: " &
                    //trim(sFileName)//"  row num: "//TRIM( asCharacter(iRow)), &
                   __SRCNAME__,__LINE__ )

                enddo
                if(len_trim(sNoDataValue) > 0) then
                  read(unit=sNoDataValue, fmt=*, iostat=iStat) pGrd%rNoDataValue
                  call assert ( iStat == 0, &
                    "Failed to read NODATA value in grid data - file: " &
                    //trim(sFileName), __SRCNAME__,__LINE__ )
                endif

              case ( DATATYPE_DOUBLE )
                do iRow=1,pGrd%iNY
                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%dpData(:,iRow)
                  call assert ( iStat == 0, &
                    "Failed to read double-precision grid data - file: "        &
                    //trim(sFileName)//"  row num: "//TRIM( asCharacter(iRow)), &
                   __SRCNAME__,__LINE__ )
                end do
                if(len_trim(sNoDataValue) > 0) then
                  read(unit=sNoDataValue, fmt=*, iostat=iStat) pGrd%dpNoDataValue
                  call assert ( iStat == 0, &
                    "Failed to read NODATA value in grid data - file: " &
                    //trim(sFileName), __SRCNAME__,__LINE__ )
                endif

              case default
                  call assert ( FALSE, &
                    "Internal error -- illegal ARC GRID data type", &
                    __SRCNAME__,__LINE__)
          end select
          exit
      end if
  end do

  if ( pGrd%iDataType == DATATYPE_INT ) call grid_checkIntegerGridValues(pGrd, sFilename)

  open ( unit=LU_GRID, iostat=iStat )
  call assert ( iStat == 0, "Failed to close grid file" )

end subroutine grid_ReadArcGrid_sub

!!***
!--------------------------------------------------------------------------
!!****f* grid/grid_ReadSurferGrid_fn
! NAME
!   grid_ReadSurferGrid_fn - Reads an Surfer ASCII grid of a specified type.
!
! SYNOPSIS
!   Reads an Surfer ASCII grid of specified data type
!
! INPUTS
!   sFileName - Character string containing the name of the file to be read
!   iDataType - Integer value corresponding to the type of data contained
!     in the input data file
!
! OUTPUTS
!   pGrd - Pointer to a grid object
!
! NOTES
!
! Valid data types are (see module 'types'):
!    DATATYPE_INT
!    DATATYPE_REAL
!
! SOURCE

function grid_ReadSurferGrid_fn ( sFileName, iDataType ) result ( pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  integer (c_int), intent(in) :: iDataType       ! T_GRID_INT or T_GRID_REAL
  ! RETURN VALUE
  type (GENERAL_GRID_T), pointer :: pGrd
  ! LOCALS
  character (len=4) :: sSentinel                      ! Better be "DSAA" for SURFER!
  integer (c_int) :: iStat                       ! For "iostat="
  integer (c_int) :: iNX,iNY                     ! Grid dimensions
  integer (c_int) :: iCol,iRow                         ! Loop indices for grid reading
  real (c_double) :: rX0,rX1                       ! Limits in X
  real (c_double) :: rY0,rY1                       ! Limits in Y
  real (c_float) :: rZ0,rZ1                       ! Limits in Z (not used)
  logical (c_bool) :: lFileExists
  logical (c_bool) :: lIsOpen

  inquire(file=trim(sFileName), EXIST=lFileExists)
  call assert( lFileExists, "The Surfer ASCII grid file "//dquote(sFilename)// &
    " could not be found.",__SRCNAME__,__LINE__)

  inquire(unit=LU_GRID, OPENED=lIsOpen )
  if (lIsOpen )  close( unit=LU_GRID )

  open ( newunit=LU_GRID, iostat=iStat, file=trim(sFileName) )
  call assert( iStat == 0, &
     "Could not open input file " // trim(sFileName) )

  read ( unit=LU_GRID, fmt=*, iostat=iStat ) sSentinel
  call assert ( iStat == 0, &
     "Could not read first record of SURFER grid" )
  call assert ( LOGICAL(trim(sSentinel) == "DSAA",c_bool), &
     "This is not a SURFER grid" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) iNX, iNY
  call assert ( iStat == 0, &
     "Error reading SURFER grid dimensions" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rX0, rX1
  call assert ( iStat == 0, &
     "Error reading SURFER X limits" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rY0, rY1
  call assert ( iStat == 0, &
     "Error reading SURFER y limits" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rZ0, rZ1
  call assert ( iStat == 0, &
     "Error reading SURFER Z limits" )

  pGrd => grid_Create ( iNX, iNY, rX0, rY0, rX1, rY1, iDataType )
  select case ( iDataType )
      case ( DATATYPE_INT )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%iData(:,iRow)
              call assert ( iStat == 0, "Failed to read integer grid data" )
          end do
      case ( DATATYPE_REAL )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%rData(:,iRow)
              call assert ( iStat == 0, "Failed to read real grid data" )
          end do
      case ( DATATYPE_DOUBLE )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%dpData(:,iRow)
              call assert ( iStat == 0, "Failed to read double-precision grid data" )
          end do
      case default
          call assert ( FALSE, "Internal error -- illegal SURFER grid data type" )
  end select

  call assert(iNX>0, "Must have a non-zero number of columns surfer grid file...")
  call assert(iNY>0, "Must have a non-zero number of rows in a surfer grid file...")

  pGrd%rGridCellSize = (rX1-rX0)/iNX

  if ( iDataType == DATATYPE_INT ) call grid_checkIntegerGridValues(pGrd, sFilename)

  open ( unit=LU_GRID, iostat=iStat )
  call assert ( iStat == 0, "Failed to close grid file" )

end function grid_ReadSurferGrid_fn

!------------------------------------------------------------------------------

subroutine grid_ReadSurferGrid_sub ( sFileName, pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  type (GENERAL_GRID_T), pointer :: pGrd

  ! LOCALS
  character (len=4) :: sSentinel                      ! Better be "DSAA" for SURFER!
  integer (c_int) :: iStat                       ! For "iostat="
  integer (c_int) :: iNX,iNY                     ! Grid dimensions
  integer (c_int) :: iCol,iRow                         ! Loop indices for grid reading
  real (c_double) :: rX0,rX1                       ! Limits in X
  real (c_double) :: rY0,rY1                       ! Limits in Y
  real (c_float) :: rZ0,rZ1                       ! Limits in Z (not used)
  logical (c_bool) :: lFileExists
  logical (c_bool) :: lIsOpen

  inquire(file=trim(sFileName), EXIST=lFileExists)
  call assert( lFileExists, "The Surfer ASCII grid file "//dquote(sFilename)// &
    " could not be found.",__SRCNAME__,__LINE__)

  inquire(unit=LU_GRID, OPENED=lIsOpen )
  if (lIsOpen )  close( unit=LU_GRID )

  open ( newunit=LU_GRID, iostat=iStat, file=trim(sFileName) )
  call assert( iStat == 0, &
     "Could not open input file " // trim(sFileName) )

  read ( unit=LU_GRID, fmt=*, iostat=iStat ) sSentinel
  call assert ( iStat == 0, &
     "Could not read first record of SURFER grid" )
  call assert ( LOGICAL(trim(sSentinel) == "DSAA",c_bool), &
     "This is not a SURFER grid" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) iNX, iNY
  call assert ( iStat == 0, &
     "Error reading SURFER grid dimensions" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rX0, rX1
  call assert ( iStat == 0, &
     "Error reading SURFER X limits" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rY0, rY1
  call assert ( iStat == 0, &
     "Error reading SURFER y limits" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rZ0, rZ1
  call assert ( iStat == 0, &
     "Error reading SURFER Z limits" )

  select case ( pGrd%iDataType )
      case ( DATATYPE_INT )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%iData(:,iRow)
              call assert ( iStat == 0, "Failed to read integer grid data" )
          end do
      case ( DATATYPE_REAL )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%rData(:,iRow)
              call assert ( iStat == 0, "Failed to read real grid data" )
          end do
      case ( DATATYPE_DOUBLE )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%dpData(:,iRow)
              call assert ( iStat == 0, "Failed to read double-precision grid data" )
          end do
      case default
          call assert ( FALSE, "Internal error -- illegal SURFER grid data type" )
  end select

  call assert(iNX > 0,"Must have a non-zero number of grid cells in a surfer grid file...")
  call assert(iNY > 0,"Must have a non-zero number of grid cells in a surfer grid file...")

  if ( pGrd%iDataType == DATATYPE_INT ) call grid_checkIntegerGridValues(pGrd, sFilename)

  open ( unit=LU_GRID, iostat=iStat )
  call assert ( iStat == 0, "Failed to close grid file" )

end subroutine grid_ReadSurferGrid_sub

!------------------------------------------------------------------------------

subroutine grid_WriteGrid(sFilename, pGrd, iOutputFormat)

  ! [ ARGUMENTS ]
  character (len=*),intent(in) :: sFilename
  type (GENERAL_GRID_T), pointer :: pGrd
  integer (c_int) :: iOutputFormat

  if ( iOutputFormat == OUTPUT_ARC ) then

    call grid_WriteArcGrid(trim(OUTPUT_GRID_DIRECTORY_NAME)//sFilename, pGrd)

  elseif ( iOutputFormat == OUTPUT_SURFER ) then

    call grid_WriteSurferGrid(trim(OUTPUT_GRID_DIRECTORY_NAME)//sFilename, pGrd)

  endif

end subroutine grid_WriteGrid

!------------------------------------------------------------------------------

subroutine grid_WriteArcGrid(sFilename, pGrd)

  ! [ ARGUMENTS ]
  character (len=*),intent(in) :: sFilename
  type (GENERAL_GRID_T), pointer :: pGrd


  ! [ LOCALS ]
  integer (c_int) :: iCol,iRow
  integer (c_int) :: iNumCols, iNumRows
  integer (c_int) ::  iStat
  character(len=256) :: sBuf

  if ( pGrd%iDataType == DATATYPE_INT ) then
    iNumCols = size(pGrd%iData,1)
    iNumRows = size(pGrd%iData,2)
  elseif ( pGrd%iDataType == DATATYPE_REAL ) then
    iNumCols = size(pGrd%rData,1)
    iNumRows = size(pGrd%rData,2)
  elseif ( pGrd%iDataType == DATATYPE_DOUBLE ) then
    iNumCols = size(pGrd%dpData,1)
    iNumRows = size(pGrd%dpData,2)
  else
    call assert(FALSE, "Internal programming error - Unsupported grid type", &
      __SRCNAME__, __LINE__)
  endif

  ! dynamically create the Fortran output format
  write(sBuf,FMT="(a,a,a)") '(',TRIM( asCharacter(iNumCols)),'(a,1x))'

  open ( LU_TEMP, file=trim(OUTPUT_GRID_DIRECTORY_NAME)//sFilename, iostat=istat, status="REPLACE" )
  call assert( istat==0, "Could not open output file "//dQuote(sFilename), &
      __SRCNAME__,__LINE__)

  write ( unit=LU_TEMP, fmt="('NCOLS ',i10)", iostat=istat ) iNumCols
  call assert( istat==0, "Error writing grid file header", __SRCNAME__, __LINE__)

  write ( unit=LU_TEMP, fmt="('NROWS ',i10)", iostat=istat ) iNumRows
  call assert( istat==0, "Error writing grid file header", __SRCNAME__, __LINE__)

  write ( unit=LU_TEMP, fmt="('XLLCORNER ',f14.3)", iostat=istat ) pGrd%rX0
  call assert( istat==0, "Error writing X limits", __SRCNAME__, __LINE__)

  write ( unit=LU_TEMP, fmt="('YLLCORNER ',f14.3)", iostat=istat ) pGrd%rY0
  call assert( istat==0, "Error writing Y limits", __SRCNAME__, __LINE__)

  write ( unit=LU_TEMP, fmt="('CELLSIZE ',f14.3)", iostat=istat ) pGrd%rGridCellSize
  call assert( istat==0, "Error writing cell size", __SRCNAME__, __LINE__)

  if ( pGrd%iDataType == DATATYPE_INT ) then

    write ( unit=LU_TEMP, fmt="('NODATA_VALUE ',i14)", iostat=istat ) pGrd%iNoDataValue
    call assert( istat==0, "Error writing NODATA value", __SRCNAME__, __LINE__)
    do iRow=1,iNumRows
      write( unit=LU_TEMP, fmt=TRIM(sBuf), iostat=istat ) &
        (TRIM( asCharacter(pGrd%iData(iCol,iRow) ) ),iCol=1,iNumCols)
      call assert( istat==0, "Error writing Arc ASCII INTEGER grid data", &
        __SRCNAME__, __LINE__)
    end do

  elseif ( pGrd%iDataType == DATATYPE_REAL ) then

    write ( unit=LU_TEMP, fmt="('NODATA_VALUE ',g14.4)", iostat=istat ) pGrd%rNoDataValue
    call assert( istat==0, "Error writing NODATA value", __SRCNAME__, __LINE__)
    do iRow=1,iNumRows
      write( unit=LU_TEMP, fmt=TRIM(sBuf), iostat=istat ) &
        (TRIM(asCharacter( pGrd%rData(iCol,iRow) )),iCol=1,iNumCols)
      call assert( istat==0, "Error writing Arc ASCII REAL grid data", &
        __SRCNAME__, __LINE__)
    end do

  elseif ( pGrd%iDataType == DATATYPE_DOUBLE ) then

    write ( unit=LU_TEMP, fmt="('NODATA_VALUE ',g14.4)", iostat=istat ) pGrd%dpNoDataValue
    call assert( istat==0, "Error writing NODATA value", __SRCNAME__, __LINE__)
    do iRow=1,iNumRows
      write( unit=LU_TEMP, fmt=TRIM(sBuf), iostat=istat ) &
        (TRIM(asCharacter( pGrd%dpData(iCol,iRow) )),iCol=1,iNumCols)
      call assert( istat==0, "Error writing Arc ASCII REAL grid data", &
        __SRCNAME__, __LINE__)
    end do

  endif

  close (unit=LU_TEMP)

end subroutine grid_WriteArcGrid

!------------------------------------------------------------------------------

subroutine grid_WriteSurferGrid(sFilename, pGrd)

  ! [ ARGUMENTS ]
  character (len=*),intent(in) :: sFilename
  type (GENERAL_GRID_T), pointer :: pGrd

  ! [ LOCALS ]
  integer (c_int) :: iCol,iRow
  integer (c_int) :: iNumCols, iNumRows
  integer (c_int) ::  iStat
  character(len=256) :: sBuf
  real (c_float) :: fHalfCell

  if ( pGrd%iDataType == DATATYPE_INT ) then
    iNumCols = size(pGrd%iData,1)
    iNumRows = size(pGrd%iData,2)
  elseif ( pGrd%iDataType == DATATYPE_REAL ) then
    iNumCols = size(pGrd%rData,1)
    iNumRows = size(pGrd%rData,2)
  elseif ( pGrd%iDataType == DATATYPE_DOUBLE ) then
    iNumCols = size(pGrd%dpData,1)
    iNumRows = size(pGrd%dpData,2)
  else
    call assert(FALSE, "Internal programming error - Unsupported grid type", &
      __SRCNAME__, __LINE__)
  endif

  fHalfCell = pGrd%rGridCellSize * 0.5_c_float

  ! dynamically create the Fortran output format
  write(sBuf,FMT="(a,a,a)") '(',TRIM( asCharacter(iNumCols)),'(a,1x))'

  open ( LU_TEMP, file=trim(OUTPUT_GRID_DIRECTORY_NAME)//sFilename, iostat=istat, status="REPLACE" )
  call assert( istat==0, "Could not open output file "//dQuote(sFilename), &
      __SRCNAME__,__LINE__)

  write ( unit=LU_TEMP, fmt="('DSAA')", iostat=istat )
  call assert( istat==0, "Error writing SURFER header", &
    __SRCNAME__, __LINE__)

  write ( unit=LU_TEMP, fmt="(2i8)", iostat=istat ) iNumCols, iNumRows
  call assert( istat==0, "Error writing SURFER dimensions", &
    __SRCNAME__, __LINE__)

  write ( unit=LU_TEMP, fmt="(2f14.3)", iostat=istat ) &
           pGrd%rX0 + fHalfCell , pGrd%rX1 - fHalfCell
  call assert( istat==0, "Error writing SURFER X limits", &
    __SRCNAME__, __LINE__)

  write ( unit=LU_TEMP, fmt="(2f14.3)", iostat=istat ) &
           pGrd%rY0 + fHalfCell, pGrd%rY1 - fHalfCell
  call assert( istat==0, "Error writing SURFER Y limits", &
    __SRCNAME__, __LINE__)

  if ( pGrd%iDataType == DATATYPE_INT ) then

    write ( unit=LU_TEMP, fmt="(2i14)", iostat=istat ) minval(pGrd%iData),maxval(pGrd%iData)
    call assert( istat==0, "Error writing SURFER Z limits", &
      __SRCNAME__, __LINE__)


    do iRow=iNumRows,1,-1
      write( unit=LU_TEMP, fmt=TRIM(sBuf), iostat=istat ) &
        (TRIM(asCharacter(pGrd%iData(iCol,iRow) ) ),iCol=1,iNumCols)
      call assert( istat==0, "Error writing SURFER grid data" , &
        __SRCNAME__, __LINE__)
    end do

  elseif ( pGrd%iDataType == DATATYPE_REAL ) then

    write ( unit=LU_TEMP, fmt="(2f14.3)", iostat=istat ) minval(pGrd%rData),maxval(pGrd%rData)
    call assert( istat==0, "Error writing SURFER Z limits", &
      __SRCNAME__, __LINE__)

    do iRow=iNumRows,1,-1
      write( unit=LU_TEMP, fmt=TRIM(sBuf), iostat=istat ) &
        (TRIM( asCharacter(pGrd%rData(iCol,iRow) ) ),iCol=1,iNumCols)
      call assert( istat==0, "Error writing SURFER grid data" , &
        __SRCNAME__, __LINE__)
    end do

  elseif ( pGrd%iDataType == DATATYPE_DOUBLE ) then

    write ( unit=LU_TEMP, fmt="(2f14.3)", iostat=istat ) minval(pGrd%dpData),maxval(pGrd%dpData)
    call assert( istat==0, "Error writing SURFER Z limits", &
      __SRCNAME__, __LINE__)

    do iRow=iNumRows,1,-1
      write( unit=LU_TEMP, fmt=TRIM(sBuf), iostat=istat ) &
        (TRIM( asCharacter(pGrd%dpData(iCol,iRow) ) ),iCol=1,iNumCols)
      call assert( istat==0, "Error writing SURFER grid data" , &
        __SRCNAME__, __LINE__)
    end do

  endif

  close (unit=LU_TEMP)

end subroutine grid_WriteSurferGrid
!!***
!--------------------------------------------------------------------------
!!****f* grid/grid_Conform
! NAME
!   grid_WriteArcGrid - Reads an Surfer ASCII grid of a specified type.
!
! SYNOPSIS
!   Write an ARC ASCII grid of specified data type
!
! INPUTS
!
!  pGrd1 - Pointer to first grid
!  pGrd2 - Pointer to second grid
!  rTolerance - OPTIONAL - Allowable tolerance between the two grids
!
! OUTPUTS
!  lConform - Logical, TRUE if the grids are compatible within the
!    specified tolerance, FALSE if otherwise
!
! SOURCE

function grid_Conform ( pGrd1, pGrd2, rTolerance ) result ( lConform )
  !! Returns .true. if the T_GRID objects conform (in terms of cell sizes and extents)
  !! The optional argument rTolerance is the precision for checking the (floating-point)
  !! extent coordinates (this defaults to rDEFAULT_TOLERANCE, below). The tolerance
  !! is set to abs(rTolerance * ( rX1-rX1 ) )
  ! ARGUMENTS
  type (GENERAL_GRID_T), pointer :: pGrd1,pGrd2
  real (c_float), intent(in), optional :: rTolerance
  ! RETURN VALUE
  logical (c_bool) :: lConform
  ! LOCALS
  real (c_float) :: rTol
  real (c_float), parameter :: rDEFAULT_TOLERANCE = 0.5_c_float

  if ( present ( rTolerance ) ) then
      rTol = rTolerance * ( pGrd1%rX1 - pGrd1%rX0 )
  else
      rTol = rDEFAULT_TOLERANCE * ( pGrd1%rX1 - pGrd1%rX0 )
  end if

  lConform = TRUE

  if ( pGrd1%iNX /= pGrd2%iNX ) then
    lConform = FALSE
    call LOGS%write("Unequal number of columns between grids", iLogLevel=LOG_ALL)
  endif

  if ( pGrd1%iNY /= pGrd2%iNY ) then
    lConform = FALSE
    call LOGS%write("Unequal number of rows between grids", iLogLevel=LOG_ALL)
  endif

  if( abs ( pGrd1%rX0 - pGrd2%rX0 ) > rTol ) then
     call LOGS%write("Lower left-hand side X coordinates do not match:", iLogLevel=LOG_ALL)
     call LOGS%write("Grid 1 value: "//asCharacter(pGrd1%rX0)  &
          //"; grid 2 value: "//asCharacter(pGrd2%rX0), iLogLevel=LOG_ALL)
    lConform = FALSE
  endif

  if( abs ( pGrd1%rY0 - pGrd2%rY0 ) > rTol ) then
    call LOGS%write("Lower left-hand side Y coordinates do not match:", iLogLevel=LOG_ALL)
    call LOGS%write("Grid 1 value: "//asCharacter(pGrd1%rY0)  &
          //"; grid 2 value: "//asCharacter(pGrd2%rY0), iLogLevel=LOG_ALL)
    lConform = FALSE
  endif

  if( abs ( pGrd1%rX1 - pGrd2%rX1 ) > rTol ) then
     call LOGS%write("Upper right-hand side X coordinates do not match:", iLogLevel=LOG_ALL)
     call LOGS%write("Grid 1 value: "//asCharacter(pGrd1%rX1)  &
          //"; grid 2 value: "//asCharacter(pGrd2%rX1), iLogLevel=LOG_ALL)
    lConform = FALSE
  endif

  if( abs ( pGrd1%rY1 - pGrd2%rY1 ) > rTol ) then
    call LOGS%write("Upper right-hand side Y coordinates do not match:", iLogLevel=LOG_ALL)
    call LOGS%write("Grid 1 value: "//asCharacter(pGrd1%rY1)  &
          //"; grid 2 value: "//asCharacter(pGrd2%rY1), iLogLevel=LOG_ALL)
    lConform = FALSE
  endif

end function grid_Conform

!------------------------------------------------------------------------------

function grid_CompletelyCover( pBaseGrd, pOtherGrd, rTolerance ) result ( lCompletelyCover )
  !! Returns .true. if the T_GRID objects conform (in terms of cell sizes and extents)
  !! The optional argument rTolerance is the precision for checking the (floating-point)
  !! extent coordinates (this defaults to rDEFAULT_TOLERANCE, below). The tolerance
  !! is set to abs(rTolerance * ( rX1-rX1 ) )
  ! ARGUMENTS
  type (GENERAL_GRID_T), pointer :: pBaseGrd,pOtherGrd
  real (c_float), intent(in), optional :: rTolerance
  ! RETURN VALUE
  logical (c_bool) :: lCompletelyCover
  ! LOCALS
  real (c_float) :: rTol
  real (c_float) :: rDEFAULT_TOLERANCE

  rDEFAULT_TOLERANCE = -pBaseGrd%rGridCellSize / 100.
  !rDEFAULT_TOLERANCE = rZERO

  if ( present ( rTolerance ) ) then
      rTol = rTolerance
  else
      rTol = rDEFAULT_TOLERANCE
  end if

  if ( (pBaseGrd%rX0 - pOtherGrd%rX0 >= rTol) .and. &
       (pBaseGrd%rY0 - pOtherGrd%rY0 >= rTol) .and. &
       (pOtherGrd%rX1 - pBaseGrd%rX1 >= rTol) .and. &
       (pOtherGrd%rY1 - pBaseGrd%rY1 >= rTol) ) then
      lCompletelyCover = TRUE
  else
      lCompletelyCover = FALSE

      call LOGS%write("Extents of the data grid file "//dquote(pOtherGrd%sFilename) &
          //" do not cover the base grid extents.")

      call LOGS%write("BASE GRID EXTENTS        DATA GRID EXTENTS        DIFFERENCE", &
        iTab=18, iLinesBefore=1)
      call LOGS%write("X (lower-left):   "//trim(asCharacter(pBaseGrd%rX0))    &
        //"       "//trim(asCharacter(pOtherGrd%rX0))                          &
        //"       "//trim(asCharacter(pBaseGrd%rX0 - pOtherGrd%rX0)) )
      call LOGS%write("Y (lower-left):   "//trim(asCharacter(pBaseGrd%rY0))    &
        //"       "//trim(asCharacter(pOtherGrd%rY0))                          &
        //"       "//trim(asCharacter(pBaseGrd%rY0 - pOtherGrd%rY0)) )
      call LOGS%write("X (upper-right):  "//trim(asCharacter(pBaseGrd%rX1))    &
        //"       "//trim(asCharacter(pOtherGrd%rX1))                          &
        //"       "//trim(asCharacter(pOtherGrd%rX1 - pBaseGrd%rX1)) )
      call LOGS%write("Y (upper-right):  "//trim(asCharacter(pBaseGrd%rY1))    &
        //"       "//trim(asCharacter(pOtherGrd%rY1))                          &
        //"       "//trim(asCharacter(pOtherGrd%rY1 - pBaseGrd%rY1)), iLinesAfter=1 )

  end if

end function grid_CompletelyCover

!!***
!--------------------------------------------------------------------------
!!****s* grid/grid_LookupColumn
! NAME
!  grid_LookupColumn - Finds the column position of the value rXval
!     in the grid
!
! SYNOPSIS
!  Finds the column position of the value rXval in the grid and returns:
!   iBefore =  column before the value rYval
!   iAfter  =  column after the value rYval
!   rFrac    =  fraction of the distance between iBefore and iAfter
!  If iBefore or iAfter are outside the grid, they're set to -1
!
! INPUTS
!  pGrd - Pointer to a data grid
!  rXval - Real value to be interpolated from the gridded data.
!  rTolerance - OPTIONAL - Allowable tolerance between the two grids
!
! OUTPUTS
!  iBefore - column before the value rXval
!  iAfter - column after the value rXval
!  rFrac - fraction of the distance between iBefore and iAfter
!
!  If iBefore or iAfter are outside the grid, they're set to -1
!
! NOTES
!  The grid lookup functions were designed specifically to work with the
!  Thornthwaite-Mather soil moisture retention table. The functions will
!  work with any data that are similarly arranged.
!
!  If X=0.65, for example, and the following table were queried, the subroutine
! will return: iBefore=1, iAfter=2, rFrac=0.3
!
!                 X=Max SM Capacity
!
!                0.50  1.00  1.50
!               _________________l
!          0.0 | 0.50  1.00  1.50
!  Y=APWL  0.1 | 0.45  0.90  1.40
!          0.2 | 0.40  0.80  1.30
!
! SOURCE

subroutine grid_LookupColumn(pGrd,rXval,iBefore,iAfter,rFrac)

  ! [ ARGUMENTS ]
  type ( GENERAL_GRID_T ),pointer :: pGrd
  real (c_float),intent(in) :: rXval
  integer (c_int),intent(out) :: iBefore,iAfter
  real (c_float),intent(out) :: rFrac
  ! [ LOCALS ]
  real (c_float) :: rColPosition

  rColPosition = (pGrd%iNX - 1) * (rXval - pGrd%rX0) / (pGrd%rX1 - pGrd%rX0)
  rFrac = rZERO
  iBefore = floor(rColPosition) + 1
  iAfter = iBefore + 1
  if ( iBefore > pGrd%iNX .or. iBefore < 1) then
    iBefore = -1
    iAfter = -1
  else if ( iAfter > pGrd%iNX ) then
    iAfter = -1
  else
    rFrac = mod(rColPosition, 1.0_c_float )
  end if

end subroutine grid_LookupColumn
!!***
!--------------------------------------------------------------------------
!!****s* grid/grid_LookupRow
! NAME
!  grid_LookupRow - Finds the row position of the value rYval
!     in the grid
!
! SYNOPSIS
!  Finds the column position of the value rYval in the grid and returns:
!    iBefore =  column before the value rYval
!    iAfter  =  column after the value rYval
!    rFrac    =  fraction of the distance between iBefore and iAfter
!  If iBefore or iAfter are outside the grid, they're set to -1
!
! INPUTS
!  pGrd - Pointer to a data grid
!  rYval - Real value to be interpolated from the gridded data.
!  rTolerance - OPTIONAL - Allowable tolerance between the two grids
!
! OUTPUTS
!  iBefore - column before the value rXval
!  iAfter - column after the value rXval
!  rFrac - fraction of the distance between iBefore and iAfter
!
!  If iBefore or iAfter are outside the grid, they're set to -1
!
! NOTES
!  The grid lookup functions were designed specifically to work with the
!  Thornthwaite-Mather soil moisture retention table. The functions will
!  work with any data that are similarly arranged.
!
!  If Y=0.15, for example, and the following table were queried, the subroutine
! will return: iBefore=2, iAfter=3, rFrac=0.5
!
!                 X=Max SM Capacity
!
!                0.50  1.00  1.50
!               _________________l
!          0.0 | 0.50  1.00  1.50
!  Y=APWL  0.1 | 0.45  0.90  1.40
!          0.2 | 0.40  0.80  1.30
!
! SOURCE

subroutine grid_LookupRow(pGrd,rYval,iBefore,iAfter,rFrac)

  ! [ ARGUMENTS ]
  type ( GENERAL_GRID_T ),pointer :: pGrd
  real (c_float),intent(in) :: rYval
  integer (c_int),intent(out) :: iBefore,iAfter
  real (c_float),intent(out) :: rFrac
  ! [ LOCALS ]
  real (c_float) :: rColPosition

  rColPosition = (pGrd%iNY - 1) * (pGrd%rY1 - rYval) / (pGrd%rY1 - pGrd%rY0)
  rFrac = rZERO
  iBefore = floor(rColPosition) + 1
  iAfter = iBefore + 1
  if ( iBefore > pGrd%iNY ) then
    iBefore = -1
    iAfter = -1
  else if ( iBefore < 1) then
    iBefore = 1
    iAfter = 1
  else if ( iAfter > pGrd%iNY ) then
    iAfter = iBefore
  else
    rFrac = mod(rColPosition, 1.0_c_float )
  end if

end subroutine grid_LookupRow

!------------------------------------------------------------------------------

!> Call PROJ4 to transform coordinates.
!! @details This subroutine calls a Fortran wrapper to the C library
!! PROJ4. A set of input coordinates is transformed to the base SWB
!! coordinate system.
!!
!! The idea is to first create a 2D array of the X and Y coordinates in
!! the projection system of the input data set. These coordinate values are
!! then fed to PROJ4, which modifies the values and returns the coordinate
!! values in the projection system of the base grid. Thereafter, mapping
!! source to target is a matter of finding the value of the cell closest to
!! a SWB grid coordinate pair.
!! @param[inout] pGrd
!! @param[in] sFromPROJ4
!! @param[in] sToPROJ4

subroutine grid_Transform(pGrd, sFromPROJ4, sToPROJ4 )

  type ( GENERAL_GRID_T ),pointer :: pGrd
  character (len=*) :: sFromPROJ4, sToPROJ4
  character (kind=C_CHAR, len=len_trim(sFromPROJ4)) :: csFromPROJ4
  character (kind=C_CHAR, len=len_trim(sToPROJ4)) :: csToPROJ4

  ! [ LOCALS ]
  integer (c_int) :: iRetVal
  integer (c_int) :: i
  logical (c_bool), dimension(pGrd%iNY, pGrd%iNX) :: lMask

  !> calculate X and Y for the untransformed data
  call grid_PopulateXY(pGrd)

  csFromPROJ4 = trim(sFromPROJ4)
  csToPROJ4 = trim(sToPROJ4)

  !> PROJ4 expects unprojected coordinates (i.e. lat lon) to be provided
  !> in RADIANS. Therefore, we convert to radians prior to the call...

  if (      ( csFromPROJ4 .containssimilar. "latlon" )            &
       .or. ( csFromPROJ4 .containssimilar. "latlong" )           &
       .or. ( csFromPROJ4 .containssimilar. "lonlat" )            &
       .or. ( csFromPROJ4 .containssimilar. "longlat" ) ) then

    pGrd%rX = pGrd%rX * DEGREES_TO_RADIANS
    pGrd%rY = pGrd%rY * DEGREES_TO_RADIANS

  endif

  iRetVal = pj_init_and_transform(csFromPROJ4//C_NULL_CHAR, &
                                  csToPROJ4//C_NULL_CHAR,   &
                                  __SRCNAME__//C_NULL_CHAR,    &
                                  __LINE__,                 &
                                  int(pGrd%iNumGridCells, c_long), pGrd%rX, pGrd%rY)

  call grid_CheckForPROJ4Error(iRetVal, sFromPROJ4, sToPROJ4)


  !> If the coordinates have been converted TO latlon, convert back to degrees

  if( index(string=csToPROJ4, substring="latlon") > 0 &
      .or. index(string=csToPROJ4, substring="lonlat") > 0 ) then

    pGrd%rX = pGrd%rX * RADIANS_TO_DEGREES
    pGrd%rY = pGrd%rY * RADIANS_TO_DEGREES

  endif

  ! now update the grid boundaries based on the transformed coordinate values
  pGrd%rGridCellSize = ( maxval(pGrd%rX) - minval(pGrd%rX) ) &
             / real(pGrd%iNX - 1, c_double)

  pGrd%rX0 = minval(pGrd%rX) - pGrd%rGridCellSize / 2_c_float
  pGrd%rX1 = maxval(pGrd%rX) + pGrd%rGridCellSize / 2_c_float
  pGrd%rY0 = minval(pGrd%rY) - pGrd%rGridCellSize / 2_c_float
  pGrd%rY1 = maxval(pGrd%rY) + pGrd%rGridCellSize / 2_c_float

  ! finally, change the projection string to reflect the new coordinate system
  pGrd%sPROJ4_string = trim(sToPROJ4)

  ! at this point, what we have is the same old values that were read into the
  ! input grid, but a new set of coordinate and boundary definitions that reference
  ! the SWB grid projection.

end subroutine grid_Transform

!--------------------------------------------------------------------------

subroutine grid_checkIntegerGridValues(pGrd, sFilename)

  type ( GENERAL_GRID_T ),pointer :: pGrd
  character (len=*), intent(in)   :: sFilename

  ! [ LOCALS ]
  integer (c_int) :: iRunningSum
  integer (c_int) :: iIndex
  integer (c_int) :: iCount
  integer (c_int) :: iRecord
  character (len=10)   :: sBuf0
  character (len=14)   :: sBuf1
  character (len=10)   :: sBuf2
  character (len=40)   :: sBuf3

  iRunningSum = 0
  iRecord = 0

!  call LOGS%set_echo(FALSE)
!  call LOGS%write("### Summary of integer grid data values for file "//dquote(sFilename)//" ###", &
!     iLogLevel=LOG_DEBUG, iLinesBefore=1, iLinesAfter=1 )

!   call LOGS%write("number     | count          | value     ", iLogLevel=LOG_DEBUG )
!   call LOGS%write("---------- | -------------- | ----------", iLogLevel=LOG_DEBUG )
!   do iIndex=0,maxval(pGrd%iData)
!     iCount=COUNT( pGrd%iData==iIndex )
!     if ( iCount > 0 ) then
!       iRecord = iRecord + 1
!       iRunningSum = iRunningSum + iCount
!       write (sBuf0, fmt="(i10)") iRecord
!       write (sBuf1, fmt="(i14)") iCount
!       write (sBuf2, fmt="(i10)")  iIndex
!       write (sBuf3, fmt="(a10,' | ', a14,' | ',a10)") adjustl(sBuf0), adjustl(sBuf1), adjustl(sBuf2)
!       call LOGS%write( sBuf3, iLogLevel=LOG_DEBUG )
!     end if
!   end do

!   call LOGS%write("   Total number of grid cells with value NODATA: " &
!     //asCharacter( COUNT(pGrd%iData == pGrd%iNoDataValue ) ), iLinesBefore=1, iLogLevel=LOG_ALL )

!   call LOGS%write("   Total number of grid cells: "//asCharacter( size(pGrd%iData) ), iLogLevel=LOG_ALL )

!   call LOGS%write("   Total number of grid cells with value >= 0: "//asCharacter(iRunningSum), iLogLevel=LOG_ALL )


!   if (size(pGrd%iData) /= iRunningSum) then
!     call LOGS%write(repeat("*",80), iLogLevel=LOG_ALL)
!     call LOGS%write("Possible illegal or missing values in integer grid file: "//trim(sFileName), iLogLevel=LOG_ALL)
!     call LOGS%write(repeat("*",80), iLogLevel=LOG_ALL)
!   endif

end subroutine grid_checkIntegerGridValues

!--------------------------------------------------------------------------

subroutine grid_CheckForPROJ4Error(iRetVal, sFromPROJ4, sToPROJ4)

  ! [ ARGUMENTS ]
  integer (c_int) :: iRetVal
  character (len=*) :: sFromPROJ4
  character (len=*) :: sToPROJ4

  ! [ LOCALS ]
  integer (c_int) :: i
  logical (c_bool) :: lFound
  character (len=256) :: sErrorMessage

  sErrorMessage = ""

  if (iRetVal /= 0) then

    write(sErrorMessage,fmt="(a,a,a,a,a)") &
      "There was an error transforming a grid from this projection:~", &
      dquote(sFromPROJ4), "    to:~", &
      dquote(sToPROJ4)

    do i=1,49
      if(iRetVal == tErrorMessage(i)%iErrorNumber) then
          sErrorMessage = sErrorMessage &
          //"~ PROJ4 Error number "// asCharacter(iRetVal)//" reported.~" &
          //"   ==> error description: "//trim(tErrorMessage(i)%cErrorMessageText)
          lFound = TRUE
        exit
      endif
    enddo

    call assert(FALSE, trim(sErrorMessage), __SRCNAME__, __LINE__)

  endif

end subroutine grid_CheckForPROJ4Error

!!***
!--------------------------------------------------------------------------
!!****f* grid/grid_Interpolate
! NAME
!  grid_Interpolate - Returns an interpolated table value given a row
!     and column position
!

! SYNOPSIS
!  Returns an interpolated table value given a row and column position
!
! INPUTS
!  pGrd - Pointer to a data grid
!  rXval - X value for which we want to interpolate table data.
!  rYval - Y value for which we want to interpolate table data.
!
! OUTPUTS
!   rValue - Value interpolated from input grid.
!
! NOTES
!  The grid lookup functions were designed specifically to work with the
!  Thornthwaite-Mather soil moisture retention table. The functions will
!  work with any data that are similarly arranged.
!
! SOURCE

function grid_Interpolate(pGrd,rXval,rYval) result ( rValue )
  !! Interpolates values from the grid 'grd' for the row position 'yval' and
  !! the column position 'xval'. Assumes that the row and column spacing
  !! are constant. Applicable only to DATATYPE_REAL grids.
  ! [ ARGUMENTS ]
  type ( GENERAL_GRID_T ),pointer :: pGrd
  real (c_float),intent(in) :: rXval,rYval
  ! [ RETURN VALUE ]
  real (c_float) :: rValue
  ! [ LOCALS ]
  integer (c_int) :: ib,jb,ia,ja
  real (c_float) :: ylocal,u,v

  call grid_LookupColumn(pGrd,rXval,ib,ia,u)

  call assert (ib>0 .and. ia>0 .and. ib <= ubound(pGrd%rData,1) &
     .and. ia <= ubound(pGrd%rData,1), &
    "Internal programming error: illegal bounds caught~requested column value " &
    //trim( asCharacter(rXval, fmt_string="F0.3")) &
    //" out of range", __SRCNAME__, __LINE__)

  ! In some cases, when things really dry out, the y value
  ! goes out of range - enforce bounds.
  if ( rYval < pGrd%rY0 ) then
    ylocal = pGrd%rY0
  else if ( rYval > pGrd%rY1 ) then
    ylocal = pGrd%rY1
  else
    ylocal = rYval
  end if

  call grid_LookupRow(pGrd=pGrd, &
                      rYval=ylocal, &
                      iBefore=jb, &
                      iAfter=ja, &
                      rFrac=v)

  call assert (jb>0 .and. ja>0 .and. jb <= ubound(pGrd%rData,2) &
     .and. ja <= ubound(pGrd%rData,2), &
    "Internal programming error: illegal bounds caught~requested row value " &
    //trim( asCharacter(rXval, fmt_string="F0.3")) &
    //" out of range", __SRCNAME__, __LINE__)

  rValue = ( 1.0_c_float -u) * ( 1.0_c_float -v) * pGrd%rData(ib,jb)   + &
              u  * ( 1.0_c_float -v) * pGrd%rData(ib,ja)   + &
           ( 1.0_c_float -u) *       v  * pGrd%rData(ia,jb)   + &
              u  *       v  * pGrd%rData(ia,ja)

end function grid_Interpolate
!!***
!--------------------------------------------------------------------------
!!****f* grid/grid_SearchColumn
! NAME
!  grid_SearchColumn - Returns the value of table Y value given the X value
!     and the table Z value
!
! SYNOPSIS
!  Searches in the y-direction, given the x value and the table value
!  z, and returns the value of y. Assumes that the row and
!  column spacing are constant. The search begins at the _top_l of the
!  table (y=pGrd%rY1). Applicable only to DATATYPE_REAL grids.
!  Parameter 'rNoData' is the missing value code for the grid.

! INPUTS
!  pGrd - Pointer to a data grid
!  rXval - X value corresponding to one or more rows of the table.
!  rZval - Table value to scan for.
!  rNoData - Real value assigned to "No Data" cells.
!
! OUTPUTS
!  rYval - Returned value associated with the input x and z values.
!
! NOTES
!  The grid lookup functions were designed specifically to work with the
!  Thornthwaite-Mather soil moisture retention table. The functions will
!  work with any data that are similarly arranged.
!
!  If X=1.00 and Z=0.90, for example, and the following table were queried,
!  the subroutine will return: rValue=0.1
!
!                 X=Max SM Capacity
!
!                 0.50  1.00  1.50
!                _________________l
!         0.0 | 0.50  1.00  1.50
!  Y=APWL 0.1 | 0.45  0.90  1.40
!         0.2 | 0.40  0.80  1.30
!
! SOURCE

function grid_SearchColumn(pGrd,rXval,rZval,rNoData) result ( rValue )
  !! Searches in the y-direction, given the x value 'xval', for the value
  !! 'zval', and returns the value of y in 'ry'. Assumes that the row and
  !! column spacing are constant. The search begins at the _top_l of the
  !! table (y=pGrd%rY1). Applicable only to DATATYPE_REAL grids.
  !! Parameter 'rmv' is the missing value code for the grid.
  ! [ ARGUMENTS ]
  type ( GENERAL_GRID_T ),pointer :: pGrd
  real (c_float),intent(in) :: rXval,rZval,rNoData
  ! [ RETURN VALUE ]
  real (c_float) :: rValue
  ! [ LOCALS ]
  integer (c_int) :: ib,ia,istat,iRow
  real (c_float) :: u,v,frac,rprev
  real (c_float),dimension(:),allocatable :: rCol
  character (len=128) :: buf

  ! allocate space for all ROWS of data that come from a single COLUMN
  allocate ( rCol(pGrd%iNY), stat=istat )
  call assert( LOGICAL(istat==0,c_bool), &
     "Couldn't allocate temporary array" )

  call grid_LookupColumn(pGrd=pGrd, &
                         rXval=rXval, &
                         iBefore=ib, &
                         iAfter=ia, &
                         rFrac=u)

#ifdef DEBUG_PRINT
  write(UNIT=LU_LOG,FMT=*)'lookup ',rXval,ib,ia
#endif

  call assert (ib>0 .and. ia>0 .and. ib <= ubound(pGrd%rData,1) &
     .and. ia <= ubound(pGrd%rData,1), &
    "Internal programming error: requested X value " &
    //trim( asCharacter(rXval, fmt_string="F0.3")) &
    //" out of range", __SRCNAME__, __LINE__)

  call assert(ubound(rCol,1) == ubound(pGrd%rData,2), &
    "Internal programming error: upper bound of rCol /= upper " &
    //"bound of first array element of pGrd%rData~" &
    //"ubound(rCol)="//trim( asCharacter(ubound(rCol,1))) &
    //"~ubound(pGrd%rData)="//trim( asCharacter(ubound(pGrd%rData,1))), &
    __SRCNAME__, __LINE__)

  ! interpolate the column of values based on the columns of values
  ! that bracket the real value rXval
!  rCol = u*pGrd%rData(:,ia) + ( 1.0_c_float -u)*pGrd%rData(:,ib)
  rCol = u * pGrd%rData(ia,:) + ( 1.0_c_float -u)*pGrd%rData(ib,:)
  ! Fix missing values

  do iRow=1,pGrd%iNY
    if ( pGrd%rData(ia, iRow) == rNoData .and. pGrd%rData(ib, iRow) == rNoData ) then
      rCol(iRow) = rNoData
    else if ( pGrd%rData(ib,iRow) == rNoData .and. u>0.9_c_float ) then
      rCol(iRow) = pGrd%rData(ia,iRow)
    else if ( pGrd%rData(ia,iRow) == rNoData .and. u<0.1_c_float ) then
      rCol(iRow) = pGrd%rData(ib,iRow)
    else if ( pGrd%rData(ia,iRow) == rNoData .or. pGrd%rData(ib,iRow) == rNoData ) then
      rCol(iRow) = rNoData
    end if
  end do

  ! Search for the specified value, assuming that the data vary monotonically
  ! Skip MVs
  rprev = rNoData
  rValue = rNoData
  do iRow=1,pGrd%iNY
    if ( rCol(iRow) == rNoData ) then
      if ( rprev == rNoData ) then
        ! keep skipping missing values...
        continue
      else
        ! end of the line -- we didn't find the value, so return the limit
        v = real(iRow-1) / real(pGrd%iNY-1)
        rValue = ( 1.0_c_float -v)*pGrd%rY0 + v*pGrd%rY1
        exit
      endif
    else
      if ( rprev == rNoData ) then
        rprev = rCol(iRow)
      else
        if ( sign( 1.0_c_float ,rCol(iRow)-rZval) /= sign( 1.0_c_float ,rprev-rZval) ) then
          ! Found it!
          frac = (rZval-rCol(iRow-1)) / (rCol(iRow)-rCol(iRow-1))
          ! Note: it's 'iRow-2' in the next expr to account for one-based indexing
          v = ( real(iRow-2)+frac ) / real(pGrd%iNY-1)
          rValue = v*pGrd%rY0 + ( 1.0_c_float -v)*pGrd%rY1
          exit
        else if ( rZval < rprev .and. rCol(iRow) > rprev ) then
          ! does not exist, choose the limit
          v = real(iRow-2) / real(pGrd%iNY-1)
          rValue = v*pGrd%rY0 + ( 1.0_c_float -v)*pGrd%rY1
          exit
        else if ( rZval > rprev .and. rCol(iRow) < rprev ) then
          v = real(iRow-2) / real(pGrd%iNY-1)
          rValue = v*pGrd%rY0 + ( 1.0_c_float -v)*pGrd%rY1
          exit
        else if ( iRow == pGrd%iNY ) then
          v =  1.0_c_float
          rValue = v*pGrd%rY0 + ( 1.0_c_float -v)*pGrd%rY1
          exit
        end if
      end if
    end if
  end do

  deallocate ( rCol )

end function grid_SearchColumn
!!***
!--------------------------------------------------------------------------
!!****f* grid/grid_LookupReal
! NAME
!  grid_LookupReal - Returns the grid cell value nearest to that for a
!     given a row and column position
!
! SYNOPSIS
!   Returns the grid cell value nearest to that for a given a row and
!   column position.  No interpolation is performed.
!
! INPUTS
!  pGrd - Pointer to a data grid
!  rXval - X value for which we want to interpolate table data.
!  rYval - Y value for which we want to interpolate table data.
!
! OUTPUTS
!   rValue - Value of the grid cell nearest the given row, column combination.
!
! NOTES
!  The grid lookup functions were designed specifically to work with the
!  Thornthwaite-Mather soil moisture retention table. The functions will
!  work with any data that are similarly arranged.
!
! SOURCE

function grid_LookupReal(pGrd,rXval,rYval) result(rValue)
  !! Returns the grid value for the cell containing (rXval,rYval)
  type ( GENERAL_GRID_T ),pointer :: pGrd
  real (c_float),intent(in) :: rXval,rYval
  real (c_float) :: rValue
  integer (c_int) :: iCol,iRow

  iRow = int( pGrd%iNY * (pGrd%rY0 - rYval) / (pGrd%rY0 - pGrd%rY1) ) + 1
  if ( iRow > pGrd%iNY ) iRow = pGrd%iNY
  iCol = int( pGrd%iNX * (rXval - pGrd%rX0) / (pGrd%rX1 - pGrd%rX0) ) + 1
  if ( iCol > pGrd%iNX ) iCol = pGrd%iNX
  rValue = pGrd%rData(iCol,iRow)

end function grid_LookupReal
!!***

!----------------------------------------------------------------------

function grid_GetGridColNum(pGrd,rX)  result(iColumnNumber)

  type ( GENERAL_GRID_T ),pointer :: pGrd
  real (c_double) :: rX
  integer (c_int) :: iColumnNumber

 ! print *, "rX: ",rX
 ! print *, pGrd%rX0, pGrd%rX1, pGrd%iNX

  !! this only works if the data are in the originally supplied projection. If the coordinates have been
  !! transformed, there is no guarantee that the assumption used in this calculation will hold.
  iColumnNumber = NINT(real(pGrd%iNX, c_double) &
               * ( rX - pGrd%rX0 ) / (pGrd%rX1 - pGrd%rX0) + 0.5_c_double, c_int)

!               * ( rX - pGrd%rX0 ) / (pGrd%rX1 - pGrd%rX0) + 0.5_c_double, c_int)

 ! print *, "iColumnNumber = ", iColumnNumber
 ! print *, "calc: ",  real(pGrd%iNX, c_double) &
 !              * ( rX - pGrd%rX0 ) / (pGrd%rX1 - pGrd%rX0) + 0.5_c_double
 ! print *, "numerator: ", real(pGrd%iNX, c_double) * ( rX - pGrd%rX0 )
 ! print *, "numerator (LHS): ", real(pGrd%iNX, c_double)
 ! print *, "numerator (RHS): ", rX, pGrd%rX0, ( rX - pGrd%rX0 )
 ! print *, "denominator: ", (pGrd%rX1 - pGrd%rX0)
 !
!   if ( iColumnNumber < 1 .or. iColumnNumber > pGrd%iNX ) then
!     call grid_DumpGridExtent(pGrd)
!     write(*, fmt="(a)") "was attempting to find column associated with X: "//trim(asCharacter(rX))
!     call assert(FALSE,  "INTERNAL PROGRAMMING ERROR: Column number out of bounds (value: " &
!      //trim( asCharacter(iColumnNumber))//")", &
!      __SRCNAME__, __LINE__)
!   endif

end function grid_GetGridColNum

!----------------------------------------------------------------------

function grid_GetGridRowNum(pGrd,rY)  result(iRowNumber)

  type ( GENERAL_GRID_T ),pointer :: pGrd
  real (c_double) :: rY
  integer (c_int) :: iRowNumber

  iRowNumber = pGrd%iNY - NINT(real(pGrd%iNY, c_double) &
               * ( rY - pGrd%rY0 ) / (pGrd%rY1 - pGrd%rY0) - 0.5_c_double, c_int)

!   if ( iRowNumber < 1 .or. iRowNumber > pGrd%iNY ) then
!     call grid_DumpGridExtent(pGrd)
!     write(*, fmt="(a)") "was attempting to find row associated with Y: "//trim(asCharacter(rY))
!     call assert(FALSE,  "INTERNAL PROGRAMMING ERROR: Row number out of bounds (value: " &
!      //trim( asCharacter(iRowNumber))//")", &
!      __SRCNAME__, __LINE__)
!   endif

end function grid_GetGridRowNum

!----------------------------------------------------------------------

function grid_GetGridColRowNum(pGrd, rX, rY)    result(iColRow)

  type ( GENERAL_GRID_T ),pointer :: pGrd
  real (c_double) :: rX, rY
  integer (c_int), dimension(2) :: iColRow

  ! [ LOCALS ]
  real (c_float) :: rDist, rMinDistance, rDist2

  integer (c_int), save :: iLastColNum, iLastRowNum
  integer (c_int) :: iStartingColNum, iStartingRowNum
  integer (c_int) :: iRowBoundLower, iRowBoundUpper
  integer (c_int) :: iColBoundLower, iColBoundUpper
  integer (c_int) :: iCol, iRow
  integer (c_int) :: iCandidateRow, iCandidateCol
  integer (c_int) :: iStat
  logical (c_bool) :: lChanged

  ! this will get us close to where we need to be; however, since the
  ! transformed grid may contain significant nonlinearity between
  ! adjacent grid coordinates, we need to do a more thorough search
  ! in the neighborhood of these points

  iStartingColNum = grid_GetGridColNum(pGrd,rX)
  iStartingRowNum = grid_GetGridRowNum(pGrd,rY)

  if (iStartingColNum > 0 .and. iStartingColNum <= pGrd%iNX &
     .and. iStartingRowNum > 0 .and. iStartingRowNum <= pGrd%iNY) then
    rDist = hypot(pGrd%rX(iStartingColNum,iStartingRowNum) - rX, &
                pGrd%rY(iStartingColNum,iStartingRowNum) - rY)

    ! need to ensure that the leftover column and row numbers from
    ! perhaps an entirely different grid are not used as indices
    if (iLastColNum > 0 .and. iLastColNum <= pGrd%iNX &
       .and. iLastRowNum > 0 .and. iLastRowNum <= pGrd%iNY) then
      rDist2 = hypot(pGrd%rX(iLastColNum,iLastRowNum) - rX, &
                pGrd%rY(iLastColNum,iLastRowNum) - rY)
    else
      rDist2 = rBIGVAL
    endif

    if (rDist > rDist2) then
      iCandidateRow = iLastRowNum
      iCandidateCol = iLastColNum
    else
      iCandidateRow = iStartingRowNum
      iCandidateCol = iStartingColNum
    endif

    rMinDistance = rBIGVAL

    do

      !> need to ensure that whatever bound is calculated
      !> is within the declared array bounds or we get a segfault
      iRowBoundLower = min(max( 1, iCandidateRow - 1), pGrd%iNY)
      iRowBoundUpper = max(min( pGrd%iNY, iCandidateRow + 1), 1)

      iColBoundLower = min(max( 1, iCandidateCol - 1), pGrd%iNX)
      iColBoundUpper = max(min( pGrd%iNX, iCandidateCol + 1), 1)

      lChanged = FALSE

      do iRow=iRowBoundLower,iRowBoundUpper
        do iCol=iColBoundLower,iColBoundUpper

          rDist = hypot(pGrd%rX(iCol,iRow) - rX, pGrd%rY(iCol,iRow) - rY)

          if (rDist < rMinDistance ) then

            rMinDistance = rDist
            iCandidateCol = iCol
            iCandidateRow = iRow
            lChanged = TRUE

          endif

        enddo
      enddo

      if (.not. lChanged ) exit

    enddo

    iLastColNum = iCandidateCol
    iLastRowNum = iCandidateRow

    iColRow(COLUMN) = iCandidateCol
    iColRow(ROW) = iCandidateRow

  else

    iColRow(COLUMN) = iStartingColNum
    iColRow(ROW) = iStartingRowNum
  endif

end function grid_GetGridColRowNum

!----------------------------------------------------------------------
subroutine grid_set_nodata_value( pGrd, iValue, fValue )
  type ( GENERAL_GRID_T ),pointer             :: pGrd
  integer (c_int), intent(in), optional  :: iValue
  real (c_float), intent(in), optional   :: fValue

  if ( present( iValue ) )  pGrd%iNoDataValue = iValue
  if ( present( fValue ) )  pGrd%rNoDataValue = fValue
end subroutine grid_set_nodata_value
!--------------------------------------------------------------------------------------------------

function grid_GetGridX(pGrd,iColumnNumber)  result(rX)

  type ( GENERAL_GRID_T ),pointer :: pGrd
  real (c_double) :: rX
  integer (c_int) :: iColumnNumber

  rX = pGrd%rX0 + pGrd%rGridCellSize * (REAL(iColumnNumber, c_double) -  0.5_c_double )

end function grid_GetGridX

!----------------------------------------------------------------------

function grid_GetGridY(pGrd,iRowNumber)  result(rY)

  type ( GENERAL_GRID_T ),pointer :: pGrd
  real (c_double) :: rY
  integer (c_int) :: iRowNumber

  rY = pGrd%rY1 &
          - pGrd%rGridCellSize * (REAL(iRowNumber, c_double) -  0.5_c_double )

end function grid_GetGridY

!----------------------------------------------------------------------

subroutine grid_PopulateXY(pGrd)

  ! [ ARGUMENTS ]
  type ( GENERAL_GRID_T ),pointer :: pGrd         ! pointer to model grid
    ! model options, flags, and other settings

  ! [ LOCALS ]
  integer (c_int) :: iCol,iRow
  integer (c_int) :: iStat

  if ( .not. allocated(pGrd%rX) ) then

    ALLOCATE (pGrd%rX(pGrd%iNX, pGrd%iNY), STAT=iStat)
    call assert( iStat == 0, &
       "Could not allocate memory for x-coordinates within grid data structure", &
       __SRCNAME__, __LINE__)
  endif

  if ( .not. allocated(pGrd%rY) ) then
    ALLOCATE (pGrd%rY(pGrd%iNX, pGrd%iNY), STAT=iStat)
    call assert( iStat == 0, &
       "Could not allocate memory for y-coordinates within grid data structure", &
       __SRCNAME__, __LINE__)
  endif

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      pGrd%rX(iCol, iRow) = grid_GetGridX(pGrd, iCol)
      pGrd%rY(iCol, iRow) = grid_GetGridY(pGrd, iRow)
    enddo
  enddo

end subroutine grid_PopulateXY

!----------------------------------------------------------------------

subroutine grid_DumpGridExtent(pGrd)

  type ( GENERAL_GRID_T ),pointer :: pGrd

  if ( associated( pGrd) ) then

    call LOGS%write("---------------------------------------------------")
    call LOGS%write("GRID DETAILS:")
    call LOGS%write("---------------------------------------------------")
    call LOGS%write("file: "//dquote(pGrd%sFilename) )
    call LOGS%write("nx: "//trim( asCharacter(pGrd%iNX) ) )
    call LOGS%write("ny: "//trim( asCharacter(pGrd%iNY) ) )
    call LOGS%write("cellsize: "//trim(asCharacter(pGrd%rGridCellSize) ) )
    call LOGS%write("X0: "//trim(asCharacter(pGrd%rX0) ) )
    call LOGS%write("Y0: "//trim(asCharacter(pGrd%rY0) ) )
    call LOGS%write("X1: "//trim(asCharacter(pGrd%rX1) ) )
    call LOGS%write("Y1: "//trim(asCharacter(pGrd%rY1) ) )
    call LOGS%write("Type: "//trim(asCharacter(pGrd%iDataType) ) )
    call LOGS%write("PROJ4 string: "//dquote(pGrd%sPROJ4_string) )
    call LOGS%write("---------------------------------------------------")

  else

    call LOGS%write("---------------------------------------------------")
    call LOGS%write("   ** grid not allocated **                        ")
    call LOGS%write("---------------------------------------------------")

  endif


end subroutine grid_DumpGridExtent

!----------------------------------------------------------------------
function grid_GridToPoint_int(pGrdFrom, pGrdTo, iCol, iRow)  result(iValue)

  type ( GENERAL_GRID_T ), pointer :: pGrdFrom
  type ( GENERAL_GRID_T ), pointer :: pGrdTo
  integer (c_int), intent(in) :: iCol
  integer (c_int), intent(in) :: iRow
  integer (c_int)             :: iValue

  ! [ LOCALS ]
  integer (c_int), dimension(2) :: iColRow
  integer (c_int) :: iSrcCol, iSrcRow
  integer (c_int) :: iSpread

  ! must ensure that there are coordinates associated with the "to" grid...
  ! by default, these are left unpopulated during a "normal" swb run
  if( .not. allocated(pGrdTo%rX)   )  call grid_PopulateXY(pGrdTo)
  if( .not. allocated(pGrdFrom%rX) )  call grid_PopulateXY(pGrdFrom)

  iColRow = grid_GetGridColRowNum(pGrd=pGrdFrom, &
             rX=real(pGrdTo%rX(iCol, iRow), c_double), &
             rY=real(pGrdTo%rY(iCol, iRow), c_double))

  if ( iColRow(COLUMN) < 1 .or. iColRow(COLUMN) > pGrdFrom%iNX )                       &
    call die( "Illegal column number supplied: "//trim(asCharacter(iColRow(COLUMN))),  &
      __SRCNAME__, __LINE__)

  if ( iColRow(ROW) < 1 .or. iColRow(ROW) > pGrdFrom%iNY )                             &
    call die( "Illegal row number supplied: "//trim(asCharacter(iColRow(ROW))),        &
      __SRCNAME__, __LINE__)

  !> @todo check the logic here...intent is to ensure that the majority filter is
  !!       searching an area that adequately covers corresponding gridcell areas
  iSpread = max(1, nint(pGrdTo%rGridCellSize / pGrdFrom%rGridCellSize / 2.))

  iValue = grid_majorityFilter_int(pGrdFrom=pGrdFrom,  &
             iTargetCol=iColRow(COLUMN), &
             iTargetRow=iColRow(ROW), &
             iNoDataValue=pGrdFrom%iNoDataValue, &
             iSpread=iSpread)


end function grid_GridToPoint_int


!----------------------------------------------------------------------

subroutine grid_GridToGrid_int( pGrdFrom, pGrdTo, lUseMajorityFilter )

  type ( GENERAL_GRID_T ), pointer                    :: pGrdFrom            ! pointer to source grid
  type ( GENERAL_GRID_T ), pointer                    :: pGrdTo              ! pointer to destination grid
  logical (c_bool), intent(in)                   :: lUseMajorityFilter

  ! [ LOCALS ]
  integer (c_int) :: iCol, iRow
  integer (c_int), dimension(2) :: iColRow
  integer (c_int) :: iSrcCol, iSrcRow
  integer (c_int) :: iSpread
  real (c_float)  :: fGridcellRatio

  ! must ensure that there are coordinates associated with the "to" grid...
  ! by default, these are left unpopulated during a "normal" swb run
  if(.not. allocated(pGrdTo%rX) )  call grid_PopulateXY(pGrdTo)
  if(.not. allocated(pGrdFrom%rX) )  call grid_PopulateXY(pGrdFrom)

  fGridcellRatio = pGrdTo%rGridCellSize / pGrdFrom%rGridCellSize

  ! Allow USER to trigger whether the majority filter is employed or not
  if ( lUseMajorityFilter ) then

    call LOGS%write( "** Majority filter in use for data from grid file "//dquote(pGrdFrom%sFilename), lEcho=TRUE )

    iSpread = max( 1, nint( fGridcellRatio / 2.0_c_float ) )

    do iRow=1,pGrdTo%iNY
      do iCol=1,pGrdTo%iNX

        iColRow = grid_GetGridColRowNum(pGrd=pGrdFrom,                          &
                   rX=real(pGrdTo%rX(iCol, iRow), c_double),                    &
                   rY=real(pGrdTo%rY(iCol, iRow), c_double) )

        call assert(iColRow(COLUMN) > 0 .and. iColRow(COLUMN) <= pGrdFrom%iNX,    &
          "Illegal column number supplied: "//trim(asCharacter(iColRow(COLUMN))), &
          __SRCNAME__, __LINE__)

        call assert(iColRow(ROW) > 0 .and. iColRow(ROW) <= pGrdFrom%iNY,        &
          "Illegal row number supplied: "//trim(asCharacter(iColRow(ROW))),     &
          __SRCNAME__, __LINE__)

        pGrdTo%iData(iCol,iRow) = grid_majorityFilter_int( pGrdFrom=pGrdFrom,   &
             iTargetCol=iColRow(COLUMN),                                        &
             iTargetRow=iColRow(ROW),                                           &
             iNoDataValue=pGrdFrom%iNoDataValue,                                &
             iSpread=iSpread)

      enddo
    enddo

  else  ! target grid resolution similar to or more dense than source grid resolution: NEAREST NEIGHBOR [DEFAULT]

    do iRow=1,pGrdTo%iNY
      do iCol=1,pGrdTo%iNX

        iColRow = grid_GetGridColRowNum(pGrd=pGrdFrom,                          &
                   rX=real(pGrdTo%rX(iCol, iRow), c_double),                    &
                   rY=real(pGrdTo%rY(iCol, iRow), c_double))

        call assert(iColRow(COLUMN) > 0 .and. iColRow(COLUMN) <= pGrdFrom%iNX,    &
          "Illegal column number supplied: "//trim(asCharacter(iColRow(COLUMN))), &
          __SRCNAME__, __LINE__)

        call assert(iColRow(ROW) > 0 .and. iColRow(ROW) <= pGrdFrom%iNY,        &
          "Illegal row number supplied: "//trim(asCharacter(iColRow(ROW))),     &
          __SRCNAME__, __LINE__)

        pGrdTo%iData(iCol,iRow) = pGrdFrom%iData( iColRow(COLUMN), iColRow(ROW) )

      enddo
    enddo

  endif

end subroutine grid_GridToGrid_int

!----------------------------------------------------------------------

subroutine grid_GridToGrid_sgl(pGrdFrom,  pGrdTo )

  ! [ ARGUMENTS ]
  type ( GENERAL_GRID_T ),pointer :: pGrdFrom   ! pointer to source grid
  type ( GENERAL_GRID_T ),pointer :: pGrdTo     ! pointer to destination grid

  ! [ LOCALS ]
  integer (c_int), dimension(2) :: iColRow
  integer (c_int) :: iCol, iRow
  integer (c_int) :: iSrcCol, iSrcRow
  real (c_float), dimension(3,3) :: rKernel

  rKernel = 1.
  rKernel(2,2) = 8.

  ! must ensure that there are coordinates associated with the "to" grid...
  ! by default, these are left unpopulated during a "normal" swb run
  if(.not. allocated(pGrdTo%rX) )  call grid_PopulateXY(pGrdTo)
  if(.not. allocated(pGrdFrom%rX) )  call grid_PopulateXY(pGrdFrom)

!!!   *$OMP PARALLEL DO ORDERED PRIVATE(iRow, iCol, iColRow)

  do iRow=1,pGrdTo%iNY
    do iCol=1,pGrdTo%iNX

      iColRow = grid_GetGridColRowNum(pGrd=pGrdFrom,                            &
                 rX=real(pGrdTo%rX(iCol, iRow), c_double),                      &
                 rY=real(pGrdTo%rY(iCol, iRow), c_double))

      if ( iColRow(COLUMN) < 1 .or. iColRow(COLUMN) > pGrdFrom%iNX) cycle
      if ( iColRow(ROW) < 1 .or. iColRow(ROW) > pGrdFrom%iNY) cycle

      pGrdTo%rData(iCol,iRow) = pGrdFrom%rData( iColRow(COLUMN), iColRow(ROW) )

    enddo
  enddo

!!!   *$OMP END PARALLEL DO

end subroutine grid_GridToGrid_sgl

!----------------------------------------------------------------------

function grid_MajorityFilter_int(pGrdFrom, iTargetCol, &
   iTargetRow, iNoDataValue, iSpread)  result(iRetVal)

  type ( GENERAL_GRID_T ),pointer :: pGrdFrom   ! pointer to source grid
  integer (c_int) :: iTargetCol          ! column number of target cell
  integer (c_int) :: iTargetRow          ! row number of target cell
  integer (c_int) :: iNoDataValue
  integer (c_int) :: iRetVal
  integer (c_int) :: iSpread             ! integer representing how many
                                              ! cells should be searched

  ! [ LOCALS ]
  integer (c_int), dimension(625) :: iValue
  integer (c_int), dimension(625) :: iCount
  logical (c_bool) :: lMatch
  integer (c_int) :: iRow
  integer (c_int) :: iCol
  integer (c_int) :: i, iSize, iLast, iIndex, iCellNum

  iValue = 0
  iCount = 0
  iLast = 0
  iCellNum = 0

  ! need to set a reasonable upper bound on the spread to consider
  iSpread = min(iSpread, 12)

   do iRow=max(1,iTargetRow - iSpread), min(pGrdFrom%iNY, iTargetRow + iSpread)
     do iCol=max(1,iTargetCol - iSpread), min(pGrdFrom%iNX, iTargetCol + iSpread)
!
       iCellNum = iCellNum + 1
!
       lMatch = FALSE
!
       do i=1, iLast
         if (iValue(i) == pGrdFrom%iData(iCol, iRow) ) then
           iCount(i) = iCount(i) + 1
           lMatch = TRUE
           exit
         endif
       enddo

       ! this integer value has not been found previously; add the value to the next position
       ! and increment counter
       if (.not. lMatch) then
         iLast = iLast + 1
         iValue(iLast) = pGrdFrom%iData(iCol, iRow)
         iCount(iLast) = iCount(iLast) + 1
         lMatch = TRUE
       endif

     enddo
   enddo

   iIndex = MAXLOC(iCount, dim=1)
   iRetVal = iValue(iIndex)

end function grid_majorityFilter_int

function grid_Convolve_sgl( pGrdFrom, iTargetCol, &
   iTargetRow, rKernel, rNoDataValue)  result(rRetVal)

  type ( GENERAL_GRID_T ),pointer :: pGrdFrom   ! pointer to source grid
  integer (c_int) :: iNX
  integer (c_int) :: iNY
  integer (c_int) :: iTargetCol          ! column number of target cell
  integer (c_int) :: iTargetRow          ! row number of target cell
  real (c_float), dimension(:,:) :: rKernel
  real (c_float), optional :: rNoDataValue
  real (c_float) :: rRetVal

  ! [ LOCALS ]
  integer (c_int) :: iRowMin, iRowMax
  integer (c_int) :: iColMin, iColMax
  integer (c_int) :: iKernelSize         ! i.e. 3, 5, 7, 9, 11, etc.
  integer (c_int) :: iIncValue
  integer (c_int) :: iCol, iRow, iRownum, iColnum
  real (c_float) :: rKernelSum

  rKernelSum = rZERO
  rRetVal = rZERO

  iKernelSize = size(rKernel, dim=1)
  iIncValue = (iKernelSize - 1) / 2

  iNX = ubound(pGrdFrom%rData,1)
  iNY = ubound(pGrdFrom%rData,2)

  iRowMin = max(1,iTargetRow - iIncValue)
  iRowMax = min(iNY, iTargetRow + iIncValue)

  iColMin = max(1,iTargetCol - iIncValue)
  iColMax = min(iNY, iTargetCol + iIncValue)

  if( (iRowMax - iRowMin + 1)  /= iKernelsize &
    .or. (iColMax - iColMin + 1)  /= iKernelsize ) then

    ! This is a simple, but less than desirable way to treat cells that
    ! fall near the edge of the source grid. Ideally, the source grid will
    ! be greater than the target grid by a wide enough margin that this
    ! code will rarely be used.
    rRetVal = pGrdFrom%rData(iTargetCol, iTargetRow)

  else

    do iCol=0,iKernelSize-1
      do iRow=0,iKernelSize-1

        iRownum = iRow + iRowMin
        iColnum = iCol + iColmin

        if (iRownum > iNY .or. iColnum > iNX) then
          cycle   ! our calculated row or column number is outside of the
                  ! bounds of the rValues array
        else
          rRetVal = rRetVal + pGrdFrom%rData(iColnum,iRownum) * rKernel(iCol+1, iRow+1)
          rKernelSum = rKernelSum + rKernel(iCol+1, iRow+1)
        endif
      enddo
    enddo

    if (rKernelSum > 0.) rRetVal = rRetVal / rKernelSum

  endif

end function grid_Convolve_sgl

end module grid
