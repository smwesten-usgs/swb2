!
! concept: for each significant gridded data input, keep track of the
!          native coordinates, the transformed base (project) coordinates,
!          and provide methods for extracting data from appropriate locations
!          as needed.
!

module data_catalog_entry

  use constants_and_conversions
  use datetime
  use exceptions
  use file_operations, only      : fully_qualified_filename
  use logfiles
  use fstring
  use fstring_list
  use grid
  use netcdf4_support
  use parameters
  use iso_c_binding
  implicit none

  private

  integer (c_int), public, parameter :: NETCDF_FILE_OPEN = 27
  integer (c_int), public, parameter :: NETCDF_FILE_CLOSED = 42

  integer (c_int), parameter, public :: FILE_TEMPLATE_CAPITALIZED_MONTHNAME = 0
  integer (c_int), parameter, public :: FILE_TEMPLATE_LOWERCASE_MONTHNAME   = 1
  integer (c_int), parameter, public :: FILE_TEMPLATE_UPPERCASE_MONTHNAME   = 2

  integer (c_int), parameter         :: CONSTANT_GRID = 0
  integer (c_int), parameter         :: STATIC_GRID = 1
  integer (c_int), parameter         :: STATIC_NETCDF_GRID = 2
  integer (c_int), public, parameter :: DYNAMIC_GRID = 3
  integer (c_int), parameter         :: DYNAMIC_NETCDF_GRID = 4
  integer (c_int), parameter         :: NO_GRID = 5
  integer (c_int), parameter         :: TABLE_LOOKUP = 6

  integer (c_int), parameter :: FILETYPE_ARC_ASCII = 0
  integer (c_int), parameter :: FILETYPE_SURFER = 1
  integer (c_int), parameter :: FILETYPE_NETCDF = 2
  integer (c_int), parameter :: FILETYPE_ASCII_TABLE = 3
  integer (c_int), parameter :: FILETYPE_NONE = 4

  type, public :: DATA_CATALOG_ENTRY_T
    character (len=:), allocatable :: sKeyword
    type (DATA_CATALOG_ENTRY_T), pointer :: previous => null()
    type (DATA_CATALOG_ENTRY_T), pointer :: next     => null()

    integer (c_int)               :: iSourceDataForm = NO_GRID        ! constant, static grid, dynamic grid
    integer (c_int)               :: iSourceDataType = DATATYPE_NA    ! real, short, integer, etc.
    integer (c_int)               :: iSourceFileType = FILETYPE_NONE  ! Arc ASCII, Surfer, NetCDF
    integer (c_int)               :: iTargetDataType = DATATYPE_NA    ! Fortran real, integer, etc.

    character (len=256)       :: sDescription           = ""
    character (len=256)       :: sSourcePROJ4_string    = ""
    character (len=256)       :: sTargetPROJ4_string    = ""
    character (len=256)       :: sSourceFileType        = ""
    character (len=512)       :: sSourceFilename        = ""
    character (len=512)       :: sFilenameTemplate      = ""
    integer (c_int)           :: iFilename_Monthname_Capitalization_Rule = FILE_TEMPLATE_CAPITALIZED_MONTHNAME
    character (len=512)       :: sOldFilename           = ""
    character (len=256)       :: sDateColumnName        = ""
    character (len=10)        :: sDefaultDateFormat     = "YYYY-MM-DD"
    character (len=256)       :: sValueColumnName       = ""

    real (c_float), allocatable    :: table_values_real(:)
    type (DATETIME_T), allocatable :: table_dates(:)
    integer (c_int)                :: table_indx 

    logical (c_bool)          :: lTableValuesHaveBeenRetrieved = FALSE

    integer (c_int)      :: iFileCount = -1
    integer (c_int)      :: iFileCountYear = -9999
    real (c_float)       :: rMinAllowedValue = -rBIGVAL     ! default condition is to impose
    real (c_float)       :: rMaxAllowedValue = rBIGVAL      ! no bounds on data
    integer (c_int)      :: iMinAllowedValue = -iBIGVAL     ! default condition is to impose
    integer (c_int)      :: iMaxAllowedValue = iBIGVAL      ! no bounds on data
    real (c_float)       :: rMissingValuesCode = -rBIGVAL
    integer (c_int)      :: iMissingValuesCode = -iBIGVAL
    character (len=2)    :: sMissingValuesOperator = "&&"
    integer (c_int)      :: iMissingValuesAction = 0

    real (c_double)      :: rUserScaleFactor = 1_c_double
    real (c_double)      :: rUserAddOffset = 0_c_double
    real (c_double)      :: rX_Coord_AddOffset = 0.0_c_double
    real (c_double)      :: rY_Coord_AddOffset = 0.0_c_double

    logical (c_bool)     :: lAllowMissingFiles = FALSE
    logical (c_bool)     :: lAllowAutomaticDataFlipping = TRUE
    logical (c_bool)     :: lFlipHorizontal = FALSE
    logical (c_bool)     :: lFlipVertical = FALSE
    logical (c_bool)     :: lUseMajorityFilter = FALSE
    logical (c_bool)     :: lRequireCompleteSpatialCoverage = TRUE

    integer (c_int)  :: iDaysToPadAtYearsEnd = 0
    integer (c_int)  :: iDaysToPadIfLeapYear = 1
    integer (c_int)  :: iStartYear = -9999
    integer (c_int)  :: iEndYear = -9999
    logical (c_bool) :: lPadReplaceWithZero = FALSE
    logical (c_bool) :: lPadValues = FALSE
    logical (c_bool) :: lIsAnnualGrid = FALSE

    ! the following are only used if data are being read from a NetCDF file
    character (len=32)       :: sVariableName_x = "x"
    character (len=32)       :: sVariableName_y = "y"
    character (len=32)       :: sVariableName_z = ""
    character (len=32)       :: sVariableName_time = "time"
    character (len=8)        :: sVariableOrder = "tyx"

    type (GRID_BOUNDS_T)     :: GRID_BOUNDS_NATIVE
    type (GRID_BOUNDS_T)     :: GRID_BOUNDS_BASE

    integer (c_int)     :: iNC_FILE_STATUS = NETCDF_FILE_CLOSED
    type (T_NETCDF4_FILE)    :: NCFILE

    integer (c_int)     :: iNC_ARCHIVE_STATUS = NETCDF_FILE_CLOSED
    type (T_NETCDF4_FILE)    :: NCFILE_ARCHIVE
    integer (c_size_t)  :: iNCFILE_RECNUM = 0

    integer (c_int)     :: iConstantValue = 0
    real (c_float)      :: rConstantValue = 0.0

    ! pGrdNative is a grid created to serve as an intermediary between
    ! the native coordinate of the data source file and the project coordinates
    ! in use by swb
    type (GENERAL_GRID_T), pointer :: pGrdNative                 => null()
    logical (c_bool)          :: lGridIsPersistent          = FALSE
    logical (c_bool)          :: lGridHasChanged            = FALSE
    logical (c_bool)          :: lPerformFullInitialization = TRUE
    logical (c_bool)          :: lCreateLocalNetCDFArchive  = FALSE

    ! pGrdBase takes the coordinate system and dimensions as defined
    ! for the overall SWB project (i.e. BASE_PROJECTION_DEFINITION )
    type (GENERAL_GRID_T), pointer :: pGrdBase => null()

  contains

    procedure  :: setkey => set_keyword_sub

    procedure  :: initialize_constant_int_data_object_sub
    procedure  :: initialize_constant_real_data_object_sub
    procedure  :: initialize_gridded_data_object_sub
    procedure  :: initialize_table_sub
    generic    :: initialize => initialize_constant_int_data_object_sub,    &
                                initialize_constant_real_data_object_sub,   &
                                initialize_gridded_data_object_sub,         &
                                initialize_table_sub

    procedure  :: initialize_netcdf => initialize_netcdf_data_object_sub

    procedure  :: set_scale    => set_scale_sub
    procedure  :: set_offset   => set_offset_sub
    procedure  :: set_X_offset => set_X_coord_offset_sub
    procedure  :: set_Y_offset => set_Y_coord_offset_sub

    procedure  :: set_majority_filter_flag => set_majority_filter_flag_sub

    procedure  :: set_minimum_allowable_value_int_sub
    procedure  :: set_minimum_allowable_value_real_sub
    generic    :: set_valid_minimum => set_minimum_allowable_value_int_sub,    &
                                       set_minimum_allowable_value_real_sub

    procedure  :: set_maximum_allowable_value_int_sub
    procedure  :: set_maximum_allowable_value_real_sub
    generic    :: set_valid_maximum => set_maximum_allowable_value_int_sub,    &
                                       set_maximum_allowable_value_real_sub

    procedure  :: set_grid_flip_horizontal => set_grid_flip_horizontal_sub
    procedure  :: set_grid_flip_vertical => set_grid_flip_vertical_sub
    procedure  :: allow_missing_files => set_allow_missing_files_flag_sub
    procedure  :: do_not_allow_netcdf_grid_data_flipping                      &
                     => set_do_not_allow_netcdf_grid_data_flipping_sub

    procedure  :: getvalues_constant => getvalues_constant_sub
    procedure  :: getvalues_gridded => getvalues_gridded_sub

    procedure  :: getvalues_dynamic_netcdf_sub
    procedure  :: getvalues_static_netcdf_sub
    generic    :: getvalues_netcdf => getvalues_dynamic_netcdf_sub,   &
                                      getvalues_static_netcdf_sub

    procedure  :: getvalues => getvalues_sub

    procedure  :: get_value_int_sub
    procedure  :: get_value_float_sub
    generic    :: getvalue => get_value_int_sub, &
                              get_value_float_sub

 !   procedure :: update => update_data_object_sub
 !   procedure :: destroy => create_data_object_sub
    procedure  :: get_filetype => get_source_filetype_fn

    procedure  :: set_filecount => set_filecount
    procedure  :: reset_filecount => reset_filecount
    procedure  :: reset_at_yearend_filecount => reset_at_yearend_filecount
    procedure  :: increment_filecount => increment_filecount

    procedure  :: set_constant_value_int
    procedure  :: set_constant_value_real
    generic    :: set_constant => set_constant_value_int,   &
                                  set_constant_value_real

    procedure  :: make_filename => make_filename_from_template
    procedure  :: set_target_PROJ4 => set_target_PROJ4_string_sub
    procedure  :: set_source_PROJ4 => set_source_PROJ4_string_sub
    procedure  :: set_variable_order => set_variable_order_sub
    procedure  :: set_complete_spatial_coverage_flag => set_complete_spatial_coverage_flag_sub
    procedure  :: dump_data_structure => dump_data_structure_sub
    procedure  :: set_make_local_archive => set_archive_local_sub
    procedure  :: put_values_to_archive => put_values_to_local_NetCDF_sub
    procedure  :: transform_native_to_base => transform_grid_to_grid_sub
    procedure  :: nullify_pointers => nullify_pointers_sub

    procedure  :: data_GridEnforceLimits_real
    procedure  :: data_GridEnforceLimits_int
    generic    :: enforce_limits => data_GridEnforceLimits_real,   &
                                    data_GridEnforceLimits_int

    procedure  :: data_GridHandleMissingData_real
    procedure  :: data_GridHandleMissingData_int
    generic    :: handle_missing_values => data_GridHandleMissingData_real,   &
                                           data_GridHandleMissingData_int

    procedure  :: calc_project_boundaries => calc_project_boundaries_sub
    procedure  :: test_for_need_to_pad_values => test_for_need_to_pad_values_fn

  end type DATA_CATALOG_ENTRY_T

  integer (c_int), parameter, public :: MISSING_VALUES_ZERO_OUT = 0
  integer (c_int), parameter, public :: MISSING_VALUES_REPLACE_WITH_MEAN = 1

  type (GENERAL_GRID_T), public, pointer :: pGrd => null()

  interface apply_scale_and_offset
    module procedure :: apply_scale_and_offset_float
    module procedure :: apply_scale_and_offset_int
  end interface apply_scale_and_offset

contains

  subroutine nullify_pointers_sub(this)

    class (DATA_CATALOG_ENTRY_T)         :: this

    if (associated(this%pGrdNative))  call grid_Destroy(this%pGrdNative)
    if (associated(this%pGrdBase))  call grid_Destroy(this%pGrdBase)
    if (associated(pGrd))  call grid_Destroy(pGrd)

    nullify(this%pGrdNative)
    nullify(this%pGrdBase)
    nullify( pGrd )

    call netcdf_nullify_data_struct( NCFILE=this%NCFILE )
    call netcdf_nullify_data_struct( NCFILE=this%NCFILE_ARCHIVE )

  end subroutine nullify_pointers_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_keyword_sub(this, sKeyword)

    class (DATA_CATALOG_ENTRY_T)         :: this
    character (len=*), intent(in)        :: sKeyword

    this%sKeyword = sKeyword

  end subroutine set_keyword_sub

!--------------------------------------------------------------------------------------------------

  subroutine get_value_int_sub(this, iCol, iRow, iValue)

    class (DATA_CATALOG_ENTRY_T), intent(in)  :: this
    integer (c_int), intent(in)          :: iCol
    integer (c_int), intent(in)          :: iRow
    integer (c_int), intent(out)         :: iValue

    if ( .not. associated(this%pGrdBase) ) &
      call die("Internal programming error--attempt to use null pointer", __SRCNAME__, __LINE__)

    if (iCol <= ubound(this%pGrdBase%iData,1) .and. iRow <= ubound(this%pGrdBase%iData,2) ) then
      iValue = this%pGrdBase%iData(iCol, iRow)
    else
      call die ("Row/column indices out of bounds: ~row: "//asCharacter(iRow)//"~ col:"//asCharacter(iCol), &
        __SRCNAME__, __LINE__ )
    endif

  end subroutine get_value_int_sub

!--------------------------------------------------------------------------------------------------

  subroutine get_value_float_sub(this, iCol, iRow, fValue)

    class (DATA_CATALOG_ENTRY_T), intent(in)  :: this
    integer (c_int), intent(in)          :: iCol
    integer (c_int), intent(in)          :: iRow
    real (c_float), intent(out)          :: fValue

    if ( .not. associated(this%pGrdBase) ) &
      call die("Internal programming error--attempt to use null pointer", __SRCNAME__, __LINE__)

    if (iCol <= ubound(this%pGrdBase%iData,1) .and. iRow <= ubound(this%pGrdBase%iData,2) ) then
      fValue = this%pGrdBase%rData(iCol, iRow)
    else
      call die ("Row/column indices out of bounds: ~row: "//asCharacter(iRow)//"~ col:"//asCharacter(iCol), &
        __SRCNAME__, __LINE__ )
    endif

  end subroutine get_value_float_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_constant_int_data_object_sub( this, &
    sDescription, &
    iConstant )

    class (DATA_CATALOG_ENTRY_T)    :: this
    character (len=*), intent(in)   :: sDescription
    integer (c_int), intent(in)     :: iConstant

    this%iConstantValue = iConstant
    this%sDescription = trim(sDescription)
    this%iSourceDataForm = CONSTANT_GRID
    this%iSourceDataType = DATATYPE_INT
    this%iTargetDataType = DATATYPE_INT
    this%iSourceFileType = FILETYPE_NONE

    call this%nullify_pointers()

    this%pGrdBase => grid_Create(iNX=BNDS%iNumCols, iNY=BNDS%iNumRows, &
      rX0=BNDS%fX_ll, rY0=BNDS%fY_ll, rGridCellSize=BNDS%fGridCellSize, iDataType=DATATYPE_INT)

    this%pGrdBase%sPROJ4_string = trim( BNDS%sPROJ4_string )
    this%pGrdBase%sFilename = "None: constant value entered from control file."

  end subroutine initialize_constant_int_data_object_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_constant_real_data_object_sub( this, &
    sDescription, &
    rConstant )

    class (DATA_CATALOG_ENTRY_T)    :: this
    character (len=*), intent(in)   :: sDescription
    real (c_float), intent(in) :: rConstant

    this%rConstantValue = rConstant
    this%sDescription = trim(sDescription)
    this%iSourceDataForm = CONSTANT_GRID
    this%iSourceDataType = DATATYPE_REAL
    this%iTargetDataType = DATATYPE_REAL
    this%iSourceFileType = FILETYPE_NONE

    call this%nullify_pointers()

    this%pGrdBase => grid_Create(iNX=BNDS%iNumCols, iNY=BNDS%iNumRows, &
      rX0=BNDS%fX_ll, rY0=BNDS%fY_ll, rGridCellSize=BNDS%fGridCellSize, iDataType=DATATYPE_REAL)

    this%pGrdBase%sPROJ4_string = trim( BNDS%sPROJ4_string )
    this%pGrdBase%sFilename = "None: constant value entered from control file."

  end subroutine initialize_constant_real_data_object_sub

!--------------------------------------------------------------------------------------------------

  subroutine initialize_table_sub( this, sDescription, sDateColumnName, sValueColumnName, sType)

    class (DATA_CATALOG_ENTRY_T)  :: this
    character (len=*), intent(in) :: sDescription
    character (len=*), intent(in) :: sDateColumnName
    character (len=*), intent(in) :: sValueColumnName
    character (len=*), intent(in) :: sType

    this%sDescription     = trim(sDescription)
    this%iSourceDataForm  = TABLE_LOOKUP
    this%iSourceDataType  = DATATYPE_REAL
    this%iTargetDataType  = DATATYPE_REAL
    this%iSourceFileType  = FILETYPE_ASCII_TABLE
    this%sDateColumnName  = sDateColumnName
    this%sValueColumnName = sValueColumnName

    call this%nullify_pointers()

    select case (asLowercase(sType))

      case ("float", "real" )

        this%iSourceDataType  = DATATYPE_REAL
        this%iTargetDataType  = DATATYPE_REAL
        this%pGrdBase => grid_Create(iNX=BNDS%iNumCols, iNY=BNDS%iNumRows, &
        rX0=BNDS%fX_ll, rY0=BNDS%fY_ll, rGridCellSize=BNDS%fGridCellSize, iDataType=DATATYPE_REAL)
      
      case ("int", "integer")

        this%iSourceDataType  = DATATYPE_INT
        this%iTargetDataType  = DATATYPE_INT
        this%pGrdBase => grid_Create(iNX=BNDS%iNumCols, iNY=BNDS%iNumRows, &
        rX0=BNDS%fX_ll, rY0=BNDS%fY_ll, rGridCellSize=BNDS%fGridCellSize, iDataType=DATATYPE_INT)
      
    end select  

    this%pGrdBase%sPROJ4_string = BNDS%sPROJ4_string
    this%pGrdBase%sFilename = "None: daily value found in table of values."

  end subroutine initialize_table_sub

!--------------------------------------------------------------------------------------------------

subroutine initialize_gridded_data_object_sub( this, &
  sDescription, &
  sFileType, &
  iDataType, &
  sFilename, &
  sPROJ4_string )

  class (DATA_CATALOG_ENTRY_T)             :: this
  character (len=*), intent(in)            :: sDescription
  character (len=*), intent(in)            :: sFileType
  character (len=*), intent(in)            :: sFilename
  integer (c_int)                     :: iDataType
  character (len=*), intent(in), optional  :: sPROJ4_string

  if (present(sPROJ4_string) ) then
    this%sSourcePROJ4_string = trim(sPROJ4_string)
  else
    this%sSourcePROJ4_string =  BNDS%sPROJ4_string
  endif

  this%sSourceFilename = fully_qualified_filename( sFilename )

  !> if either a '%' or '#' character is present in the filename
  !! treat it as a template, not as a normal filename.
  if ( scan(string=sFilename, set="%#") > 0 ) then

    this%iSourceDataForm = DYNAMIC_GRID
    this%lGridIsPersistent = TRUE
    this%sFilenameTemplate = trim( this%sSourceFilename )

  else

    this%iSourceDataForm = STATIC_GRID
    this%lGridIsPersistent = FALSE
    this%sFilenameTemplate = ""

  endif

  this%sSourceFileType = sFileType
  this%iSourceFileType = this%get_filetype()

  this%iSourceDataType = iDataType
  this%iTargetDataType = iDataType

  this%sDescription = trim(sDescription)

  call assert(this%iSourceFileType == FILETYPE_ARC_ASCII .or. &
    this%iSourceFileType == FILETYPE_SURFER, "Only Arc ASCII or " &
    //"Surfer grids are supported as static grid inputs (for now).", &
    __SRCNAME__, __LINE__)

  call assert(this%iSourceDataType == DATATYPE_INT .or. &
    this%iSourceDataType == DATATYPE_REAL, "Only integer or " &
    //"real data types are supported as static grid inputs.", &
    __SRCNAME__, __LINE__)

  call this%nullify_pointers()

  this%pGrdBase => grid_Create(iNX=BNDS%iNumCols, iNY=BNDS%iNumRows, &
    rX0=BNDS%fX_ll, rY0=BNDS%fY_ll, rGridCellSize=BNDS%fGridCellSize, iDataType=iDataType)

  this%pGrdBase%sPROJ4_string = BNDS%sPROJ4_string
  this%pGrdBase%sFilename = this%sSourceFilename

end subroutine initialize_gridded_data_object_sub

!--------------------------------------------------------------------------------------------------

subroutine initialize_netcdf_data_object_sub( this, &
   sDescription, &
   iDataType, &
   sFilename, &
   sPROJ4_string )

   class (DATA_CATALOG_ENTRY_T)               :: this
   character (len=*), intent(in)              :: sDescription
   integer (c_int), intent(in)           :: iDataType
   character (len=*), intent(in)              :: sFilename
   character (len=*), intent(in), optional    :: sPROJ4_string

  ! [ LOCALS ]
  type ( GENERAL_GRID_T ), pointer           :: pGrdBase


   if (present(sPROJ4_string) ) then
     this%sSourcePROJ4_string = trim(sPROJ4_string)
   else
     this%sSourcePROJ4_string =  BNDS%sPROJ4_string
   endif

  this%sSourceFilename = fully_qualified_filename( sFilename )

  !> if either a '%' or '#' character is present in the filename,
  !! treat it as a template, not as a normal filename.
  !! if there is a template, the implication is that there is
  !! a series of files that will be read in successively, thus "dynamic" NetCDF
!  if ( scan(string=sFilename, set="%#") > 0 ) then


!> @TODO Implement a way to read variables in via "static" NetCDF grid.
!!    In other words, a NetCDF grid having no "time" dimension or variable.


    this%iSourceDataForm   = DYNAMIC_NETCDF_GRID
    this%lGridIsPersistent = TRUE
    this%sFilenameTemplate = trim(sFilename)

!   else

!     !> intent of "static" NetCDF file is to house large non-changing
!     !! input grids (i.e. landuse, soils )
!     this%iSourceDataForm   = STATIC_NETCDF_GRID
!     this%lGridIsPersistent = FALSE
!     this%sFilenameTemplate = ""

!   endif

    call this%nullify_pointers()

    this%pGrdBase => grid_Create(iNX=BNDS%iNumCols, iNY=BNDS%iNumRows, &
     rX0=BNDS%fX_ll, rY0=BNDS%fY_ll, rGridCellSize=BNDS%fGridCellSize, iDataType=iDataType)

    this%pGrdBase%sPROJ4_string = BNDS%sPROJ4_string
    this%pGrdBase%sFilename = this%sSourceFilename

    this%sSourceFileType = "NETCDF"
    this%iSourceFileType = this%get_filetype()

    this%iTargetDataType = iDataType
    this%iNC_FILE_STATUS = NETCDF_FILE_CLOSED

end subroutine initialize_netcdf_data_object_sub

!--------------------------------------------------------------------------------------------------

  subroutine getvalues_sub( this, dt )

    class (DATA_CATALOG_ENTRY_T) :: this
    type (DATETIME_T), optional  :: dt

    if(this%iSourceDataForm == DYNAMIC_GRID ) then

      call getvalues_gridded_sub( this, dt )

    elseif ( this%iSourceDataForm == DYNAMIC_NETCDF_GRID ) then

      call getvalues_dynamic_netcdf_sub( this, dt )

    elseif ( this%iSourceDataForm == STATIC_NETCDF_GRID ) then

      call getvalues_static_netcdf_sub( this )

    elseif( this%iSourceDataForm == TABLE_LOOKUP ) then

      call getvalues_from_lookup_table( this, dt )

    elseif(this%iSourceDataForm == STATIC_GRID ) then

      call getvalues_gridded_sub( this )

    elseif(this%iSourceDataForm == CONSTANT_GRID ) then

      call getvalues_constant_sub( this )

    else

      call assert(FALSE, "Unsupported data source specified", &
        __SRCNAME__, __LINE__)

    endif

    ! if grid data hasn't changed this timestep, we do not want to *reapply* the
    ! scale and offset values
    if ( this%lGridHasChanged ) then

      !> Now apply the user scale and offset amounts
      if (this%iTargetDataType == DATATYPE_REAL) then

        call apply_scale_and_offset(fResult=this%pGrdBase%rData, fValue=this%pGrdBase%rData,          &
              dUserScaleFactor=this%rUserScaleFactor, dUserAddOffset=this%rUserAddOffset )

      elseif ( this%iTargetDataType == DATATYPE_INT ) then

         call apply_scale_and_offset(iResult=this%pGrdBase%iData, iValue=this%pGrdBase%iData,          &
              dUserScaleFactor=this%rUserScaleFactor, dUserAddOffset=this%rUserAddOffset )

      else

        call die("Unsupported data type specified", __SRCNAME__, __LINE__)

      endif

    endif

  end subroutine getvalues_sub

!--------------------------------------------------------------------------------------------------

elemental subroutine apply_scale_and_offset_float(fResult, fValue, dUserScaleFactor, dUserAddOffset )

  real (c_float), intent(out)  :: fResult
  real (c_float), intent(in)   :: fValue
  real (c_double), intent(in)   :: dUserScaleFactor
  real (c_double), intent(in)   :: dUserAddOffset

  fResult = ( fValue * dUserScaleFactor ) + dUserAddOffset

end subroutine apply_scale_and_offset_float

!--------------------------------------------------------------------------------------------------

elemental subroutine apply_scale_and_offset_int(iResult, iValue, dUserScaleFactor, dUserAddOffset )

  integer (c_int), intent(out) :: iResult
  integer (c_int), intent(in)  :: iValue
  real (c_double), intent(in)   :: dUserScaleFactor
  real (c_double), intent(in)   :: dUserAddOffset

  iResult = ( real( iValue, c_float) * dUserScaleFactor ) + dUserAddOffset

end subroutine apply_scale_and_offset_int

!--------------------------------------------------------------------------------------------------

subroutine getvalues_constant_sub( this  )

  class (DATA_CATALOG_ENTRY_T) :: this

  if ( .not. associated(this%pGrdBase) ) &
    call die("Internal programming error--attempt to use null pointer", __SRCNAME__, __LINE__)

  select case (this%iSourceDataType)

    case ( DATATYPE_REAL )

      this%lGridHasChanged = TRUE

      this%pGrdBase%rData = this%rConstantValue

    case ( DATATYPE_INT)

      this%lGridHasChanged = TRUE

      this%pGrdBase%iData = this%iConstantValue

    case default

      call dump_data_structure_sub(this)

      call assert(FALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: " &
        //"name="//dquote(this%sDescription) &
        //"; value="//trim(asCharacter(this%iSourceDataType)), &
        __SRCNAME__, __LINE__)

    end select

  end subroutine getvalues_constant_sub

!--------------------------------------------------------------------------------------------------

  subroutine getvalues_from_lookup_table( this, dt )

    class (DATA_CATALOG_ENTRY_T)   :: this
    type (DATETIME_T), intent(in)  :: dt

    integer (c_int) :: indx
    integer (c_int) :: n
    integer (c_int) :: status_code
  
    ! [ LOCALS ]
    type (FSTRING_LIST_T) :: slDateValues

    if ( .not. associated(this%pGrdBase) ) &
      call die("Internal programming error--attempt to use null pointer", __SRCNAME__, __LINE__)

    if ( .not. this%lTableValuesHaveBeenRetrieved ) then

      this%lTableValuesHaveBeenRetrieved = TRUE
      this%table_indx = 1

      select case (this%iSourceDataType)

      case ( DATATYPE_REAL )

        call PARAMS%get_parameters(sKey="date", slValues=slDateValues)

        n = slDateValues%count

        !@TODO: more tests needed to ensure user can't feed in more than one column with same name, 
        !!      dates out of order, dates missing, mismatched numbers of dates versus values, etc.
        allocate(this%table_dates(n), stat=status_code)
        allocate(this%table_values_real(n), stat=status_code)

        call this%table_dates(1)%setDateFormat(this%sDefaultDateFormat)
        
        call PARAMS%get_parameters(sKey=this%sValueColumnName, fValues=this%table_values_real)

        if (     ( size( this%table_values_real,1) /= n)      &
            .or. ( size( this%table_values_real,1) == 1 ) )   &
            call die("Did not find values associated with a required table entry ("//squote(this%sValueColumnName)//"). ",   &
            __SRCNAME__, __LINE__)

        do indx=1, n
          call this%table_dates(indx)%parseDate(slDateValues%get(indx),__SRCNAME__, __LINE__)
        enddo 

      case ( DATATYPE_INT)


      case default

        call dump_data_structure_sub(this)

        call assert(FALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: " &
          //"name="//dquote(this%sDescription) &
          //"; value="//trim(asCharacter(this%iSourceDataType)), &
          __SRCNAME__, __LINE__)

      end select

    endif

    do

      if ( this%table_indx < lbound(this%table_dates,1))  exit
      if ( this%table_indx > ubound(this%table_dates,1))  exit

      if (this%table_dates(this%table_indx) < dt) then
        this%table_indx = this%table_indx + 1
      elseif (this%table_dates(this%table_indx) > dt) then
        this%table_indx = this%table_indx - 1
      else 
        exit 
      endif

    enddo

    if ( this%table_dates(this%table_indx) == dt ) then

      select case (this%iSourceDataType)
    
        case ( DATATYPE_REAL )
    
          this%lGridHasChanged = TRUE
    
          this%pGrdBase%rData = this%table_values_real(this%table_indx)
    
        case ( DATATYPE_INT)
    
        case default
    
          call dump_data_structure_sub(this)
    
          call assert(FALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: " &
            //"name="//dquote(this%sDescription) &
            //"; value="//trim(asCharacter(this%iSourceDataType)), &
            __SRCNAME__, __LINE__)
    
      end select

    else 

      call die("Missing or out-of-order value supplied for "//squote(this%sValueColumnName),   &
          __SRCNAME__, __LINE__)
        
    endif

    end subroutine getvalues_from_lookup_table

!--------------------------------------------------------------------------------------------------

  subroutine dump_data_structure_sub(this)

  class (DATA_CATALOG_ENTRY_T) :: this

  call LOGS%write("---------------------------------------------------")
  call LOGS%write("DATA STRUCTURE DETAILS:")
  call LOGS%write("---------------------------------------------------")

  call LOGS%write("  catalog key word: "//dquote( this%sKeyWord ) )

  call LOGS%write("  source data form: "//trim(asCharacter(this%iSourceDataForm)) )
  call LOGS%write("  source data type: "//trim(asCharacter(this%iSourceDataType)) )
  call LOGS%write("  source file type: "//trim(asCharacter(this%iSourceFileType)) )
  call LOGS%write("  description: "//trim(this%sDescription) )
  call LOGS%write("  source PROJ4 string: "//trim(this%sSourcePROJ4_string) )
  call LOGS%write("  source file type: "//trim(this%sSourceFileType) )
  call LOGS%write("  filename template: "//trim(this%sFilenameTemplate) )
  call LOGS%write("  source filename: "//trim(this%sSourceFilename) )

  if (associated(this%pGrdNative))  call grid_DumpGridExtent(this%pGrdNative)
  if (associated(this%pGrdBase))  call grid_DumpGridExtent(this%pGrdBase)

  end subroutine dump_data_structure_sub

!--------------------------------------------------------------------------------------------------

  subroutine getvalues_gridded_sub( this, dt )

    class (DATA_CATALOG_ENTRY_T)   :: this
    type (DATETIME_T), optional    :: dt
    logical (c_bool) :: lExist
    logical (c_bool) :: lOpened

    this%lGridHasChanged = FALSE

    do

      call assert(this%iSourceFileType == FILETYPE_ARC_ASCII .or. &
        this%iSourceFileType == FILETYPE_SURFER, "INTERNAL PROGRAMMING ERROR -" &
        //" improper file type in use for a call to this subroutine", &
        __SRCNAME__, __LINE__)

      if(this%iSourceDataForm == DYNAMIC_GRID ) then

        if(.not. present(dt) ) &
          call assert(FALSE, "INTERNAL PROGRAMMING ERROR - datetime object"   &
            //" must be supplied when calling this subroutine in a "           &
            //"dynamic mode.", __SRCNAME__, __LINE__)


        call this%make_filename(dt)

      endif

      ! if the source filename hasn't changed, we do not need to be here
      if ( this%sOldFilename .strequal. this%sSourceFilename ) exit

      this%sOldFilename = this%sSourceFilename

      inquire(file=this%sSourceFilename, exist=lExist, opened=lOpened)

      ! if the file does not exist, EXIT
      if (.not. lExist ) then
        if ( this%lAllowMissingFiles ) then
         exit
        else
          call assert( FALSE, &
            "Could not find input data file~filename:"//dquote(this%sSourceFilename) &
            //"~data description: "//trim(this%sDescription))
        endif
      endif

      call LOGS%write("Opening file "//dQuote(this%sSourceFilename) &
        //" for "//trim(this%sDescription)//" data.", iLogLevel=LOG_ALL, lEcho=TRUE )

      if ( this%lGridIsPersistent .and. associated(this%pGrdNative) ) then

        call grid_ReadExisting ( sFileName=this%sSourceFilename, &
          sFileType=this%sSourceFileType, &
          pGrd=this%pGrdNative )
      else

        ! create a grid in native coordinates of the source dataset.
        this%pGrdNative => grid_Read( sFileName=this%sSourceFilename, &
          sFileType=this%sSourceFileType, &
          iDataType=this%iSourceDataType )

        ! ensure that PROJ4 string is associated with the native grid
        this%pGrdNative%sPROJ4_string = this%sSourcePROJ4_string
      endif

      this%lGridHasChanged = TRUE

      select case (this%iTargetDataType)

        case ( GRID_DATATYPE_REAL )

          call this%handle_missing_values(this%pGrdNative%rData)
          call this%enforce_limits(this%pGrdNative%rData)

        case ( GRID_DATATYPE_INT )

!          call this%handle_missing_values(this%pGrdNative%iData)
!          call this%enforce_limits(this%pGrdNative%iData)

        case default

          call assert(FALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: value=" &
            //trim(asCharacter(this%iSourceDataType)), &
            __SRCNAME__, __LINE__)

      end select

      call this%transform_native_to_base()

      if ( .not. this%lGridIsPersistent )  call grid_Destroy( this%pGrdNative )

      exit

    enddo

  end subroutine getvalues_gridded_sub

!--------------------------------------------------------------------------------------------------

subroutine transform_grid_to_grid_sub(this)

  class (DATA_CATALOG_ENTRY_T) :: this

  if (.not. associated(this%pGrdNative) )  &
    call die("INTERNAL PROGRAMMING ERROR--Null pointer detected.", __SRCNAME__, __LINE__)

  if ( .not. associated(this%pGrdBase) ) &
    this%pGrdBase => grid_Create( iNX=BNDS%iNumCols, iNY=BNDS%iNumRows, rX0=BNDS%fX_ll, rY0=BNDS%fY_ll, &
      rGridCellSize=BNDS%fGridCellSize, iDataType=this%iTargetDataType )

  ! only invoke the transform procedure if the PROJ4 strings are different
  if (.not. ( this%pGrdNative%sPROJ4_string .strequal. this%pGrdBase%sPROJ4_string ) ) then

    call LOGS%write("Transforming gridded data in file: "//dquote(this%sSourceFilename), iLinesBefore=1 )
    call LOGS%write("FROM: "//squote(this%sSourcePROJ4_string), iTab=2 )
    call LOGS%write("TO:   "//squote(this%pGrdBase%sPROJ4_string), iTab=2 )

    call grid_Transform(pGrd=this%pGrdNative, &
                      sFromPROJ4=this%sSourcePROJ4_string, &
                      sToPROJ4=BNDS%sPROJ4_string )

    !! following this call, the pGrdNative%rX and pGrdNative%rY values will be given in the
    !! base SWB project projection

  endif

  if ( this%lRequireCompleteSpatialCoverage )   &
    call assert( grid_CompletelyCover( this%pGrdBase, this%pGrdNative ), &
        "Transformed grid read from file "//dquote(this%sSourceFilename) &
        //" does not completely cover your model domain.")

  select case (this%iTargetDataType)

    case ( GRID_DATATYPE_REAL )

      call grid_gridToGrid_sgl(pGrdFrom=this%pGrdNative,&
                               pGrdTo=this%pGrdBase )

    case ( GRID_DATATYPE_INT )

      call grid_gridToGrid_int(pGrdFrom=this%pGrdNative, &
                               pGrdTo=this%pGrdBase,     &
                               lUseMajorityFilter=this%lUseMajorityFilter )

    case default

      call assert(FALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: value=" &
        //trim(asCharacter(this%iSourceDataType)), &
        __SRCNAME__, __LINE__)

  end select


end subroutine transform_grid_to_grid_sub

!--------------------------------------------------------------------------------------------------

subroutine set_constant_value_int( this, iValue )

    class (DATA_CATALOG_ENTRY_T) :: this
    integer (c_int) :: iValue

    this%iConstantValue = iValue

end subroutine set_constant_value_int

!--------------------------------------------------------------------------------------------------

subroutine set_constant_value_real( this, rValue )

    class (DATA_CATALOG_ENTRY_T) :: this
    real (c_float) :: rValue

    this%rConstantValue = rValue

end subroutine set_constant_value_real

!--------------------------------------------------------------------------------------------------

  subroutine set_filecount( this, iValue, iYear)

    class (DATA_CATALOG_ENTRY_T) :: this
    integer (c_int) :: iValue
    integer (c_int), optional :: iYear

    this%iFileCount = iValue

    if (present(iYear) ) this%iFileCountYear = iYear

  end subroutine set_filecount

!--------------------------------------------------------------------------------------------------

  subroutine increment_filecount( this )

    class (DATA_CATALOG_ENTRY_T) :: this

    this%iFileCount = this%iFileCount + 1

  end subroutine increment_filecount

!--------------------------------------------------------------------------------------------------

  subroutine reset_filecount( this )

    class (DATA_CATALOG_ENTRY_T) :: this

    this%iFileCount = 0

  end subroutine reset_filecount

!--------------------------------------------------------------------------------------------------

  subroutine reset_at_yearend_filecount( this, iYear )

    class (DATA_CATALOG_ENTRY_T) :: this
    integer (c_int) :: iYear

    if (iYear /= this%iFileCountYear )  then
      this%iFileCount = 0
      this%iFileCountYear = iYear
    endif

  end subroutine reset_at_yearend_filecount

!--------------------------------------------------------------------------------------------------

  subroutine make_filename_from_template( this, dt )

    class (DATA_CATALOG_ENTRY_T)            :: this
    type (DATETIME_T), intent(in), optional :: dt

    ! [ LOCALS ]
    character (len=256) :: sNewFilename
    character (len=256) :: sUppercaseFilename
    character (len=256) :: sCWD
    character (len=256) :: sBuf2
    integer (c_int) :: iPos_Y, iPos_D, iPos_M, iPos_0D, iPos_0M, iPos_B,  &
                            iPos_BF, iPos_j, iPos, iPos2, iLen, iCount
    integer (c_int) :: iNumZeros, iNumZerosToPrint
    logical (c_bool) :: lMatch
    logical (c_bool) :: lExist
    character (len=16) :: sBuf
    character (len=12) :: sNumber
    character (len=1) :: sDelimiter
    integer (c_int) :: iStatus
    logical (c_bool) :: lAnnual

    iPos_Y = 0; iPos_M = 0; iPos_D = 0; iPos = 0; iPos_B = 0; iPos_BF = 0; sNumber = ""
    iPos_j = 0
    lAnnual = FALSE

    ! EXAMPLES of the kinds of templates that we need to be able to understand:
    ! tars1980\prcp.nc   template => "tars%Y\prcp.nc"
    ! prcp_1980_00.nc    template => "prcp_%Y_%m.nc"

!    iStatus = getcwd(sCWD )

!    call assert(iStatus==0, "Problem detemining what the current working" &
!      //" directory is", __SRCNAME__, __LINE__)

    sNewFilename = this%sFilenameTemplate

    iCount = 0

    do

      lMatch = FALSE

      iPos_Y = max(index(sNewFilename, "%Y"), index(sNewFilename, "%y") )

      if (iPos_Y > 0) then
        lMatch = TRUE
        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_Y - 1)//trim(asCharacter(dt%iYear)) &
                       //sNewFilename(iPos_Y + 2:iLen)

        lAnnual = TRUE

      endif

      ! evaluate template string for "#" characters
      iPos = index(sNewFilename, "#")

      if (iPos > 0) then

        ! example:  %000#
        ! trying to determine how many zero values have been inserted between % and # characters
        iPos2 = index(sNewFilename(1:iPos),"%", BACK=TRUE)
        sBuf2 = trim(asCharacter(this%iFileCount))
        iNumZeros = max(0, iPos - iPos2 - 1)

        if (iNumZeros > 0) then
          iNumZerosToPrint = max(0,iNumZeros - len_trim(sBuf2) + 1)
          sNumber = repeat("0", iNumZerosToPrint )//trim(sBuf2)
        else
          sNumber = trim(sBuf2)
        endif

        lMatch = TRUE
        lAnnual = FALSE
        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos-2-iNumZeros)//trim(sNumber) &
                       //sNewFilename(iPos+1:iLen)
      endif

      ! evaluate template string for "%m": month number

      iPos_M = index(sNewFilename, "%m")
      iPos_0M = index(sNewFilename, "%0m")
      iPos_B = index(sNewFilename, "%b")
      iPos_BF = index(sNewFilename, "%B")

      if ( iPos_0M > 0 ) then

        lMatch = TRUE
        lAnnual = FALSE
        write (unit=sBuf, fmt="(i2.2)") dt%iMonth

        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_0M - 1)//trim(sBuf) &
                       //sNewFilename(iPos_0M + 3:iLen)

      elseif ( iPos_M > 0 ) then

        lMatch = TRUE
        lAnnual = FALSE
        sBuf = asCharacter( dt%iMonth )

        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_M - 1)//trim(sBuf) &
                       //sNewFilename(iPos_M + 2:iLen)

      elseif ( iPos_B > 0 ) then

        lMatch = TRUE
        lAnnual = FALSE

        select case ( this% iFilename_Monthname_Capitalization_Rule )

          case ( FILE_TEMPLATE_UPPERCASE_MONTHNAME )

            sBuf = MONTHS( dt%iMonth )%sName
            call toUppercase( sBuf )

          case ( FILE_TEMPLATE_LOWERCASE_MONTHNAME )

            sBuf = MONTHS( dt%iMonth )%sName
            call toLowercase ( sBuf )

          case default

            sBuf = MONTHS( dt%iMonth )%sName

        end select

        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_B - 1)//trim(sBuf) &
                       //sNewFilename(iPos_B + 2:iLen)

      elseif ( iPos_BF > 0 ) then

        lMatch = TRUE
        lAnnual = FALSE

        select case ( this% iFilename_Monthname_Capitalization_Rule )

          case ( FILE_TEMPLATE_UPPERCASE_MONTHNAME )

            sBuf = MONTHS( dt%iMonth )%sFullName
            call toUppercase( sBuf )

          case ( FILE_TEMPLATE_LOWERCASE_MONTHNAME )

            sBuf = MONTHS( dt%iMonth )%sFullName
            call toLowercase( sBuf )

          case default

            sBuf = MONTHS( dt%iMonth )%sFullName

        end select

        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_BF - 1)//trim(sBuf) &
                       //sNewFilename( ( iPos_BF + len_trim(sBuf) - 1):iLen)

      endif

      ! evaluate template string for DOY number (%j)
      iPos_j = max(index(sNewFilename, "%J"),index(sNewFilename, "%j") )

      if (iPos_j > 0) then
        lMatch = TRUE
        lAnnual = FALSE
        write (unit=sBuf, fmt="(i3.3)") dt%getDayOfYear()
        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_j - 1)//trim(sBuf) &
                       //sNewFilename(iPos_j + 2:iLen)

       endif

      ! evaluate template string for "%d": day number

      iPos_D = max(index(sNewFilename, "%D"),index(sNewFilename, "%d") )
      iPos_0D = max(index(sNewFilename, "%0D"), index(sNewFilename, "%0d") )

      if (iPos_0D > 0) then
        lMatch = TRUE
        lAnnual = FALSE
        write (unit=sBuf, fmt="(i2.2)") dt%iDay
        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_0D - 1)//trim(sBuf) &
                       //sNewFilename(iPos_0D + 3:iLen)

      elseif ( iPos_D > 0 ) then

        lMatch = TRUE
        lAnnual = FALSE
        sBuf = asCharacter( dt%iDay )

        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_D - 1)//trim(sBuf) &
                       //sNewFilename(iPos_D + 2:iLen)

      endif

      if (.not. lMatch) exit

      iCount = iCount + 1

      ! failsafe
      if (iCount > 4) exit

    enddo

    if( index(string=sCWD, substring=FORWARDSLASH) > 0 ) then
      sDelimiter = FORWARDSLASH
    else
      sDelimiter = BACKSLASH
    endif

!    this%sSourceFilename = trim(sCWD)//trim(sDelimiter)//trim(sNewFilename)
    this%sSourceFilename = trim(sNewFilename)

    this%lIsAnnualGrid = lAnnual

  end subroutine make_filename_from_template

!--------------------------------------------------------------------------------------------------

  function test_for_need_to_pad_values_fn(this, dt )    result(lNeedToPadData)

    class (DATA_CATALOG_ENTRY_T)  :: this
    type (DATETIME_T), intent(in) :: dt

    ! [ LOCALS ]
    logical (c_bool) :: lExist
    integer (c_int)  :: iDaysLeftInMonth
    integer (c_int)  :: iPos
    logical (c_bool) :: lNeedToPadData

    do

      lNeedToPadData = FALSE

      iPos = scan(string=trim(this%sSourceFilename), set="http://")

      ! if this is a URL, we do not want to test for file existence using
      ! the Fortran "inquire" function
      if (this%sSourceFilename(iPos:iPos+6) == "http://") then

        exit

      else

        ! does this file actually exist?
        inquire( file=this%sSourceFilename, exist=lExist )

        ! if the file exists, do not bother with padding any values, unless
        ! we are dealing with a file that does exist but drops the last day
        ! or two of data (looking at you, DayMet)
        if ( lExist .and. ( .not. this%lIsAnnualGrid ) )   exit

        ! if file does not exist, and we are close to the end of the year,
        ! assume that we should pad values at the end of the year
        if (dt%iMonth == 12 ) then

          iDaysLeftInMonth = 31 - dt%iDay

          if ( dt%isLeapYear() ) then

            if ( iDaysLeftInMonth <= this%iDaysToPadIfLeapYear ) then

              lNeedToPadData = TRUE
              exit

            endif

          else    ! it's not leap year

            if ( iDaysLeftInMonth <= this%iDaysToPadAtYearsEnd ) then

              lNeedToPadData = TRUE
              exit

            endif

          endif

        endif

        ! if we have reached this point, we cannot locate the proper file and
        ! we are not within the proper range of dates to allow for padding.
        call assert(lExist, "The filename created from your template refers to " &
          //"a nonexistent file. ~ Attempted to open filename "&
          //dquote(this%sSourceFilename), __SRCNAME__, __LINE__)

        exit

      endif

    enddo


  end function test_for_need_to_pad_values_fn

!--------------------------------------------------------------------------------------------------

  subroutine getvalues_dynamic_netcdf_sub( this, dt )

    class (DATA_CATALOG_ENTRY_T)   :: this
    type (DATETIME_T), intent(in)  :: dt

    ! [ LOCALS ]
    integer (c_int) :: iTimeIndex
    integer (c_int) :: iStat
    logical (c_bool) :: lDateTimeFound
    real (c_double) :: dAddOffset
    real (c_double) :: dScaleFactor

    if ( .not. associated(this%pGrdBase) ) &
      call die("Internal programming error--attempt to use null pointer", __SRCNAME__, __LINE__)

    this%lPadValues = FALSE

    ! call once at start of run...
    if ( this%iFileCountYear < 0 ) call this%set_filecount(-1, dt%iYear)

    do

      if (this%iNC_FILE_STATUS == NETCDF_FILE_OPEN) then

        ! check to see whether currently opened file is within date range
        ! if past date range, close file

        if ( netcdf_date_within_range(NCFILE=this%NCFILE,                      &
           iJulianDay=int(dt%iJulianDay, c_int) ) ) then
          exit
        else
          call netcdf_close_file( NCFILE=this%NCFILE )
          this%iNC_FILE_STATUS = NETCDF_FILE_CLOSED
        endif

      endif

      if ( this%iNC_FILE_STATUS == NETCDF_FILE_CLOSED ) then

        ! increment or reset file counter based on current year value
        call this%increment_filecount()

        ! the numerical counter used in creating filenames is reset at the end of each year
        call this%reset_at_yearend_filecount(dt%iYear)

        ! based on the template information, create the filename that SWB
        ! is to look for
        call this%make_filename( dt )

        this%lPadValues = this%test_for_need_to_pad_values(dt)

        ! call to test_for_need_to_pad_values return value of "TRUE" if
        ! if attempts to open a nonexistent file within the last few days of a year.
        ! The assumption is that values missing at the end of a calendar year
        ! translates into a missing file at the year's end

        if (.not. this%lPadValues) then

          if (this%lPerformFullInitialization ) then

            if( ( len_trim( this%sSourcePROJ4_string ) > 0 )                    &
              .and. ( .not. ( this%sSourcePROJ4_string .strequal. "<NA>") ) ) then

              ! calculate the project boundaries in the coordinate system of
              ! the native data file
              call this%calc_project_boundaries(pGrdBase=this%pGrdBase)

              if ( this%lRequireCompleteSpatialCoverage ) then

                call netcdf_open_and_prepare_as_input(NCFILE=this%NCFILE, &
                  sFilename=this%sSourceFilename, &
                  lFlipHorizontal=this%lFlipHorizontal, &
                  lFlipVertical=this%lFlipVertical, &
                  lAllowAutomaticDataFlipping=this%lAllowAutomaticDataFlipping, &
                  rX_Coord_AddOffset = this%rX_Coord_AddOffset, &
                  rY_Coord_AddOffset = this%rY_Coord_AddOffset, &
                  sVariableOrder=this%sVariableOrder, &
                  sVarName_x=this%sVariableName_x, &
                  sVarName_y=this%sVariableName_y, &
                  sVarName_z=this%sVariableName_z, &
                  sVarName_time=this%sVariableName_time, &
                  tGridBounds=this%GRID_BOUNDS_NATIVE )

              else

                call netcdf_open_and_prepare_as_input(NCFILE=this%NCFILE, &
                  sFilename=this%sSourceFilename, &
                  lFlipHorizontal=this%lFlipHorizontal, &
                  lFlipVertical=this%lFlipVertical, &
                  lAllowAutomaticDataFlipping=this%lAllowAutomaticDataFlipping, &
                  rX_Coord_AddOffset = this%rX_Coord_AddOffset, &
                  rY_Coord_AddOffset = this%rY_Coord_AddOffset, &
                  sVariableOrder=this%sVariableOrder, &
                  sVarName_x=this%sVariableName_x, &
                  sVarName_y=this%sVariableName_y, &
                  sVarName_z=this%sVariableName_z, &
                  sVarName_time=this%sVariableName_time )

              endif

            else  ! PROJ4 string is blank

              ! assume source NetCDF file is in same projection and
              ! of same dimensions as base grid
              call netcdf_open_and_prepare_as_input(NCFILE=this%NCFILE, &
                sFilename=this%sSourceFilename, &
                lFlipHorizontal=this%lFlipHorizontal, &
                lFlipVertical=this%lFlipVertical, &
                lAllowAutomaticDataFlipping=this%lAllowAutomaticDataFlipping, &
                sVariableOrder=this%sVariableOrder, &
                sVarName_x=this%sVariableName_x, &
                sVarName_y=this%sVariableName_y, &
                sVarName_z=this%sVariableName_z, &
                sVarName_time=this%sVariableName_time )

              this%NCFILE%iNX = this%pGrdBase%iNX
              this%NCFILE%iNY = this%pGrdBase%iNY
              this%NCFILE%rX(NC_LEFT) = this%pGrdBase%rX0
              this%NCFILE%rY(NC_BOTTOM) = this%pGrdBase%rY0
              this%NCFILE%rX(NC_RIGHT) = this%pGrdBase%rX1
              this%NCFILE%rY(NC_TOP) = this%pGrdBase%rY1

            endif

            this%iNC_FILE_STATUS = NETCDF_FILE_OPEN

            this%iSourceDataType = this%NCFILE%iVarType(NC_Z)

            ! Amongst other things, the call to netcdf_open_and_prepare
            ! finds the nearest column and row that correspond to the
            ! project bounds, then back-calculates the coordinate values
            ! of the column and row numbers in the *NATIVE* coordinate system
            if ( associated(this%pGrdNative) )  call grid_Destroy (this%pGrdNative)

            this%pGrdNative => grid_Create ( iNX=this%NCFILE%iNX, &
                      iNY=this%NCFILE%iNY, &
                      rX0=this%NCFILE%rX(NC_LEFT), &
                      rY0=this%NCFILE%rY(NC_BOTTOM), &
                      rX1=this%NCFILE%rX(NC_RIGHT), &
                      rY1=this%NCFILE%rY(NC_TOP), &
                      iDataType=this%iTargetDataType )

            if( len_trim( this%sSourcePROJ4_string ) > 0 ) then
              ! ensure that PROJ4 string is associated with the native grid
              this%pGrdNative%sPROJ4_string = this%sSourcePROJ4_string
            endif

            this%pGrdNative%sFilename = this%sSourceFilename

            ! we do not need to perform all these steps for the next file; we are
            ! assuming, of course, that all of the subsequent files cover the same
            ! extents and are in the same projection as this first file
            this%lPerformFullInitialization = FALSE

          else
            ! Projection settings can be left alone; read values from new
            ! NetCDF file with same grid boundaries, projection, etc.

  !          call netcdf_open_file(NCFILE=this%NCFILE, sFilename=this%sSourceFilename, iLU=LU_LOG)
            call netcdf_open_file(NCFILE=this%NCFILE, sFilename=this%sSourceFilename)

            this%iNC_FILE_STATUS = NETCDF_FILE_OPEN

          endif


          if ( netcdf_date_within_range(NCFILE=this%NCFILE,                    &
               iJulianDay=int(dt%iJulianDay, c_int) ) ) then

            exit

          elseif ( scan(this%sSourceFilename, "#") /= 0 ) then

            call netcdf_close_file( NCFILE=this%NCFILE )
            this%iNC_FILE_STATUS = NETCDF_FILE_CLOSED
            call LOGS%write("Did not find the current date in the file "//dquote(this%sSourceFilename)//"~" &
              //"JD range: "//asCharacter(this%NCFILE%iFirstDayJD)//" to "//asCharacter(this%NCFILE%iLastDayJD) &
              //"~current JD: "//asCharacter(dt%iJulianDay)//"~ Will increment sequential file number and try again.", &
              iLinesBefore=1, iLinesAfter=1 )

          else

            call LOGS%write("Valid date range (NetCDF): "//trim(asCharacter(this%NCFILE%iFirstDayJD)) &
              //" to "//trim(asCharacter(this%NCFILE%iLastDayJD)) )

            call LOGS%write("Current Julian Day value: "//trim(asCharacter(dt%iJulianDay)) )

            call assert (FALSE, "Date range for currently open NetCDF file" &
              //" does not include the present simulation date.", &
              __SRCNAME__, __LINE__)

          endif

        endif   ! if(lPadValues)

        exit

      endif  ! If (NC_FILE_STATUS == NETCDF_CLOSED)

    enddo

    if (.not. this%lPadValues) then

      do
        lDateTimeFound = netcdf_update_time_starting_index(NCFILE=this%NCFILE, &
                                  iJulianDay=int(dt%iJulianDay, c_int) )

        if (.not. lDateTimeFound) then
          this%lPadValues = TRUE
          exit
        endif

        call netcdf_get_variable_slice(NCFILE=this%NCFILE, rValues=this%pGrdNative%rData)
        this%lGridHasChanged = TRUE

       ! this initialization must take place here so that initialization may
       ! occur *after* the netCDF file has been opened. previously initialization
       ! took place tens of lines above, which resulted in an 'add_offset' of 0.0
       ! and a 'scale_factor' of 1.0 being applied for the first time step.
        dAddOffset = this%NCFILE%rAddOffset(NC_Z)
        dScaleFactor = this%NCFILE%rScaleFactor(NC_Z)

        this%pGrdNative%rData = this%pGrdNative%rData * dScaleFactor + dAddOffset

        call this%handle_missing_values(this%pGrdNative%rData)
        call this%enforce_limits(this%pGrdNative%rData)
        exit
      enddo

    endif

    if (this%lPadValues) then

      if (this%lPadReplaceWithZero) then

        this%pGrdNative%rData = 0_c_float
        this%pGrdNative%iData = 0_c_int

      endif

      call LOGS%write( repeat("=", 60) )
      call LOGS%write( "Missing day found in NetCDF file - padding values" )
!      call stats_WriteMinMeanMax( iLU=6, &
!        sText=trim(this%NCFILE%sFilename), &
!        rData=this%pGrdNative%rData)
!      call stats_WriteMinMeanMax( iLU=6, &
!        sText=trim(this%NCFILE%sFilename), &
!        rData=this%pGrdNative%rData)
      call LOGS%write( repeat("=", 60) )

    endif

    if (this%lCreateLocalNetCDFArchive) &
             call this%put_values_to_archive(int(dt%iMonth,c_int),             &
                 int(dt%iDay,c_int), dt%iYear)

    call this%transform_native_to_base( )

  end subroutine getvalues_dynamic_netcdf_sub


  subroutine minmaxmean_float( variable , varname, nodata_value )

    real (c_float), dimension(:,:)  :: variable
    character (len=*), intent(in)        :: varname
    real (c_float), intent(in)      :: nodata_value

    ! [ LOCALS ]
    integer (c_int) :: iCount
    character (len=20)   :: sVarname
    character (len=14)   :: sMin
    character (len=14)   :: sMax
    character (len=14)   :: sMean
    character (len=10)   :: sCount

    write (sVarname, fmt="(a20)") adjustl(varname)

    if (size( variable, 1) > 0 ) then
      write (sMin, fmt="(g14.3)")   minval(variable, variable < nodata_value )
      write (sMax, fmt="(g14.3)")   maxval(variable, variable < nodata_value )
      write (sMean, fmt="(g14.3)")  sum(variable, variable < nodata_value ) / count( variable < nodata_value )
      write (sCount, fmt="(i10)") count( variable < nodata_value )
    else
      write (sMin, fmt="(g14.3)")   -9999.
      write (sMax, fmt="(g14.3)")   -9999.
      write (sMean, fmt="(g14.3)")  -9999.
      write (sCount, fmt="(i10)")       0
    endif


    print *, adjustl(sVarname)//" | "//adjustl(sMin)//" | "//adjustl(sMax) &
       //" | "//adjustl(sMean)//" | "//adjustl(sCount)


  end subroutine minmaxmean_float

!--------------------------------------------------------------------------------------------------

  subroutine getvalues_static_netcdf_sub( this )

    class (DATA_CATALOG_ENTRY_T) :: this

    ! [ LOCALS ]
    integer (c_int) :: iStat
    real (c_double) :: dAddOffset
    real (c_double) :: dScaleFactor

    if ( .not. associated(this%pGrdBase) ) &
      call die("Internal programming error--attempt to use null pointer", __SRCNAME__, __LINE__)

    if ( this%iNC_FILE_STATUS == NETCDF_FILE_CLOSED ) then

       if (this%lPerformFullInitialization ) then

          if( ( len_trim( this%sSourcePROJ4_string ) > 0 )                    &
            .and. ( .not. ( this%sSourcePROJ4_string .strequal. "<NA>") ) ) then

            ! calculate the project boundaries in the coordinate system of
            ! the native data file
            call this%calc_project_boundaries(pGrdBase=this%pGrdBase)

            if ( this%lRequireCompleteSpatialCoverage ) then
              call netcdf_open_and_prepare_as_input(NCFILE=this%NCFILE, &
                sFilename=this%sSourceFilename, &
                lFlipHorizontal=this%lFlipHorizontal, &
                lFlipVertical=this%lFlipVertical, &
                sVariableOrder=this%sVariableOrder, &
                sVarName_x=this%sVariableName_x, &
                sVarName_y=this%sVariableName_y, &
                sVarName_z=this%sVariableName_z, &
                sVarName_time=this%sVariableName_time, &
                tGridBounds=this%GRID_BOUNDS_NATIVE )
            else
              call netcdf_open_and_prepare_as_input(NCFILE=this%NCFILE, &
                sFilename=this%sSourceFilename, &
                lFlipHorizontal=this%lFlipHorizontal, &
                lFlipVertical=this%lFlipVertical, &
                sVariableOrder=this%sVariableOrder, &
                sVarName_x=this%sVariableName_x, &
                sVarName_y=this%sVariableName_y, &
                sVarName_z=this%sVariableName_z, &
                sVarName_time=this%sVariableName_time )
            endif

          else  ! PROJ4 string is blank

            ! assume source NetCDF file is in same projection and
            ! of same dimensions as base grid
            call netcdf_open_and_prepare_as_input(NCFILE=this%NCFILE, &
              sFilename=this%sSourceFilename, &
              lFlipHorizontal=this%lFlipHorizontal, &
              lFlipVertical=this%lFlipVertical, &
              sVariableOrder=this%sVariableOrder, &
              sVarName_x=this%sVariableName_x, &
              sVarName_y=this%sVariableName_y, &
              sVarName_z=this%sVariableName_z, &
              sVarName_time=this%sVariableName_time )

            this%NCFILE%iNX = this%pGrdBase%iNX
            this%NCFILE%iNY = this%pGrdBase%iNY
            this%NCFILE%rX(NC_LEFT) = this%pGrdBase%rX0
            this%NCFILE%rY(NC_BOTTOM) = this%pGrdBase%rY0
            this%NCFILE%rX(NC_RIGHT) = this%pGrdBase%rX1
            this%NCFILE%rY(NC_TOP) = this%pGrdBase%rY1

          endif

          this%iNC_FILE_STATUS = NETCDF_FILE_OPEN

          this%iSourceDataType = this%NCFILE%iVarType(NC_Z)

          ! Amongst other things, the call to netcdf_open_and_prepare
          ! finds the nearest column and row that correspond to the
          ! project bounds, then back-calculates the coordinate values
          ! of the column and row numbers in the *NATIVE* coordinate system
          if ( associated(this%pGrdNative) )  call grid_Destroy (this%pGrdNative)

          this%pGrdNative => grid_Create ( iNX=this%NCFILE%iNX, &
                    iNY=this%NCFILE%iNY, &
                    rX0=this%NCFILE%rX(NC_LEFT), &
                    rY0=this%NCFILE%rY(NC_BOTTOM), &
                    rX1=this%NCFILE%rX(NC_RIGHT), &
                    rY1=this%NCFILE%rY(NC_TOP), &
                    iDataType=this%iTargetDataType )

          if( len_trim( this%sSourcePROJ4_string ) > 0 ) then
            ! ensure that PROJ4 string is associated with the native grid
            this%pGrdNative%sPROJ4_string = this%sSourcePROJ4_string
          endif

          this%pGrdNative%sFilename = this%sSourceFilename

          ! we do not need to perform all these steps for the next file; we are
          ! assuming, of course, that all of the subsequent files cover the same
          ! extents and are in the same projection as this first file
          this%lPerformFullInitialization = FALSE

        else
          ! Projection settings can be left alone; read values from new
          ! NetCDF file with same grid boundaries, projection, etc.

!          call netcdf_open_file(NCFILE=this%NCFILE, sFilename=this%sSourceFilename, iLU=LU_LOG)
          call netcdf_open_file(NCFILE=this%NCFILE, sFilename=this%sSourceFilename)

          this%iNC_FILE_STATUS = NETCDF_FILE_OPEN

        endif


    endif  ! If (NC_FILE_STATUS == NETCDF_CLOSED)

    call netcdf_get_variable_slice(NCFILE=this%NCFILE, rValues=this%pGrdNative%rData)

    dAddOffset = this%NCFILE%rAddOffset(NC_Z)
    dScaleFactor = this%NCFILE%rScaleFactor(NC_Z)
    this%pGrdNative%rData = this%pGrdNative%rData * dScaleFactor + dAddOffset

    call this%handle_missing_values(this%pGrdNative%rData)

    call this%enforce_limits(this%pGrdNative%rData)

    call this%transform_native_to_base( )

  end subroutine getvalues_static_netcdf_sub

!--------------------------------------------------------------------------------------------------

  subroutine put_values_to_local_NetCDF_sub(this, iMonth, iDay, iYear)

    class (DATA_CATALOG_ENTRY_T) :: this
    integer (c_int) :: iMonth
    integer (c_int) :: iDay
    integer (c_int) :: iYear

    ! [ LOCALS ]
    integer (c_size_t) :: iNumRows, iNumCols, iNumRecs

    if (this%iNC_ARCHIVE_STATUS == NETCDF_FILE_CLOSED) then

      call netcdf_open_and_prepare_as_output_archive(NCFILE=this%NCFILE, &
               NCFILE_ARCHIVE=this%NCFILE_ARCHIVE, &
               iOriginMonth=iMonth, iOriginDay=iDay, iOriginYear=iYear, &
               iStartYear=this%iStartYear, iEndYear=this%iEndYear)

      this%iNC_ARCHIVE_STATUS = NETCDF_FILE_OPEN

    endif

    iNumRows = int(size(this%pGrdNative%rData, 2), c_size_t)
    iNumCols = int(size(this%pGrdNative%rData, 1), c_size_t)
    iNumRecs = this%iNCFILE_RECNUM

    call netcdf_put_variable_array(NCFILE=this%NCFILE_ARCHIVE, &
       iVarID=this%NCFILE_ARCHIVE%iVarID(NC_Z), &
       iStart=[iNumRecs, 0_c_size_t, 0_c_size_t], &
       iCount=[1_c_size_t, iNumRows, iNumCols], &
       iStride=[1_c_size_t,1_c_size_t,1_c_size_t], &
       rValues=this%pGrdNative%rData)

    call netcdf_put_variable_vector(NCFILE=this%NCFILE_ARCHIVE, &
       iVarID=this%NCFILE_ARCHIVE%iVarID(NC_TIME), &
       iStart=[this%iNCFILE_RECNUM], &
       iCount=[1_c_size_t], &
       iStride=[1_c_size_t], &
       dpValues=[real(this%iNCFILE_RECNUM, c_double)])

    this%iNCFILE_RECNUM = this%iNCFILE_RECNUM + 1

  end subroutine put_values_to_local_NetCDF_sub

!--------------------------------------------------------------------------------------------------

  function get_source_filetype_fn(this)  result(iFileType)

     class (DATA_CATALOG_ENTRY_T) :: this
     integer (c_int) :: iFileType

     if ( (this%sSourceFileType .strequal. "ARC_GRID") &
         .or. (this%sSourceFileType .strequal. "ARC_ASCII") ) then

       iFileType = FILETYPE_ARC_ASCII

     elseif ( this%sSourceFileType .strequal. "SURFER" ) then

       iFileType = FILETYPE_SURFER

     elseif ( this%sSourceFileType .strequal. "NETCDF" ) then

       iFileType = FILETYPE_NETCDF

     else

       call assert(FALSE, "Unknown input file type specified. ~"&
         //"  filename: "//dquote(this%sSourceFilename) &
         //"~  file type specified as: "//dquote(this%sSourceFileType), &
         __SRCNAME__, __LINE__)

     endif

  end function get_source_filetype_fn

!--------------------------------------------------------------------------------------------------

  subroutine set_source_PROJ4_string_sub(this, sPROJ4_string)

     class (DATA_CATALOG_ENTRY_T) :: this
     character (len=*), optional :: sPROJ4_string

     this%sSourcePROJ4_string = sPROJ4_string

  end subroutine set_source_PROJ4_string_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_target_PROJ4_string_sub(this, sPROJ4_string)

     class (DATA_CATALOG_ENTRY_T) :: this
     character (len=*), optional :: sPROJ4_string

     this%sTargetPROJ4_string = sPROJ4_string

  end subroutine set_target_PROJ4_string_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_do_not_allow_netcdf_grid_data_flipping_sub(this)

    class (DATA_CATALOG_ENTRY_T) :: this

    this%lAllowAutomaticDataFlipping = FALSE

  end subroutine set_do_not_allow_netcdf_grid_data_flipping_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_grid_flip_horizontal_sub(this)

    class (DATA_CATALOG_ENTRY_T) :: this

    this%lFlipHorizontal = TRUE

  end subroutine set_grid_flip_horizontal_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_grid_flip_vertical_sub(this)

    class (DATA_CATALOG_ENTRY_T) :: this

    this%lFlipVertical = TRUE

  end subroutine set_grid_flip_vertical_sub

  !--------------------------------------------------------------------------------------------------

  subroutine set_allow_missing_files_flag_sub(this)

    class (DATA_CATALOG_ENTRY_T) :: this

    this%lAllowMissingFiles = TRUE

  end subroutine set_allow_missing_files_flag_sub

!--------------------------------------------------------------------------------------------------

  subroutine set_variable_order_sub(this, sVariableOrder)

    class (DATA_CATALOG_ENTRY_T) :: this
    character (len=*) :: sVariableOrder

    this%sVariableOrder = sVariableOrder

  end subroutine set_variable_order_sub

!--------------------------------------------------------------------------------------------------

subroutine set_scale_sub(this, rScaleFactor)

   class (DATA_CATALOG_ENTRY_T) :: this
   real (c_float) :: rScaleFactor

   this%rUserScaleFactor = rScaleFactor

end subroutine set_scale_sub

!--------------------------------------------------------------------------------------------------

subroutine set_archive_local_sub(this, lValue)

   class (DATA_CATALOG_ENTRY_T) :: this
   logical (c_bool)        :: lValue

   this%lCreateLocalNetCDFArchive = lValue

end subroutine set_archive_local_sub

!--------------------------------------------------------------------------------------------------

subroutine set_X_coord_offset_sub(this, rXOffset)

   class (DATA_CATALOG_ENTRY_T) :: this
   real (c_double)         :: rXOffset

   this%rX_Coord_AddOffset = rXOffset

end subroutine set_X_coord_offset_sub

!----------------------------------------------------------------------

subroutine set_Y_coord_offset_sub(this, rYOffset)

   class (DATA_CATALOG_ENTRY_T) :: this
   real (c_double)         :: rYOffset

   this%rY_Coord_AddOffset = rYOffset

end subroutine set_Y_coord_offset_sub

!----------------------------------------------------------------------

subroutine set_offset_sub(this, rAddOffset)

   class (DATA_CATALOG_ENTRY_T) :: this
   real (c_float)          :: rAddOffset

   this%rUserAddOffset = rAddOffset

end subroutine set_offset_sub

!--------------------------------------------------------------------------------------------------

subroutine set_majority_filter_flag_sub(this, lUseMajorityFilter)

   class (DATA_CATALOG_ENTRY_T) :: this
   logical (c_bool)        :: lUseMajorityFilter

   this%lUseMajorityFilter = lUseMajorityFilter

end subroutine set_majority_filter_flag_sub

!--------------------------------------------------------------------------------------------------

subroutine set_missing_value_int_sub(this, iMissingVal)

  class (DATA_CATALOG_ENTRY_T) :: this
  integer (c_int)         :: iMissingVal

  this%iMissingValuesCode = iMissingVal

end subroutine set_missing_value_int_sub

!--------------------------------------------------------------------------------------------------

subroutine set_missing_value_real_sub(this, rMissingVal)

  class (DATA_CATALOG_ENTRY_T) :: this
  integer (c_int)         :: rMissingVal

  this%rMissingValuesCode = rMissingVal

end subroutine set_missing_value_real_sub

!--------------------------------------------------------------------------------------------------

subroutine set_complete_spatial_coverage_flag_sub(this, lRequireCompleteSpatialCoverage )

  class (DATA_CATALOG_ENTRY_T)       :: this
  logical (c_bool), intent(in)  :: lRequireCompleteSpatialCoverage

  this%lRequireCompleteSpatialCoverage = lRequireCompleteSpatialCoverage

end subroutine set_complete_spatial_coverage_flag_sub

!--------------------------------------------------------------------------------------------------

subroutine set_minimum_allowable_value_int_sub(this, iMinVal)

  class (DATA_CATALOG_ENTRY_T) :: this
  integer (c_int) :: iMinVal

  this%iMinAllowedValue = iMinVal

end subroutine set_minimum_allowable_value_int_sub

!--------------------------------------------------------------------------------------------------

subroutine set_maximum_allowable_value_int_sub(this, iMaxVal)

  class (DATA_CATALOG_ENTRY_T) :: this
  integer (c_int) :: iMaxVal

  this%iMaxAllowedValue = iMaxVal

end subroutine set_maximum_allowable_value_int_sub

!--------------------------------------------------------------------------------------------------

subroutine set_minimum_allowable_value_real_sub(this, rMinVal)

  class (DATA_CATALOG_ENTRY_T) :: this
  real (c_float) :: rMinVal

  this%rMinAllowedValue = rMinVal

end subroutine set_minimum_allowable_value_real_sub

!--------------------------------------------------------------------------------------------------

subroutine set_maximum_allowable_value_real_sub(this, rMaxVal)

  class (DATA_CATALOG_ENTRY_T) :: this
  real (c_float) :: rMaxVal

  this%rMaxAllowedValue = rMaxVal

end subroutine set_maximum_allowable_value_real_sub

!--------------------------------------------------------------------------------------------------

  subroutine calc_project_boundaries_sub(this, pGrdBase)

    class (DATA_CATALOG_ENTRY_T) :: this
    type ( GENERAL_GRID_T ), pointer :: pGrdBase

    ! [ LOCALS ]
    integer (c_int) :: iRetVal
    real (c_float) :: rMultiplier = 0.
    real (c_double), dimension(4) :: rX, rY

    ! ensure that there is sufficient coverage on all sides of grid
    rX(1) = pGrdBase%rX0 ! - pGrdBase%rGridCellSize * rMultiplier ! Xll
    rY(1) = pGrdBase%rY0 ! - pGrdBase%rGridCellSize * rMultiplier ! Yll
    rX(2) = pGrdBase%rX1 ! + pGrdBase%rGridCellSize * rMultiplier ! Xlr
    rY(2) = pGrdBase%rY0 ! - pGrdBase%rGridCellSize * rMultiplier ! Ylr
    rX(3) = pGrdBase%rX0 ! - pGrdBase%rGridCellSize * rMultiplier ! Xul
    rY(3) = pGrdBase%rY1 ! + pGrdBase%rGridCellSize * rMultiplier ! Yul
    rX(4) = pGrdBase%rX1 ! + pGrdBase%rGridCellSize * rMultiplier ! Xur
    rY(4) = pGrdBase%rY1 ! + pGrdBase%rGridCellSize * rMultiplier ! Yur

    ! don't invoke PROJ4 unless projections are at least superficially different
    if ( .not. trim( pGrdBase%sPROJ4_string) == trim(this%sSourcePROJ4_string)) then

      ! now transform the project coordinates to native coordinates so we can
      ! use the native coordinate boundaries to "cookie-cut" only the data
      ! pertinent to our project area.
      iRetVal = pj_init_and_transform(trim(pGrdBase%sPROJ4_string)//C_NULL_CHAR, &
                  trim(this%sSourcePROJ4_string)//C_NULL_CHAR,                   &
                  __SRCNAME__//C_NULL_CHAR,                                   &
                  __LINE__,                                                      &
                  4_c_long,                                                      &
                  rX, rY )

      call grid_CheckForPROJ4Error(iRetVal=iRetVal, &
        sFromPROJ4=trim(pGrdBase%sPROJ4_string), &
        sToPROJ4=trim(this%sSourcePROJ4_string))

    endif

  ! because PROJ4 works in RADIANS if data are unprojected (i.e. GEOGRAPHIC),
  ! we need to convert back to degrees on the assumption that the coordinates
  ! referenced in the file will also be i degrees
  !  if( index(string=trim(this%sSourcePROJ4_string), substring="latlon") > 0 &
  !      .or. index(string=trim(this%sSourcePROJ4_string), substring="lonlat") > 0 ) then

    if (      ( this%sSourcePROJ4_string .containssimilar. "latlon" )            &
         .or. ( this%sSourcePROJ4_string .containssimilar. "latlong" )           &
         .or. ( this%sSourcePROJ4_string .containssimilar. "lonlat" )            &
         .or. ( this%sSourcePROJ4_string .containssimilar. "longlat" ) ) then

      rX = rad_to_deg(rX)
      rY = rad_to_deg(rY)

    endif

    this%GRID_BOUNDS_NATIVE%rXll = rX(1); this%GRID_BOUNDS_NATIVE%rXlr = rX(2)
    this%GRID_BOUNDS_NATIVE%rYll = rY(1); this%GRID_BOUNDS_NATIVE%rYlr = rY(2)
    this%GRID_BOUNDS_NATIVE%rXul = rX(3); this%GRID_BOUNDS_NATIVE%rXur = rX(4)
    this%GRID_BOUNDS_NATIVE%rYul = rY(3); this%GRID_BOUNDS_NATIVE%rYur = rY(4)

#ifdef DEBUG_PRINT
   print *, " "
   print *, trim(__FILE__), ": ", __LINE__
   print *, "--  BASE GRID BOUNDS projected to DATA NATIVE COORDS"
   print *, "FROM: ", dquote(pGrdBase%sPROJ4_string)
   print *, "TO:   ", dquote(this%sSourcePROJ4_string)
   PRINT *, "file: ", dquote(this%sSourceFileName)
   print *, "            X                            Y"
   print *, "LL: ", this%GRID_BOUNDS_NATIVE%rXll, this%GRID_BOUNDS_NATIVE%rYll
   print *, "LR: ", this%GRID_BOUNDS_NATIVE%rXlr, this%GRID_BOUNDS_NATIVE%rYlr
   print *, "UL: ", this%GRID_BOUNDS_NATIVE%rXul, this%GRID_BOUNDS_NATIVE%rYul
   print *, "UR: ", this%GRID_BOUNDS_NATIVE%rXur, this%GRID_BOUNDS_NATIVE%rYur
#endif

  end subroutine calc_project_boundaries_sub

!--------------------------------------------------------------------------------------------------

  subroutine data_GridEnforceLimits_int(this, iValues)

    class (DATA_CATALOG_ENTRY_T) :: this
    integer (c_int), dimension(:,:) :: iValues

    ! [ LOCALS ]
    integer (c_int) :: iMin, iMax

    iMin = this%iMinAllowedValue
    iMax = this%iMaxAllowedValue

    where ( iValues < iMin )  iValues = iMin
    where ( iValues > iMax )  iValues = iMax

  end subroutine data_GridEnforceLimits_int

!--------------------------------------------------------------------------------------------------

  subroutine data_GridEnforceLimits_real(this, rValues)

    class (DATA_CATALOG_ENTRY_T) :: this
    real (c_float), dimension(:,:) :: rValues

    ! [ LOCALS ]
    real (c_float) :: rMin, rMax

    rMin = real(this%rMinAllowedValue, c_float)
    rMax = real(this%rMaxAllowedValue, c_float)

    where ( rValues < rMin )  rValues = rMin
    where ( rValues > rMax )  rValues = rMax

  end subroutine data_GridEnforceLimits_real

!--------------------------------------------------------------------------------------------------

  subroutine data_GridHandleMissingData_real(this, rValues)

    class (DATA_CATALOG_ENTRY_T) :: this
    real (c_float), dimension(:,:), intent(inout) :: rValues

    ! [ LOCALS ]
    real (c_float) :: rMissing, rMean

    rMissing = real(this%rMissingValuesCode, c_float)

    ! changing the default operation to "do nothing"
    ! user must actively choose what to do with missing values
    ! by specifying a valid operator
    if ( trim(this%sMissingValuesOperator) .ne. "&&" ) then

    select case (this%iMissingValuesAction)

      case (MISSING_VALUES_ZERO_OUT)

        select case (trim(this%sMissingValuesOperator))

          case ("<=")

            where (rValues <= rMissing) rValues = rZERO

          case ("<")

            where (rValues < rMissing) rValues = rZERO

          case (">=")

            where (rValues >= rMissing) rValues = rZERO

          case (">")

            where (rValues > rMissing) rValues = rZERO

          case default

            call assert(FALSE, "Unknown missing values code was supplied " &
              //"for processing data "//squote(this%sDescription)//": " &
              //dquote(this%sMissingValuesOperator) )

          end select

      case (MISSING_VALUES_REPLACE_WITH_MEAN)

        select case (this%sMissingValuesOperator)

          case ("<=")

            rMean = sum(rValues, rValues > rMissing ) / count(rValues > rMissing )

            where (rValues <= rMissing) rValues = rMean

          case ("<")

            rMean = sum(rValues, rValues >= rMissing ) / count(rValues >= rMissing )

            where (rValues < rMissing) rValues = rMean

          case (">=")

            rMean = sum(rValues, rValues < rMissing ) / count(rValues < rMissing )

            where (rValues >= rMissing) rValues = rMean

          case (">")

            rMean = sum(rValues, rValues <= rMissing ) / count(rValues <= rMissing )

            where (rValues > rMissing) rValues = rMean

          case default

            call assert(FALSE, "Unknown missing values code was supplied " &
              //"for processing data "//squote(this%sDescription)//": " &
              //dquote(this%sMissingValuesOperator) )

          end select

      case default

        call assert(FALSE, "INTERNAL PROGRAMMING ERROR - unhandled iMissingValuesAction", &
        __SRCNAME__, __LINE__)

    end select

    endif
  end subroutine data_GridHandleMissingData_real

!--------------------------------------------------------------------------------------------------

  subroutine data_GridHandleMissingData_int(this, iValues)

    class (DATA_CATALOG_ENTRY_T) :: this
    integer (c_int), dimension(:,:), intent(inout) :: iValues

    ! [ LOCALS ]
    integer (c_int) :: iMissing, iMean

    iMissing = this%iMissingValuesCode
    if ( trim(this%sMissingValuesOperator) .ne. "&&" ) then

    select case (this%iMissingValuesAction)

      case (MISSING_VALUES_ZERO_OUT)

        select case (trim(this%sMissingValuesOperator))

          case ("<=")

            where (iValues <= iMissing) iValues = iZERO

          case ("<")

            where (iValues < iMissing) iValues = iZERO

          case (">=")

            where (iValues >= iMissing) iValues = iZERO

          case (">")

            where (iValues > iMissing) iValues = iZERO

          case default

            call assert(FALSE, "Unknown missing values code was supplied " &
              //"for processing data "//squote(this%sDescription)//": " &
              //dquote(this%sMissingValuesOperator) )

          end select

      case (MISSING_VALUES_REPLACE_WITH_MEAN)

        select case (this%sMissingValuesOperator)

          case ("<=")

            iMean = sum(iValues, iValues > iMissing ) &
               / count(iValues > iMissing )

            where (iValues <= iMissing) iValues = iMean

          case ("<")

            iMean = sum(iValues, iValues >= iMissing ) &
               / count(iValues >= iMissing )

            where (iValues < iMissing) iValues = iMean

          case (">=")

            iMean = sum(iValues, iValues < iMissing ) &
               / count(iValues < iMissing )

            where (iValues >= iMissing) iValues = iMean

          case (">")

            iMean = sum(iValues, iValues <= iMissing ) &
               / count(iValues <= iMissing )

            where (iValues > iMissing) iValues = iMean

          case default

            call assert(FALSE, "Unknown missing values code was supplied " &
              //"for processing data "//squote(this%sDescription)//": " &
              //dquote(this%sMissingValuesOperator) )

          end select

      case default

        call assert(FALSE, "INTERNAL PROGRAMMING ERROR - unhandled iMissingValuesAction", &
        __SRCNAME__, __LINE__)

    end select

    endif
  end subroutine data_GridHandleMissingData_int

end module data_catalog_entry
