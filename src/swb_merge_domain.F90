module model_domain

  use iso_c_binding
  use data_catalog
  use data_catalog_entry
  use exceptions
  use string_list, only      : STRING_LIST_T
  use grid
  use netcdf4_support, only  : NC_FILL_FLOAT
  implicit none

  private

  ! concept: the only state variables that should appear in this module should be
  !          those that are required regardless of what other program options are active
  !
  !
  ! additionally: concept is to have a set of more or less standard procedure pointers that are common to
  !               a soil moisture balance; the pointer may be set to a different procedure thus changing
  !               the behavior and mechanisms within the model

  type, public :: MODEL_DOMAIN_T

    character (len=:), allocatable     :: PROJ4_string
    integer (kind=c_int)               :: number_of_columns
    integer (kind=c_int)               :: number_of_rows
    real (kind=c_double)               :: X_ll, Y_ll
    real (kind=c_double)               :: X_ur, Y_ur
    real (kind=c_double), allocatable  :: X(:), Y(:)
    real (kind=c_double), allocatable  :: X_lon(:,:), Y_lat(:,:)
    real (kind=c_double)               :: gridcellsize

    logical (kind=c_bool), allocatable     :: active(:,:)
    real (kind=c_float), allocatable       :: dont_care(:,:)
    real (kind=c_float), allocatable       :: array_output(:,:)
    real (kind=c_float), allocatable       :: latitude(:)

    type (GENERAL_GRID_T), pointer         :: pGrdOut
    
  contains

    procedure :: initialize_grid_sub
    generic   :: initialize_grid => initialize_grid_sub
  
  end type MODEL_DOMAIN_T


  ! creating several module-level globals
  type (MODEL_DOMAIN_T), public             :: MODEL

contains

  !
  ! current concept:
  !
  ! The only 2-D array is the array that contains the mask of active cells.
  !
  ! All remaining state variables and ancillary variables are kept in 1-D vectors
  ! that are PACK-ed and UNPACK-ed as needed by i/o routines. This is cumbersome for fully
  ! active grids, but should amount to significant memory and processing savings when running
  ! SWB for, say, an island domain.
  !

  subroutine initialize_grid_sub(this, iNumCols, iNumRows, dX_ll, dY_ll, dGridCellSize )

    class (MODEL_DOMAIN_T), intent(inout)        :: this
    integer (kind=c_int), intent(in)             :: iNumCols
    integer (kind=c_int), intent(in)             :: iNumRows
    real (kind=c_double), intent(in)             :: dX_ll
    real (kind=c_double), intent(in)             :: dY_ll
    real (kind=c_double), intent(in)             :: dGridcellSize

    ! [ LOCALS ]
    integer (kind=c_int)  :: iStat

    this%number_of_columns = iNumCols
    this%number_of_rows = iNumRows
    this%X_ll = dX_ll
    this%Y_ll = dY_ll
    this%gridcellsize = dGridcellSize

    allocate(this%active(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __FILE__, __LINE__)

    allocate(this%dont_care(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __FILE__, __LINE__)

    allocate(this%array_output(iNumCols, iNumRows), stat=iStat )
    call assert (iStat == 0, "Problem allocating memory", __FILE__, __LINE__)

    this%pGrdOut => grid_CreateSimple( iNX=iNumCols, iNY=iNumRows,        &
                      rX0=dX_ll, rY0=dY_ll, rGridCellSize=dGridCellSize,  &
                      iDataType=GRID_DATATYPE_REAL )

  end subroutine initialize_grid_sub

end module model_domain