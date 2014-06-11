module cell_collection

  use iso_c_binding
  use exceptions
  use cell_class
  implicit none

  private

  type, public :: CELL_COLLECTION_T

    class (CELL_T), dimension(:), pointer :: cell

    character (len=:), allocatable  :: sPROJ4_string
    integer (kind=c_int)            :: iNumCols
    integer (kind=c_int)            :: iNumRows
    real (kind=c_double)            :: fX_ll, fY_ll
    real (kind=c_double)            :: fX_ur, fY_ur
    real (kind=c_float)             :: fGridcellSize

  contains

    procedure :: initialize_cells_sub
    generic   :: initialize => initialize_cells_sub

    procedure :: solve_cells_sub
    generic   :: solve => solve_cells_sub

  end type CELL_COLLECTION_T

  !type (CELL_PTR), allocatable :: CELLS(:,:)

  enum, bind(c)
    enumerator :: CELL_INACTIVE=0, CELL_NORMAL=1, CELL_IRRIGATED=2
  end enum  

  type (CELL_COLLECTION_T), public :: CELLS

contains  

  subroutine initialize_cells_sub( this, iNumCols, iNumRows, fX_ll, fY_ll, fGridCellSize )

    class (CELL_COLLECTION_T), intent(inout)     :: this
    integer (kind=c_int), intent(in)             :: iNumCols
    integer (kind=c_int), intent(in)             :: iNumRows
    real (kind=c_double), intent(in)             :: fX_ll
    real (kind=c_double), intent(in)             :: fY_ll
    real (kind=c_double), intent(in)             :: fGridcellSize

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iRow, iCol, iIndex
    class (CELL_T), pointer              :: pCell

    this%iNumCols = iNumCols
    this%iNumRows = iNumRows
    this%fX_ll = fX_ll
    this%fY_ll = fY_ll
    this%fGridcellSize = fGridcellSize

    allocate( this%cell(iNumCols * iNumRows), stat=iStat )

    if (iStat /=0) stop("Could not allocate memory for cells")

    do iRow=1, iNumRows
      do iCol=1, iNumCols

        iIndex = iCol + (iRow - 1) * iNumCols

        if ( iIndex <= ubound(this%cell, 1) ) then

          pCell => this%cell(iIndex)
          call pCell%set_col_row( iCol, iRow )
 
        else

          call die("Index out of bounds", __FILE__, __LINE__)

        endif  

      enddo

    enddo  

  end subroutine initialize_cells_sub


  ! march through a single iteration of the solution
  subroutine solve_cells_sub( this )

    class (CELL_COLLECTION_T), intent(inout)     :: this
    
    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    class (CELL_T), pointer              :: pCell
    integer (kind=c_int)                 :: iIndex

    ! $OMP PARALLEL

    ! $OMP DO PRIVATE(iIndex, pCell)

    do iIndex=1, ubound(this%cell, 1)

      pCell => this%cell(iIndex)

      call pCell%solve()

    enddo  

    ! $OMP END DO

    ! $OMP END PARALLEL

  end subroutine solve_cells_sub




end module cell_collection