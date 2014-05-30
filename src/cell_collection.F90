module cell_collection

  use iso_c_binding
  use cell_class
  implicit none

  private

  type, public :: CELL_COLLECTION_T

    class (CELL_BASE_CLASS_T), dimension(:,:), pointer :: cell => null()

  contains

    procedure :: initialize_cells_sub
    generic   :: initialize => initialize_cells_sub

  end type CELL_COLLECTION_T

  !type (CELL_PTR), allocatable :: CELLS(:,:)

  enum, bind(c)
    enumerator :: CELL_INACTIVE=0, CELL_NORMAL=1, CELL_IRRIGATED=2
  end enum  

  type (CELL_COLLECTION_T), public :: CELLS

contains  

  subroutine initialize_cells_sub( this, iNRows, iNCols, iMask )

    class (CELL_COLLECTION_T), intent(inout)  :: this
    integer (kind=c_int), intent(in)          :: iNRows
    integer (kind=c_int), intent(in)          :: iNCols
    integer (kind=c_int), intent(in)          :: iMask(:,:)


    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iRow, iCol
    class (CELL_BASE_CLASS_T), pointer   :: pCell

    allocate( this%cell(iNRows, iNCols), stat=iStat )

    if (iStat /=0) stop("Could not allocate memory for cells")

    do iRow=1, iNRows
      do iCol=1, iNCols

        pCell => this%cell(iRow, iCol)

        if (iMask( iRow, iCol) == CELL_INACTIVE ) then

          allocate( CELL_BASE_CLASS_T::pCell)

        elseif ( iMask( iRow, iCol ) == CELL_NORMAL ) then

          allocate( CELL_NORMAL_T::pCell )
          
        endif  

        call pCell%print()

      enddo

    enddo  



  end subroutine initialize_cells_sub


end module cell_collection