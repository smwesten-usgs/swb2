module cell_collection

  use iso_c_binding
  use data_catalog_entry
  use exceptions
  use cell_class
  use parameters
  use simulation_datetime
  implicit none

  private

  type, public :: CELL_COLLECTION_T

    class (CELL_T), dimension(:,:), pointer :: cell

    character (len=:), allocatable  :: sPROJ4_string
    integer (kind=c_int)            :: iNumCols
    integer (kind=c_int)            :: iNumRows
    real (kind=c_double)            :: fX_ll, fY_ll
    real (kind=c_double)            :: fX_ur, fY_ur
    real (kind=c_float)             :: fGridcellSize

  contains

    procedure :: initialize_cells_sub
    generic   :: initialize => initialize_cells_sub

    procedure :: initialize_cells_landuse_sub
    generic   :: initialize_landuse => initialize_cells_landuse_sub

    procedure :: initialize_cells_soil_groups_sub
    generic   :: initialize_soil_groups => initialize_cells_soil_groups_sub

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

    allocate( this%cell(iNumCols, iNumRows), stat=iStat )

    if (iStat /=0) stop("Could not allocate memory for cells")

  end subroutine initialize_cells_sub


  subroutine initialize_cells_landuse_sub( this )

    class (CELL_COLLECTION_T), intent(inout)     :: this

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iRow, iCol, iIndex
    class (CELL_T), pointer              :: pCell
    integer (kind=c_int), allocatable    :: iLandUseCodes(:)
    integer (kind=c_int)                 :: iLandUseIndices(256)
    integer (kind=c_int)                 :: iLU, iCount
    
    !> Determine how many landuse codes are present
    call PARAMS%get_values( sKey="LU_Code", iValues=iLanduseCodes )

    ! $OMP PARALLEL

    ! $OMP DO PRIVATE(iRow, iCol, iIndex, pCell)

    CELLS%cell%iLandUseCode = LULC%pGrdBase%iData

    !CELLS%cell%iSoilGroup = HSG%pGrdBase%iData

    call LOGS%write("Landuse codes as read into SWB data structure", iLinesBefore=1, iLinesAfter=1, iLogLevel=LOG_DEBUG)

    do iIndex = 1, ubound(iLandUseCodes, 1)

      where (CELLS%cell%iLandUseCode == iLandUseCodes(iIndex) )

        CELLS%cell%iLandUseIndex = iIndex

      end where

      call LOGS%write( asCharacter(count(CELLS%cell%iLandUseIndex == iIndex) )//" cells have a value of " &
        //asCharacter(iLandUseCodes(iIndex) )//" and an index value of "//asCharacter(iIndex), iLogLevel=LOG_DEBUG )
      
    end do    

    call LOGS%write("", iLinesBefore=1, iLogLevel=LOG_DEBUG)

  end subroutine initialize_cells_landuse_sub


  subroutine initialize_cells_soil_groups_sub( this )

    class (CELL_COLLECTION_T), intent(inout)     :: this

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iRow, iCol, iIndex
    class (CELL_T), pointer              :: pCell
    
    CELLS%cell%iSoilGroup = HSG%pGrdBase%iData

    where ( CELLS%cell%iSoilGroup  < 1 )

      CELLS%cell%lActive = lFALSE

    end where

    call LOGS%write("Inactivated "//asCharacter(count(CELLS%cell%iSoilGroup < 1))//" cells " &
      //"(out of a total of "//asCharacter(size(CELLS%cell%iSoilGroup) )//") with soil group " &
      //"values of zero or less", iLinesBefore=1, iLinesAfter=1 )

    call LOGS%write("Soil hydrologic groups as read into SWB data structure", iLinesBefore=1, iLinesAfter=1, iLogLevel=LOG_DEBUG)

    do iIndex = 1, maxval(HSG%pGrdBase%iData)

      call LOGS%write( asCharacter(count(CELLS%cell%iSoilGroup == iIndex) )//" cells belong to soils group " &
        //asCharacter(iIndex), iLogLevel=LOG_DEBUG )
      
    end do    

    call LOGS%write("", iLinesBefore=1, iLogLevel=LOG_DEBUG)

  end subroutine initialize_cells_soil_groups_sub



  ! march through a single iteration of the solution
  subroutine solve_cells_sub( this )

    class (CELL_COLLECTION_T), intent(inout)     :: this
    
    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    class (CELL_T), pointer              :: pCell
    integer (kind=c_int)                 :: iCol, iRow

    ! $OMP PARALLEL

    ! $OMP DO PRIVATE(iCol, iRow, pCell)

    do iRow=1, this%iNumRows

      do iCol=1, this%iNumCols

        pCell => this%cell(iCol, iRow)
        call pCell%solve()

      enddo

    enddo  

    ! $OMP END DO

    ! $OMP END PARALLEL

  end subroutine solve_cells_sub




end module cell_collection