module routing__D8

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_bool
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use exceptions
  use grid
  use strings
  implicit none

  private

  public :: routing_D8_initialize, D8_UNDETERMINED, TARGET_INDEX, ORDER_INDEX

  type (DATA_CATALOG_ENTRY_T), pointer :: pD8_FLOWDIR    ! data catalog object => D8 flow direction grid

  integer (kind=c_int), allocatable     :: iTargetRow(:,:)
  integer (kind=c_int), allocatable     :: iTargetCol(:,:)
  logical (kind=c_bool), allocatable    :: lDownhillMarked(:,:)
  integer (kind=c_int), allocatable     :: iSumOfUpslopeCells(:,:)
  integer (kind=c_int), allocatable     :: iNumberOfUpslopeConnections(:,:)

  !> @TODO remove redundant data elements; row1d, col1d, etc. are now also stored in the model data structure.

  integer (kind=c_int), allocatable     :: ROW2D(:,:)
  integer (kind=c_int), allocatable     :: COL2D(:,:)
  integer (kind=c_int), allocatable     :: ROW1D(:)
  integer (kind=c_int), allocatable     :: COL1D(:)

  integer (kind=c_int), allocatable     :: ROW_INDEX(:)
  integer (kind=c_int), allocatable     :: COLUMN_INDEX(:)
  integer (kind=c_int), allocatable     :: ORDER_INDEX(:)
  integer (kind=c_int), allocatable     :: TARGET_INDEX(:)

  integer (kind=c_int), parameter       :: D8_EAST         = 1
  integer (kind=c_int), parameter       :: D8_SOUTHEAST    = 2
  integer (kind=c_int), parameter       :: D8_SOUTH        = 4
  integer (kind=c_int), parameter       :: D8_SOUTHWEST    = 8
  integer (kind=c_int), parameter       :: D8_WEST         = 16
  integer (kind=c_int), parameter       :: D8_NORTHWEST    = 32
  integer (kind=c_int), parameter       :: D8_NORTH        = 64
  integer (kind=c_int), parameter       :: D8_NORTHEAST    = 128

  integer (kind=c_int), parameter       :: D8_UNDETERMINED = -999

  ! idea here is to create an array of row and column numbers, then use the
  ! PACK statement to create a vector of row-column numbers in the same order that
  ! the rest of the vectors are packed.

  ! the vector of row-column numbers may then be used to make the required D8 routing connections.
  ! there shouldn't be any difference between ROW_INDEX, COL_INDEX, and ORDER_INDEX

contains

  subroutine routing_D8_initialize( lActive, cell_order_index )

    logical (kind=c_bool), intent(in)    :: lActive(:,:)
    integer (kind=c_int), intent(inout)  :: cell_order_index(:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iNX
    integer (kind=c_int)                 :: iNY
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iColnum
    integer (kind=c_int)                 :: iRownum
    integer (kind=c_int)                 :: iIndex
    integer (kind=c_int)                 :: iCount
    character (len=256)                  :: sBuf

    integer (kind=c_int) :: iCol_lbound, iCol_ubound
    integer (kind=c_int) :: iRow_lbound, iRow_ubound
    integer (kind=c_int) :: iUnitNum

    type (GENERAL_GRID_T), pointer  :: pTempGrid

    iCol_lbound = lbound(lActive, 1)
    iCol_ubound = ubound(lActive, 1)

    iRow_lbound = lbound(lActive, 2)
    iRow_ubound = ubound(lActive, 2)

    iCount = 0

    iNX = ubound(lActive, 1)
    iNY = ubound(lActive, 2)

    allocate( iTargetRow( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( iTargetCol( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )


    allocate( lDownhillMarked( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( COL2D( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( ROW2D( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( COL1D( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( ROW1D( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( iSumOfUpslopeCells( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( iNumberOfUpslopeConnections( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( ROW_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( COLUMN_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( TARGET_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( ORDER_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    ! locate the data structure associated with the gridded flow direction entries
    pD8_FLOWDIR => DAT%find("FLOW_DIRECTION")
    if ( .not. associated(pD8_FLOWDIR) ) &
        call die("A FLOW_DIRECTION grid must be supplied in order to make use of this option.", __SRCNAME__, __LINE__)

    call pD8_FLOWDIR%getvalues()

    pTempGrid=>grid_Create( BNDS%iNumCols, BNDS%iNumRows, BNDS%fX_ll, BNDS%fY_ll, &
      BNDS%fX_ur, BNDS%fY_ur, DATATYPE_INT )

    pTempGrid%iData = pD8_FLOWDIR%pGrdBase%iData

    call grid_WriteArcGrid("D8_Flow_Direction_Grid.asc", pTempGrid)

    call routing_D8_assign_downstream_row_col( lActive )

    do iRownum=iRow_lbound, iRow_ubound
      do iColnum=iCol_lbound, iCol_ubound

        COL2D( iColnum, iRownum ) = iColNum   ! right-most index should vary slowest
        ROW2D( iColnum, iRownum ) = iRownum

      enddo
    enddo

    COL1D = pack( COL2D, lActive )
    ROW1D = pack( ROW2D, lActive )

    call routing_D8_determine_solution_order( lActive )

    open( newunit=iUnitNum, file="D8_routing_table.txt", iostat=iStat, status="REPLACE")

    write(iUnitNum,*) "ORDER_INDEX"//TAB//"TARGET_INDEX"//TAB//"From_COL"//TAB//"From_ROW" &
      //TAB//"To_COL"//TAB//"To_ROW"//TAB//"D8_flowdir"//TAB//"Num_Adjacent_Upslope_Connections"//TAB &
      //"Sum_of_Upslope_Contributing_Cells"

    do iIndex = 1, ubound(COL1D,1)

      sBuf = ""

      associate( Order_index => ORDER_INDEX( iIndex ),                                        &
                 Target_index => TARGET_INDEX( iIndex ),                                      &
                 Rownum => ROW1D( ORDER_INDEX( iIndex ) ),                                    &
                 Colnum => COL1D( ORDER_INDEX( iIndex ) ),                                    &
                 Target_row => ROW1D( TARGET_INDEX( iIndex ) ),                                &
                 Target_col => COL1D( TARGET_INDEX( iIndex ) ),                                    &
                 D8_flowdir => pD8_FLOWDIR%pGrdBase%iData( COL1D( ORDER_INDEX( iIndex ) ),    &
                                  ROW1D( ORDER_INDEX( iIndex ) ) ),                           &
                 Num_adjacent => iNumberOfUpslopeConnections( COL1D( ORDER_INDEX( iIndex ) ), &
                                     ROW1D( ORDER_INDEX( iIndex ) ) ),                        &
                 Sum_upslope => iSumOfUpslopeCells( COL1D( ORDER_INDEX( iIndex ) ),           &
                                     ROW1D( ORDER_INDEX( iIndex ) ) )  )

        write(sBuf,*)  Order_index,TAB, Target_index
        write(sBuf,*) trim(sBuf)//TAB//asCharacter( Colnum )//TAB &
          //asCharacter( Rownum )

        if ( Target_index > 0 ) &
          write(sBuf,*) trim(sBuf)//TAB//asCharacter( Colnum )//TAB &
          //asCharacter( Rownum )//TAB//asCharacter( D8_flowdir )//TAB &
          //asCharacter( Num_adjacent )//TAB//asCharacter( Sum_upslope )

        write(iUnitNum,*)  trim(sBuf)

      end associate

    enddo

    cell_order_index = ORDER_INDEX

    close ( iUnitNum )

  end subroutine routing_D8_initialize

!--------------------------------------------------------------------------------------------------

  function routing_D8_get_index( iCol, iRow )   result( iIndex )

    integer (kind=c_int), intent(in)   :: iCol
    integer (kind=c_int), intent(in)   :: iRow
    integer (kind=c_int)               :: iIndex

    ! [ LOCALS ]
    integer (kind=c_int)   :: iOrderIndex
    logical (kind=c_bool)  :: lFound

    iIndex = -999
    lFound = lFALSE

    do iOrderIndex = lbound(COL1D,1), ubound(COL1D,1)

      if( COL1D( iOrderIndex ) == iCol  .and.  ROW1D( iOrderIndex ) == iRow ) then
      	iIndex = iOrderIndex
        lFound = lTRUE
      	exit
      endif

    enddo

!    if ( .not. lFound ) call warn("Did not find matching column and row number; col=" &
!      //asCharacter(iCol)//"; row="//asCharacter(iRow), __SRCNAME__, __LINE__ )

  end function routing_D8_get_index

!--------------------------------------------------------------------------------------------------

  subroutine routing_D8_assign_downstream_row_col( lActive )

    logical (kind=c_bool), intent(in)    :: lActive(:,:)

    ! [ LOCALS ]
    integer (kind=c_int) :: iRownum
    integer (kind=c_int) :: iColnum
    integer (kind=c_int) :: iCol_lbound, iCol_ubound
    integer (kind=c_int) :: iRow_lbound, iRow_ubound

    iCol_lbound = lbound(lActive, 1)
    iCol_ubound = ubound(lActive, 1)

    iRow_lbound = lbound(lActive, 2)
    iRow_ubound = ubound(lActive, 2)

    associate ( dir => pD8_FLOWDIR%pGrdBase%iData )

	   	do iRownum=iRow_lbound, iRow_ubound
        do iColnum=iCol_lbound, iCol_ubound

          select case ( dir( iColnum, iRownum ) )

            case ( D8_EAST )

              iTargetCol(iColnum, iRownum) = iColnum + 1
              iTargetRow(iColnum, iRownum) = iRownum

            case ( D8_SOUTHEAST )

              iTargetCol(iColnum, iRownum) = iColnum + 1
              iTargetRow(iColnum, iRownum) = iRownum + 1

            case ( D8_SOUTH )

              iTargetCol(iColnum, iRownum) = iColnum
              iTargetRow(iColnum, iRownum) = iRownum + 1

            case ( D8_SOUTHWEST )

              iTargetCol(iColnum, iRownum) = iColnum - 1
              iTargetRow(iColnum, iRownum) = iRownum + 1

            case ( D8_WEST )

              iTargetCol(iColnum, iRownum) = iColnum - 1
              iTargetRow(iColnum, iRownum) = iRownum

            case ( D8_NORTHWEST )

              iTargetCol(iColnum, iRownum) = iColnum - 1
              iTargetRow(iColnum, iRownum) = iRownum - 1

            case ( D8_NORTH )

              iTargetCol(iColnum, iRownum) = iColnum
              iTargetRow(iColnum, iRownum) = iRownum - 1

            case ( D8_NORTHEAST )

              iTargetCol(iColnum, iRownum) = iColnum + 1
              iTargetRow(iColnum, iRownum) = iRownum - 1

            case default

              iTargetCol(iColnum, iRownum) = D8_UNDETERMINED
              iTargetRow(iColnum, iRownum) = D8_UNDETERMINED

          end select

        enddo

      enddo

    end associate


  end subroutine routing_D8_assign_downstream_row_col

!--------------------------------------------------------------------------------------------------

  subroutine routing_D8_determine_solution_order( lActive )

    use grid

    logical (kind=c_bool), intent(in)    :: lActive(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)  :: iRownum
    integer (kind=c_int)  :: iColnum
    integer (kind=c_int)  :: iColsrch
    integer (kind=c_int)  :: iRowsrch
    integer (kind=c_int)  :: iNumberOfChangedCells
    integer (kind=c_int)  :: iCol_lbound, iCol_ubound
    integer (kind=c_int)  :: iRow_lbound, iRow_ubound
    integer (kind=c_int)  :: iUpslopeSum, iUpslopeConnections
    logical (kind=c_bool) :: lAnyUnmarkedUpslopeCells
    logical (kind=c_bool) :: lCircular
    integer (kind=c_int)  :: iNumberRemaining
    integer (kind=c_int)  :: iIndex, k, iCount
    integer (kind=c_int)  :: iDelta
    integer (kind=c_int)  :: iPasses
    integer (kind=c_int)  :: iPassesWithoutChange
    type (GENERAL_GRID_T), pointer  :: pTempGrid

    iCol_lbound = lbound(lActive, 1)
    iCol_ubound = ubound(lActive, 1)

    iRow_lbound = lbound(lActive, 2)
    iRow_ubound = ubound(lActive, 2)

    lDownhillMarked = lFALSE
    iSumOfUpslopeCells = 0_c_int

    iIndex = 0
    iPasses = 0
    iPassesWithoutChange = 0

    pTempGrid=>grid_Create( BNDS%iNumCols, BNDS%iNumRows, BNDS%fX_ll, BNDS%fY_ll, &
      BNDS%fX_ur, BNDS%fY_ur, DATATYPE_INT )


main_loop: do

      iNumberOfChangedCells = 0_c_int
      iDelta = 0_c_int
      iPasses = iPasses + 1

      ! iterate over entire model domain
	    do iRownum=iRow_lbound, iRow_ubound
        do iColnum=iCol_lbound, iCol_ubound

	        if ( .not. lActive(iColnum, iRownum) ) cycle
	        if ( lDownhillMarked(iColnum, iRownum) ) cycle

          iNumberOfChangedCells = iNumberOfChangedCells + 1
	        iUpslopeSum = 0_c_int
	        iUpslopeConnections = 0_c_int
	        lAnyUnmarkedUpslopeCells = lFALSE
          lCircular = lFALSE

          ! search the 8 cells immediately adjacent to the current cell
	local_search: do iRowsrch=max( iRowNum-1, iRow_lbound),min( iRownum+1, iRow_ubound)
                  do iColsrch=max( iColNum-1, iCol_lbound),min( iColnum+1, iCol_ubound)

	 			            ! if adjacent cell points to current cell, determine what to do with runoff
	 			            if ( ( iTargetCol(iColsrch, iRowsrch) == iColnum ) &
	 			            	.and. ( iTargetRow(iColsrch, iRowsrch) == iRownum ) ) then

                      ! if adjacent cell falls outside the area of active cells,
                      ! ignore it and move on
                      if ( .not. lActive( iColsrch, iRowsrch ) )  cycle

                      ! if the target of the current cell points back at the adjacent
                      ! cell, mark current cell as having a "circular" connection
                      if ( ( iTargetCol( iColNum, iRowNum ) == iColsrch )                &
                        .and. ( iTargetRow( iColNum, iRowNum ) == iRowsrch ) )  lCircular = lTRUE

                      ! if the adjacent cell is marked (that is, has no unresolved
                      ! upslope contributions), add the 1 to the number of connections
                      ! and add the adjacent cells' sum of upslope cells to the
                      ! current cells' running sum of upslope cells
	 			              if(	lDownhillMarked( iColsrch, iRowsrch ) ) then

                        iUpslopeSum = iUpslopeSum + iSumOfUpslopeCells(iColsrch, iRowsrch) + 1
                        iUpslopeConnections = iUpslopeConnections + 1

                      ! add number of upslope cells and connections to temporary variables
                      ! if we get to the end of the search and find no unmarked adjacent cells,
                      ! we have determined the number of upslope contributing cells and will
                      ! be able to mark the current cell
	 			           	  else

                        lAnyUnmarkedUpslopeCells = lTRUE

	 			           	  endif

	 			            endif

	                enddo
	              enddo local_search

	              ! OK this is the end of the local search area; did we uncover any unmarked cells
	              ! contributing flow to the current cell? if so, ignore and move on. otherwise,
	              ! mark current cell as marked, update stats, and continue with next cell

	              if ( ( .not. lAnyUnmarkedUpslopeCells ) &
                  .or. (iUpslopeConnections == 1 .and. lCircular ) ) then

                  iDelta = iDelta + 1
	                iSumOfUpslopeCells( iColnum, iRownum ) = iUpslopeSum
	                iNumberOfUpslopeConnections( iColnum, iRownum ) = iUpslopeConnections
	                lDownhillMarked( iColnum, iRownum ) = lTRUE
                  iIndex = iIndex + 1
                  COLUMN_INDEX( iIndex ) = iColnum
                  ROW_INDEX( iIndex ) = iRownum
                  ORDER_INDEX( iIndex ) = routing_D8_get_index( iColnum, iRownum )
                  TARGET_INDEX( iIndex ) = routing_D8_get_index( iTargetCol( iColNum, iRowNum ), &
                    iTargetRow( iColNum, iRowNum ) )

                  if ( lCircular )  TARGET_INDEX( iIndex ) = D8_UNDETERMINED

                elseif ( iPassesWithoutChange > 10 ) then

                  iDelta = iDelta + 1
                  iSumOfUpslopeCells( iColnum, iRownum ) = iUpslopeSum
                  iNumberOfUpslopeConnections( iColnum, iRownum ) = iUpslopeConnections
                  lDownhillMarked( iColnum, iRownum ) = lTRUE
                  iIndex = iIndex + 1
                  COLUMN_INDEX( iIndex ) = iColnum
                  ROW_INDEX( iIndex ) = iRownum
                  ORDER_INDEX( iIndex ) = routing_D8_get_index( iColnum, iRownum )

                  TARGET_INDEX( iIndex ) = D8_UNDETERMINED

                endif

	      enddo
	  	enddo

!      if (iNumberOfChangedCells == 0) exit main_loop

      iNumberRemaining = count( lActive ) - count( lDownhillMarked )

      if ( iNumberRemaining==0 )   exit main_loop

      print *, "### determining solution order... ", count( lDownhillMarked ), " cells marked so far " &
        //"out of ", count( lActive ), " active cells."


      if ( iDelta==0 ) then

        iPassesWithoutChange = iPassesWithoutChange + 1

        where( lDownHillMarked .or.  ( .not. lActive ) )
          pTempGrid%iData = -1
        elsewhere
          pTempGrid%iData = pD8_FLOWDIR%pGrdBase%iData
        end where

        write(*,"(/,1x,'Summary of remaining unmarked cells')")

        ! loop over possible (legal) values of the flow direction grid
        do k=0,128
          iCount=COUNT( .not. lDownHillMarked                         &
            .and. pD8_FLOWDIR%pGrdBase%iData==k .and. lActive )
          if( iCount > 0 ) then
            write(*,FMT="(3x,i8,' unmarked grid cells have flowdir value: ',i8)") iCount, k
          end if
        end do

        write(*,FMT="(3x,a)") repeat("-",60)
        write(*,FMT="(3x,i8,' Total cells with nonzero flow " &
          //"direction values')") count( pD8_FLOWDIR%pGrdBase%iData > 0 )


        call grid_WriteArcGrid("iteration"//asCharacter(iPasses)// &
          "problem_gridcells.asc", pTempGrid)

      end if


  	enddo main_loop

    pTempGrid%iData = iSumOfUpslopeCells
    call grid_WriteArcGrid("D8_Flow_Routing__Upslope_Contributing_Area.asc", pTempGrid)

    pTempGrid%iData = iNumberOfUpslopeConnections
    call grid_WriteArcGrid("D8_Flow_Routing__Number_of_Adjacent_Upslope_Connections.asc", pTempGrid)

    call grid_Destroy( pTempGrid )

  end subroutine routing_D8_determine_solution_order

  !------------------------------------------------------------------------------------------------

!   subroutine calculate_routing_D8( indx )

!     this%runon=0.0_c_float

!     do index=lbound( ORDER_INDEX, 1 ), ubound( ORDER_INDEX, 1 )

!       call this%calc_runoff( index )

!       if ( (    TARGET_INDEX( index ) >= lbound( ORDER_INDEX, 1) ) &
!         .and. ( TARGET_INDEX( index ) <= ubound( ORDER_INDEX, 1) ) ) then

!         this%runon( TARGET_INDEX( index ) ) = this%runoff( ORDER_INDEX( index ) )

!       endif

!     enddo

!   end subroutine calculate_routing_D8



end module routing__D8
