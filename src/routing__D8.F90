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

  public :: routing_D8_initialize, D8_UNDETERMINED, TARGET_INDEX, SORT_INDEX,    &
            SUM_OF_UPSLOPE_CELLS, NUMBER_OF_UPSLOPE_CONNECTIONS

  type (DATA_CATALOG_ENTRY_T), pointer :: pD8_FLOWDIR    ! data catalog object => D8 flow direction grid

  integer (kind=c_int), allocatable     :: TARGET_ROW(:,:)
  integer (kind=c_int), allocatable     :: TARGET_COL(:,:)
  logical (kind=c_bool), allocatable    :: IS_DOWNSLOPE_TARGET_MARKED(:,:)
  integer (kind=c_int), allocatable     :: SUM_OF_UPSLOPE_CELLS(:,:)
  integer (kind=c_int), allocatable     :: NUMBER_OF_UPSLOPE_CONNECTIONS(:,:)

  !> @TODO remove redundant data elements; row1d, col1d, etc. are now also stored in the model data structure.

  integer (kind=c_int), allocatable     :: ROW2D(:,:)
  integer (kind=c_int), allocatable     :: COL2D(:,:)
  integer (kind=c_int), allocatable     :: ROW1D(:)
  integer (kind=c_int), allocatable     :: COL1D(:)

  integer (kind=c_int), allocatable     :: ROW_INDEX(:)
  integer (kind=c_int), allocatable     :: COLUMN_INDEX(:)
  integer (kind=c_int), allocatable     :: SORT_INDEX(:)
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
  ! there shouldn't be any difference between ROW_INDEX, COL_INDEX, and SORT_INDEX

contains

  subroutine routing_D8_initialize( lActive, sort_order )

    logical (kind=c_bool), intent(in)    :: lActive(:,:)
    integer (kind=c_int), intent(inout)  :: sort_order(:)

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

    allocate( TARGET_ROW( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( TARGET_COL( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )


    allocate( IS_DOWNSLOPE_TARGET_MARKED( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( COL2D( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( ROW2D( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( COL1D( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( ROW1D( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( SUM_OF_UPSLOPE_CELLS( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( NUMBER_OF_UPSLOPE_CONNECTIONS( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( ROW_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( COLUMN_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( TARGET_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( SORT_INDEX( count( lActive) ), stat=iStat )
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

    ! create 1D vectors of column and row numbers for full grid
    COL1D = pack( COL2D, lActive )
    ROW1D = pack( ROW2D, lActive )

    call routing_D8_determine_solution_order( lActive )

    pTempGrid%iData = TARGET_ROW
    call grid_WriteArcGrid("D8_TARGET_ROW_Grid.asc", pTempGrid)

    pTempGrid%iData = TARGET_COL
    call grid_WriteArcGrid("D8_TARGET_COL_Grid.asc", pTempGrid)

    open( newunit=iUnitNum, file=trim(OUTPUT_DIRECTORY_NAME)//trim("D8_routing_table.txt"),   &
      iostat=iStat, status="REPLACE")

    write(iUnitNum,*) "sort_order"//TAB//"SORT_INDEX"//TAB//"TARGET_INDEX"//TAB//"From_COL"    &
      // TAB//"From_ROW"//TAB//"To_COL"//TAB//"To_ROW"//TAB//"D8_flowdir"//TAB                     &
      //"Num_Adjacent_Upslope_Connections"//TAB//"Sum_of_Upslope_Contributing_Cells"

     ! solution order has been determined; remaining code simply writes a summary to a
     ! file for further analysis
    do iIndex = 1, ubound(COL1D,1)

      sBuf = ""

      associate( cell_sort_order => SORT_INDEX( iIndex ),                                        &
                 cell_target_index => TARGET_INDEX( SORT_INDEX( iIndex ) ),                                      &
                 Rownum => ROW1D( SORT_INDEX( iIndex ) ),                                    &
                 Colnum => COL1D( SORT_INDEX( iIndex ) ),                                    &
                 Target_row => ROW1D( TARGET_INDEX( iIndex ) ),                                &
                 Target_col => COL1D( TARGET_INDEX( iIndex ) ),                                    &
                 D8_flowdir => pD8_FLOWDIR%pGrdBase%iData( COL1D( SORT_INDEX( iIndex ) ),    &
                                  ROW1D( SORT_INDEX( iIndex ) ) ),                           &
                 Num_adjacent => NUMBER_OF_UPSLOPE_CONNECTIONS( COL1D( SORT_INDEX( iIndex ) ), &
                                     ROW1D( SORT_INDEX( iIndex ) ) ),                        &
                 Sum_upslope => SUM_OF_UPSLOPE_CELLS( COL1D( SORT_INDEX( iIndex ) ),           &
                                     ROW1D( SORT_INDEX( iIndex ) ) )  )

        write(sBuf,*)  iIndex, TAB, cell_sort_order, TAB, cell_target_index
        write(sBuf,*) trim(sBuf)//TAB//asCharacter( Colnum )//TAB &
          //asCharacter( Rownum )

        if ( cell_target_index > 0 ) &
          write(sBuf,*) trim(sBuf)//TAB//asCharacter( Target_col )//TAB &
          //asCharacter( Target_row )//TAB//asCharacter( D8_flowdir )//TAB &
          //asCharacter( Num_adjacent )//TAB//asCharacter( Sum_upslope )

        write(iUnitNum,*)  trim(sBuf)

      end associate

    enddo

    close ( iUnitNum )

  end subroutine routing_D8_initialize

!--------------------------------------------------------------------------------------------------

  function routing_D8_get_index( iCol, iRow )   result( cell_index )

    integer (kind=c_int), intent(in)   :: iCol
    integer (kind=c_int), intent(in)   :: iRow
    integer (kind=c_int)               :: cell_index

    ! [ LOCALS ]
    integer (kind=c_int)   :: iIndex
    logical (kind=c_bool)  :: lFound

    iIndex        = -9999
    cell_index    = -9999
    lFound        = lFALSE

    ! iterate over 1D vector of column numbers
    do iIndex = lbound(COL1D,1), ubound(COL1D,1)

      if( COL1D( iIndex ) == iCol  .and.  ROW1D( iIndex ) == iRow ) then
      	cell_index = iIndex
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

              TARGET_COL(iColnum, iRownum) = iColnum + 1
              TARGET_ROW(iColnum, iRownum) = iRownum

            case ( D8_SOUTHEAST )

              TARGET_COL(iColnum, iRownum) = iColnum + 1
              TARGET_ROW(iColnum, iRownum) = iRownum + 1

            case ( D8_SOUTH )

              TARGET_COL(iColnum, iRownum) = iColnum
              TARGET_ROW(iColnum, iRownum) = iRownum + 1

            case ( D8_SOUTHWEST )

              TARGET_COL(iColnum, iRownum) = iColnum - 1
              TARGET_ROW(iColnum, iRownum) = iRownum + 1

            case ( D8_WEST )

              TARGET_COL(iColnum, iRownum) = iColnum - 1
              TARGET_ROW(iColnum, iRownum) = iRownum

            case ( D8_NORTHWEST )

              TARGET_COL(iColnum, iRownum) = iColnum - 1
              TARGET_ROW(iColnum, iRownum) = iRownum - 1

            case ( D8_NORTH )

              TARGET_COL(iColnum, iRownum) = iColnum
              TARGET_ROW(iColnum, iRownum) = iRownum - 1

            case ( D8_NORTHEAST )

              TARGET_COL(iColnum, iRownum) = iColnum + 1
              TARGET_ROW(iColnum, iRownum) = iRownum - 1

            case default

              TARGET_COL(iColnum, iRownum) = D8_UNDETERMINED
              TARGET_ROW(iColnum, iRownum) = D8_UNDETERMINED

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
    integer (kind=c_int)  :: indx, k, iCount
    integer (kind=c_int)  :: num_cells_marked_this_iteration
    integer (kind=c_int)  :: iPasses
    integer (kind=c_int)  :: iPassesWithoutChange
    type (GENERAL_GRID_T), pointer  :: pTempGrid

    iCol_lbound = lbound(lActive, 1)
    iCol_ubound = ubound(lActive, 1)

    iRow_lbound = lbound(lActive, 2)
    iRow_ubound = ubound(lActive, 2)

    IS_DOWNSLOPE_TARGET_MARKED = lFALSE
    SUM_OF_UPSLOPE_CELLS = 0_c_int

    indx = 0
    iPasses = 0
    iPassesWithoutChange = 0

    pTempGrid=>grid_Create( BNDS%iNumCols, BNDS%iNumRows, BNDS%fX_ll, BNDS%fY_ll, &
      BNDS%fX_ur, BNDS%fY_ur, DATATYPE_INT )


main_loop: do

      iNumberOfChangedCells = 0_c_int
      num_cells_marked_this_iteration = 0_c_int
      iPasses = iPasses + 1

      print *, "**** PASS NUMBER "//asCharacter(iPasses)//" ****"

      ! iterate over entire model domain
	    do iRownum=iRow_lbound, iRow_ubound
        do iColnum=iCol_lbound, iCol_ubound

          print *, "cell ("//asCharacter(iRowNum)//", "//asCharacter(iColnum), ") is marked? ", IS_DOWNSLOPE_TARGET_MARKED( iColNum, iRowNum )

	        if ( .not. lActive(iColnum, iRownum) ) cycle

          ! we've already determined the identiy of the downslope cell; skip this one
	        if ( IS_DOWNSLOPE_TARGET_MARKED(iColnum, iRownum) ) cycle

          iNumberOfChangedCells = iNumberOfChangedCells + 1
	        iUpslopeSum = 0_c_int
	        iUpslopeConnections = 0_c_int
	        lAnyUnmarkedUpslopeCells = lFALSE
          lCircular = lFALSE

          ! search the 8 cells immediately adjacent to the current cell
	local_search: do iRowsrch=max( iRowNum-1, iRow_lbound),min( iRownum+1, iRow_ubound)
                  do iColsrch=max( iColNum-1, iCol_lbound),min( iColnum+1, iCol_ubound)

	 			            ! if adjacent cell points to current cell, determine what to do with runoff
	 			            if ( ( TARGET_COL(iColsrch, iRowsrch) == iColnum ) &
	 			            	.and. ( TARGET_ROW(iColsrch, iRowsrch) == iRownum ) ) then

                      ! if adjacent cell falls outside the area of active cells,
                      ! ignore it and move on
                      if ( .not. lActive( iColsrch, iRowsrch ) )  cycle

                      ! if the target of the current cell points back at the adjacent
                      ! cell, mark current cell as having a "circular" connection
                      if ( ( TARGET_COL( iColNum, iRowNum ) == iColsrch )                &
                        .and. ( TARGET_ROW( iColNum, iRowNum ) == iRowsrch ) )  lCircular = lTRUE

                      ! if the adjacent cell is marked (that is, has no unresolved
                      ! upslope contributions), add the 1 to the number of connections
                      ! and add the adjacent cells' sum of upslope cells to the
                      ! current cells' running sum of upslope cells
	 			              if(	IS_DOWNSLOPE_TARGET_MARKED( iColsrch, iRowsrch ) ) then

                        iUpslopeSum = iUpslopeSum + SUM_OF_UPSLOPE_CELLS(iColsrch, iRowsrch)
                        iUpslopeConnections = iUpslopeConnections + 1

                      ! add number of upslope cells and connections to temporary variables
                      ! if we get to the end of the search and find no unmarked adjacent cells,
                      ! we have determined the number of upslope contributing cells and will
                      ! be able to mark the current cell
	 			           	  else

                        lAnyUnmarkedUpslopeCells = lTRUE

	 			           	  endif

                      print *, "  ==> found adjacent cell that targets current cell"
                      print *, "    searching adjacent cell: ("//asCharacter(iColsrch)  &
                         //", "//asCharacter(iRowsrch)//")"
                      print *, "    adjacent cell points to current cell and is marked: ", &
                              IS_DOWNSLOPE_TARGET_MARKED( iColsrch, iRowsrch )
                      print *, "    circular connection? ", lCircular
                      print *, "    any unmarked upslope cells? ", lAnyUnmarkedUpslopeCells

	 			            endif

	                enddo
	              enddo local_search

	              ! OK this is the end of the local search area; did we uncover any unmarked cells
	              ! contributing flow to the current cell? if so, ignore and move on. otherwise,
	              ! mark current cell as marked, update stats, and continue with next cell

	              if ( .not. lAnyUnmarkedUpslopeCells ) then
!                  .or. (iUpslopeConnections == 1 .and. lCircular ) ) then

                  num_cells_marked_this_iteration = num_cells_marked_this_iteration + 1
                  indx = indx + 1

                  ! need to add one to the sum already tabulated to account for the
                  ! current cell as well
	                SUM_OF_UPSLOPE_CELLS( iColnum, iRownum ) = iUpslopeSum + 1
	                NUMBER_OF_UPSLOPE_CONNECTIONS( iColnum, iRownum ) = iUpslopeConnections
	                IS_DOWNSLOPE_TARGET_MARKED( iColnum, iRownum ) = lTRUE
                  COLUMN_INDEX( indx )  = iColnum
                  ROW_INDEX( indx )     = iRownum
                  SORT_INDEX( indx )    = routing_D8_get_index( iColnum, iRownum )
                  TARGET_INDEX( indx )  = routing_D8_get_index( TARGET_COL( iColNum, iRowNum ), &
                                                                                   TARGET_ROW( iColNum, iRowNum ) )

                  print *, "   ===> assigning order number: "
                  print *, "        indx                =",indx
                  print *, "        sort_order                =", SORT_INDEX( indx )
                  print *, "        target_index              =", TARGET_INDEX( indx )
                  print *, "        target_index(SORT_INDEX) =", TARGET_INDEX( SORT_INDEX( indx ) )

                  if ( lCircular )  TARGET_INDEX( SORT_INDEX( indx ) ) = D8_UNDETERMINED

                ! if cells are not being marked, record cell as D8_UNDETERMINED
                ! and move on
                elseif ( iPassesWithoutChange > 10 ) then

                  num_cells_marked_this_iteration = num_cells_marked_this_iteration + 1
                  indx = indx + 1

                  SUM_OF_UPSLOPE_CELLS( iColnum, iRownum ) = iUpslopeSum
                  NUMBER_OF_UPSLOPE_CONNECTIONS( iColnum, iRownum ) = iUpslopeConnections
                  IS_DOWNSLOPE_TARGET_MARKED( iColnum, iRownum ) = lTRUE
                  COLUMN_INDEX( indx ) = iColnum
                  ROW_INDEX( indx ) = iRownum
                  SORT_INDEX( indx ) = routing_D8_get_index( iColnum, iRownum )

                  TARGET_INDEX( SORT_INDEX( indx ) ) = D8_UNDETERMINED

                endif

	      enddo
	  	enddo

!      if (iNumberOfChangedCells == 0) exit main_loop

      iNumberRemaining = count( lActive ) - count( IS_DOWNSLOPE_TARGET_MARKED )

      if ( iNumberRemaining==0 )   exit main_loop

      print *, "### determining solution order... ", count( IS_DOWNSLOPE_TARGET_MARKED ), " cells marked so far " &
        //"out of ", count( lActive ), " active cells."


      if ( num_cells_marked_this_iteration==0 ) then

        iPassesWithoutChange = iPassesWithoutChange + 1

        where( IS_DOWNSLOPE_TARGET_MARKED .or.  ( .not. lActive ) )
          pTempGrid%iData = -1
        elsewhere
          pTempGrid%iData = pD8_FLOWDIR%pGrdBase%iData
        end where

        write(*,"(/,1x,'Summary of remaining unmarked cells')")

        ! loop over possible (legal) values of the flow direction grid
        do k=0,128
          iCount=COUNT( .not. IS_DOWNSLOPE_TARGET_MARKED                         &
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

    pTempGrid%iData = SUM_OF_UPSLOPE_CELLS
    call grid_WriteArcGrid("D8_Flow_Routing__Upslope_Contributing_Area.asc", pTempGrid)

    pTempGrid%iData = NUMBER_OF_UPSLOPE_CONNECTIONS
    call grid_WriteArcGrid("D8_Flow_Routing__Number_of_Adjacent_Upslope_Connections.asc", pTempGrid)

    call grid_Destroy( pTempGrid )

  end subroutine routing_D8_determine_solution_order

  !------------------------------------------------------------------------------------------------

  ! subroutine calculate_routing_D8( indx )
  !
  !     if ( (    TARGET_INDEX( indx ) >= lbound( SORT_INDEX, 1) ) &
  !       .and. ( TARGET_INDEX( indx ) <= ubound( SORT_INDEX, 1) ) ) then
  !
  !       this%runon( TARGET_INDEX( index ) ) = this%runoff( SORT_INDEX( index ) )
  !
  !     endif
  !
  !   enddo
  !
  ! end subroutine calculate_routing_D8

end module routing__D8
