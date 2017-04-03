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

  public :: routing_D8_initialize, D8_UNDETERMINED,                                 &
            SUM_OF_UPSLOPE_CELLS, NUMBER_OF_UPSLOPE_CONNECTIONS,                    &
            get_cell_index, get_target_index, get_sort_order

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
  integer (kind=c_int), allocatable     :: SORT_ORDER_(:)
  integer (kind=c_int), allocatable     :: TARGET_INDEX_(:)

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
  ! there shouldn't be anumber_of_rows difference between ROW_INDEX, COL_INDEX, and SORT_ORDER_

contains

!
! concept:
!          ^
! +-----+--|--+-----+
! |  -> |     |  <- |    with cell indices of  1   2   3      d8 flow dirs of    1  64  16
! +-----+--^--+-----+                              4                                64
!       |  |  |
!       +-----+
!
! solution must be ordered upslope to downslope: cell 1, 3, 4, then 2 as one possible sort order
!
! iteration_index            cell_index       target_index
! -----------------         ------------     --------------
!         1                       1                2
!         2                       3                2
!         3                       4                2
!         4                       2              -9999
!
! function 'get_cell_index' simply looks up the appropriate cell index given a predefined
! cell sort order.
!
  elemental function get_cell_index( iteration_index )  result( cell_index )

    integer (kind=c_int), intent(in)    :: iteration_index
    integer (kind=c_int)                :: cell_index

    ! if D8 flow routing has not been initialized, report back the iteration index
    cell_index = iteration_index

    if ( allocated( SORT_ORDER_ ) )   cell_index = SORT_ORDER_( iteration_index )


  end function get_cell_index

!---------------------------------------------------------------------------------------------------

elemental function get_target_index( iteration_index )  result( target_index )

  integer (kind=c_int), intent(in)    :: iteration_index
  integer (kind=c_int)                :: target_index

   if ( allocated( TARGET_INDEX_ ) ) then

     target_index = TARGET_INDEX_( iteration_index )

  else

    target_index = -9999

  endif

end function get_target_index

!---------------------------------------------------------------------------------------------------

  elemental function get_sort_order( cell_index )  result( iteration_index )

    integer (kind=c_int), intent(in)    :: cell_index
    integer (kind=c_int)                :: iteration_index

    ! [ LOCALS ]
    integer (kind=c_int)  :: current_cell_index
    logical (kind=c_bool) :: found_match

    found_match = FALSE

    if ( allocated( SORT_ORDER_ ) ) then

      do iteration_index=1, ubound( SORT_ORDER_, 1 )

         current_cell_index = SORT_ORDER_( iteration_index )
         if ( current_cell_index == cell_index ) then
           found_match = TRUE
          exit
        endif

      enddo

    endif

    ! if this function is called and routing is turned off, the 'sort_order'
    ! will be the same as the cell_index
    if ( .not. found_match )  iteration_index = cell_index

  end function get_sort_order

!---------------------------------------------------------------------------------------------------

  subroutine routing_D8_initialize( lActive, sort_order )

    logical (kind=c_bool), intent(in)    :: lActive(:,:)
    integer (kind=c_int), intent(inout)  :: sort_order(:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: number_of_cols
    integer (kind=c_int)                 :: number_of_rows
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: column_num
    integer (kind=c_int)                 :: row_num
    integer (kind=c_int)                 :: iteration_index
    integer (kind=c_int)                 :: iCount
    character (len=256)                  :: sBuf

    integer (kind=c_int) :: col_lbound, col_ubound
    integer (kind=c_int) :: row_lbound, row_ubound
    integer (kind=c_int) :: cell_index, target_index
    integer (kind=c_int) :: iUnitNum

    type (GENERAL_GRID_T), pointer  :: pTempGrid

    col_lbound = lbound(lActive, 1)
    col_ubound = ubound(lActive, 1)

    row_lbound = lbound(lActive, 2)
    row_ubound = ubound(lActive, 2)

    iCount = 0

    number_of_cols = ubound(lActive, 1)
    number_of_rows = ubound(lActive, 2)

    allocate( TARGET_ROW( number_of_cols, number_of_rows ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( TARGET_COL( number_of_cols, number_of_rows ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )


    allocate( IS_DOWNSLOPE_TARGET_MARKED( number_of_cols, number_of_rows ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( COL2D( number_of_cols, number_of_rows ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( ROW2D( number_of_cols, number_of_rows ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( COL1D( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( ROW1D( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( SUM_OF_UPSLOPE_CELLS( number_of_cols, number_of_rows ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( NUMBER_OF_UPSLOPE_CONNECTIONS( number_of_cols, number_of_rows ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( ROW_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( COLUMN_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( TARGET_INDEX_( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( SORT_ORDER_( count( lActive) ), stat=iStat )
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

    do row_num=row_lbound, row_ubound
      do column_num=col_lbound, col_ubound

        COL2D( column_num, row_num ) = column_num   ! right-most index should vary slowest
        ROW2D( column_num, row_num ) = row_num

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

    write(iUnitNum,*) "Sort_Order"//TAB//"CELL_INDEX"//TAB//"TARGET_INDEX"//TAB//"From_COL"    &
      // TAB//"From_ROW"//TAB//"To_COL"//TAB//"To_ROW"//TAB//"D8_flowdir"//TAB                     &
      //"Num_Adjacent_Upslope_Connections"//TAB//"Sum_of_Upslope_Contributing_Cells"

     ! solution order has been determined; remaining code simply writes a summary to a
     ! file for further analysis
    do iteration_index = 1, ubound(COL1D,1)

      sBuf = ""

      cell_index = get_cell_index( iteration_index )
      target_index = get_target_index( iteration_index )

      sort_order(iteration_index) = cell_index

      associate(                                                               &
        Rownum => ROW1D( cell_index ),                                         &
        Colnum => COL1D( cell_index ),                                         &
        Target_row => ROW1D( target_index ),                                   &
        Target_col => COL1D( target_index ),                                   &
        D8_flowdir => pD8_FLOWDIR%pGrdBase%iData( COL1D( cell_index ),         &
                        ROW1D( cell_index ) ),                                 &
        Num_adjacent => NUMBER_OF_UPSLOPE_CONNECTIONS( COL1D( cell_index ),    &
                        ROW1D( cell_index ) ),                                 &
        Sum_upslope => SUM_OF_UPSLOPE_CELLS( COL1D( cell_index ),              &
                        ROW1D( cell_index ) )  )

        write(sBuf,*)  iteration_index, TAB, cell_index, TAB, target_index
        write(sBuf,*) trim(sBuf)//TAB//asCharacter( Colnum )//TAB              &
          //asCharacter( Rownum )

        if ( get_target_index( iteration_index ) > 0 )                         &
          write(sBuf,*) trim(sBuf)//TAB//asCharacter( Target_col )//TAB        &
          //asCharacter( Target_row )//TAB//asCharacter( D8_flowdir )//TAB     &
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
    integer (kind=c_int) :: row_num
    integer (kind=c_int) :: column_num
    integer (kind=c_int) :: col_lbound, col_ubound
    integer (kind=c_int) :: row_lbound, row_ubound

    col_lbound = lbound(lActive, 1)
    col_ubound = ubound(lActive, 1)

    row_lbound = lbound(lActive, 2)
    row_ubound = ubound(lActive, 2)

    associate ( dir => pD8_FLOWDIR%pGrdBase%iData )

	   	do row_num=row_lbound, row_ubound
        do column_num=col_lbound, col_ubound

          select case ( dir( column_num, row_num ) )

            case ( D8_EAST )

              TARGET_COL(column_num, row_num) = column_num + 1
              TARGET_ROW(column_num, row_num) = row_num

            case ( D8_SOUTHEAST )

              TARGET_COL(column_num, row_num) = column_num + 1
              TARGET_ROW(column_num, row_num) = row_num + 1

            case ( D8_SOUTH )

              TARGET_COL(column_num, row_num) = column_num
              TARGET_ROW(column_num, row_num) = row_num + 1

            case ( D8_SOUTHWEST )

              TARGET_COL(column_num, row_num) = column_num - 1
              TARGET_ROW(column_num, row_num) = row_num + 1

            case ( D8_WEST )

              TARGET_COL(column_num, row_num) = column_num - 1
              TARGET_ROW(column_num, row_num) = row_num

            case ( D8_NORTHWEST )

              TARGET_COL(column_num, row_num) = column_num - 1
              TARGET_ROW(column_num, row_num) = row_num - 1

            case ( D8_NORTH )

              TARGET_COL(column_num, row_num) = column_num
              TARGET_ROW(column_num, row_num) = row_num - 1

            case ( D8_NORTHEAST )

              TARGET_COL(column_num, row_num) = column_num + 1
              TARGET_ROW(column_num, row_num) = row_num - 1

            case default

              TARGET_COL(column_num, row_num) = D8_UNDETERMINED
              TARGET_ROW(column_num, row_num) = D8_UNDETERMINED

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
    integer (kind=c_int)  :: row_num
    integer (kind=c_int)  :: column_num
    integer (kind=c_int)  :: iColsrch
    integer (kind=c_int)  :: iRowsrch
    integer (kind=c_int)  :: iNumberOfChangedCells
    integer (kind=c_int)  :: col_lbound, col_ubound
    integer (kind=c_int)  :: row_lbound, row_ubound
    integer (kind=c_int)  :: iUpslopeSum, iUpslopeConnections
    logical (kind=c_bool) :: are_there_unmarked_upslope_cells
    logical (kind=c_bool) :: lCircular
    integer (kind=c_int)  :: iNumberRemaining
    integer (kind=c_int)  :: indx, k, iCount
    integer (kind=c_int)  :: num_cells_marked_this_iteration
    integer (kind=c_int)  :: iPasses
    integer (kind=c_int)  :: iPassesWithoutChange
    type (GENERAL_GRID_T), pointer  :: pTempGrid

    col_lbound = lbound(lActive, 1)
    col_ubound = ubound(lActive, 1)

    row_lbound = lbound(lActive, 2)
    row_ubound = ubound(lActive, 2)

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

      ! iterate over entire model domain
	    do row_num=row_lbound, row_ubound
        do column_num=col_lbound, col_ubound

	        if ( .not. lActive(column_num, row_num) ) cycle

          ! we've already determined the identiy of the downslope cell; skip this one
	        if ( IS_DOWNSLOPE_TARGET_MARKED(column_num, row_num) ) cycle

          iNumberOfChangedCells = iNumberOfChangedCells + 1
	        iUpslopeSum = 0_c_int
	        iUpslopeConnections = 0_c_int
	        are_there_unmarked_upslope_cells = lFALSE
          lCircular = lFALSE

          ! search the 8 cells immediately adjacent to the current cell
	local_search: do iRowsrch=max( row_num-1, row_lbound),min( row_num+1, row_ubound)
                  do iColsrch=max( column_num-1, col_lbound),min( column_num+1, col_ubound)

	 			            ! if adjacent cell points to current cell, determine what to do with runoff
	 			            if ( ( TARGET_COL(iColsrch, iRowsrch) == column_num ) &
	 			            	.and. ( TARGET_ROW(iColsrch, iRowsrch) == row_num ) ) then

                      ! if adjacent cell falls outside the area of active cells,
                      ! ignore it and move on
                      if ( .not. lActive( iColsrch, iRowsrch ) )  cycle

                      ! if the target of the current cell points back at the adjacent
                      ! cell, mark current cell as having a "circular" connection
                      if ( ( TARGET_COL( column_num, row_num ) == iColsrch )                &
                        .and. ( TARGET_ROW( column_num, row_num ) == iRowsrch ) )  lCircular = lTRUE

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

                        are_there_unmarked_upslope_cells = lTRUE

	 			           	  endif

	 			            endif

	                enddo
	              enddo local_search

	              ! OK this is the end of the local search area; did we uncover anumber_of_rows unmarked cells
	              ! contributing flow to the current cell? if so, ignore and move on. otherwise,
	              ! mark current cell as marked, update stats, and continue with next cell

	              if ( .not. are_there_unmarked_upslope_cells ) then
!                  .or. (iUpslopeConnections == 1 .and. lCircular ) ) then

                  num_cells_marked_this_iteration = num_cells_marked_this_iteration + 1
                  indx = indx + 1

                  ! need to add one to the sum already tabulated to account for the
                  ! current cell as well
	                SUM_OF_UPSLOPE_CELLS( column_num, row_num ) = iUpslopeSum + 1
	                NUMBER_OF_UPSLOPE_CONNECTIONS( column_num, row_num ) = iUpslopeConnections
	                IS_DOWNSLOPE_TARGET_MARKED( column_num, row_num ) = lTRUE
                  COLUMN_INDEX( indx )  = column_num
                  ROW_INDEX( indx )     = row_num
                  SORT_ORDER_( indx )    = routing_D8_get_index( column_num, row_num )
                  TARGET_INDEX_( indx )  = routing_D8_get_index( TARGET_COL( column_num, row_num ), &
                                                                                   TARGET_ROW( column_num, row_num ) )

                  ! print *, "   ===> assigning order number: "
                  ! print *, "        indx                =",indx
                  ! print *, "        sort_order                =", SORT_ORDER_( indx )
                  ! print *, "        target_index              =", TARGET_INDEX_( indx )
                  ! print *, "        target_index(SORT_ORDER_) =", TARGET_INDEX_( SORT_ORDER_( indx ) )

                  if ( lCircular )  TARGET_INDEX_( SORT_ORDER_( indx ) ) = D8_UNDETERMINED

                ! if cells are not being marked, record cell as D8_UNDETERMINED
                ! and move on
                elseif ( iPassesWithoutChange > 10 ) then

                  num_cells_marked_this_iteration = num_cells_marked_this_iteration + 1
                  indx = indx + 1

                  SUM_OF_UPSLOPE_CELLS( column_num, row_num ) = iUpslopeSum
                  NUMBER_OF_UPSLOPE_CONNECTIONS( column_num, row_num ) = iUpslopeConnections
                  IS_DOWNSLOPE_TARGET_MARKED( column_num, row_num ) = lTRUE
                  COLUMN_INDEX( indx ) = column_num
                  ROW_INDEX( indx ) = row_num
                  SORT_ORDER_( indx ) = routing_D8_get_index( column_num, row_num )

                  TARGET_INDEX_( SORT_ORDER_( indx ) ) = D8_UNDETERMINED

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
  !     if ( (    TARGET_INDEX_( indx ) >= lbound( SORT_ORDER_, 1) ) &
  !       .and. ( TARGET_INDEX_( indx ) <= ubound( SORT_ORDER_, 1) ) ) then
  !
  !       this%runon( TARGET_INDEX_( index ) ) = this%runoff( SORT_ORDER_( index ) )
  !
  !     endif
  !
  !   enddo
  !
  ! end subroutine calculate_routing_D8

end module routing__D8
