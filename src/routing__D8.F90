module routing__D8

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_bool
  use data_catalog
  use data_catalog_entry
  use exceptions
  implicit none

  integer (kind=c_int), allocatable :: DOWNSTREAM_CELL_INDEX(:)

  private

  public :: routing_D8_initialize, routing_D8_calculate

  type (DATA_CATALOG_ENTRY_T), pointer :: pD8_FLOWDIR    ! data catalog object => D8 flow direction grid

  integer (kind=c_int), allocatable     :: iRownum_2D(:,:)
  integer (kind=c_int), allocatable     :: iColnum_2D(:,:)
  integer (kind=c_int), allocatable     :: iTargetRow(:,:)
  integer (kind=c_int), allocatable     :: iTargetCol(:,:)
  logical (kind=c_bool), allocatable    :: lDownhillMarked(:,:)
  integer (kind=c_int), allocatable     :: iSumOfUpslopeCells(:,:)
  integer (kind=c_int), allocatable     :: iNumberOfConnections(:,:)

  integer (kind=c_int), allocatable     :: iRownum_1D(:)
  integer (kind=c_int), allocatable     :: iColnum_1D(:)

  integer (kind=c_int), parameter       :: D8_EAST         = 1
  integer (kind=c_int), parameter       :: D8_SOUTHEAST    = 2
  integer (kind=c_int), parameter       :: D8_SOUTH        = 4
  integer (kind=c_int), parameter       :: D8_SOUTHWEST    = 8
  integer (kind=c_int), parameter       :: D8_WEST         = 16
  integer (kind=c_int), parameter       :: D8_NORTHWEST    = 32
  integer (kind=c_int), parameter       :: D8_NORTH        = 64
  integer (kind=c_int), parameter       :: D8_NORTHEAST    = 128

  integer (kind=c_int), parameter       :: D8_UNDETERMINED = -999

contains

  subroutine routing_D8_initialize( lActive )

    logical (kind=c_bool), intent(in)    :: lActive(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iNX
    integer (kind=c_int)                 :: iNY
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iColnum
    integer (kind=c_int)                 :: iRownum    

    iNX = ubound(lActive, 1)
    iNY = ubound(lActive, 2)

    allocate( iRownum_2D( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( iColnum_2D( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( iTargetRow( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( iTargetCol( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )


    allocate( lDownhillMarked( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )


    allocate( iSumOfUpslopeCells( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( iNumberOfConnections( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

!     allocate( iRownum_1D( iNX * iNY ), stat=iStat )
!     call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

!     allocate( iColnum_1D( iNX * iNY ), stat=iStat )
!     call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( DOWNSTREAM_CELL_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    ! locate the data structure associated with the gridded rainfall zone entries
    pD8_FLOWDIR => DAT%find("FLOW_DIRECTION")
    if ( .not. associated(pD8_FLOWDIR) ) &
        call die("A FLOW_DIRECTION grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    call pD8_FLOWDIR%getvalues()

    call routing_D8_assign_downstream_row_col( lActive )

   	do iRownum=1, iNY
      do iColnum=1, iNX
    		iRownum_2D( iColnum, iRownum ) = iRownum
    		iColnum_2D( iColnum, iRownum ) = iColnum
      enddo
    enddo

print *, __FILE__, ": ", __LINE__
    call routing_D8_determine_solution_order( lActive )
print *, __FILE__, ": ", __LINE__
!     iRownum_1D = pack( iRownum_2D, lActive )
!     iColnum_1D = pack( iColnum_2D, lActive )


  end subroutine routing_D8_initialize

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


	    do iColnum=iCol_lbound, iCol_ubound
	    	do iRownum=iRow_lbound, iRow_ubound

          select case ( dir(iColnum, iRownum) )

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

    logical (kind=c_bool), intent(in)    :: lActive(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)  :: iRownum
    integer (kind=c_int)  :: iColnum
    integer (kind=c_int)  :: iColsrch
    integer (kind=c_int)  :: iRowsrch
    integer (kind=c_int)  :: iNumberOfChangedCells
    integer (kind=c_int)  :: iCol_lbound, iCol_ubound
    integer (kind=c_int)  :: iRow_lbound, iRow_ubound
    integer (kind=c_int)  :: iTempSum, iTempConnections
    logical (kind=c_bool) :: lAnyUnmarkedUpslopeCells 

    iCol_lbound = lbound(lActive, 1)
    iCol_ubound = ubound(lActive, 1)

    iRow_lbound = lbound(lActive, 2)
    iRow_ubound = ubound(lActive, 2)

    lDownhillMarked = lFALSE
    iSumOfUpslopeCells = 0_c_int

main_loop: do

      iNumberOfChangedCells = 0_c_int

	    do iColnum=iCol_lbound, iCol_ubound
	    	do iRownum=iRow_lbound, iRow_ubound

	        if ( .not. lActive(iColnum, iRownum) ) cycle
	        if ( lDownhillMarked(iColnum, iRownum) ) cycle

          iNumberOfChangedCells = iNumberOfChangedCells + 1
	        iTempSum = 0_c_int
	        iTempConnections = 0_c_int
	        lAnyUnmarkedUpslopeCells = lFALSE

	local_search: do iColsrch=max( iColNum-1, iCol_lbound),min( iColnum+1, iCol_ubound) 			    
	                do iRowsrch=max( iRowNum-1, iRow_lbound),min( iRownum+1, iRow_ubound) 

	 			            ! if adjacent cell points to current cell, note it and move on	
	 			            if ( ( iTargetCol(iColsrch, iRowsrch) == iColnum ) &
	 			            	.and. ( iTargetRow(iColsrch, iRowsrch) == iRownum ) ) then
	 			               			              
	 			              if ( .not. lDownhillMarked( iColsrch, iRowsrch ) ) then

	                      lAnyUnmarkedUpslopeCells = lTRUE
	 			              	exit local_search
	 			           	  
	 			           	  else

	 			                iTempSum = iTempSum + iSumOfUpslopeCells(iColsrch, iRowsrch)
	 			                iTempConnections = iTempConnections + 1 
	 			           	  
	 			           	  endif

	 			            endif

	                enddo
	              enddo local_search

	              ! OK this is the end of the local search area; did we uncover any unmarked cells
	              ! contributing flow to the current cell? if so, ignore and move on. otherwise,
	              ! mark current cell as marked, update stats, and continue with next cell

	              if ( .not. lAnyUnmarkedUpslopeCells ) then

	                iSumOfUpslopeCells(iColnum, iRownum) = iTempSum
	                iNumberOfConnections(iColnum, iRownum) = iTempConnections
	                lDownhillMarked(iColnum, iRownum) = lTRUE

	              endif

	      enddo  		
	  	enddo

      if (iNumberOfChangedCells == 0) exit main_loop

      print *, "### determining solution order... ", count( lDownhillMarked ), " cells marked so far " &
        //"out of ", count( lActive ), " active cells."

  	enddo main_loop

  end subroutine routing_D8_determine_solution_order


!--------------------------------------------------------------------------------------------------

  subroutine routing_D8_calculate


  end subroutine routing_D8_calculate

end module routing__D8