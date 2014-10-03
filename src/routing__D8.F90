module routing__D8

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_bool
  use data_catalog
  use data_catalog_entry
  use exceptions
  implicit none

  integer (kind=c_int), pointer :: DOWNSTREAM_CELL(:)

  private

  type (DATA_CATALOG_ENTRY_T), pointer :: pD8_FLOWDIR    ! data catalog object => D8 flow direction grid

  integer (kind=c_int), allocatable     :: iRownum_2D(:,:)
  integer (kind=c_int), allocatable     :: iColnum_2D(:,:)
  integer (kind=c_int), allocatable     :: iTargetRow(:,:)
  integer (kind=c_int), allocatable     :: iTargetCol(:,:)

  integer (kind=c_int), allocatable     :: iRownum_1D(:)
  integer (kind=c_int), allocatable     :: iColnum_1D(:)

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

    allocate( iRownum_1D( iNX * iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( iRownum_1D( iNX * iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    ! locate the data structure associated with the gridded rainfall zone entries
    pD8_FLOWDIR => DAT%find("FLOW_DIRECTION")
    if ( .not. associated(pD8_FLOWDIR) ) &
        call die("A FLOW_DIRECTION grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    call pD8_FLOWDIR%getvalues()

   	do iRownum=1, iNY
      do iColnum=1, iNX
    		iRownum_2D( iColnum, iRownum ) = iRownum
    		iColnum_2D( iColnum, iRownum ) = iColnum
      enddo
    enddo

    iRownum_1D = pack( iRownum_2D, lActive )
    iColnum_1D = pack( iColnum_2D, lActive )


  end subroutine routing_D8_initialize

!--------------------------------------------------------------------------------------------------

  subroutine routing_D8_calculate


  end subroutine routing_D8_calculate

end module routing__D8