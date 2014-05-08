
  type, public :: LOOKUP_TABLE_T

    type (LOOKUP_TABLE_DATA_T), allocatable    :: data(:)
    
  contains

    procedure :: read_lookup_table_sub
    generic :: readfile => read_lookup_table_sub
 
    procedure :: map_columns_to_fields_sub
    generic :: map => map_columns_to_fields_sub

  end type LOOKUP_TABLE_T

  type (DATA_FRAME_T) :: DF
  type (DATA_FILE_T)  :: FILE
  
contains

!--------------------------------------------------------------------------------------------------

  subroutine read_lookup_table_sub(this, sFilename, sCommentChars, sDelimiters)

    class (LOOKUP_TABLE_T) :: this
    character (len=*), intent(in)  :: sFilename
    character (len=*), intent(in)  :: sCommentChars
    character (len=*), intent(in)  :: sDelimiters
    
    ! [ LOCALS ]
    character (len=MAX_STR_LEN) :: sBuf
    integer (kind=c_int)        :: iCount

    call FILE%open(sFilename = sFilename,    &
                   sCommentChars = sCommentChars,      &
                   sDelimiters = sDelimiters )

    FILE%slColNames = FILE%readHeader()

    iCount = FILE%slColNames%count

    call DF%initialize( slColNames=FILE%slColNames, &
        iRecordCount = FILE%iNumberOfRecords )

    do while (FILE%isOpen() )

      sBuf = FILE%readLine()

      call DF%putrow( sBuf, sDelimiters )

    enddo

  end subroutine read_lookup_table_sub

!--------------------------------------------------------------------------------------------------

  subroutine map_columns_to_fields_sub(this)

    class (LOOKUP_TABLE_T)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex 
    integer (kind=c_int) :: iCount
    integer (kind=c_int) :: iStat
    type (T_DATA_COLUMN), pointer :: pCOL
    real (kind=c_float), allocatable   :: rValues(:)
    integer (kind=c_int), allocatable  :: iValues(:)

    iCount = FILE%iNumberOfRecords

    if (.not. allocated( this%data) ) then
      allocate( this%data( iCount ), stat=iStat)
      if( iStat /= 0)  call die("Failed to allocate memory for data table.")
    endif        


    pCOL => DF%getcol("RZ_1")

    if (associated(pCOL) ) then

      iCount = pCol%count()

      rValues = pCOL%getColumnFloatVals()

      print *, rValues

    endif  


    pCOL => DF%getcol("CN_1")

    if (associated(pCOL) ) then

      iCount = pCol%count()

      iValues = pCOL%getColumnIntVals()

      print *, iValues

    endif  


  end subroutine map_columns_to_fields_sub  
