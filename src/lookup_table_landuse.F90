module lookup_table_landuse

  use iso_c_binding, only : c_int, c_bool, c_short, c_float, c_double
  use data_file
  use data_column
  use data_frame
  use constants_and_conversions, only : sTAB
  use strings
  use string_list
  use types_new
  implicit none

  private

  type, public :: TABLE_RECORD_LANDUSE_T

  	integer (kind=c_short)              :: iLandUseType
    integer (kind=c_short)              :: iSoilsGroup
    character (len=:), allocatable      :: sLandUseDescription
  	character (len=:), allocatable      :: sAssumedPercentImperviousness
    real (kind=c_float)                 :: rCN_base
    real (kind=c_float)                 :: rRootingDepth
    real (kind=c_float)                 :: rMaxInfiltration
  	real (kind=c_float)                 :: rIntercept_GrowingSeason
  	real (kind=c_float)                 :: rIntercept_NonGrowingSeason

  contains

    procedure :: initialize_column_headings_sub
    generic :: initialize => initialize_column_headings_sub

    procedure :: read_landuse_lookup_table_sub
    generic :: readfile => read_landuse_lookup_table_sub
 
    procedure :: map_columns_to_fields_sub
    generic :: map => map_columns_to_fields_sub

  end type TABLE_RECORD_LANDUSE_T

  type (DATA_FRAME_T) :: LU_DF
  type (DATA_FILE_T) :: LU_FILE

  public :: LU  
  type (TABLE_RECORD_LANDUSE_T) :: LU

contains

  subroutine initialize_column_headings_sub(this, iNumRecords)

    class (TABLE_RECORD_LANDUSE_T)     :: this 
    integer (kind=c_int), intent(in)   :: iNumRecords 

     ! [ LOCALS ]
     type (STRING_LIST_T)   :: stl

     call stl%append("LU_type")
     call stl%append("LU_description")
     call stl%append("LU_pct_imperviousness")
     call stl%append("LU_interception_grow")
     call stl%append("LU_interception_nongrow")

     call LU_DF%initialize( slColNames=stl, &
        iDataTypes = [ INTEGER_DATA, STRING_DATA, STRING_DATA, FLOAT_DATA, FLOAT_DATA ], &
        iRecordCount = iNumRecords )


  end subroutine initialize_column_headings_sub

!--------------------------------------------------------------------------------------------------

  subroutine read_landuse_lookup_table_sub(this, sFilename)

    class (TABLE_RECORD_LANDUSE_T) :: this
    character (len=*), intent(in)  :: sFilename
    
    ! [ LOCALS ]
    character (len=MAX_STR_LEN) :: sBuf
    integer (kind=c_int)        :: iCount

    call LU_FILE%open(sFilename = sFilename,    &
                      sCommentChars = "#",      &
                      sDelimiters = sTAB )

    LU_FILE%slColNames = LU_FILE%readHeader()

    iCount = LU_FILE%slColNames%count

    call LU_DF%initialize( slColNames=LU_FILE%slColNames, &
        iRecordCount = LU_FILE%iNumberOfRecords )

    do while (LU_FILE%isOpen() )

      sBuf = LU_FILE%readLine()

      call LU_DF%putrow( sBuf, sTAB )

    enddo


    call LU_DF%summarize()


  end subroutine read_landuse_lookup_table_sub

!--------------------------------------------------------------------------------------------------

  subroutine map_columns_to_fields_sub(this)

    class (TABLE_RECORD_LANDUSE_T)  :: this

    


  end subroutine map_columns_to_fields_sub  

end module lookup_table_landuse