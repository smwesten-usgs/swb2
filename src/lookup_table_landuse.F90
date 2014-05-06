module lookup_table_landuse

  use iso_c_binding, only : c_int, c_bool, c_short, c_float, c_double
  use data_file
  use data_frame
  use strings
  use string_list
  implicit none

  private

  type, public :: TABLE_RECORD_LANDUSE_T

  	integer (kind=c_int)                :: iLandUseType
    character (len=:), allocatable      :: sLandUseDescription
  	character (len=:), allocatable      :: sAssumedPercentImperviousness
  	real (kind=c_float)                 :: rIntercept_GrowingSeason
  	real (kind=c_float)                 :: rIntercept_NonGrowingSeason



  contains

    procedure :: initialize_column_headings_sub
    generic :: initialize => initialize_column_headings_sub

    procedure :: read_landuse_lookup_table_sub
    generic :: readfile => read_landuse_lookup_table_sub
 
    procedure :: map_columns_to_fields_sub
    generic :: map => map_columns_to_fields_sub

  end type LOOKUP_TABLE_LANDUSE_T

  type (T_DATA_FRAME) :: LU

contains

  subroutine initialize_column_headings_sub(this, iNumRecords)

    type (TABLE_RECORD_LANDUSE_T)     :: this 
    integer (kind=c_int), intent(in)  :: iNumRecords 

     ! [ LOCALS ]
     type (STRING_LIST_T)   :: stl

     slt%append("LU_type")
     stl%append("LU_description")
     stl%append("LU_pct_imperviousness")
     stl%append("LU_interception_grow")
     stl%append("LU_interception_nongrow")

     call LU%initialize( stColNames=stl, &
        iDataTypes = [ INTEGER_DATA, STRING_DATA, STRING_DATA, FLOAT_DATA, FLOAT_DATA ], &
        iRecordCount = iNumRecords )


  end subroutine initialize_column_headings_sub

end module lookup_table_landuse