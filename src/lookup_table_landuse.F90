
module table_record_landuse
  
  use iso_c_binding, only : c_short, c_float
  implicit none

  type, public :: TABLE_RECORD_LANDUSE_T

    integer (kind=c_short) :: iLandUseType
    integer (kind=c_short) :: iSoilsGroup
    character (len=:), allocatable :: sLandUseDescription
    character (len=:), allocatable :: sAssumedPercentImperviousness
    real (kind=c_float) :: rCN_base
    real (kind=c_float) :: rRootingDepth
    real (kind=c_float) :: rMaxInfiltration
    real (kind=c_float) :: rIntercept_GrowingSeason
    real (kind=c_float) :: rIntercept_NonGrowingSeason

  end type TABLE_RECORD_LANDUSE_T

end module table_record_landuse

module lookup_table_landuse

  use iso_c_binding, only : c_int, c_bool, c_short, c_float, c_double
  use exceptions
  use data_file
  use data_column
  use data_frame
  use constants_and_conversions, only : sTAB
  use strings
  use string_list
  use types_new
  use table_record_landuse, LOOKUP_TABLE_DATA_T => TABLE_RECORD_LANDUSE_T
  implicit none

  private
     
  include "lookup_table_general.F90"

end module lookup_table_landuse