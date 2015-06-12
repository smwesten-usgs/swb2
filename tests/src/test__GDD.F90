program test__GDD

  use iso_c_binding, only  : c_int, c_float, c_bool
  use constants_and_conversions
  use datetime
  use logfiles
  use parameters, only          : PARAMS, PARAMS_DICT
  use strings
  use string_list
  implicit none

  integer (kind=c_int) :: iIndex
  real (kind=c_float)  :: fKcb1, fKcb2, fKcb3

  call LOGS%initialize( iLogLevel = LOG_GENERAL, sFilePrefix="LOGFILE__test__GDD", &
                        lWrite_SWB_Info = lFALSE )	

  call PARAMS%add_parameters( sKey="GDD_Base", fValues=[ 50., 50., 50. ] )
  call PARAMS%add_parameters( sKey="GDD_Max", fValues=[ 86., 86., 86. ] )

  call PARAMS_DICT%print_all()


  

end program test__GDD