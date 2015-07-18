program test__CN_update

  use iso_c_binding, only  : c_int, c_float, c_bool
  use constants_and_conversions
  use datetime
  use logfiles
  use runoff__curve_number
  use parameters, only          : PARAMS, PARAMS_DICT
  use strings
  use string_list
  implicit none

  integer (kind=c_int)  :: iIndex, iLU, iSoil
  real (kind=c_float)   :: CN
  real (kind=c_float)   :: SoilStorage

  call LOGS%initialize( iLogLevel = LOG_GENERAL,                     &
                        sFilePrefix="LOGFILE__test__CN_update",      &
                        lWrite_SWB_Info = lFALSE )	

  call PARAMS%add_parameters( sKey="LU_Code", iValues=[ 21, 22, 23 ] )
  call PARAMS%add_parameters( sKey="CN_1", fValues=[ 45., 50., 55. ] )
  call PARAMS%add_parameters( sKey="CN_2", fValues=[ 52., 55., 60. ] )
  call PARAMS%add_parameters( sKey="CN_3", fValues=[ 63., 65., 70. ] )
  call PARAMS%add_parameters( sKey="CN_4", fValues=[ 71., 76., 81. ] )

  call PARAMS_DICT%print_all()

  call runoff_curve_number_initialize()

  do iLU=1,3
    do iSoil=1,4
      do iIndex = 1,3

        SoilStorage = 0.5 + real( iIndex ) 

        CN = update_curve_number_fn( iLanduseIndex=iLU,                     &
                                     iSoilsIndex=iSoil,                     &
                                     fSoilStorage=SoilStorage,              &
                                     fSoilStorage_Max=5.,                   &
                                     fCFGI=0.0 )

        print *, iLU, iSoil, SoilStorage, CN

      enddo

    enddo

  enddo  
  print *, "--------------"
  print *, CN_AMCI
  print *, "--------------"
  print *, CN_AMCII
  print *, "--------------"
  print *, CN_AMCIII
  print *, "--------------"
  print *, Smax


end program test__CN_update