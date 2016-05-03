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

  integer (kind=c_int)  :: iIndex, iIndex1, iLU, iSoil
  real (kind=c_float)   :: CN
  real (kind=c_float)   :: SoilStorage, SoilStorage_Max
  real (kind=c_float)   :: fFraction_FC, frac1, frac2
  real (kind=c_float)   :: Infiltration

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

  SoilStorage_Max = 5.0

  do iLU=1,3
    do iSoil=1,4
      do iIndex = 1,3
        SoilStorage = 0.5 + real( iIndex ) 
        fFraction_FC = min( 1.0_c_float, SoilStorage / SoilStorage_Max )
        if( fFraction_FC > 0.5) frac1 = ( fFraction_FC - 0.5_c_float ) / 0.5_c_float
        if( fFraction_FC < 0.5) frac1 = ( fFraction_FC / 0.5_c_float )
        CN = update_curve_number_fn( iLanduseIndex=iLU,                     &
                                     iSoilsIndex=iSoil,                     &
                                     fSoilStorage=SoilStorage,              &
                                     fSoilStorage_Max=SoilStorage_Max,                   &
                                     fCFGI=0.0 )

        print *, iLU, iSoil, SoilStorage, fFraction_FC, frac1, CN

      enddo

    enddo

  enddo  
  print *, "--------------"
  print *, CN_ARCI
  print *, "--------------"
  print *, CN_ARCII
  print *, "--------------"
  print *, CN_ARCIII
  print *, "--------------"
  print *, Smax

  do iLU=1,3
    do iSoil=1,4
      do iIndex = 1,3
        do iIndex1=1,10

        SoilStorage = 0.5 + real( iIndex ) 

        Infiltration = runoff_curve_number_calculate(   &
          iLanduseIndex=iLU,                            &
          iSoilsIndex=iSoil,                            &
          fSoilStorage=SoilStorage,                     &
          fSoilStorage_Max=SoilStorage_Max,             &
          fInflow=real(iIndex1),                        &
          fCFGI=0.0_c_float ) 

        print *, iLU, iSoil, SoilStorage, real(iIndex1), Infiltration, real(iIndex1)-Infiltration

        enddo
      enddo
    enddo
  enddo

end program test__CN_update