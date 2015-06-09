program test__FAO56

  use iso_c_binding, only  : c_int, c_float, c_bool
  use constants_and_conversions
  use logfiles
  use parameters, only          : PARAMS, PARAMS_DICT
  use soil_moisture__FAO_56
  use string_list
  implicit none

! soil_moisture_FAO56_calculate( fSoilStorage, fActual_ET,                     &
!     fSoilStorage_Excess, fInfiltration, fGDD, fAvailableWaterCapacity, fReference_ET0,    &
!     fRootingDepth, iLanduseIndex, iSoilGroup )

  integer (kind=c_int) :: iIndex
  real (kind=c_float)  :: fKcb_


  call LOGS%initialize( iLogLevel = LOG_GENERAL, sFilePrefix="LOGFILE__test__FAO56", &
                        lWrite_SWB_Info = lFALSE )	

  call PARAMS%add_parameters( sKey="LU_Code", iValues=[ 1, 41, 81 ] )
  call PARAMS%add_parameters( sKey="L_ini", iValues=[ 50, 90, 0 ] )
  call PARAMS%add_parameters( sKey="L_dev", iValues=[ 107, 110, 0 ] )  
  call PARAMS%add_parameters( sKey="L_mid", iValues=[ 331, 120, 0 ] )
  call PARAMS%add_parameters( sKey="L_late", iValues=[ 152, 200, 0 ] )

  call PARAMS%add_parameters( sKey="Kcb_ini", fValues=[ 0.5, 0.95, 0. ] )
  call PARAMS%add_parameters( sKey="Kcb_mid", fValues=[ 1.25, 0.95, 0. ] )
  call PARAMS%add_parameters( sKey="Kcb_end", fValues=[ 0.88, 0.95, 0. ] )
  call PARAMS%add_parameters( sKey="Kcb_min", fValues=[ 0.15, 0.95, 0. ] )

  call PARAMS%add_parameters( sKey="Kcb_jan", fValues=[ 0.0, 0.0, 0.25 ] )
  call PARAMS%add_parameters( sKey="Kcb_feb", fValues=[ 0.0, 0.0, 0.35 ] )
  call PARAMS%add_parameters( sKey="Kcb_mar", fValues=[ 0.0, 0.0, 0.45 ] )
  call PARAMS%add_parameters( sKey="Kcb_apr", fValues=[ 0.0, 0.0, 0.55 ] )
  call PARAMS%add_parameters( sKey="Kcb_may", fValues=[ 0.0, 0.0, 0.75 ] )
  call PARAMS%add_parameters( sKey="Kcb_jun", fValues=[ 0.0, 0.0, 0.95 ] )
  call PARAMS%add_parameters( sKey="Kcb_jul", fValues=[ 0.0, 0.0, 1.0 ] )
  call PARAMS%add_parameters( sKey="Kcb_aug", fValues=[ 0.0, 0.0, 0.96 ] )
  call PARAMS%add_parameters( sKey="Kcb_sep", fValues=[ 0.0, 0.0, 0.76 ] )
  call PARAMS%add_parameters( sKey="Kcb_oct", fValues=[ 0.0, 0.0, 0.55 ] )
  call PARAMS%add_parameters( sKey="Kcb_nov", fValues=[ 0.0, 0.0, 0.35 ] )
  call PARAMS%add_parameters( sKey="Kcb_dec", fValues=[ 0.0, 0.0, 0.25 ] )
  
  call PARAMS%add_parameters( sKey="Planting_date", fValues=[ 76.0, 1.0, 1.0 ] )  

  call PARAMS%add_parameters( sKey="Units_Are_Days", lValues=[ lTRUE, lTRUE, lTRUE ] )
  call PARAMS%add_parameters( sKey="Mean_Plant_Height", fValues=[ 5.0, 30.0, 5.0 ] )
  call PARAMS%add_parameters( sKey="Depletion_Fraction", fValues=[ 0.5, 0.5, 0.5 ] )
  call PARAMS%add_parameters( sKey="FEW_1", fValues=[ 0.196, 0.196, 0.196] )
  call PARAMS%add_parameters( sKey="FEW_2", fValues=[ 0.295, 0.295, 0.295] )
  call PARAMS%add_parameters( sKey="FEW_3", fValues=[ 0.393, 0.393, 0.393] )
  call PARAMS%add_parameters( sKey="FEW_4", fValues=[ 0.472, 0.472, 0.472] )
  call PARAMS%add_parameters( sKey="TEW_1", fValues=[ 0.354, 0.354, 0.354] )
  call PARAMS%add_parameters( sKey="TEW_2", fValues=[ 0.669, 0.669, 0.669] )
  call PARAMS%add_parameters( sKey="TEW_3", fValues=[ 0.906, 0.906, 0.906] )
  call PARAMS%add_parameters( sKey="TEW_4", fValues=[ 1.063, 1.063, 1.063] )



  call PARAMS_DICT%print_all()

  call soil_moisture_FAO56_initialize( 4 )

  do iIndex = 1,365

    fKcb_ = sm_FAO56_UpdateCropCoefficient( iLanduseIndex=1,     &
                                           iThreshold=iIndex,   & 
                                           iMonth=1 )

    print *, iIndex, fKcb_

  enddo

  do iIndex = 1,12

    fKcb_ = sm_FAO56_UpdateCropCoefficient( iLanduseIndex=3,     &
                                           iThreshold=1,   & 
                                           iMonth=iIndex )

    print *, iIndex, fKcb_

  enddo

  

end program test__FAO56