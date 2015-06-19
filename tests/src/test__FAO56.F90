program test__FAO56

  use iso_c_binding, only  : c_int, c_float, c_bool
  use constants_and_conversions
  use datetime
  use logfiles
  use parameters, only          : PARAMS, PARAMS_DICT
  use simulation_datetime
  use soil_moisture__FAO_56
  use string_list
  implicit none

! soil_moisture_FAO56_calculate( fSoilStorage, fActual_ET,                     &
!     fSoilStorage_Excess, fInfiltration, fGDD, fAvailableWaterCapacity, fReference_ET0,    &
!     fRootingDepth, iLanduseIndex, iSoilGroup )

  integer (kind=c_int) :: iIndex
  real (kind=c_float)  :: fKcb1, fKcb2, fKcb3
  logical (kind=c_bool) :: lActive(2,2)

  lActive = lTRUE

  call SIM_DT%start%parseDate( "01/01/1999" )
  call SIM_DT%start%calcJulianDay()

  call SIM_DT%end%parseDate( "12/31/2000" )
  call SIM_DT%end%calcJulianDay()

  SIM_DT%curr = SIM_DT%start


  call LOGS%initialize( iLogLevel = LOG_GENERAL, sFilePrefix="LOGFILE__test__FAO56", &
                        lWrite_SWB_Info = lFALSE )	

  call PARAMS%add_parameters( sKey="LU_Code", iValues=[ 1, 41, 17 ] )
  call PARAMS%add_parameters( sKey="L_ini", iValues=[ 50, 20, 0 ] )
  call PARAMS%add_parameters( sKey="L_dev", iValues=[ 107, 30, 0 ] )  
  call PARAMS%add_parameters( sKey="L_mid", iValues=[ 331, 40, 0 ] )
  call PARAMS%add_parameters( sKey="L_late", iValues=[ 152, 30, 0 ] )

  call PARAMS%add_parameters( sKey="Kcb_ini", fValues=[ 0.5, 0.4, 0. ] )
  call PARAMS%add_parameters( sKey="Kcb_mid", fValues=[ 1.25, 1.15, 0. ] )
  call PARAMS%add_parameters( sKey="Kcb_end", fValues=[ 0.88, 0.85, 0. ] )
  call PARAMS%add_parameters( sKey="Kcb_min", fValues=[ 0.15, 0.15, 0. ] )

  call PARAMS%add_parameters( sKey="Kcb_jan", fValues=[ 0.0, 0.0, 0.44 ] )
  call PARAMS%add_parameters( sKey="Kcb_feb", fValues=[ 0.0, 0.0, 0.44 ] )
  call PARAMS%add_parameters( sKey="Kcb_mar", fValues=[ 0.0, 0.0, 0.44 ] )
  call PARAMS%add_parameters( sKey="Kcb_apr", fValues=[ 0.0, 0.0, 0.44 ] )
  call PARAMS%add_parameters( sKey="Kcb_may", fValues=[ 0.0, 0.0, 0.44 ] )
  call PARAMS%add_parameters( sKey="Kcb_jun", fValues=[ 0.0, 0.0, 0.44 ] )
  call PARAMS%add_parameters( sKey="Kcb_jul", fValues=[ 0.0, 0.0, 0.44 ] )
  call PARAMS%add_parameters( sKey="Kcb_aug", fValues=[ 0.0, 0.0, 0.44 ] )
  call PARAMS%add_parameters( sKey="Kcb_sep", fValues=[ 0.0, 0.0, 0.44 ] )
  call PARAMS%add_parameters( sKey="Kcb_oct", fValues=[ 0.0, 0.0, 0.44 ] )
  call PARAMS%add_parameters( sKey="Kcb_nov", fValues=[ 0.0, 0.0, 0.44 ] )
  call PARAMS%add_parameters( sKey="Kcb_dec", fValues=[ 0.0, 0.0, 0.44 ] )
  
  call PARAMS%add_parameters( sKey="Planting_date", sValues=["05/01","04/25","05/10"] )  

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

  call soil_moisture_FAO56_initialize( lActive )

  do while ( SIM_DT%curr <= SIM_DT%end )

    call soil_moisture_FAO56_update_growth_stage_dates()

    fKcb1 = update_crop_coefficient_date_as_threshold( iLanduseIndex=1 )
    fKcb2 = update_crop_coefficient_date_as_threshold( iLanduseIndex=2 )
    fKcb3 = update_crop_coefficient_date_as_threshold( iLanduseIndex=3 )

!    print *, SIM_DT%curr%prettydate(), fKcb1, fKcb2, fKcb3
    
    !calc_effective_root_depth( iLanduseIndex, fZr_max, fKCB )

    call SIM_DT%addDay()

  enddo

  

end program test__FAO56