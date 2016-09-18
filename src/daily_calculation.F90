module daily_calculation

  use continuous_frozen_ground_index, only   : update_continuous_frozen_ground_index
  use model_domain, only                     : MODEL_DOMAIN_T
  use storm_drain_capture, only              : storm_drain_capture_calculate,             &
                                               STORM_DRAIN_CAPTURE_FRACTION
  use direct_recharge__gridded_data, only    : DIRECT_RECHARGE_ACTIVE_FRACTION  
  use exceptions, only                       : assert
  use iso_c_binding, only                    : c_short, c_int, c_float, c_double, c_bool

  use mass_balance__impervious_surface, only : calculate_impervious_surface_mass_balance
  use mass_balance__interception, only       : calculate_interception_mass_balance
  use mass_balance__snow, only               : calculate_snow_mass_balance
  use mass_balance__soil, only               : calculate_soil_mass_balance

  use strings
  use logfiles

  implicit none

  private

  public :: perform_daily_calculation

contains

  subroutine perform_daily_calculation(cells)

    class (MODEL_DOMAIN_T), intent(inout)  :: cells

    ! [ LOCALS ]
    integer (kind=c_int) :: indx, jndx
    real (kind=c_float)  :: recharge_fraction
    integer (kind=c_int) :: landuse_index

    ! calls elemental
    call cells%calc_GDD()

    ! calls elemental
    call cells%update_growing_season()
    call cells%update_crop_coefficient()
    call cells%calc_reference_et()


    ! update crop evapotranspiration; crop_coefficient_kcb defaults to 1.0
    cells%crop_etc = cells%reference_et0 * cells%crop_coefficient_kcb

    call cells%calc_snowfall()
    call cells%calc_snowmelt()

    call cells%calc_continuous_frozen_ground_index()

    
    ! fog calculation does not explicitly consider canopy fraction   
    call cells%calc_fog()

    ! interception calculation *does* reflect the canopy fraction
    call cells%calc_interception()

    ! irrigation calculated as though entire cell is to be irrigated
    call cells%calc_irrigation()

    call calculate_interception_mass_balance( interception_storage=cells%interception_storage,     &
                                              actual_et=cells%actual_et_interception,              &
                                              interception=cells%interception,                     &
                                              reference_et0=cells%reference_et0 )

    call calculate_snow_mass_balance( snow_storage=cells%snow_storage,                 &
                                      potential_snowmelt=cells%potential_snowmelt,     &
                                      snowmelt=cells%snowmelt,                         &
                                      snowfall=cells%snowfall )


     cells%runon = 0.0_c_float
     cells%runoff = 0.0_c_float

    !> if flow routing is enabled, the calculations will be made in order from upslope to downslope; 
    !! otherwise, the calculations are made in the natural packing order of the data structure
    do jndx=1, ubound( cells%order_index, 1 )

      indx = cells%order_index( jndx )

      landuse_index = cells%landuse_index( indx )

      associate ( recharge                     => cells%potential_recharge( indx ),                      &
                  pervious_fraction            => cells%pervious_fraction( indx ),                       &
                  irrigation                   => cells%irrigation( indx ),                              &
                  direct_recharge              => cells%direct_recharge( indx ),                         &
                  direct_soil_moisture         => cells%direct_soil_moisture( indx ),                    &
                  direct_recharge_fraction     => DIRECT_RECHARGE_ACTIVE_FRACTION( indx ),               &
                  surface_storage              => cells%surface_storage( indx ),                         & 
                  actual_et_impervious         => cells%actual_et_impervious( indx ),                    &   
                  actual_et_soil               => cells%actual_et_soil( indx ),                          &  
                  actual_et                    => cells%actual_et( indx ),                               &                   
                  surface_storage_excess       => cells%surface_storage_excess( indx ),                  &
                  surface_storage_max          => cells%surface_storage_max( indx ),                     &
                  soil_storage_max             => cells%soil_storage_max( indx ),                        &
                  soil_storage                 => cells%soil_storage( indx ),                            &
                  storm_drain_capture          => cells%storm_drain_capture( indx ),                     &
                  storm_drain_capture_fraction => STORM_DRAIN_CAPTURE_FRACTION( landuse_index ),         &
                  gross_precipitation          => cells%gross_precip( indx ),                            &                     
                  rainfall                     => cells%rainfall( indx ),                                &
                  snowmelt                     => cells%snowmelt( indx ),                                &
                  snowfall                     => cells%snowfall( indx ),                                &                  
                  runon                        => cells%runon( indx ),                                   &
                  runoff                       => cells%runoff( indx ),                                  &
                  inflow                       => cells%inflow( indx ),                                  &
                  infiltration                 => cells%infiltration( indx ),                            &
                  fog                          => cells%fog( indx ),                                     &
                  interception                 => cells%interception( indx ),                            &
                  reference_et0                => cells%reference_et0( indx ) )

        ! inflow calculated over the entire cell (pervious + impervious) area

        if ( rainfall > 0.0_c_float ) then

          inflow = runon + rainfall + fog + snowmelt - interception

        else 
        
          inflow = runon + fog + snowmelt 

        endif
          

        call cells%calc_runoff( indx )

        call calculate_impervious_surface_mass_balance(                                         &
          surface_storage=surface_storage,                                                      & 
          actual_et=actual_et_impervious,                                                       &   
          surface_storage_excess=surface_storage_excess,                                        &
          surface_storage_max=surface_storage_max,                                              &
          storm_drain_capture=storm_drain_capture,                                              &
          storm_drain_capture_fraction=storm_drain_capture_fraction,                            &
          rainfall=rainfall,                                                                    &
          snowmelt=snowmelt,                                                                    &
          runoff=runoff,                                                                        &
          fog=fog,                                                                              &
          interception=interception,                                                            &
          reference_et0=reference_et0 )

        ! modify the surface storage in inches as if the amount calculated for the impervious area
        ! were to be redistributed uniformly over the total area of the cell
        surface_storage_excess = surface_storage_excess * ( 1.0_c_float - pervious_fraction )  


        ! prevent calculated runoff from exceeding the day's inflow
        if ( inflow - runoff < 0.0_c_float ) then

           ! call LOGS%write( "line "//asCharacter(__LINE__)//": runoff > inflow?, indx= "//asCharacter(indx)//" col, row= "    &
           ! //asCharacter(cells%col_num_1D( indx ))//", "//asCharacter( cells%row_num_1D( indx ) ) )
           ! call LOGS%write("          runoff: "//asCharacter(runoff))
           ! call LOGS%write("          inflow: "//asCharacter(inflow))
           ! call LOGS%write("           runon: "//asCharacter(runon))
           ! call LOGS%write("        snowmelt: "//asCharacter(snowmelt)) 
           ! call LOGS%write("        snowfall: "//asCharacter(snowfall))
           ! call LOGS%write("    interception: "//asCharacter(interception))
           ! call LOGS%write("             fog: "//asCharacter(fog)) 
           ! call LOGS%write("        rainfall: "//asCharacter(rainfall))                     
           ! call LOGS%write("    gross_precip: "//asCharacter(gross_precipitation))
           ! call LOGS%write("   curve_num_adj: "//asCharacter( cells%curve_num_adj( indx )))
           ! call LOGS%write("    landuse_code: "//asCharacter( cells%landuse_code( indx ) ) )
           runoff = inflow
         endif
                                   
        ! e.g. septic system discharge enters here...
        call cells%calc_direct_soil_moisture( indx )

        if ( runoff < 0.)  call LOGS%write( "line "//asCharacter(__LINE__)//": Negative runoff, indx= "   &
                                            //asCharacter(indx)//" col, row= "                            &
                                            //asCharacter(cells%col_num_1D( indx ))//", "                 &
                                            //asCharacter( cells%row_num_1D( indx ) ) )

        ! irrigation not considered to be a contributor to runoff...in addition, infiltration
        ! term is calculated with respect to the pervious fraction of the cell
        infiltration = max( 0.0_c_float,                                                                &
                            runon                                                                       &
                            + rainfall                                                                  &
                            + fog                                                                       &
                            + surface_storage_excess                                                    &
                            + snowmelt                                                                  &
                            - interception                                                              &
                            + irrigation                                                                &
                            + direct_soil_moisture                                                      &
                            - runoff )

        ! the following call updates bound variable actual_et_soil
        call cells%calc_actual_et( indx )


        if ( runoff < 0.)                                                                               &
          call LOGS%write( "line "//asCharacter(__LINE__)//": Negative runoff, indx= "                  &
                           //asCharacter(indx)//" col, row= "//asCharacter(cells%col_num_1D( indx ))    &
                           //", "//asCharacter( cells%row_num_1D( indx ) ) )

        ! actual et for the entire cell is the weighted average of the ET for pervious and impervious
        ! fractions of the cell
        actual_et = actual_et_soil * pervious_fraction                            &
    !                      + cells%actual_et_interception * cells%canopy_cover_fraction             &
                          + actual_et_impervious * ( 1.0_c_float - pervious_fraction )

        call calculate_soil_mass_balance( potential_recharge=recharge,                 &
                                          soil_storage=soil_storage,                   &
                                          soil_storage_max=soil_storage_max,           &
                                          actual_et=actual_et_soil,                    &
                                          infiltration=infiltration,                   &
                                          runoff=runoff )


        if ( runoff < 0.)                                                                               &
          call LOGS%write( "line "//asCharacter(__LINE__)//": Negative runoff, indx= "                  &
                           //asCharacter(indx)//" col, row= "//asCharacter(cells%col_num_1D( indx ))    &
                           //", "//asCharacter( cells%row_num_1D( indx ) ) )

        call cells%calc_direct_recharge( indx )
        
        ! reporting of potential recharge and irrigation must be adjusted to account for zero
        ! irrigation and potential recharge associated with the impervious areas

        ! modify potential recharge and irrigation terms 

        recharge_fraction= min( pervious_fraction, 1.0_c_float - direct_recharge_fraction )

        recharge = recharge * recharge_fraction + direct_recharge_fraction * direct_recharge

        if ( runoff < 0.)                                                                               &
          call LOGS%write( "line "//asCharacter(__LINE__)//": Negative runoff, indx= "                  &
                           //asCharacter(indx)//" col, row= "//asCharacter(cells%col_num_1D( indx ))    &
                           //", "//asCharacter( cells%row_num_1D( indx ) ) )

        irrigation = irrigation * pervious_fraction

        call cells%calc_maximum_potential_recharge( indx )

        ! NOTE: only way for "runon" to be positive is if D8 flow routing 
        !       is enabled.

        ! rejected recharge + runoff will be routed downslope if routing option is turned on
        call cells%calc_routing( indx )

        if ( runoff < 0.)                                                                               &
          call LOGS%write( "line "//asCharacter(__LINE__)//": Negative runoff, indx= "                  &
                           //asCharacter(indx)//" col, row= "//asCharacter(cells%col_num_1D( indx ))    &
                           //", "//asCharacter( cells%row_num_1D( indx ) ) )


      end associate

    enddo  

  end subroutine perform_daily_calculation

!--------------------------------------------------------------------------------------------------

  subroutine minmaxmean( variable , varname, logical_vector )

    real (kind=c_float), dimension(:)           :: variable
    character (len=*), intent(in)               :: varname
    logical, intent(in), optional               :: logical_vector(:)

    ! [ LOCALS ] 
    integer (kind=c_int) :: iCount
    character (len=30)   :: sVarname
    character (len=14)   :: sMin
    character (len=14)   :: sMax
    character (len=14)   :: sMean
    character (len=10)   :: sCount

    write (sVarname, fmt="(a30)") adjustl(varname)

    if (size( variable, 1) > 0 .and. present( logical_vector ) ) then
      write (sMin, fmt="(g14.3)")   minval(variable, logical_vector)
      write (sMax, fmt="(g14.3)")   maxval(variable, logical_vector)
      write (sMean, fmt="(g14.3)")  sum(variable, logical_vector)     &
                                     / count( logical_vector )
      write (sCount, fmt="(i10)") count( logical_vector )

    elseif (size( variable, 1) > 0 ) then
      write (sMin, fmt="(g14.3)")   minval(variable)
      write (sMax, fmt="(g14.3)")   maxval(variable)
      write (sMean, fmt="(g14.3)")  sum(variable) / size(variable,1)
      write (sCount, fmt="(i10)") size(variable,1)

    else
      write (sMin, fmt="(g14.3)")   -9999.
      write (sMax, fmt="(g14.3)")   -9999.
      write (sMean, fmt="(g14.3)")  -9999.
      write (sCount, fmt="(i10)")       0
    endif

    call LOGS%write( adjustl(sVarname)//" | "//adjustl(sMin)//" | "//adjustl(sMax) &
       //" | "//adjustl(sMean)//" | "//adjustl(sCount), iLogLevel=LOG_DEBUG, lEcho=.true._c_bool )

  end subroutine minmaxmean


end module daily_calculation