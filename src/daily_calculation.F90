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
    integer (kind=c_int) :: indx
    real (kind=c_float)  :: fraction

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

    call calculate_impervious_surface_mass_balance(                                        &
        surface_storage=cells%surface_storage,                                             & 
        actual_et=cells%actual_et_impervious,                                              &   
        surface_storage_excess=cells%surface_storage_excess,                               &
        surface_storage_max=cells%surface_storage_max,                                     &
        storm_drain_capture=cells%storm_drain_capture,                                     &
        storm_drain_capture_fraction=STORM_DRAIN_CAPTURE_FRACTION( cells%landuse_index ),  &
        rainfall=cells%rainfall,                                                           &
        snowmelt=cells%snowmelt,                                                           &
        runoff=cells%runoff,                                                               &
        fog=cells%fog,                                                                     &
        interception=cells%interception,                                                   &
        reference_et0=cells%reference_et0 )

    ! modify the surface storage in inches as if the amount calculated for the impervious area
    ! were to be redistributed uniformly over the total area of the cell
    cells%surface_storage_excess = cells%surface_storage_excess * ( 1.0_c_float - cells%pervious_fraction )  

!                                                  / cells%pervious_fraction                             

! ***** NOTE: removed the division by pervious_fraction; original code does not appear to divide by pervious fraction
!             ( see line 2090, HI_WB_3_3.f )


    ! call to calc_routing also triggers an embedded call to calc_runoff
    ! NOTE: only way for "runon" to be positive is if D8 flow routing 
    !       is enabled.
    call cells%calc_routing()

    ! inflow calculated over the entire cell (pervious + impervious) area
    ! surface_storage_excess is commented out because it represents an *internal* transfer of water, 
    ! it is not a true 'inflow' in sense that water is crossing the cell boundary
    cells%inflow = cells%runon                                                                       &
                   + cells%rainfall                                                                  &
                   + cells%fog                                                                       &
!                   + cells%surface_storage_excess * ( 1.0_c_float - cells%pervious_fraction )        &
                   + cells%snowmelt                                                                  &
                   - cells%interception

    ! prevent calculated runoff from exceeding the day's inflow
    where ( cells%inflow - cells%runoff < 0.0_c_float )

      cells%runoff = cells%inflow

    end where

    ! e.g. septic system discharge enters here...
    call cells%calc_direct_soil_moisture()

    ! irrigation not considered to be a contributor to runoff...in addition, infiltration
    ! term is calculated with respect to the pervious fraction of the cell
    cells%infiltration = max( 0.0_c_float,                                                           &
                     cells%runon                                                                     &
                   + cells%rainfall                                                                  &
                   + cells%fog                                                                       &
                   + cells%surface_storage_excess                                                    &
                   + cells%snowmelt                                                                  &
                   - cells%interception                                                              &
                   + cells%irrigation                                                                &
                   + cells%direct_soil_moisture                                                      &
                   - cells%runoff )

    ! the following call updates bound variable actual_et_soil
    call cells%calc_actual_et()

    ! actual et for the entire cell is the weighted average of the ET for pervious and impervious
    ! fractions of the cell
    cells%actual_et = cells%actual_et_soil * cells%pervious_fraction                            &
!                      + cells%actual_et_interception * cells%canopy_cover_fraction             &
                      + cells%actual_et_impervious * ( 1.0_c_float - cells%pervious_fraction )

    call calculate_soil_mass_balance( potential_recharge=cells%potential_recharge,       &
                                      soil_storage=cells%soil_storage,                   &
                                      soil_storage_max=cells%soil_storage_max,           &
                                      actual_et=cells%actual_et_soil,                    &
                                      infiltration=cells%infiltration )

    ! reporting of potential recharge and irrigation must be adjusted to account for zero
    ! irrigation and potential recharge associated with the impervious areas

!    cells%potential_recharge = cells%potential_recharge * cells%pervious_fraction

    call cells%calc_direct_recharge()
    
    ! reporting of potential recharge and irrigation must be adjusted to account for zero
    ! irrigation and potential recharge associated with the impervious areas

    !
    ! ***** John, check the calculation below please...it seems that we were multiplying the calculated 
    ! potential recharge by the pervious fraction *twice*
    !
    ! OLD CALCULATION:
    ! line 133: cells%potential_recharge = cells%potential_recharge * cells%pervious_fraction
    ! line 146: cells%potential_recharge = cells%potential_recharge * cells%pervious_fraction + cells%direct_recharge

    ! modify potential recharge and irrigation terms 

    associate ( recharge             => cells%potential_recharge,                    &
                perv_fraction        => cells%pervious_fraction,                     &
                irr                  => cells%irrigation,                            &
                dir_recharge         => cells%direct_recharge,                       &
                f                    => DIRECT_RECHARGE_ACTIVE_FRACTION )

      call assert( ubound( perv_fraction, 1) == ubound( f, 1), "INTERNAL PROGRAMMING ERROR--"    &
        //"unequal vector lengths 'perv_fraction' and 'f'."                                      &
        //" length 'perv_fraction'="//asCharacter( ubound( perv_fraction, 1))                    &
        //"; length f="//asCharacter(ubound(f,1)), __FILE__, __LINE__)

      ! running into weird segfault issues when attempting this using whole-array operations

      do indx=1, ubound( f, 1)

        fraction= min( perv_fraction( indx ), 1.0_c_float - f( indx) ) 

        recharge( indx ) = recharge( indx ) * fraction + f( indx ) * dir_recharge( indx )
        irr( indx )      = irr( indx ) * perv_fraction( indx )

      enddo

    end associate

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