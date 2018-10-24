module daily_calculation

  use continuous_frozen_ground_index, only   : update_continuous_frozen_ground_index
  use model_domain, only                     : MODEL_DOMAIN_T
  use storm_drain_capture, only              : storm_drain_capture_calculate,             &
                                               STORM_DRAIN_CAPTURE_FRACTION
  use exceptions, only                       : assert
  use iso_c_binding, only                    : c_short, c_int, c_float, c_double, c_bool

  use mass_balance__impervious_surface, only : calculate_impervious_surface_mass_balance
  use mass_balance__interception, only       : calculate_interception_mass_balance
  use mass_balance__snow, only               : calculate_snow_mass_balance
  use mass_balance__soil, only               : calculate_soil_mass_balance

  use simulation_datetime, only       : SIM_DT

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
    integer (kind=c_int) :: landuse_index

    ! calls elemental
    call cells%calc_GDD()

    ! calls elemental
    call cells%update_crop_coefficient()
    call cells%update_growing_season()
    call cells%update_rooting_depth()
    call cells%calc_reference_et()

    ! update crop evapotranspiration; crop_coefficient_kcb defaults to 1.0
    cells%crop_etc = cells%reference_et0 * cells%crop_coefficient_kcb

    ! interception calculation *does* reflect the canopy fraction
    call cells%calc_interception()

    call cells%calc_snowfall()

    ! actually calculating *potential* snowmelt here; actual snowmelt determined
    ! in 'calculate_snow_mass_balance'
    call cells%calc_snowmelt()

    call cells%calc_continuous_frozen_ground_index()

    ! fog calculation does not explicitly consider canopy fraction
    call cells%calc_fog()

    ! ! irrigation calculated as though entire cell is to be irrigated
    ! call cells%calc_irrigation()

    call calculate_interception_mass_balance( interception_storage=cells%interception_storage,     &
                                              actual_et_interception=cells%actual_et_interception, &
                                              interception=cells%interception,                     &
                                              reference_et0=cells%reference_et0 )

    call calculate_snow_mass_balance( snow_storage=cells%snow_storage,                 &
                                      potential_snowmelt=cells%potential_snowmelt,     &
                                      snowmelt=cells%snowmelt,                         &
                                      net_snowfall=cells%net_snowfall )

     cells%runon          = 0.0_c_float
     cells%runoff         = 0.0_c_float
     cells%runoff_outside = 0.0_c_float

    !> if flow routing is enabled, the calculations will be made in order from upslope to downslope;
    !! otherwise, the calculations are made in the natural packing order of the data structure
    do jndx=1, ubound( cells%sort_order, 1 )

      ! sort_order is a simple series of index values from 1 to # active cells
      ! indx represents the index of the cell if processed in upstream to downstream order

      indx = cells%sort_order( jndx )

      landuse_index = cells%landuse_index( indx )

      associate (                                                                                   &
        net_infiltration                  => cells%net_infiltration( indx ),                        &
        pervious_fraction                 => cells%pervious_fraction( indx ),                       &
        irrigation                        => cells%irrigation( indx ),                              &
        direct_net_infiltration           => cells%direct_net_infiltration( indx ),                 &
        direct_soil_moisture              => cells%direct_soil_moisture( indx ),                    &
        surface_storage                   => cells%surface_storage( indx ),                         &
        actual_et_impervious              => cells%actual_et_impervious( indx ),                    &
        actual_et_soil                    => cells%actual_et_soil( indx ),                          &
        actual_et                         => cells%actual_et( indx ),                               &
        surface_storage_excess            => cells%surface_storage_excess( indx ),                  &
        surface_storage_max               => cells%surface_storage_max( indx ),                     &
        soil_storage_max                  => cells%soil_storage_max( indx ),                        &
        soil_storage                      => cells%soil_storage( indx ),                            &
        storm_drain_capture               => cells%storm_drain_capture( indx ),                     &
        gross_precipitation               => cells%gross_precip( indx ),                            &
        rainfall                          => cells%rainfall( indx ),                                &
        net_rainfall                      => cells%net_rainfall( indx ),                            &
        snowmelt                          => cells%snowmelt( indx ),                                &
        snowfall                          => cells%snowfall( indx ),                                &
        runon                             => cells%runon( indx ),                                   &
        runoff                            => cells%runoff( indx ),                                  &
        inflow                            => cells%inflow( indx ),                                  &
        delta_soil_storage                => cells%delta_soil_storage( indx ),                      &
        infiltration                      => cells%infiltration( indx ),                            &
        fog                               => cells%fog( indx ),                                     &
        interception                      => cells%interception( indx ),                            &
        reference_et0                     => cells%reference_et0( indx ),                           &
        actual_et_interception            => cells%actual_et_interception( indx ),                  &
        canopy_cover_fraction             => cells%canopy_cover_fraction(indx) )

        ! inflow is calculated over the entire cell (pervious + impervious) area
        inflow = max( 0.0_c_float, runon + net_rainfall + fog + snowmelt )

        call cells%calc_runoff( indx )

        ! calculating irrigation here because the Hawaii Water Budget method
        ! needs an updated monthly runoff value to accurately calculate the
        ! estimated irrigation demand; previously this value was wrong on the
        ! first day of each month, since the previous month's runoff value was
        ! being used to estimate the current month's irrigation demand
        call cells%calc_irrigation( indx )

        ! this is a convoluted call: we're getting an individual capture fraction,
        ! but supplying a pointer to the active cells so that the first time through,
        ! gridded data (if supplied) are updated
        call storm_drain_capture_calculate( storm_drain_capture, indx, cells%active )

        ! prevent calculated runoff from exceeding the day's inflow;
        ! this can happen when using the monthly runoff fraction method
        !runoff = max( min( inflow, runoff ), 0.0_c_float )

        ! this routine now generates *all* outputs CORRECTED FOR PERVIOUS AREA
        call calculate_impervious_surface_mass_balance(                                         &
          surface_storage=surface_storage,                                                      &
          actual_et_impervious=actual_et_impervious,                                            &
          paved_to_unpaved=surface_storage_excess,                                              &
          surface_storage_max=surface_storage_max,                                              &
          storm_drain_capture=storm_drain_capture,                                              &
          storm_drain_capture_fraction=STORM_DRAIN_CAPTURE_FRACTION(indx),                      &
          net_rainfall=net_rainfall,                                                            &
          snowmelt=snowmelt,                                                                    &
          runon=runon,                                                                          &
          runoff=runoff,                                                                        &
          fog=fog,                                                                              &
          reference_et0=reference_et0,                                                          &
          pervious_fraction=pervious_fraction )

        ! ** this statement commented out since pervious fraction correction is
        !    now being performed in the 'mass_balance__impervious_surface' directly
        ! modify the surface storage in inches as if the amount calculated for the impervious area
        ! were to be redistributed uniformly over the total area of the cell
        !surface_storage_excess = surface_storage_excess * ( 1.0_c_float - pervious_fraction )

        ! e.g. septic system discharge enters here...
        call cells%calc_direct_soil_moisture( indx )

        ! irrigation not considered to be a contributor to runoff...in addition, infiltration
        ! term is calculated with respect to the pervious fraction of the cell
        infiltration = max( 0.0_c_float,                                       &
                             ( ( runon                                         &
                            + net_rainfall                                     &
                            + fog                                              &
                            + snowmelt                                         &
                            + irrigation                                       &
                            + direct_soil_moisture                             &
                            - runoff ) * pervious_fraction                     &
                            + surface_storage_excess ) / pervious_fraction )

        ! the following call updates bound variable actual_et_soil
        call cells%calc_actual_et( indx )

        ! if ( runoff < 0.)                                                                               &
        !   call LOGS%write( "line "//asCharacter(__LINE__)//": Negative runoff, indx= "                  &
        !                    //asCharacter(indx)//" col, row= "//asCharacter(cells%col_num_1D( indx ))    &
        !                    //", "//asCharacter( cells%row_num_1D( indx ) ) )

    !     ! actual et for the entire cell is the weighted average of the ET for pervious and impervious
    !     ! fractions of the cell
    !     actual_et = actual_et_soil * pervious_fraction                            &
    ! !                      + cells%actual_et_interception * cells%canopy_cover_fraction             &
    !                       + actual_et_impervious * ( 1.0_c_float - pervious_fraction )

        call calculate_soil_mass_balance( net_infiltration=net_infiltration,             &
                                          soil_storage=soil_storage,                     &
                                          soil_storage_max=soil_storage_max,             &
                                          delta_soil_storage=delta_soil_storage,         &
                                          actual_et_soil=actual_et_soil,                 &
                                          reference_et0=reference_et0,                   &
                                          infiltration=infiltration,                     &
                                          runoff=runoff )

        ! actual et for the entire cell is the weighted average of the ET for pervious and impervious
        ! fractions of the cell
        actual_et = actual_et_soil * pervious_fraction                                   &
                   + actual_et_impervious * ( 1.0_c_float - pervious_fraction )          &
                          + actual_et_interception * canopy_cover_fraction

        if ( runoff < 0.)                                                                               &
          call LOGS%write( "line "//asCharacter(__LINE__)//": Negative runoff, indx= "                  &
                           //asCharacter(indx)//" col, row= "//asCharacter(cells%col_num_1D( indx ))    &
                           //", "//asCharacter( cells%row_num_1D( indx ) ) )

        call cells%calc_direct_net_infiltration( indx )

        ! reporting of net_infiltration and irrigation must be adjusted to account for zero
        ! irrigation and net_infiltration associated with the impervious areas

        ! modify net_infiltration and irrigation terms

        net_infiltration = net_infiltration * pervious_fraction + direct_net_infiltration
!        net_infiltration = net_infiltration + direct_net_infiltration

        ! if ( runoff < 0.)                                                                               &
        !   call LOGS%write( "line "//asCharacter(__LINE__)//": Negative runoff, indx= "                  &
        !                    //asCharacter(indx)//" col, row= "//asCharacter(cells%col_num_1D( indx ))    &
        !                    //", "//asCharacter( cells%row_num_1D( indx ) ) )

        irrigation = irrigation * pervious_fraction

        call cells%calc_maximum_net_infiltration( indx )

        ! NOTE: only way for "runon" to be positive is if D8 flow routing
        !       is enabled.

        ! rejected net_infiltration + runoff will be routed downslope if routing option is turned on
        call cells%calc_routing( index=indx )

        ! if ( runoff < 0.)                                                                               &
        !   call LOGS%write( "line "//asCharacter(__LINE__)//": Negative runoff, indx= "                  &
        !                    //asCharacter(indx)//" col, row= "//asCharacter(cells%col_num_1D( indx ))    &
        !                    //", "//asCharacter( cells%row_num_1D( indx ) ) )

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
