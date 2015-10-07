module daily_calculation

  use continuous_frozen_ground_index, only   : update_continuous_frozen_ground_index
  use model_domain, only                     : MODEL_DOMAIN_T
  use storm_drain_capture, only              : storm_drain_capture_calculate,             &
                                               STORM_DRAIN_CAPTURE_FRACTION
  use iso_c_binding, only                    : c_short, c_int, c_float, c_double, c_bool

  use mass_balance__impervious_surface, only : calculate_impervious_surface_mass_balance
  use mass_balance__interception, only       : calculate_interception_mass_balance
  use mass_balance__snow, only               : calculate_snow_mass_balance
  use mass_balance__soil, only               : calculate_soil_mass_balance

  use logfiles

  implicit none

  private

  public :: perform_daily_calculation

contains

  subroutine perform_daily_calculation(cells)

    class (MODEL_DOMAIN_T), intent(inout)  :: cells

    call cells%calc_GDD()

!    call minmaxmean_float( cells%crop_coefficient_kcb, "kcb, before call")

    call cells%update_crop_coefficient()
    call cells%calc_reference_et()


    ! update crop evapotranspiration; crop_coefficient_kcb defaults to 1.0
    cells%crop_etc = cells%reference_et0 * cells%crop_coefficient_kcb

    call cells%calc_snowfall()
    call cells%calc_snowmelt()

    call update_continuous_frozen_ground_index( cells%continuous_frozen_ground_index, cells%tmin,    &
                                                cells%tmax, cells%snow_storage )

    
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


    ! call to calc_routing also triggers an embedded call to calc_runoff
    ! NOTE: only way for "runon" to be positive is if D8 flow routing 
    !       is enabled.
    call cells%calc_routing()

    ! inflow calculated over the entire cell (pervious + impervious) area
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

    ! irrigation not considered to be a contributor to runoff...in addition, infiltration
    ! term is calculated with respect to the pervious fraction of the cell
    cells%infiltration = max( 0.0_c_float,                                                           &
                     cells%runon                                                                     &
                   + cells%rainfall                                                                  &
                   + cells%fog                                                                       &
                   + cells%surface_storage_excess * ( 1.0_c_float - cells%pervious_fraction )        &
                                                  / cells%pervious_fraction                          &
                   + cells%snowmelt                                                                  &
                   - cells%interception                                                              &
                   + cells%irrigation                                                                &
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

    cells%potential_recharge = cells%potential_recharge * cells%pervious_fraction

    call cells%calc_direct_recharge()
    
    ! reporting of potential recharge and irrigation must be adjusted to account for zero
    ! irrigation and potential recharge associated with the impervious areas
    cells%potential_recharge = cells%potential_recharge * cells%pervious_fraction     &
                              + cells%direct_recharge

    cells%irrigation = cells%irrigation * cells%pervious_fraction


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