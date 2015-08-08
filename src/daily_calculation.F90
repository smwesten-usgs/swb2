module daily_calculation

  use continuous_frozen_ground_index, only   : update_continuous_frozen_ground_index
  use model_domain, only                     : MODEL_DOMAIN_T
  use iso_c_binding, only                    : c_short, c_int, c_float, c_double

  use mass_balance__impervious_surface, only : calculate_impervious_surface_mass_balance
  use mass_balance__interception, only       : calculate_interception_mass_balance
  use mass_balance__snow, only               : calculate_snow_mass_balance
  use mass_balance__soil, only               : calculate_soil_mass_balance

  implicit none

  private

  public :: perform_daily_calculation

contains

  subroutine perform_daily_calculation(cells)

    class (MODEL_DOMAIN_T), intent(inout)  :: cells

    call cells%calc_GDD()
    call cells%update_crop_coefficient()

    call cells%calc_reference_et()
    call cells%calc_snowfall()
    call cells%calc_snowmelt()

    call update_continuous_frozen_ground_index( cells%continuous_frozen_ground_index, cells%tmin,    &
                                                cells%tmax, cells%snow_storage )
       
    call cells%calc_fog()
    call cells%calc_interception()

    call cells%calc_irrigation()

    call calculate_interception_mass_balance( interception_storage=cells%interception_storage,     &
                                              actual_et=cells%actual_et,                           &
                                              interception=cells%interception,                     &
                                              canopy_cover_fraction=cells%canopy_cover_fraction,   &
                                              reference_et0=cells%reference_et0 )

    call calculate_snow_mass_balance( snow_storage=cells%snow_storage,                 &
                                      potential_snowmelt=cells%potential_snowmelt,     &
                                      snowmelt=cells%snowmelt,                         &
                                      snowfall=cells%snowfall )

    call calculate_impervious_surface_mass_balance( surface_storage=cells%surface_storage,                   & 
                                                    actual_et=cells%actual_et,                               &   
                                                    surface_storage_excess=cells%surface_storage_excess,     &
                                                    surface_storage_max=cells%surface_storage_max,           &
                                                    rainfall=cells%rainfall,                                 &
                                                    snowmelt=cells%snowmelt,                                 &
                                                    impervious_fraction=cells%impervious_fraction,           &
                                                    reference_et0=cells%reference_et0 ) 

    ! call to calc_routing also triggers an embedded call to calc_runoff
    call cells%calc_routing()

    cells%inflow = cells%runon + cells%rainfall + cells%fog + cells%snowmelt                  &
                   + cells%surface_storage_excess - cells%interception + cells%irrigation
    cells%infiltration = cells%inflow - cells%runoff

    call cells%calc_actual_et()


    call calculate_soil_mass_balance( potential_recharge=cells%potential_recharge,       &
                                      soil_storage=cells%soil_storage,                   &
                                      max_soil_storage=cells%max_soil_storage,           &
                                      actual_et=cells%actual_et,                         &
                                      infiltration=cells%infiltration )

    call cells%calc_direct_recharge()

  end subroutine perform_daily_calculation

end module daily_calculation