module mass_balance__soil

  use iso_c_binding, only        : c_float
  implicit none

  private

contains

   elemental subroutine calculate_soil_mass_balance( potential_recharge,    &
                                                     soil_moisture,         &
                                                     max_soil_moisture,     &
                                                     actual_et,             &
                                                     infiltration )

    real (kind=c_float), intent(inout)      :: potential_recharge
    real (kind=c_float), intent(in)         :: soil_moisture
    real (kind=c_float), intent(in)         :: max_soil_moisture
    real (kind=c_float), intent(in)         :: actual_et
    real (kind=c_float), intent(in)         :: infiltration
 
    soil_moisture = soil_moisture + infiltration - actual_et

    if ( soil_moisture > max_soil_moisture ) then

      potential_recharge = soil_moisture - max_soil_moisture
      soil_moisture = max_soil_moisture

    else

      potential_recharge = 0.0_c_float

    endif

  end subroutine calculate_soil_mass_balance

end module mass_balance__soil