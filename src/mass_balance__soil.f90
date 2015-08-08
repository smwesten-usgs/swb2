module mass_balance__soil

  use iso_c_binding, only        : c_float
  implicit none

  private

  public :: calculate_soil_mass_balance

contains

   elemental subroutine calculate_soil_mass_balance( potential_recharge,    &
                                                     soil_storage,          &
                                                     max_soil_storage,      &
                                                     actual_et,             &
                                                     infiltration )

    real (kind=c_float), intent(inout)      :: potential_recharge
    real (kind=c_float), intent(inout)      :: soil_storage
    real (kind=c_float), intent(in)         :: max_soil_storage
    real (kind=c_float), intent(in)         :: actual_et
    real (kind=c_float), intent(in)         :: infiltration
 
    soil_storage = soil_storage + infiltration - actual_et

    if ( soil_storage > max_soil_storage ) then

      potential_recharge = soil_storage - max_soil_storage
      soil_storage = max_soil_storage

    else

      potential_recharge = 0.0_c_float

    endif

  end subroutine calculate_soil_mass_balance

end module mass_balance__soil