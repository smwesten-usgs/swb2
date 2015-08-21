module mass_balance__soil

  use iso_c_binding, only        : c_float
  implicit none

  private

  public :: calculate_soil_mass_balance

contains

   elemental subroutine calculate_soil_mass_balance( potential_recharge,    &
                                                     soil_storage,          &
                                                     soil_storage_max,      &
                                                     actual_et,             &
                                                     infiltration )

    real (kind=c_float), intent(inout)      :: potential_recharge
    real (kind=c_float), intent(inout)      :: soil_storage
    real (kind=c_float), intent(inout)      :: actual_et
    real (kind=c_float), intent(in)         :: soil_storage_max
    real (kind=c_float), intent(in)         :: infiltration

 
    ! [ LOCALS ]
    real (kind=c_float) :: new_soil_storage

    new_soil_storage = soil_storage + infiltration - actual_et

    if ( new_soil_storage < 0.0_c_float ) then

      actual_et = max( 0.0_c_float, soil_storage + infiltration )
      soil_storage = 0.0_c_float
!      potential_recharge = 0.0_c_float

    elseif ( new_soil_storage > soil_storage_max ) then

      potential_recharge = potential_recharge + new_soil_storage - soil_storage_max
      soil_storage = soil_storage_max

    else

      soil_storage = new_soil_storage
!      potential_recharge = 0.0_c_float

    endif

! NOTE: lines 34 and 44 commented out because an earlier call to direct_recharge
!       zeroes out all potential recharge terms. if direct_recharge module is ever moved or 
!       eliminated, lines 34 and 44 will need to be restored.

  end subroutine calculate_soil_mass_balance

end module mass_balance__soil