module mass_balance__interception

  use iso_c_binding, only        : c_short, c_int, c_float, c_double
  implicit none

  private

  public :: calculate_interception_mass_balance

contains

  elemental subroutine calculate_interception_mass_balance( interception_storage,     &
                                                            actual_et_interception,   &
 	                                                          interception,             &
                                                            interception_storage_max, &
 	                                                          reference_et0 )

    real (c_float), intent(inout)      :: interception_storage
    real (c_double), intent(inout)     :: actual_et_interception
    real (c_float), intent(inout)      :: interception
    real (c_float), intent(in)         :: interception_storage_max
    real (c_double), intent(in)        :: reference_et0

    real (c_float) :: temp_storage

    temp_storage = interception_storage + interception

    if ( temp_storage > interception_storage_max ) then
      ! reduce the previously calculated interception value to account for
      ! limited interception storage
      interception = interception_storage_max - interception_storage
      interception_storage = interception_storage_max
    else
      interception_storage = temp_storage
    endif

    actual_et_interception = min( reference_et0, interception_storage )
    interception_storage = max( 0.0_c_float, interception_storage - actual_et_interception )

   end subroutine calculate_interception_mass_balance

end module mass_balance__interception
