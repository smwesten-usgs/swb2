module mass_balance__interception

  use iso_c_binding, only        : c_short, c_int, c_float, c_double
  implicit none

  private

  public :: calculate_interception_mass_balance

contains

  elemental subroutine calculate_interception_mass_balance( interception_storage,     &
                                                            actual_et_interception,   &
 	                                                          interception,             &
 	                                                          reference_et0 )

    real (kind=c_float), intent(inout)      :: interception_storage
    real (kind=c_float), intent(inout)      :: actual_et_interception
    real (kind=c_float), intent(in)         :: interception
    real (kind=c_float), intent(in)         :: reference_et0

    interception_storage = interception_storage + interception

    actual_et_interception = min( reference_et0, interception_storage )

    interception_storage = max( 0.0_c_float, interception_storage - actual_et_interception )


   end subroutine calculate_interception_mass_balance


end module mass_balance__interception
