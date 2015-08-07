module mass_balance__interception

  use constants_and_conversions
  use iso_c_binding, only        : c_short, c_int, c_float, c_double
  implicit none

  private

contains

  elemental subroutine calculate_interception_mass_balance( interception_storage,     &
                                                            actual_et,                &
 	                                                          interception,             &
 	                                                          canopy_cover_fraction,    &
 	                                                          reference_et0 )

    real (kind=c_float), intent(inout)      :: interception_storage
    real (kind=c_float), intent(inout)      :: actual_et
    real (kind=c_float), intent(in)         :: interception
    real (kind=c_float), intent(in)         :: canopy_cover_fraction
    real (kind=c_float), intent(in)         :: reference_et0

!     call cells%calc_reference_et()
!     call cells%calc_fog()
!     call cells%calc_interception()


    interception_storage = interception_storage + interception * cells%canopy_cover_fraction

    actual_et = min( reference_et0, interception_storage ) * cells%canopy_cover_fraction

    interception_storage = interception_storage - actual_et

   end subroutine calculate_interception_mass_balance


end module mass_balance__interception