module mass_balance__impervious_surface

  use constants_and_conversions
  use iso_c_binding, only        : c_short, c_int, c_float, c_double
  implicit none

  private

  public :: calculate_impervious_surface_mass_balance

  real (kind=c_float), parameter   :: NEAR_ZERO = 1.0e-6_c_float

contains

   elemental subroutine calculate_impervious_surface_mass_balance(      &
                                          surface_storage,              &
                                          actual_et_impervious,         &
   	                                      surface_storage_excess,       &
   	                                      surface_storage_max,          &
                                          storm_drain_capture,          &
                                          storm_drain_capture_fraction, &
   	                                      rainfall,                     &
   	                                      snowmelt,                     &
                                          runoff,                       &
                                          fog,                          &
                                          interception,                 &
                                          reference_et0,                &
                                          pervious_fraction )

    real (kind=c_float), intent(inout)      :: surface_storage
    real (kind=c_float), intent(inout)      :: actual_et_impervious
    real (kind=c_float), intent(inout)      :: surface_storage_excess
    real (kind=c_float), intent(inout)      :: storm_drain_capture
    real (kind=c_float), intent(in)         :: storm_drain_capture_fraction
    real (kind=c_float), intent(in)         :: surface_storage_max
    real (kind=c_float), intent(in)         :: rainfall
    real (kind=c_float), intent(in)         :: snowmelt
    real (kind=c_float), intent(in)         :: runoff
    real (kind=c_float), intent(in)         :: fog
    real (kind=c_float), intent(in)         :: interception
    real (kind=c_float), intent(in)         :: reference_et0
    real (kind=c_float), intent(in)         :: pervious_fraction

    ! [ LOCALS ]
    real (kind=c_float) :: paved_to_unpaved   ! 'wadd' in HWB

!    if ( storm_drain_capture_fraction >= 0.0_c_float ) then
    if ( surface_storage_max > NEAR_ZERO ) then

      surface_storage = surface_storage               &
                        + rainfall                    &
                        + fog                         &
                        + snowmelt                    &
                        - interception                &
                        - runoff

      surface_storage = min( surface_storage, surface_storage_max )

      ! amount is reduced proportional to amount of impervious surface present
      paved_to_unpaved = max( 0.0_c_float,                                     &
                             ( surface_storage - surface_storage_max )         &
                              * ( 1.0_c_float - pervious_fraction)      )
      storm_drain_capture = paved_to_unpaved * storm_drain_capture_fraction
      surface_storage_excess = max( 0.0_c_float,                               &
                                    paved_to_unpaved - storm_drain_capture )

      ! now allow for evaporation
      actual_et_impervious = min( reference_et0, surface_storage )
      surface_storage = max( surface_storage - actual_et_impervious, 0.0_c_float )

    else

      ! no surface storage capacity; assume no impervious surface calcs desired
      surface_storage = 0.0_c_float
      storm_drain_capture = 0.0_c_float
      surface_storage_excess = 0.0_c_float

    endif

  end subroutine calculate_impervious_surface_mass_balance

end module mass_balance__impervious_surface
