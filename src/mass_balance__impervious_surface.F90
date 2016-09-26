module mass_balance__impervious_surface

  use constants_and_conversions
  use iso_c_binding, only        : c_short, c_int, c_float, c_double
  implicit none

  private

  public :: calculate_impervious_surface_mass_balance

contains

   elemental subroutine calculate_impervious_surface_mass_balance(      &
                                          surface_storage,              & 
                                          actual_et,                    &   
   	                                      surface_storage_excess,       &
   	                                      surface_storage_max,          &
                                          storm_drain_capture,          &
                                          storm_drain_capture_fraction, &                                                                   
   	                                      rainfall,                     &
   	                                      snowmelt,                     &
                                          runoff,                       &
                                          fog,                          &
                                          interception,                 &
                                          reference_et0 )

    real (kind=c_float), intent(inout)      :: surface_storage
    real (kind=c_float), intent(inout)      :: actual_et
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

    ! [ LOCALS ]
    real (kind=c_float) :: difference


    surface_storage = surface_storage               &
                      + rainfall                    &
                      + fog                         &
                      + snowmelt                    &
                      - interception                &
                      - runoff

    ! first determine if surface inputs exceed storage capacity
    if ( surface_storage > surface_storage_max ) then
 
      difference = surface_storage - surface_storage_max
      storm_drain_capture = difference * storm_drain_capture_fraction
      surface_storage_excess = difference - storm_drain_capture

      surface_storage = surface_storage_max

    elseif ( surface_storage < 0.0_c_float ) then

      surface_storage = 0.0_c_float
      surface_storage_excess = 0.0_c_float
      storm_drain_capture = 0.0_c_float

    else

      surface_storage_excess = 0.0_c_float
      storm_drain_capture = 0.0_c_float

    endif  

    ! now allow for evaporation
    if (surface_storage > reference_et0) then

      actual_et = reference_et0
      surface_storage = surface_storage - reference_et0

    else

      actual_et = surface_storage
      surface_storage = 0.0

    endif

    
  end subroutine calculate_impervious_surface_mass_balance    

end module mass_balance__impervious_surface