module mass_balance__impervious_surface

  use constants_and_conversions
  use iso_c_binding, only        : c_short, c_int, c_float, c_double
  implicit none

  private

  public :: calculate_impervious_surface_mass_balance

  real (c_float), parameter   :: NEAR_ZERO = 1.0e-6_c_float

contains

   elemental subroutine calculate_impervious_surface_mass_balance(      &
                                          surface_storage,              &
                                          actual_et_impervious,         &
   	                                      paved_to_unpaved,             &
   	                                      surface_storage_max,          &
                                          storm_drain_capture,          &
                                          storm_drain_capture_fraction, &
   	                                      net_rainfall,                 &
   	                                      snowmelt,                     &
                                          runon,                        &
                                          runoff,                       &
                                          fog,                          &
                                          reference_et0,                &
                                          pervious_fraction )

    real (c_double), intent(inout)     :: surface_storage
    real (c_double), intent(inout)     :: actual_et_impervious
    real (c_float), intent(inout)      :: paved_to_unpaved           ! 'wadd' in HWB
    real (c_float), intent(inout)      :: storm_drain_capture
    real (c_float), intent(in)         :: storm_drain_capture_fraction
    real (c_float), intent(in)         :: surface_storage_max
    real (c_float), intent(in)         :: net_rainfall
    real (c_float), intent(in)         :: snowmelt
    real (c_float), intent(in)         :: runon
    real (c_float), intent(in)         :: runoff
    real (c_float), intent(in)         :: fog
    real (c_double), intent(in)        :: reference_et0
    real (c_float), intent(in)         :: pervious_fraction

    ! [ LOCALS ]
    real (c_float) :: surface_storage_excess
    real (c_float) :: impervious_fraction
    real (c_float) :: surface_storage_l

!    if ( storm_drain_capture_fraction >= 0.0_c_float ) then


    ! NOTE: removed this conditional 16 May 2018: it appears that
    !       testing of this sort will underrepresent water in a cell
    !       because although the surface storage may be zero, the
    !       fraction impervious area may be nonzero and therefore will
    !       intercept water that should be shunted to the pervious area of
    !       the cell.

!    if ( surface_storage_max > NEAR_ZERO ) then

    impervious_fraction = 1.0_c_float - pervious_fraction

    surface_storage = surface_storage                                        &
                      + net_rainfall                                         &
                      + fog                                                  &
                      + snowmelt                                             &
                      - runoff

    ! **amount is reduced proportional to amount of impervious surface present**
    surface_storage_excess = max( 0.0_c_float,                               &
                           ( surface_storage - surface_storage_max )         &
                            * impervious_fraction )

    surface_storage = min( surface_storage, surface_storage_max )

    storm_drain_capture = surface_storage_excess * storm_drain_capture_fraction

    paved_to_unpaved = max( 0.0_c_float,                                         &
                                  surface_storage_excess - storm_drain_capture )

    ! now allow for evaporation
    actual_et_impervious = min( reference_et0, surface_storage )
    surface_storage = max( surface_storage - actual_et_impervious, 0.0_c_double )

  end subroutine calculate_impervious_surface_mass_balance

end module mass_balance__impervious_surface
