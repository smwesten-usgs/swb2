module snowfall__original

  use iso_c_binding
  implicit none

  private

  public :: snowfall_original_calculate

  real (c_double), parameter :: FREEZING = 32.0_c_double

contains

  elemental subroutine snowfall_original_calculate( snowfall, net_snowfall,    &
                                                    rainfall, net_rainfall,    &
                                                    tmin, tmax,                &
                                                    interception,              &
                                                    gross_precipitation)

    real (c_double), intent(inout)  :: snowfall
    real (c_double), intent(inout)  :: net_snowfall
    real (c_double), intent(inout)  :: rainfall
    real (c_double), intent(inout)  :: net_rainfall
    real (c_double), intent(in)     :: tmin
    real (c_double), intent(in)     :: tmax
    real (c_double), intent(in)     :: interception
    real (c_double), intent(in)     :: gross_precipitation

    ! classify gross_precipitation as snowfall if condition is met
    if ( ( (tmin + tmax) / 2.0_c_double - ( tmax - tmin ) / 3.0_c_double ) <= FREEZING ) then

      snowfall = gross_precipitation
      net_snowfall = gross_precipitation - interception
      rainfall = 0.0_c_double
      net_rainfall = 0.0_c_double

    else  ! rainfall

      snowfall = 0.0_c_double
      net_snowfall = 0.0_c_double
      rainfall = gross_precipitation
      net_rainfall = gross_precipitation - interception

    endif

  end subroutine snowfall_original_calculate

end module snowfall__original
