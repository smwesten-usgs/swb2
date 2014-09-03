module snowfall__original

  use iso_c_binding
  implicit none

  private

  public :: snowfall_original_calculate

  real (kind=c_float), parameter :: FREEZING = 32.0_c_float

contains

  elemental subroutine snowfall_original_calculate( fSnowfall, fRainfall, fTMin, fTMax, fPrecip)

    real (kind=c_float), intent(inout)  :: fSnowfall
    real (kind=c_float), intent(inout)  :: fRainfall
    real (kind=c_float), intent(in)     :: fTMin
    real (kind=c_float), intent(in)     :: fTMax
    real (kind=c_float), intent(in)     :: fPrecip

    if ( ( (fTMin + fTMax) / 2.0_c_float - ( fTMax - fTMin ) / 3.0_c_float ) <= FREEZING ) then 

      fSnowfall = fPrecip
      fRainfall = 0.0_c_float

    else

      fSnowfall = 0.0_c_float
      fRainfall = fPrecip

    endif

  end subroutine snowfall_original_calculate

end module snowfall__original