module snowmelt__original

  use iso_c_binding
  implicit none

  private

  public :: calculate_snowmelt_original

  real (kind=c_float), parameter    :: FREEZING      = 32.0_c_float
  real (kind=c_float), parameter    :: MELT_INDEX    = 1.5_c_float
  real (kind=c_double), parameter   :: DEGC_PER_DEGF = 0.55555555555555555555556_c_double
  real (kind=c_double), parameter   :: MM_PER_INCH   = 25.4_c_double

contains

  elemental subroutine calculate_snowmelt_original( fSnowmelt, fSnow_storage, fTMin, fTMax )

    real (kind=c_float), intent(inout)    :: fSnowmelt
    real (kind=c_float), intent(inout)    :: fSnow_storage
    real (kind=c_float), intent(in)       :: fTMin
    real (kind=c_float), intent(in)       :: fTMax

    ! [ LOCALS ]
    real (kind=c_float) :: fPotentialMelt

    fSnowmelt = 0.0_c_float

    ! If average air temp > freezing, calculate potential meltwater
    if ( ( (fTMin + fTMax) / 2.0_c_float ) > FREEZING ) then

      fPotentialMelt = MELT_INDEX * ( fTMax - FREEZING ) * DEGC_PER_DEGF / MM_PER_INCH

      if( fSnow_storage > fPotentialMelt) then

        fSnowmelt = fPotentialMelt

      else   ! not enough snowcover to satisfy the amount that *could* melt
        
        fSnowmelt = fSnow_storage
      
      end if

    end if

  end subroutine calculate_snowmelt_original  

end module snowmelt__original 