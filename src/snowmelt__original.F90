module snowmelt__original

  use iso_c_binding, only   : c_int, c_float, c_double, c_bool
  implicit none

  private

  public :: snowmelt_original_calculate

  real (c_double), parameter    :: FREEZING_F    = 32.0_c_double
  real (c_double), parameter    :: FREEZING_C    = 0.0_c_double
  real (c_double), parameter    :: MELT_INDEX    = 1.5_c_double
  real (c_double), parameter    :: DEGC_PER_DEGF = 0.55555555555555555555556_c_double
  real (c_double), parameter    :: MM_PER_INCH   = 25.4_c_double

contains

  elemental subroutine snowmelt_original_calculate( potential_snowmelt, tmin, tmax, imperial_units )

    real (c_float), intent(inout)      :: potential_snowmelt 
    real (c_float), intent(in)         :: tmin
    real (c_float), intent(in)         :: tmax
    logical ( c_bool), intent(in)      :: imperial_units

    if ( imperial_units ) then

      !> temperatures in degrees Fahrenheit, snowmelt in inches

      ! Vic's port of Wes's code simply tests to see whether the 

      ! from Vic's earliest port of Wes's code: (TMax in Fahrenheit)
      ! 1.5 * (5_T_INT * ( cel%rTMax - rFREEZING) / 228.6)

      if ( ( ( tmin + tmax ) / 2.0_c_double ) > FREEZING_F ) then

        potential_snowmelt = MELT_INDEX * ( tmax - FREEZING_F ) * DEGC_PER_DEGF / MM_PER_INCH

      else 

        potential_snowmelt = 0.0_c_float

      end if

    else

      !> temperatures in degrees Celcius, snowmelt in mm

      if ( ( ( tmin + tmax ) / 2.0_c_double ) > FREEZING_C ) then

        potential_snowmelt = MELT_INDEX * tmax 

      else 

        potential_snowmelt = 0.0_c_float

      end if

    endif  

  end subroutine snowmelt_original_calculate

end module snowmelt__original 