module mass_balance__snow

  use iso_c_binding, only        : c_short, c_int, c_float, c_double
  implicit none

contains

  elemental subroutine calculate_snow_mass_balance( snow_storage,         &
                                                    potential_snowmelt,   &
                                                    snowmelt,             &
                                                    net_snowfall )

    real (c_double), intent(inout)      :: snow_storage
    real (c_double), intent(inout)      :: snowmelt
    real (c_double), intent(in)         :: potential_snowmelt
    real (c_double), intent(in)         :: net_snowfall


    snow_storage = max(0.0_c_double, snow_storage + net_snowfall )

    if( snow_storage > potential_snowmelt ) then

      snowmelt = potential_snowmelt
      snow_storage = snow_storage - snowmelt

    else   ! not enough snowcover to satisfy the amount that *could* melt

      snowmelt = snow_storage
      snow_storage = 0.0_c_double

    end if

  end subroutine calculate_snow_mass_balance

end module mass_balance__snow
