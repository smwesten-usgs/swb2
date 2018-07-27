module mass_balance__snow

  use iso_c_binding, only        : c_short, c_int, c_float, c_double
  implicit none

contains

  elemental subroutine calculate_snow_mass_balance( snow_storage,         &
                                                    potential_snowmelt,   &
                                                    snowmelt,             &
                                                    interception,         &
                                                    snowfall )

    real (kind=c_float), intent(inout)      :: snow_storage
    real (kind=c_float), intent(inout)      :: snowmelt
    real (kind=c_float), intent(in)         :: potential_snowmelt
    real (kind=c_float), intent(in)         :: interception
    real (kind=c_float), intent(in)         :: snowfall


    snow_storage = max(0.0_c_float, snow_storage + snowfall - interception)

    if( snow_storage > potential_snowmelt ) then

      snowmelt = potential_snowmelt

    else   ! not enough snowcover to satisfy the amount that *could* melt

      snowmelt = snow_storage

    end if

    snow_storage = max( snow_storage - snowmelt, 0.0_c_float )


  end subroutine calculate_snow_mass_balance

end module mass_balance__snow
