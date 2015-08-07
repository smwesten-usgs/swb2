module actual_evapotranspiration__thornthwaite_mather


  use iso_c_binding, only : c_short, c_int, c_float, c_double
  implicit none

contains

  elemental subroutine calculate_actual_et_thornthwaite_mather(                      &
                                                  soil_storage,                      &
                                                  max_soil_storage,                  &
                                                  precipitation,                     &
                                                  reference_et0,                     &
                                                  actual_et,                         &
                                                  crop_coefficient_kcb )

    real (kind=c_float), intent(in)                :: soil_storage
    real (kind=c_float), intent(in)                :: max_soil_storage
    real (kind=c_float), intent(in)                :: precipitation
    real (kind=c_float), intent(in)                :: reference_et0
    real (kind=c_float), intent(out)               :: actual_et
    real (kind=c_float), intent(in), optional      :: crop_coefficient_kcb

    ! [ LOCALS ]
    real (kind=c_float)  :: P_minus_PE
    real (kind=c_float)  :: Kcb

    if ( present( crop_coefficient_kcb ) ) then
      Kcb = crop_coefficient_kcb
    else
      Kcb = 1.0_c_float
    endif  

    P_minus_PE = precipitation - reference_et0 * Kcb

    select case( P_minus_PE )

      !> $P - PE >= 0$: more precip than evaporation; actual ET equals potential ET
      case ( >= 0.0_c_float )

        actual_et = reference_et0 * Kcb

      !> $P - PE < 0$: less precip than evaporation; actual ET limited by amount that can be
      !!               extracted from soil at current saturation fraction
      case ( < 0.0_c_float )

        if ( max_soil_storage > 0.0_c_float ) then

          actual_et = precipitation + soil_storage * ( 1.0_c_float - exp( P_minus_PE / max_soil_storage ) )

        else
       
          actual_et = reference_et0

        endif     

      case default

    end select  

  end subroutine calculate_actual_et_thornthwaite_mather

end module actual_evapotranspiration__thornthwaite_mather
