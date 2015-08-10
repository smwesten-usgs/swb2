module actual_et__thornthwaite_mather


  use iso_c_binding, only : c_short, c_int, c_float, c_double
  implicit none

contains

  elemental subroutine calculate_actual_et_thornthwaite_mather(                      &
                                                  actual_et,                         &
                                                  impervious_fraction,               &
                                                  soil_storage,                      &
                                                  max_soil_storage,                  &
                                                  precipitation,                     &
                                                  reference_et0,                     &
                                                  crop_coefficient_kcb )

    real (kind=c_float), intent(inout)             :: actual_et
    real (kind=c_float), intent(in)                :: impervious_fraction
    real (kind=c_float), intent(in)                :: soil_storage
    real (kind=c_float), intent(in)                :: max_soil_storage
    real (kind=c_float), intent(in)                :: precipitation
    real (kind=c_float), intent(in)                :: reference_et0
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

    if ( P_minus_PE >= 0.0_c_float ) then

      actual_et = reference_et0 * Kcb

    elseif ( P_minus_PE < 0.0_c_float ) then

      if ( max_soil_storage > 0.0_c_float ) then

        actual_et = precipitation + soil_storage * ( 1.0_c_float - exp( P_minus_PE / max_soil_storage ) )

      else
     
        actual_et = reference_et0

      endif     

    endif

    ! scale actual et value in proportion to the fraction of pervious land area present
    actual_et = actual_et * ( 1.0_c_float - impervious_fraction )

  end subroutine calculate_actual_et_thornthwaite_mather

end module actual_et__thornthwaite_mather
