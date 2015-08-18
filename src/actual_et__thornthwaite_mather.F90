module actual_et__thornthwaite_mather


  use iso_c_binding, only : c_short, c_int, c_float, c_double
  implicit none

  real (kind=c_float), parameter :: NEAR_ZERO = 3.0_c_float * tiny( 0.0_c_float )

contains

  elemental subroutine calculate_actual_et_thornthwaite_mather(                      &
                                                  actual_et,                         &
                                                  soil_storage,                      &
                                                  soil_storage_max,                  &
                                                  precipitation,                     &
                                                  crop_etc )

    real (kind=c_float), intent(inout)             :: actual_et
    real (kind=c_float), intent(in)                :: soil_storage
    real (kind=c_float), intent(in)                :: soil_storage_max
    real (kind=c_float), intent(in)                :: precipitation
    real (kind=c_float), intent(in)                :: crop_etc

    ! [ LOCALS ]
    real (kind=c_float)  :: P_minus_PE

    P_minus_PE = precipitation - crop_etc

    if ( P_minus_PE >= 0.0_c_float ) then

      actual_et = crop_etc

    elseif ( P_minus_PE < 0.0_c_float ) then

      if ( soil_storage_max > NEAR_ZERO ) then

        actual_et = precipitation + soil_storage * ( 1.0_c_float - exp( P_minus_PE / soil_storage_max ) )

      else
     
        actual_et = crop_etc

      endif     

    endif

  end subroutine calculate_actual_et_thornthwaite_mather

end module actual_et__thornthwaite_mather
