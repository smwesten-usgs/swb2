module actual_et__thornthwaite_mather


  use iso_c_binding, only : c_short, c_int, c_float, c_double
  implicit none

  real (kind=c_float), parameter :: NEAR_ZERO = 3.0_c_float * tiny( 0.0_c_float )

contains

  elemental subroutine calculate_actual_et_thornthwaite_mather(                      &
                                                  actual_et,                         &
                                                  actual_et_2,                       &
                                                  actual_et_3,                       &
                                                  soil_storage,                      &
                                                  soil_storage_max,                  &
                                                  precipitation,                     &
                                                  crop_etc )

    real (kind=c_float), intent(inout)             :: actual_et
    real (kind=c_float), intent(inout)             :: actual_et_2
    real (kind=c_float), intent(inout)             :: actual_et_3    
    real (kind=c_float), intent(in)                :: soil_storage
    real (kind=c_float), intent(in)                :: soil_storage_max
    real (kind=c_float), intent(in)                :: precipitation
    real (kind=c_float), intent(in)                :: crop_etc

    ! [ LOCALS ]
    real (kind=c_float)  :: P_minus_PE
    real (kind=c_double)  :: soil_storage_temp, soil_storage_temp_2, soil_storage_temp_3
    real (kind=c_double)  :: soil_storage_delta, soil_storage_temp_1
    real (kind=c_double)  :: k1, k2, k3, k4
    real (kind=c_double)  :: AET_1, AET_2, AET_3, AET_4, AET_mean

    P_minus_PE = precipitation - crop_etc

    if ( P_minus_PE >= 0.0_c_float ) then

      actual_et = crop_etc
      actual_et_2 = -9999.

    elseif ( P_minus_PE < 0.0_c_float ) then

      if ( soil_storage_max > NEAR_ZERO ) then

        soil_storage_temp = max( 0.0, min( soil_storage_max, precipitation + soil_storage ) )

        ! AET based on current + infil conditions
        k1 = soil_storage_temp / soil_storage_max
        AET_1 = crop_etc * soil_storage_temp / soil_storage_max

        soil_storage_temp_1 = soil_storage_temp - AET_1 / 2.0

        ! calculate AET_2 based on interim soil moisture value
        k2 = soil_storage_temp_1 / soil_storage_max
        AET_2 = crop_etc * soil_storage_temp_1 / soil_storage_max

        soil_storage_temp_2 = soil_storage_temp - AET_2 / 2.0

        k3 = soil_storage_temp_2 / soil_storage_max
        AET_3 = crop_etc * soil_storage_temp_2 / soil_storage_max

        ! calculate AET/PET as though AET_1 were correct
        soil_storage_temp_3 = soil_storage_temp - AET_1
        k4 = soil_storage_temp_3 / soil_storage_max
        AET_4 = crop_etc * soil_storage_temp_3 / soil_storage_max
 
        actual_et_2 = AET_1
        actual_et_3 = ( AET_1 + 2. * AET_2 + 2. * AET_3 + AET_4 ) / 6.0_c_float

                    ! precipitation + 

        actual_et =  soil_storage_temp * ( 1.0_c_float - exp( -crop_etc / soil_storage_max ) )

      else
      
        actual_et_2 = -9999.
        actual_et = crop_etc

      endif     

    endif

  end subroutine calculate_actual_et_thornthwaite_mather

end module actual_et__thornthwaite_mather
