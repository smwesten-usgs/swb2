!> @file
!>  Contains a single module, \ref actual_et__thornthwaite_mather, which
!>  provides support for calculating actual evapotranspiration by means of an 
!>  approximation to the Thornthwaite-Mather soil-moisture retention tables. 

!> Calculate actual ET by means of an approximation to the Thornthwaite-Mather
!> soil-moisture-retention tables.
module actual_et__thornthwaite_mather


  use iso_c_binding, only : c_short, c_int, c_float, c_double
  implicit none

  real (c_float), parameter :: NEAR_ZERO = 1.0e-9_c_float

contains

  elemental subroutine calculate_actual_et_thornthwaite_mather(                      &
                                                  actual_et,                         &
                                                  soil_storage,                      &
                                                  soil_storage_max,                  &
                                                  infiltration,                      &
                                                  crop_etc )

    real (c_double), intent(inout)            :: actual_et
    real (c_double), intent(in)               :: soil_storage
    real (c_float), intent(in)                :: soil_storage_max
    real (c_float), intent(in)                :: infiltration
    real (c_float), intent(in)                :: crop_etc

    ! [ LOCALS ]
    real (c_double) :: P_minus_PE
    real (c_double) :: soil_storage_temp

    P_minus_PE = real(infiltration, c_double) - real(crop_etc, c_double)

    if ( P_minus_PE >= 0.0_c_double ) then

      actual_et = crop_etc

    elseif ( P_minus_PE < 0.0_c_double ) then

      if ( soil_storage_max > NEAR_ZERO ) then

!        soil_storage_temp = max( 0.0, min( soil_storage_max, precipitation + soil_storage ) )
!        actual_et =  soil_storage_temp * ( 1.0_c_float - exp( -crop_etc / soil_storage_max ) )

        ! NOTE: there is a difference in approaches between various practitioners... the Hawaii Water Budget code
        !       calculates actual et on a temporary soil moisture value that includes the moisture received 
        !       on the current day. Alley calculates actual et on the basis of the previous days' soil moisture

        soil_storage_temp = soil_storage * exp( P_minus_PE / real(soil_storage_max, c_double))
        actual_et = soil_storage - soil_storage_temp

      else
      ! open water - limit actual et to the lesser of the reference ET *or* the amount of water 'infiltrated'

        actual_et = min(real(infiltration, c_double), real(crop_etc, c_double))

      endif

    endif

  end subroutine calculate_actual_et_thornthwaite_mather

end module actual_et__thornthwaite_mather
