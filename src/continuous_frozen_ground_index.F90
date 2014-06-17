module continuous_frozen_ground_index

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_long_long
  use constants_and_conversions
  implicit none

  private

  public :: CFGI_LL, CFGI_UL
  public :: CFGI
  public update_continuous_frozen_ground_index


  real (kind=c_float)               :: CFGI_LL = 55.
  real (kind=c_float)               :: CFGI_UL = 83.
  
  real (kind=c_float), allocatable  :: CFGI(:)

contains  



  !> Update the continuous frozen ground index (CFGI) for a cell.
  !!
  !! Computes the continuous frozen ground index 
  !! @param[inout] rCFGI   Continuous frozen ground index to be updated.
  !! @param[in]    rTAvg_F Mean daily air temperature, in \degF
  !!
  !! @note Implemented as per Molnau and Bissel (1983).
  !!
  !! @note Molnau, M. and Bissell, V.C., 1983, A continuous frozen ground index for 
  !! flood forecasting: In Proceedings 51st Annual Meeting Western Snow Conference, 
  !! 109–119, Canadian Water Resources Assoc. Cambridge, Ont.
  elemental subroutine update_continuous_frozen_ground_index( rCFGI, rTMax_F, rTMin_F, rSnowCover )

    ! [ ARGUMENTS ]
    real (kind=c_float), intent(inout)       :: rCFGI
    real (kind=c_float), intent(in)          :: rTMax_F
    real (kind=c_float), intent(in)          :: rTMin_F
    real (kind=c_float), intent(in)          :: rSnowCover

    ! [ LOCALS ]
    real (kind=c_float), parameter    :: rDecay_Coefficient_A                      = 0.97_c_float
    real (kind=c_float), parameter    :: rSnow_Reduction_Coefficient_Freezing      = 0.08_c_float
    real (kind=c_float), parameter    :: rSnow_Reduction_Coefficient_Thawing       = 0.5_c_float     
    real (kind=c_float), parameter    :: rCM_PER_INCH                              = 2.54_c_float

    real (kind=c_float) :: rTAvg_C              ! temporary variable holding avg temp in C
    real (kind=c_float) :: rSnowDepthCM         ! snow depth in centimeters


    rTAvg_C = F_to_C( (rTMax_F + rTMin_F) / 2.0_c_float )

    ! assuming snow depth is 10 times the water content of the snow in inches
    rSnowDepthCM = rSnowCover * 10.0_c_float * rCM_PER_INCH

    associate( Tavg => rTavg_C,                                              &
               A => rDecay_Coefficient_A,                                    &
               CFGI => rCFGI,                                                &
               K_freeze => rSnow_Reduction_Coefficient_Freezing,             &
               K_thaw => rSnow_Reduction_Coefficient_Thawing )

      if( Tavg > 0.0_c_float ) then

        CFGI = max( A * CFGI - Tavg * exp ( -0.4_c_float * K_thaw * rSnowDepthCM ), 0.0_c_float )

      else ! temperature is below freezing
        
        CFGI = max( A * CFGI - Tavg * exp ( -0.4_c_float * K_freeze * rSnowDepthCM ), 0.0_c_float )

      end if

    end associate  

  end subroutine update_continuous_frozen_ground_index

end module continuous_frozen_ground_index