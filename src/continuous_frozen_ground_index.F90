module continuous_frozen_ground_index

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_long_long
  use constants_and_conversions
  use exceptions
  implicit none

  private

  public :: CFGI_LL, CFGI_UL
  public :: CFGI
  public update_continuous_frozen_ground_index, initialize_continuous_frozen_ground_index


  real (kind=c_float)               :: CFGI_LL = 55.
  real (kind=c_float)               :: CFGI_UL = 83.
  
  real (kind=c_float), allocatable  :: CFGI(:)

  ! concept: any necessary state or ancillary variables that may not be of use in all
  !          applications should be kept in the module where they are used. In other words,
  !          the "model_domain" object will contain just the bare minimum state and ancillary
  !          variables needed to calculate a soil moisture mass balance. Any other variables that are
  !          of use only to specific modules will be kept in the modules where they are used.
  !
  !          CFGI is one of these variables.

contains  

  subroutine initialize_continuous_frozen_ground_index( iNumActiveCells )

    integer (kind=c_int), intent(in)  :: iNumActiveCells

    ! [ LOCALS ]
    integer (kind=c_int)  :: iStat

    allocate( CFGI( iNumActiveCells ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    CFGI = 0.0_c_float 

  end subroutine initialize_continuous_frozen_ground_index
  
!--------------------------------------------------------------------------------------------------     

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
  elemental subroutine update_continuous_frozen_ground_index( fCFGI, fTMax_F, fTMin_F, fSnowCover )

    ! [ ARGUMENTS ]
    real (kind=c_float), intent(inout)       :: fCFGI
    real (kind=c_float), intent(in)          :: fTMax_F
    real (kind=c_float), intent(in)          :: fTMin_F
    real (kind=c_float), intent(in)          :: fSnowCover

    ! [ LOCALS ]
    real (kind=c_float), parameter    :: fDecay_Coefficient_A                      = 0.97_c_float
    real (kind=c_float), parameter    :: fSnow_Reduction_Coefficient_Freezing      = 0.08_c_float
    real (kind=c_float), parameter    :: fSnow_Reduction_Coefficient_Thawing       = 0.5_c_float     
    real (kind=c_float), parameter    :: fCM_PER_INCH                              = 2.54_c_float

    real (kind=c_float) :: fTAvg_C              ! temporary variable holding avg temp in C
    real (kind=c_float) :: fSnowDepthCM         ! snow depth in centimeters


    fTAvg_C = F_to_C( (fTMax_F + fTMin_F) / 2.0_c_float )

    ! assuming snow depth is 10 times the water content of the snow in inches
    fSnowDepthCM = fSnowCover * 10.0_c_float * fCM_PER_INCH

    associate( Tavg => fTavg_C,                                              &
               A => fDecay_Coefficient_A,                                    &
               CFGI => fCFGI,                                                &
               K_freeze => fSnow_Reduction_Coefficient_Freezing,             &
               K_thaw => fSnow_Reduction_Coefficient_Thawing )

      if( Tavg > 0.0_c_float ) then

        CFGI = max( A * CFGI - Tavg * exp ( -0.4_c_float * K_thaw * fSnowDepthCM ), 0.0_c_float )

      else ! temperature is below freezing
        
        CFGI = max( A * CFGI - Tavg * exp ( -0.4_c_float * K_freeze * fSnowDepthCM ), 0.0_c_float )

      end if

    end associate  

  end subroutine update_continuous_frozen_ground_index

end module continuous_frozen_ground_index