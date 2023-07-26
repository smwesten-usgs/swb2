module actual_et__thornthwaite_mather_eqns

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use exceptions
  implicit none

  private

  public :: initialize_actual_et_thornthwaite_mather_eqns
  public :: calculate_actual_et_thornthwaite_mather_eqns

  real (c_float), parameter    :: NEAR_ZERO = 1.0e-9_c_float
  real (c_float), allocatable  :: ACCUMULATED_POTENTIAL_WATER_LOSS(:)

  real (c_double), parameter :: TM_SLOPE_TERM      = 0.478769194198665_c_double
  real (c_double), parameter :: TM_EXPONENT_TERM   = -1.03678439421169_c_double

contains

subroutine initialize_actual_et_thornthwaite_mather_eqns ( soil_moisture, max_soil_moisture )

  real (c_double), intent(in)     :: soil_moisture(:)
  real (c_float), intent(in)     :: max_soil_moisture(:)

  ! [ LOCALS ]
  integer (c_int) :: status

  allocate( ACCUMULATED_POTENTIAL_WATER_LOSS( size( soil_moisture) ), stat=status )
  call assert( status==0, "Problem allocating memory.", __FILE__, __LINE__ )

 ! calculate APWL from equation
 call thornthwaite_mather_APWL(ACCUMULATED_POTENTIAL_WATER_LOSS, max_soil_moisture, soil_moisture)

end subroutine initialize_actual_et_thornthwaite_mather_eqns

!--------------------------------------------------------------------------------------------------

!> Return the current soil moisture given the max soil-water-capacity and current APWL.

elemental subroutine thornthwaite_mather_soil_moisture(soil_moisture, max_soil_moisture, APWL)

  real (c_double), intent(inout) :: soil_moisture
  real (c_float), intent(in)    :: max_soil_moisture
  real (c_float), intent(in)    :: APWL


  ! equation as implemented in R;
  ! sm.df$y = maximum soil-water capacity
  ! sm.df$x = APWL
  ! 10^(log10(sm.df$y) - (s.opt[[1]]*sm.df$y^e.opt[[1]]) * sm.df$x)

  if( max_soil_moisture > 0.0_c_float ) then

    soil_moisture = 10_c_double**( log10( max_soil_moisture )        &
                      - ( abs( APWL ) * TM_SLOPE_TERM              &
                      * max_soil_moisture**TM_EXPONENT_TERM ) )

  else

    soil_moisture = 0.0_c_float

  endif

end subroutine thornthwaite_mather_soil_moisture

!------------------------------------------------------------------------------

  !> Return an updated APWL given the max soil-water-capacity and current soil storage.

  elemental subroutine thornthwaite_mather_APWL(APWL, max_soil_moisture, soil_moisture)

    real (c_float), intent(inout)  :: APWL
    real (c_float), intent(in)     :: max_soil_moisture
    real (c_double), intent(in)     :: soil_moisture

    ! equation as implemented in R;
    ! sm.df$y = maximum soil-water capacity
    ! sm.df$x = APWL
    ! (log10(sm.df$y) - log10(sm.df$pred)) / (s.opt[[1]] * sm.df$y^e.opt[[1]])

    if(max_soil_moisture > 0.0_c_float .and. soil_moisture > 0.0_c_float ) then

      APWL = -( log10( max_soil_moisture ) - log10( soil_moisture ) )            &
                  / ( TM_SLOPE_TERM * max_soil_moisture**TM_EXPONENT_TERM )

    else

      APWL = 0.0_c_float

    endif

  end subroutine thornthwaite_mather_APWL

!--------------------------------------------------------------------------------------------------

subroutine calculate_actual_et_thornthwaite_mather_eqns(                             &
                                                  actual_et,                         &
                                                  soil_moisture,                     &
                                                  max_soil_moisture,                 &
                                                  precipitation,                     &
                                                  crop_etc,                          &
                                                  indx )

    real (c_double), intent(inout)            :: actual_et
    real (c_double), intent(in)               :: soil_moisture
    real (c_float), intent(in)                :: max_soil_moisture
    real (c_float), intent(in)                :: precipitation
    real (c_float), intent(in)                :: crop_etc
    integer (c_int), intent(in)               :: indx

    ! [ LOCALS ]
    real (c_float)  :: P_minus_PE
    real (c_double)  :: temp_soil_moisture

    P_minus_PE = precipitation - crop_etc

    associate ( APWL => ACCUMULATED_POTENTIAL_WATER_LOSS( indx ) )

      if ( P_minus_PE >= 0.0_c_float ) then

        actual_et = crop_etc

        if ( max_soil_moisture > NEAR_ZERO ) then

          temp_soil_moisture = min( max_soil_moisture, soil_moisture + P_minus_PE )
          call thornthwaite_mather_APWL( APWL, max_soil_moisture, temp_soil_moisture )

        endif

      elseif ( P_minus_PE < 0.0_c_float ) then

        if ( max_soil_moisture > NEAR_ZERO ) then

          ! more PET than can be extracted from soil; update APWL and back-calculate
          ! updated soil moisture
          APWL = APWL + P_minus_PE
          call thornthwaite_mather_soil_moisture( temp_soil_moisture, max_soil_moisture, APWL )
          actual_et = soil_moisture - temp_soil_moisture

        else

          actual_et = precipitation

        endif

      endif

    end associate

  end subroutine calculate_actual_et_thornthwaite_mather_eqns

end module actual_et__thornthwaite_mather_eqns
