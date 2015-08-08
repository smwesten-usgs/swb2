module actual_evapotranspiration__fao56


  use iso_c_binding, only               : c_short, c_int, c_float, c_double
  use constants_and_conversions, only   : in_to_mm, lTRUE, lFALSE
  use parameters, only                  : PARAMS
  implicit none

  real (kind=c_float), allocatable   :: DEPLETION_FRACTION(:)

contains

  subroutine initialize_actual_et_FAO56( ) 

    call PARAMS%get_parameters( sKey="Depletion_Fraction", fValues=DEPLETION_FRACTION, lFatal=lTRUE )

  end subroutine initialize_actual_et_FAO56

  !> Adjust the depletion fraction based on current reference ET0.
  !!
  !! From FAO-56: "The fraction p is a function of the evaporation power of the atmosphere.
  !! At low rates of ETc, the p values listed in Table 22 are higher than at high rates of ETc.
  !! For hot dry weather conditions, where ETc is high, p is 10-25% less than the values 
  !! presented in Table 22, and the soil is still relatively wet when the stress starts to occur.
  !! When the crop evapotranspiration is low, p will be up to 20% more than the listed values.
  !! 
  !! @param[in] p_table_22 This is the unadjusted depletion fraction value; FAO-56
  !!     table 22 gives values of the depletion fraction relative to a reference ET0 value of 5mm.
  !! @param[in] reference_et0 The reference ET0 to which the depletion fraction will be
  !!     adjusted.
  !! @note Discussed as a footnote to Table 22, FAO-56, Allen and others.
  !!   See @ref http://www.fao.org/docrep/x0490e/x0490e0e.htm#TopOfPage for details.

  elemental function adjust_depletion_fraction_p( p_table_22, reference_et0 )   result( p )

    real (kind=c_float), intent(in)      :: p_table_22
    real (kind=c_float), intent(in)      :: reference_et0
    real (kind=c_float)                  :: p

    p = p_table_22 + 0.04_c_float * ( 5.0_c_float - in_to_mm( reference_et0 ) )

    p = min( p, 0.8_c_float )
    p = max( p, 0.1_c_float )

  end function adjust_depletion_fraction_p

!----------------------------------------------------------------------------------------------------

	elemental subroutine calculate_actual_et_fao56( actual_et,                         &

                                                  soil_storage,                      &
                                                  depletion_fraction_p,              &
                                                  max_soil_storage,                  &
                                                  precipitation,                     &
                                                  reference_et0,                     &
                                                  crop_coefficient_kcb )

    real (kind=c_float), intent(inout)             :: actual_et
    real (kind=c_float), intent(in)                :: depletion_fraction_p
    real (kind=c_float), intent(in)                :: soil_storage
    real (kind=c_float), intent(in)                :: max_soil_storage
    real (kind=c_float), intent(in)                :: precipitation
    real (kind=c_float), intent(in)                :: reference_et0
    real (kind=c_float), intent(in), optional      :: crop_coefficient_kcb

    ! [ LOCALS ]
    real (kind=c_float)  :: P_minus_PE
    real (kind=c_float)  :: Kcb
    real (kind=c_float)  :: depletion_amount
    real (kind=c_float)  :: p

    if ( present( crop_coefficient_kcb ) ) then
      Kcb = crop_coefficient_kcb
    else
      Kcb = 1.0_c_float
    endif  

    P_minus_PE = precipitation - reference_et0 * Kcb

    p = adjust_depletion_fraction_p( p_table_22=depletion_fraction_p,  &
                                     reference_et0=reference_et0 )

    if ( P_minus_PE >= 0.0_c_float ) then

      actual_et = reference_et0 * Kcb

    elseif( P_minus_PE < 0.0_c_float ) then

      if ( max_soil_storage > 0.0_c_float ) then

        actual_et = precipitation + soil_storage * ( 1.0_c_float - exp( P_minus_PE / max_soil_storage ) )

      else
     
        actual_et = reference_et0

      endif     

     endif

  end subroutine calculate_actual_et_fao56

end module actual_evapotranspiration__fao56
