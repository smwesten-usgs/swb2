module actual_et__fao56


  use iso_c_binding, only               : c_short, c_int, c_float, c_double
  use constants_and_conversions, only   : in_to_mm, lTRUE, lFALSE
  use parameters, only                  : PARAMS
  implicit none

  real (kind=c_float), allocatable   :: DEPLETION_FRACTION(:)
  real (kind=c_float), parameter     :: NEAR_ZERO = 3.0_c_float * tiny( 0.0_c_float )

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
                                                  adjusted_depletion_fraction_p,     &
                                                  soil_storage,                      &
                                                  depletion_fraction_p,              &
                                                  soil_storage_max,                  &
                                                  infiltration,                      &
                                                  crop_etc )


    real (kind=c_double), intent(inout)            :: actual_et
    real (kind=c_double), intent(inout)            :: adjusted_depletion_fraction_p
    real (kind=c_float), intent(in)                :: depletion_fraction_p
    real (kind=c_double), intent(in)               :: soil_storage
    real (kind=c_float), intent(in)                :: soil_storage_max
    real (kind=c_float), intent(in)                :: infiltration
    real (kind=c_float), intent(in)                :: crop_etc

    ! [ LOCALS ]
    real (kind=c_float)  :: Kcb
    real (kind=c_float)  :: depletion_amount
    real (kind=c_float)  :: p
    real (kind=c_double) :: interim_soil_storage
    real (kind=c_float)  :: fraction_full_PET
    real (kind=c_float)  :: root_constant_ci

    if ( soil_storage_max >= 0.0_c_float .and. soil_storage_max <= NEAR_ZERO ) then

      ! by convention, if the soil_storage_max is approximately zero, we are dealing
      ! with a water body and the actual et should be the potential/crop et
      actual_et = crop_etc

    else

      p = adjust_depletion_fraction_p( p_table_22=depletion_fraction_p,  &
                                       reference_et0=crop_etc )

      adjusted_depletion_fraction_p = p

      ! soil storage value at which actual et begins to decline
      root_constant_ci = ( 1.0_c_float - p ) * soil_storage_max

      interim_soil_storage = soil_storage + infiltration

      ! root constant of 0 implies a depletion fraction of 1; in other words,
      ! there is no declining portion of the AET/PET to AW/AWC relation.
      ! ENTIRE DAY at PET
      if ( root_constant_ci  <= 0.0_c_float ) then

        actual_et = min( crop_etc, interim_soil_storage )

      ! ALL or PARTIAL DAY at PET
      elseif ( interim_soil_storage > root_constant_ci ) then

        ! calculate fraction of day that would be at full reference ET values
        if ( crop_etc > 0.0_c_float ) then

          fraction_full_PET = (interim_soil_storage - root_constant_ci) / crop_etc

        else

          fraction_full_PET = 99.

        endif

        ! there is enough soil storage to cover withdrawal of soil moisture at full reference ET values
        ! for the entire day
        if ( fraction_full_PET >= 1 ) then

          actual_et = crop_etc

        ! part of day at full PET
        else

          actual_et = crop_etc * fraction_full_PET                                                                &
                      + root_constant_ci                                                                          &
                         * ( 1.0_c_float - exp( - crop_etc * ( 1.0_c_float - fraction_full_PET )                  &
                            / root_constant_ci ) )

        endif

      ! ENTIRE DAY at LESS THAN PET
      else

          actual_et = interim_soil_storage * ( 1.0_c_float - exp( - crop_etc / root_constant_ci ) )

      endif

    endif

  end subroutine calculate_actual_et_fao56


end module actual_et__fao56
