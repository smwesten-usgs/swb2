!> @file
!>  Contains a single module, \ref et_crop_coefficients, which
!>  provides support for modifying reference ET through the use of
!> crop coefficients

!>  Provide support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module actual_et__fao56__two_stage

  use iso_c_binding, only              : c_short, c_int, c_float, c_double
  use constants_and_conversions, only  : TRUE, M_PER_FOOT, in_to_mm
  use fstring_list, only                : FSTRING_LIST_T, create_list
  use parameters, only                 : PARAMS
  use crop_coefficients__FAO56, only   : KCB_l, KCB_MIN, KCB_INI, KCB_MID, KCB_END,    &
                                         JAN, DEC, KCB_METHOD_MONTHLY_VALUES,         &
                                         KCB_METHOD
  implicit none

  real (c_float), allocatable   :: REW_l(:,:)
  real (c_float), allocatable   :: TEW_l(:,:)
  real (c_float), allocatable   :: DEPLETION_FRACTION(:)
  real (c_float), allocatable   :: MEAN_PLANT_HEIGHT(:)

contains

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

  elemental function adjust_depletion_fraction_p( landuse_index, reference_et0 )   result( p )

    integer ( c_int), intent(in)    :: landuse_index
    real (c_float), intent(in)      :: reference_et0
    real (c_double)                 :: p

    p = real(DEPLETION_FRACTION( landuse_index ),c_double) + 0.04_c_double         &
        * ( 5.0_c_double - in_to_mm( real(reference_et0, c_double ) ) )

    p = min( p, 0.8_c_double )
    p = max( p, 0.1_c_double )

  end function adjust_depletion_fraction_p

!------------------------------------------------------------------------------

  subroutine actual_et_FAO56_two_stage_initialize( )

    use parameters, only        : PARAMS, PARAMS_DICT

    integer (c_int)               :: number_of_landuses
    type(FSTRING_LIST_T)                :: slList
    integer (c_int), allocatable  ::  landuse_table_codes(:)

    ! create list of possible table headings to look for...
    call slList%append( "LU_Code" )
    call slList%append( "Landuse_Lookup_Code" )

    !> Determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=slList, iValues=landuse_table_codes )
    number_of_landuses = count( landuse_table_codes >= 0 )
    call slList%clear()

   ! Retrieve and populate the Readily Evaporable Water (REW_l table values
   CALL PARAMS%get_parameters( fValues=REW_l, sPrefix="REW_", iNumRows=number_of_landuses, lFatal=TRUE )

   ! Retrieve and populate the Total Evaporable Water (TEW_l table values
   CALL PARAMS%get_parameters( fValues=TEW_l, sPrefix="TEW_", iNumRows=number_of_landuses, lFatal=TRUE )

   call PARAMS%get_parameters( sKey="Mean_Plant_Height", fValues=MEAN_PLANT_HEIGHT, lFatal=TRUE )

   slList = create_list("Depletion_fraction, Plant_stress_depletion_fraction")

   call PARAMS%get_parameters( slKeys=slList, fValues=DEPLETION_FRACTION, lFatal=TRUE )
   call slList%clear()

 end subroutine actual_et_FAO56_two_stage_initialize

!------------------------------------------------------------------------------

elemental function calculate_evaporation_reduction_coefficient_Kr( landuse_index,                          &
                                                                   soil_group,                             &
                                                                   soil_moisture_deficit )   result( Kr )

  ! [ ARGUMENTS ]
  integer (c_int), intent(in)  :: landuse_index
  integer (c_int), intent(in)  :: soil_group
  real (c_double), intent(in)  :: soil_moisture_deficit

  ! [ RESULT ]
  real (c_double) :: Kr

  associate( REW => REW_l( landuse_index, soil_group ),         &
             TEW => TEW_l( landuse_index, soil_group ) )

    if ( soil_moisture_deficit <= REW ) then
      Kr = 1._c_double
    elseif ( soil_moisture_deficit < TEW ) then
      Kr = ( real(TEW, c_double) - soil_moisture_deficit )                &
          / ( real(TEW, c_double) - real(REW, c_double) + 1.0E-8)
    else
      Kr = 0._c_double
    endif

  end associate

end function calculate_evaporation_reduction_coefficient_Kr

!------------------------------------------------------------------------------

!> This function estimates the fraction of the ground covered by
!> vegetation during the growing season
!> @note Implemented as equation 76, FAO-56, Allen and others
elemental function calculate_fraction_exposed_and_wetted_soil_fc( landuse_index, Kcb )   result ( few )

  ! [ ARGUMENTS ]
  integer (c_int), intent(in)    :: landuse_index
  real (c_float), intent(in)     :: Kcb

  ! [ RESULT ]
  real (c_float) :: few

  ! [ LOCALS ]
  real (c_float) :: r_fc
  real (c_double) :: numerator
  real (c_double) :: denominator
  real (c_double) :: exponent

  numerator = Kcb - KCB_l( KCB_MIN, landuse_index)
  denominator =  KCB_l( KCB_MID, landuse_index)  -  KCB_l( KCB_MIN, landuse_index)
  exponent = 1.0 + 0.5 * MEAN_PLANT_HEIGHT( landuse_index ) * M_PER_FOOT

  if( denominator > 0.0_c_double ) then
    r_fc = ( numerator / denominator ) ** exponent
  else
    r_fc = 1.0_c_float
  endif

  few = 1.0_c_float - r_fc

  if ( few < 0._c_float ) few = 0.0_c_float
  if ( few > 1._c_float ) few = 1.0_c_float

end function calculate_fraction_exposed_and_wetted_soil_fc

!------------------------------------------------------------------------------

!> This function estimates Ke, the bare surface evaporation coefficient
!> @note Implemented as equation 71, FAO-56, Allen and others
elemental function calculate_surface_evap_coefficient_ke( landuse_index, Kcb, Kr )     result( Ke )

  ! [ ARGUMENTS ]
  integer (c_int), intent(in)   :: landuse_index
  real (c_float), intent(in)    :: Kcb
  real (c_double), intent(in)   :: Kr
  real (c_double)               :: Ke

  Ke = Kr * ( real(maxval(KCB_l( KCB_INI:KCB_MIN, landuse_index )), c_double) - real(Kcb, c_double) )

end function calculate_surface_evap_coefficient_ke

!------------------------------------------------------------------------------

!> This subroutine updates the total available water (TAW)
!> (water within the rootzone) for a gridcell
elemental subroutine calculate_total_available_water( taw, raw,                      &
                                                      adjusted_depletion_fraction_p, &
                                                      current_rooting_depth,         &
                                                      awc )

  real (c_double), intent(inout)   :: raw
  real (c_double), intent(inout)   :: taw
  real (c_double), intent(in)      :: adjusted_depletion_fraction_p
  real (c_float), intent(in)       :: current_rooting_depth
  real (c_float), intent(in)       :: awc

  taw = real(current_rooting_depth, c_double) * real(awc, c_double)
  raw = taw * adjusted_depletion_fraction_p

end subroutine calculate_total_available_water

!------------------------------------------------------------------------------

!> This function estimates Ks, water stress coefficient
!> @note Implemented as equation 84, FAO-56, Allen and others
elemental function calculate_water_stress_coefficient_ks( taw, raw,                             &
                                                          soil_moisture_deficit) result( Ks )

  real (c_double), intent(in)      :: raw
  real (c_double), intent(in)      :: taw
  real (c_double), intent(in)      :: soil_moisture_deficit
  real (c_double)                  :: Ks

  if ( soil_moisture_deficit < raw ) then

    Ks = 1.0_c_float

  elseif ( soil_moisture_deficit <  taw ) then

    Ks = ( taw - soil_moisture_deficit ) / ( taw - raw + 1.0e-6)

  else

    Ks = 0.0_c_float

  endif

end function calculate_water_stress_coefficient_ks

!------------------------------------------------------------------------------

elemental subroutine calculate_actual_et_fao56_two_stage(                            &
                                                  actual_et,                         &
                                                  crop_etc,                          &
                                                  bare_soil_evap,                    &
                                                  taw,                               &
                                                  raw,                               &
                                                  fraction_exposed_and_wetted_soil,  &
                                                  Kr,                                &
                                                  Ke,                                &
                                                  Ks,                                &
                                                  adjusted_depletion_fraction_p,     &
                                                  soil_moisture_deficit,             &
                                                  Kcb,                               &
                                                  landuse_index,                     &
                                                  soil_group,                        &
                                                  awc,                               &
                                                  current_rooting_depth,             &
                                                  soil_storage,                      &
                                                  soil_storage_max,                  &
                                                  reference_et0 )

  real (c_double), intent(inout)            :: actual_et
  real (c_float), intent(inout)             :: crop_etc
  real (c_float), intent(inout)             :: bare_soil_evap
  real (c_double), intent(inout)            :: taw
  real (c_double), intent(inout)            :: raw
  real (c_float), intent(inout)             :: fraction_exposed_and_wetted_soil
  real (c_double), intent(inout)            :: Kr
  real (c_double), intent(inout)            :: Ke
  real (c_double), intent(inout)            :: Ks
  real (c_double), intent(inout)            :: adjusted_depletion_fraction_p
  real (c_double), intent(inout)            :: soil_moisture_deficit
  real (c_float), intent(in)                :: Kcb
  integer (c_int), intent(in)               :: landuse_index
  integer (c_int), intent(in)               :: soil_group
  real (c_float), intent(in)                :: awc
  real (c_float), intent(in)                :: current_rooting_depth
  real (c_double), intent(in)               :: soil_storage
  real (c_float), intent(in)                :: soil_storage_max
  real (c_float), intent(in)                :: reference_et0


  adjusted_depletion_fraction_p = adjust_depletion_fraction_p( landuse_index, reference_et0 )

  soil_moisture_deficit = max( 0.0_c_double, real(soil_storage_max, c_double) - soil_storage)

  call calculate_total_available_water( taw, raw,                      &
                                        adjusted_depletion_fraction_p, &
                                        current_rooting_depth,         &
                                        awc )


  Kr = calculate_evaporation_reduction_coefficient_Kr( landuse_index,                    &
                                                       soil_group,                       &
                                                       soil_moisture_deficit )

  fraction_exposed_and_wetted_soil = calculate_fraction_exposed_and_wetted_soil_fc( landuse_index, Kcb )

  Ke = min( calculate_surface_evap_coefficient_ke( landuse_index, Kcb, Kr ),                      &
            fraction_exposed_and_wetted_soil * maxval(KCB_l( KCB_INI:KCB_MIN, landuse_index ) ) )

  Ks = calculate_water_stress_coefficient_ks(taw, raw, soil_moisture_deficit)

  bare_soil_evap = reference_et0 * Ke
  crop_etc       = reference_et0 * Kcb * Ks

  actual_et = crop_etc + bare_soil_evap

end subroutine calculate_actual_et_fao56_two_stage

end module actual_et__fao56__two_stage
