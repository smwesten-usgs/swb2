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
  use string_list, only                : STRING_LIST_T
  use parameters, only                 : PARAMS
  use crop_coefficients__FAO56, only   : KCB_, KCB_MIN, KCB_INI, KCB_MID, KCB_END,    &
                                         JAN, DEC, KCB_METHOD_MONTHLY_VALUES,         &
                                         KCB_METHOD
  implicit none

  real (kind=c_float), allocatable   :: REW_(:,:)
  real (kind=c_float), allocatable   :: TEW_(:,:)
  real (kind=c_float), allocatable   :: DEPLETION_FRACTION(:)
  real (kind=c_float), allocatable   :: MEAN_PLANT_HEIGHT(:)

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

    integer ( kind=c_int), intent(in)    :: landuse_index
    real (kind=c_float), intent(in)      :: reference_et0
    real (kind=c_float)                  :: p

    p = DEPLETION_FRACTION( landuse_index )                                      &
            + 0.04_c_float * ( 5.0_c_float - in_to_mm( reference_et0 ) )

    p = min( p, 0.8_c_float )
    p = max( p, 0.1_c_float )

  end function adjust_depletion_fraction_p

!------------------------------------------------------------------------------

  subroutine actual_et_FAO56_two_stage_initialize( number_of_landuses )

    integer (kind=c_int), intent(in)   :: number_of_landuses

   ! Retrieve and populate the Readily Evaporable Water (REW_) table values
   CALL PARAMS%get_parameters( fValues=REW_, sPrefix="REW_", iNumRows=number_of_landuses, lFatal=TRUE )

   ! Retrieve and populate the Total Evaporable Water (TEW_) table values
   CALL PARAMS%get_parameters( fValues=TEW_, sPrefix="TEW_", iNumRows=number_of_landuses, lFatal=TRUE )

   call PARAMS%get_parameters( sKey="Mean_Plant_Height", fValues=MEAN_PLANT_HEIGHT, lFatal=TRUE )

   call PARAMS%get_parameters( sKey="Depletion_Fraction", fValues=DEPLETION_FRACTION, lFatal=TRUE )

 end subroutine actual_et_FAO56_two_stage_initialize

!------------------------------------------------------------------------------

elemental function calculate_evaporation_reduction_coefficient_Kr( landuse_index,                    &
                                                                   soil_group,                       &
                                                                   soil_storage,                     &
                                                                   soil_storage_max )   result( Kr )

  ! [ ARGUMENTS ]
  integer (kind=c_int), intent(in)  :: landuse_index
  integer (kind=c_int), intent(in)  :: soil_group
  real (kind=c_float), intent(in)   :: soil_storage
  real (kind=c_float), intent(in)   :: soil_storage_max

  ! [ RESULT ]
  real (kind=c_float) :: Kr

  ! [ LOCALS ]
  real (kind=c_float) :: deficit

  associate( REW => REW_( landuse_index, soil_group ),         &
             TEW => TEW_( landuse_index, soil_group ) )

    deficit = soil_storage_max - soil_storage

    if ( deficit < REW ) then
      Kr = 1._c_float
    elseif ( deficit < TEW ) then
      Kr = ( TEW - deficit ) / ( TEW - REW + 1.0E+8)
    else
      Kr = 0._c_float
    endif

  end associate

end function calculate_evaporation_reduction_coefficient_Kr

!------------------------------------------------------------------------------

!> This function estimates the fraction of the ground covered by
!> vegetation during the growing season
!> @note Implemented as equation 76, FAO-56, Allen and others
elemental function calculate_fraction_exposed_and_wetted_soil_fc( landuse_index, Kcb )   result ( few )

  ! [ ARGUMENTS ]
  integer (kind=c_int), intent(in)    :: landuse_index
  real (kind=c_float), intent(in)     :: Kcb

  ! [ RESULT ]
  real (kind=c_float) :: few

  ! [ LOCALS ]
  real (kind=c_float) :: r_fc
  real (kind=c_float) :: numerator
  real (kind=c_float) :: denominator
  real (kind=c_float) :: exponent

  numerator = Kcb - KCB_( KCB_MIN, landuse_index)
  denominator =  KCB_( KCB_MID, landuse_index)  -  KCB_( KCB_MIN, landuse_index)
  exponent = 1.0 + 0.5 * MEAN_PLANT_HEIGHT( landuse_index ) * M_PER_FOOT

  if( denominator > 0.0_c_float ) then
    r_fc = ( numerator / denominator ) ** exponent
  else
    r_fc = 1.0_c_float
  endif

  few = 1.0_c_float - r_fc

  if ( few < 0._c_float ) few = 0.0_c_float
  if ( few > 1._c_float ) few = 1.0_c_float

end function calculate_fraction_exposed_and_wetted_soil_fc

!------------------------------------------------------------------------------

!> Calculate the effective root zone depth.
!!
!! Calculate the effective root zone depth given the current stage
!! of plant growth, the soil type, and the crop type.
!!
!! @param[in] pIRRIGATION pointer to a specific line of the irrigation
!!     lookup data structure.
!! @param[in] rZr_max The maximum rooting depth for this crop; currently this
!!     is supplied to this function as the rooting depth associated with the
!!     landuse/soil type found in the landuse lookup table.
!! @param[in] iThreshold Numeric value (either the GDD or the DOY) defining
!!     the time that the crop is planted.
!! @retval rZr_i current active rooting depth.
!! @note Implemented as equation 8-1 (Annex 8), FAO-56, Allen and others.

elemental subroutine update_rooting_depth( Zr_i, Zr_max, landuse_index, Kcb )

  real (kind=c_float), intent(inout)  :: Zr_i
  real (kind=c_float), intent(in)     :: Zr_max
  integer (kind=c_int), intent(in)    :: landuse_index
  real (kind=c_float), intent(in)     :: Kcb

  ! [ LOCALS ]
  ! 0.3048 feet equals 0.1 meters, which is seems to be the standard
  ! initial rooting depth in the FAO-56 methodology
  real (kind=c_float), parameter :: Zr_min = 0.3048
  real (kind=c_float)            :: MaxKCB
  real (kind=c_float)            :: MinKCB

  if ( KCB_METHOD( landuse_index ) == KCB_METHOD_MONTHLY_VALUES ) then
    MaxKCB = maxval( KCB_( JAN:DEC, landuse_index ) )
    MinKCB = minval( KCB_( JAN:DEC, landuse_index ) )
  else
    MaxKCB = maxval( KCB_( KCB_INI:KCB_MIN, landuse_index ) )
    MinKCB = minval( KCB_( KCB_INI:KCB_MIN, landuse_index ) )
  endif

  ! if there is not much difference between the MAX Kcb and MIN Kcb, assume that
  ! we are dealing with an area such as a forest, where we assume that the rooting
  ! depths are constant year-round
   if ( ( MaxKCB - MinKCB ) < 0.1_c_float ) then

     Zr_i = Zr_max

   elseif ( MaxKCB > 0.0_C_float ) then

     Zr_i = Zr_min + ( Kcb - MinKCB ) / ( MaxKCB - MinKCB ) * ( Zr_max - Zr_min )

!     Zr_i = ( MinKCB + (Kcb - MinKCB) / (MaxKCB - MinKCB) ) * Zr_max

   else

     Zr_i = Zr_min

   endif

!  fZr_i = fZr_max

end subroutine update_rooting_depth

!------------------------------------------------------------------------------

!> This function estimates Ke, the bare surface evaporation coefficient
!> @note Implemented as equation 71, FAO-56, Allen and others
elemental function calculate_surface_evap_coefficient_ke( landuse_index, Kcb, Kr )     result( Ke )

  ! [ ARGUMENTS ]
  integer (kind=c_int), intent(in)  :: landuse_index
  real (kind=c_float), intent(in)   :: Kcb
  real (kind=c_float), intent(in)   :: Kr
  real (kind=c_float)               :: Ke

  Ke = Kr * ( KCB_( KCB_MID, landuse_index ) - Kcb )

end function calculate_surface_evap_coefficient_ke

!------------------------------------------------------------------------------

!> This subroutine updates the total available water (TAW)
!> (water within the rootzone) for a gridcell
elemental subroutine calculate_total_available_water( taw, raw,                      &
                                                      adjusted_depletion_fraction_p, &
                                                      current_rooting_depth,         &
                                                      awc )

  real (kind=c_float), intent(inout)   :: raw
  real (kind=c_float), intent(inout)   :: taw
  real (kind=c_float), intent(inout)   :: adjusted_depletion_fraction_p
  real (kind=c_float), intent(in)      :: current_rooting_depth
  real (kind=c_float), intent(in)      :: awc

  taw = current_rooting_depth * awc
  raw = taw * adjusted_depletion_fraction_p

end subroutine calculate_total_available_water

!------------------------------------------------------------------------------

!> This function estimates Ks, water stress coefficient
!> @note Implemented as equation 84, FAO-56, Allen and others
elemental function calculate_water_stress_coefficient_ks( taw, raw,                          &
                                                          soil_storage,                      &
                                                          soil_storage_max ) result( Ks )

  real (kind=c_float), intent(in)      :: raw
  real (kind=c_float), intent(in)      :: taw
  real (kind=c_float), intent(in)      :: soil_storage
  real (kind=c_float), intent(in)      :: soil_storage_max
  real (kind=c_float)                  :: Ks

  real (kind=c_float) :: deficit

  deficit = soil_storage_max - soil_storage

  if ( deficit < raw ) then

    Ks = 1.0_c_float

  elseif ( deficit <  taw ) then

    Ks = ( taw - deficit ) / ( taw - raw )

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
                                                  Kcb,                               &
                                                  landuse_index,                     &
                                                  soil_group,                        &
                                                  awc,                               &
                                                  current_rooting_depth,             &
                                                  soil_storage,                      &
                                                  soil_storage_max,                  &
                                                  reference_et0 )

  real (kind=c_float), intent(inout)             :: actual_et
  real (kind=c_float), intent(inout)             :: crop_etc
  real (kind=c_float), intent(inout)             :: bare_soil_evap
  real (kind=c_float), intent(inout)             :: taw
  real (kind=c_float), intent(inout)             :: raw
  real (kind=c_float), intent(inout)             :: fraction_exposed_and_wetted_soil
  real (kind=c_float), intent(inout)             :: Kr
  real (kind=c_float), intent(inout)             :: Ke
  real (kind=c_float), intent(inout)             :: Ks
  real (kind=c_float), intent(inout)             :: adjusted_depletion_fraction_p
  real (kind=c_float), intent(in)                :: Kcb
  integer (kind=c_int), intent(in)               :: landuse_index
  integer (kind=c_int), intent(in)               :: soil_group
  real (kind=c_float), intent(in)                :: awc
  real (kind=c_float), intent(in)                :: current_rooting_depth
  real (kind=c_float), intent(in)                :: soil_storage
  real (kind=c_float), intent(in)                :: soil_storage_max
  real (kind=c_float), intent(in)                :: reference_et0


  adjusted_depletion_fraction_p = adjust_depletion_fraction_p( landuse_index, reference_et0 )

  call calculate_total_available_water( taw, raw,                      &
                                        adjusted_depletion_fraction_p, &
                                        current_rooting_depth,         &
                                        awc )


  Kr = calculate_evaporation_reduction_coefficient_Kr( landuse_index,                    &
                                                       soil_group,                       &
                                                       soil_storage,                     &
                                                       soil_storage_max )

  fraction_exposed_and_wetted_soil = calculate_fraction_exposed_and_wetted_soil_fc( landuse_index, Kcb )

  Ke = min( calculate_surface_evap_coefficient_ke( landuse_index, Kcb, Kr ),       &
            fraction_exposed_and_wetted_soil * KCB_( KCB_MID, landuse_index ) )

  Ks = calculate_water_stress_coefficient_ks(taw, raw,soil_storage, soil_storage_max)

  bare_soil_evap = reference_et0 * Ke
  crop_etc       = reference_et0 * Kcb * Ks

  actual_et = crop_etc + bare_soil_evap

end subroutine calculate_actual_et_fao56_two_stage

end module actual_et__fao56__two_stage
