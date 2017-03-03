!> @file
!>  Contains a single module, \ref et_crop_coefficients, which
!>  provides support for modifying reference ET through the use of
!> crop coefficients

!>  Provide support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module actual_et__fao56__two_stage

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types
  use sm_thornthwaite_mather
  implicit none

  real (kind=c_float), allocatable   :: REW(:,:)
  real (kind=c_float), allocatable   :: TEW(:,:)
  real (kind=c_float), allocatable   :: MEAN_PLANT_HEIGHT(:)

contains

!------------------------------------------------------------------------------

  subroutine crop_coefficients_FAO56_two_stage_initialize( fSoilStorage, iLanduseIndex, iSoilGroup, &
                                               fAvailable_Water_Content, lActive )


   ! [ LOCALS ]
   type (STRING_LIST_T)              :: slREW, slTEW
   type (STRING_LIST_T)              :: slList
   integer (kind=c_int), allocatable :: iTEWSeqNums(:)
   integer (kind=c_int), allocatable :: iREWSeqNums(:)
   integer (kind=c_int)              :: iNumberOfTEW, iNumberOfREW

   ! Retrieve and populate the Readily Evaporable Water (REW) table values
   CALL PARAMS%get_parameters( fValues=REW, sPrefix="REW_", iNumRows=iNumberOfLanduses )

   ! Retrieve and populate the Total Evaporable Water (TEW) table values
   CALL PARAMS%get_parameters( fValues=TEW, sPrefix="TEW_", iNumRows=iNumberOfLanduses )


   call PARAMS%get_parameters( sKey="Mean_Plant_Height", fValues=MEAN_PLANT_HEIGHT, lFatal=lTRUE )

!------------------------------------------------------------------------------

elemental function calculate_evaporation_reduction_coefficient_Kr(TEW, REW &
   rDeficit)  result( Kr )

  ! [ ARGUMENTS ]
  real (kind=c_float) :: TEW
  real (kind=c_float) :: REW
  real (kind=c_float) :: Deficit

  ! [ RESULT ]
  real (kind=c_float) :: Kr

  if(Deficit > REW .and. Deficit < TEW) then
    rKr = (TEW - Deficit) / (TEW - REW)
  else if(Deficit <= REW) then
    rKr = 1.
  else
    rKr = 0.
  endif

end function calculate_evaporation_reduction_coefficient_Kr

!------------------------------------------------------------------------------

!> This function estimates the fraction of the ground covered by
!> vegetation during the growing season
!> @note Implemented as equation 76, FAO-56, Allen and others
function calculate_fraction_exposed_and_wetted_soil_fc( pIRRIGATION, rKcb )   result (r_few)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP), pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  real (kind=c_float)                 :: rKcb

  ! [ RESULT ]
  real (kind=c_float) :: r_few

  ! [ LOCALS ]
  real (kind=c_float) :: r_fc
  real (kind=c_float) :: rNumerator
  real (kind=c_float) :: rDenominator
  real (kind=c_float) :: rExponent

!  rNumerator = pIRRIGATION%rKcb - pIRRIGATION%rKcb_min
!
! BUG? if Kcb is tracked for each cell, the value contained in the irrigation table is undefined(?)
!
  rNumerator = rKcb - pIRRIGATION%rKcb_min
  rDenominator = pIRRIGATION%rKcb_mid - pIRRIGATION%rKcb_min
  rExponent = 1.0 + 0.5 * pIRRIGATION%rMeanPlantHeight * rM_PER_FOOT

  if(rDenominator > rNEAR_ZERO) then
    r_fc = (rNumerator / rDenominator) ** rExponent
  else
    r_fc = 1.0
  endif

  r_few = 1.0 - r_fc

  if (r_few < 0.) r_few = 0.0
  if (r_few > 1.) r_few = 1.0

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

elemental function calculate_effective_root_depth( iLanduseIndex, fZr_max, fKCB )  result(fZr_i)

  integer (kind=c_int), intent(in)    :: iLanduseIndex
  real (kind=c_float), intent(in)     :: fZr_max
  real (kind=c_float), intent(in)     :: fKCB

  ! [ RESULT ]
  real (kind=c_float) :: Zr_i

  ! [ LOCALS ]
  ! 0.3048 feet equals 0.1 meters, which is seems to be the standard
  ! initial rooting depth in the FAO-56 methodology
  real (kind=c_float), parameter :: Zr_min = 0.3048
  real (kind=c_float)            :: MaxKCB
  real (kind=c_float)            :: MinKCB

  if ( KCB_METHOD( iLanduseIndex ) == KCB_METHOD_MONTHLY_VALUES ) then
    fMaxKCB = maxval( KCB( JAN:DEC, iLanduseIndex ) )
    fMinKCB = minval( KCB( JAN:DEC, iLanduseIndex ) )
  else
    fMaxKCB = maxval( KCB( KCB_INI:KCB_MIN, iLanduseIndex ) )
    fMinKCB = minval( KCB( KCB_INI:KCB_MIN, iLanduseIndex ) )
  endif

  ! if there is not much difference between the MAX Kcb and MIN Kcb, assume that
  ! we are dealing with an area such as a forest, where we assume that the rooting
  ! depths are constant year-round
   if ( ( fMaxKCB - fMinKCB ) < 0.1_c_float ) then

     Zr_i = Zr_max

   elseif ( MaxKCB > 0.0_C_float ) then

     Zr_i = Zr_min + (Zr_max - Zr_min) * KCB / MaxKCB

   else

     Zr_i = Zr_min

   endif

!  fZr_i = fZr_max

end function calculate_effective_root_depth

!------------------------------------------------------------------------------

!> This function estimates Ke, the bare surface evaporation
!> coefficient
!> @note Implemented as equation 71, FAO-56, Allen and others
function calculate_surface_evap_coefficient_ke( pIRRIGATION, rKcb, rKr )     result(rKe)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  real (kind=c_float) :: rKcb
  real (kind=c_float) :: rKr

  ! [ RESULT ]
  real (kind=c_float) :: rKe

  rKe = rKr * ( pIRRIGATION%rKcb_max - rKcb )

end function calculate_surface_evap_coefficient_ke

!------------------------------------------------------------------------------

!> This subroutine updates the total available water (TAW)
!> (water within the rootzone) for a gridcell
subroutine calculate_total_available_water( pIRRIGATION, cel)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  type (T_CELL), pointer :: cel

  cel%rTotalAvailableWater = cel%rCurrentRootingDepth * cel%rSoilWaterCapInput
  cel%rReadilyAvailableWater = cel%rTotalAvailableWater * pIRRIGATION%rDepletionFraction

end subroutine calculate_total_available_water

!------------------------------------------------------------------------------

!> This function estimates Ks, water stress coefficient
!> @note Implemented as equation 84, FAO-56, Allen and others
function calculate_water_stress_coefficient_ks( pIRRIGATION, &
                                                rDeficit, &
                                                cel)        result(rKs)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  real (kind=c_float) :: rDeficit
  type (T_CELL), pointer :: cel

  ! [ RESULT ]
  real (kind=c_float) :: rKs

  if (rDeficit < cel%rReadilyAvailableWater) then
    rKs = rONE
  elseif (rDeficit < cel%rTotalAvailableWater) then

    rKs = ( cel%rTotalAvailableWater - rDeficit )                                    &
          / ( cel%rTotalAvailableWater - cel%rReadilyAvailableWater + 1.0e-6 )
  else
    rKs = rZERO
  endif

end function calculate_water_stress_coefficient_ks

!------------------------------------------------------------------------------

subroutine et_kc_ApplyCropCoefficients(pGrd, pConfig)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other setting

  ! [ LOCALS ]
  integer (kind=c_int) :: iRow, iCol
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  type (T_CELL),pointer :: cel
  real (kind=c_float) :: rTEW          ! Total evaporable water
  real (kind=c_float) :: rREW          ! Readily evaporable water
  real (kind=c_float) :: rDeficit      ! Soil moisture deficit
  real (kind=c_float) :: rDepthOfEvap  ! Depth of evaporation
  real (kind=c_float) :: r_few         ! Fraction exposed and wetted soil
  real (kind=c_float) :: rZr_max       ! Maximum rooting depth

  integer (kind=c_int) :: iLBound
  integer (kind=c_int) :: iUBound

  iLBound = lbound( pConfig%IRRIGATION, 1 )
  iUBound = ubound( pConfig%IRRIGATION, 1 )

   ! iterate over cells; update evaporation coefficients,
   ! calculate Kc, and apply to ET0
   do iRow=1,pGrd%iNY
     do iCol=1,pGrd%iNX  ! last index in a Fortran array should be the slowest changing
       cel => pGrd%Cells(iCol, iRow)

       if ( pGrd%iMask(iCol, iRow) == iINACTIVE_CELL ) cycle

!       if(cel%rReferenceET0 < rNEAR_ZERO) cycle
       if(cel%rSoilWaterCap <= rNear_ZERO &
            .or. cel%iLandUse == pConfig%iOPEN_WATER_LU) cycle


       if ( cel%iIrrigationTableIndex < iLBound                        &
              .or. cel%iIrrigationTableIndex > iUBound )               &
         call assert( lFALSE, "Index out of bounds. Index value: "//   &
           asCharacter(cel%iIrrigationTableIndex),                     &
           __FILE__, __LINE__ )

       ! point to the line in the irrigation table pertaining to landuse of current cell
       pIRRIGATION => pConfig%IRRIGATION(cel%iIrrigationTableIndex)

       rZr_max = pConfig%ROOTING_DEPTH(cel%iLandUseIndex,cel%iSoilGroup)

       ! update crop coefficient and current rooting depth
       if(pIRRIGATION%lUnitsAreDOY) then

         cel%rKcb = et_kc_UpdateCropCoefficient(pIRRIGATION, pConfig%iDayOfYear)

         if(pConfig%iDayOfYear < pIRRIGATION%iL_dev) then
           cel%rCurrentRootingDepth = et_kc_CalcEffectiveRootDepth(pIRRIGATION, &
             rZr_max, pConfig%iDayOfYear)
         endif

       else

         cel%rKcb = et_kc_UpdateCropCoefficient(pIRRIGATION, INT(cel%rGDD, kind=c_int))

         if(int(cel%rGDD, kind=c_int) < pIRRIGATION%iL_dev) then
           cel%rCurrentRootingDepth = et_kc_CalcEffectiveRootDepth(pIRRIGATION, &
             rZr_max,INT(cel%rGDD, kind=c_int))
         endif

       endif

       rREW = pConfig%READILY_EVAPORABLE_WATER(cel%iLandUseIndex, cel%iSoilGroup)
       rTEW = pConfig%TOTAL_EVAPORABLE_WATER(cel%iLandUseIndex, cel%iSoilGroup)

       ! Deficit is defined in the sense of Thornthwaite and Mather
!       rDeficit = MAX(rZERO, cel%rSoilWaterCap - cel%rSoilMoisture)

       ! following call updates the total available water (TAW) and
       ! readily available water (RAW) on the basis of the current
       ! plant root depth
       call et_kc_CalcTotalAvailableWater( pIRRIGATION, cel)

       ! Deficit is defined in the sense of Thornthwaite and Mather, and
       ! is calculated relative to the CURRENT ROOTING DEPTH
       rDeficit = MAX(rZERO, cel%rTotalAvailableWater - cel%rSoilMoisture)

       rDepthOfEvap = MAX(rZERO, cel%rSoilWaterCap - cel%rSoilMoisture)

       ! "STANDARD" vs "NONSTANDARD": in the FAO56 publication the term
       ! "STANDARD" is used to refer to crop ET requirements under
       ! ideal conditions (i.e. plants not stressed due to scarcity
       ! of water. "NONSTANDARD" is the term used to describe ET requirements
       ! when plants are under stress, when water is scarce.

       if ( pConfig%iConfigureFAO56 == CONFIG_FAO56_TWO_FACTOR_NONSTANDARD ) then
         ! we are using the full FAO56 soil water balance approach, *INCLUDING*
         ! the adjustments for nonstandard growing conditions (e.g. plant
         ! stress and resulting decrease in ET during dry conditions).

         cel%rKr = et_kc_CalcEvaporationReductionCoefficient(rTEW, rREW, rDepthOfEvap)
         r_few = et_kc_CalcFractionExposedAndWettedSoil( pIRRIGATION, cel%rKcb )
         cel%rKe = min(et_kc_CalcSurfaceEvaporationCoefficient( pIRRIGATION, cel%rKcb, cel%rKr ), &
                   r_few * pIRRIGATION%rKcb_mid )

         cel%rKs = et_kc_CalcWaterStressCoefficient( pIRRIGATION, rDeficit, cel)

         cel%rBareSoilEvap = cel%rReferenceET0 * cel%rKe
         cel%rCropETc = cel%rReferenceET0 * (cel%rKcb * cel%rKs)

       elseif ( pConfig%iConfigureFAO56 == CONFIG_FAO56_ONE_FACTOR_NONSTANDARD ) then
         ! we are using the full FAO56 soil water balance approach, *INCLUDING*
         ! the adjustments for nonstandard growing conditions (e.g. plant
         ! stress and resulting decrease in ET during dry conditions).
         ! *EXCLUDING* explicit calculation of BareSoilEvap

         cel%rKs = et_kc_CalcWaterStressCoefficient( pIRRIGATION, rDeficit, cel)

         cel%rBareSoilEvap = rZERO
         cel%rCropETc = cel%rReferenceET0 * (cel%rKcb * cel%rKs)

       elseif ( pConfig%iConfigureFAO56 == CONFIG_FAO56_TWO_FACTOR_STANDARD ) then

         ! if we are not using the full FAO56 soil water balance approach,
         ! we should just adjust the potential ET by the crop coefficient.
         ! The Thornthwaite-Mather soil moisture retention tables already
         ! account for the fact that water becomes more difficult to extract
         ! as the APWL increases...

         ! NO reductions in Kc due to water availability

         cel%rKr = et_kc_CalcEvaporationReductionCoefficient(rTEW, rREW, rDepthOfEvap)
         r_few = et_kc_CalcFractionExposedAndWettedSoil( pIRRIGATION, cel%rKcb )
         cel%rKe = min(et_kc_CalcSurfaceEvaporationCoefficient( pIRRIGATION, cel%rKcb, cel%rKr ), &
                   r_few * pIRRIGATION%rKcb_mid )

         cel%rBareSoilEvap = cel%rReferenceET0 * cel%rKe
         cel%rCropETc = cel%rReferenceET0 * cel%rKcb

       elseif ( pConfig%iConfigureFAO56 == CONFIG_FAO56_ONE_FACTOR_STANDARD ) then

         ! NO reductions in Kc due to water availability
         ! NO explicit calculation of BareSoilEvap
         ! no real calculations required because we're applying the crop coefficient directly

         cel%rBareSoilEvap = rZERO
         cel%rCropETc = cel%rReferenceET0 * cel%rKcb

       else

         call assert(lFALSE, "Programming error - unknown FAO56 configuration option", &
           trim(__FILE__), __LINE__)

       endif

       ! "Adjusted" Reference ET is the general term being used in the water balance
       cel%rReferenceET0_adj = cel%rCropETc + cel%rBareSoilEvap

     enddo
   enddo

end subroutine et_kc_ApplyCropCoefficients

end module actual_et__fao56__two_stage
