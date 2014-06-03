!> @file
!>  Contains a single module, \ref et_crop_coefficients, which
!>  provides support for modifying reference ET through the use of
!> crop coefficients

!>  Provide support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module sm_FAO56

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types
  use parameters
	use sm_thornthwaite_mather
  implicit none

  private

  type, public :: SM_FAO56_T

    integer (kind=c_int), allocatable  :: iLanduseCode(:)
    real (kind=c_float), allocatable   :: fREW(:,:)
    real (kind=c_float), allocatable   :: fTEW(:,:)
    real (kind=c_float), allocatable   :: iL_plant(:) 
    real (kind=c_float), allocatable   :: iL_ini(:) 
    real (kind=c_float), allocatable   :: iL_mid(:) 
    real (kind=c_float), allocatable   :: iL_late(:) 
    real (kind=c_float), allocatable   :: fKcb_ini(:)
    real (kind=c_float), allocatable   :: fKcb_mid(:)
    real (kind=c_float), allocatable   :: fKcb_end(:)
    real (kind=c_float), allocatable   :: fKcb_min(:)
    real (kind=c_float), allocatable   :: fDepletion_fraction(:)
    real (kind=c_float), allocatable   :: fMean_plant_height(:)

  contains

    procedure, private   :: fao56_initialize

  end type SM_FAO56_T

  type (SM_FAO56_T) :: SM_FAO56_TABLE

  contains

  subroutine et_kc_initialize()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: slREW, slTEW
    integer (kind=c_int)             :: iTEWSeqNums, iREWSeqNums
    integer (kind=c_int)             :: iNumberOfTEW, iNumberOfREW
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sText

   ! retrieve a string list of all keys associated with REW (i.e. "REW_1", "REW_2", "REW_3", etc)
   slREW = PARAMS%grep_keys("REW")
   ! Convert the string list to an vector of integers; this call strips off the "REW_" part of label
   iREWSeqNums = slREW%asInt()
   ! count how many items are present in the vector; this should equal the number of soils groups
   iNumberOfREW = count( iREWSeqNums > 0 )

   ! retrieve a string list of all keys associated with TEW (i.e. "TEW_1", "TEW_2", "TEW_3", etc)
   slTEW = PARAMS%grep_keys("TEW")
   ! Convert the string list to an vector of integers; this call strips off the "TEW_" part of label
   iTEWSeqNums = slTEW%asInt()
   ! count how many items are present in the vector; this should equal the number of soils groups
   iNumberOfTEW = count( iTEWSeqNums > 0 )


   !> Determine how many landuse codes are present
   call PARAMS%get_values( slList, this%iLanduseCode )
   iNumberOfLanduses = count( this%iLanduseCode > 0 )

   allocate( REW(iNumberOfLanduses, iNumberOfREW), stat=iStat )
   call assert( iStat == 0, "Failed to allocate memory for readily evaporable water (REW) table", &
     __FILE__, __LINE__)

   allocate( TEW(iNumberOfLanduses, iNumberOfTEW), stat=iStat )
   call assert( iStat == 0, "Failed to allocate memory for total evaporable water (TEW) table", &
     __FILE__, __LINE__)

   ! we should have the REW table fully filled out following this block
   do iIndex = 1, iNumberOfREW
     sText = "REW_"//asCharacter(iIndex)
     call PARAMS%get_values( sText, fREW(:, iIndex) )
   enddo  

   ! we should have the TEW table fully filled out following this block
   do iIndex = 1, iNumberOfTEW
     sText = "TEW_"//asCharacter(iIndex)
     call PARAMS%get_values( sText, fTEW(:, iIndex) )
   enddo  

   call PARAMS%get_values( "L_ini", this%iL_ini(:) )
   call PARAMS%get_values( "L_mid", this%iL_mid(:) )
   call PARAMS%get_values( "L_late", this%iL_late(:) )
   call PARAMS%get_values( "L_min", this%iL_min(:) )

   call PARAMS%get_values( "Kcb_ini", this%fKcb_ini(:) )
   call PARAMS%get_values( "Kcb_mid", this%fKcb_mid(:) )
   call PARAMS%get_values( "Kcb_end", this%fKcb_end(:) )
   call PARAMS%get_values( "Kcb_min", this%fKcb_min(:) )

   call PARAMS%get_values( "Depletion_Fraction", this%fDepletion_fraction(:) )
   call PARAMS%get_values( "Mean_Plant_Height", this%fMean_plant_height(:) )


  end subroutine et_kc_initialize

!------------------------------------------------------------------------------

 !> Update the current basal crop coefficient (Kcb) for
 !! a SINGLE irrigation table entry
 !!
 !! @param[inout] pIRRIGATION pointer to a single line of information in the irrigation file.
 !! @param[in] iThreshold either the current day of year or the number of growing degree days.
 !! @retval rKcb Basal crop coefficient given the irrigation table entries and the current threshold values.
 function et_kc_UpdateCropCoefficient( iFAOIndex, iThreshold )  result(fKcb)

  integer (kind=c_int), intent(in)   :: iFAOIndex
  integer (kind=c_int), intent(in)   :: iThreshold
  real (kind=c_float)                :: fKcb

  ! [ LOCALS ]
  real (kind=c_double) :: fFrac

  associate ( L_ini => this%iL_ini(iFAOIndex), L_mid => this%iL_mid(iFAOIndex), L_late => this%iL_late(iFAOIndex), &
              L_plant => this%iL_plant(iFAOIndex), Kcb_ini => this%fKcb_ini(iFAOIndex),                            &
              Kcb_dev => this%fKcb_dev(iFAOIndex), Kcb_mid => this%fKcb_mid(iFAOIndex),                            &
              Kcb_end => this%fKcb_end(iFAOIndex) )

    ! now calculate Kcb for the given landuse
    if( iThreshold > L_late ) then

      fKcb = Kcb_min

    elseif ( iThreshold > L_mid ) then
      
      fFrac = real(iThreshold - L_mid, kind=c_double ) / real( L_late - L_mid, kind=c_double )

      fKcb =  Kcb_mid * (1_c_double - fFrac) + Kcb_end * fFrac

    elseif ( iThreshold > L_dev ) then
      
      fKcb = Kcb_mid

    elseif ( iThreshold > L_ini ) then

      fFrac = real( iThreshold - L_ini ) / real( L_dev - L_ini )

      fKcb = Kcb_ini * (1_c_double - fFrac) + Kcb_mid * fFrac

    elseif ( iThreshold >= L_plant ) then
      
      rKcb = Kcb_ini
    
    else
    
      rKcb = Kcb_min
    
    endif

  end associate

end function et_kc_UpdateCropCoefficient

!------------------------------------------------------------------------------

!>
function et_kc_CalcEvaporationReductionCoefficient(rTEW, rREW, rDeficit)  result(rKr)

  ! [ ARGUMENTS ]
  real (kind=c_float) :: rTEW
  real (kind=c_float) :: rREW
  real (kind=c_float) :: rDeficit

  ! [ RESULT ]
  real (kind=c_float) :: rKr

  if(rDeficit > rREW .and. rDeficit < rTEW) then
    rKr = (rTEW - rDeficit) / (rTEW - rREW)
  elseif(rDeficit <= rREW) then
    rKr = 1.
  else
    rKr = 0.
  endif

end function et_kc_CalcEvaporationReductionCoefficient

!------------------------------------------------------------------------------

!> This function estimates the fraction of the ground covered by
!! vegetation during the growing season
!!
!!@note Implemented as equation 76, FAO-56, Allen and others
function et_kc_CalcFractionExposedAndWettedSoil( iFAOIndex, fKcb)   result (f_few)

  integer (kind=c_int), intent(in)     :: iFAOIndex
  real (kind=c_float), intent(in)      :: fKcb
  real (kind=c_float)                  :: f_few

  ! [ LOCALS ]
  real (kind=c_float) :: f_fc
  real (kind=c_float) :: fNumerator
  real (kind=c_float) :: fDenominator
  real (kind=c_float) :: fExponent

  fNumerator = fKcb - fKcb_min(iFAOIndex)
  fDenominator = fKcb_mid(iFAOIndex) - fKcb_min(iFAOIndex)
  fExponent = 1_c_float + 0.5_c_float * fMean_plant_height * rM_PER_FOOT

  if(fDenominator >  0_c_float ) then
    f_fc = ( fNumerator / fDenominator) ** fExponent
  else
    f_fc = 1c_float
  endif

  f_few = 1c_float - f_fc

  if ( f_few < 0_c_float ) f_few = 0_c_float
  if ( f_few > 1_c_float ) f_few = 1_c_float

end function et_kc_CalcFractionExposedAndWettedSoil

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
function et_kc_CalcEffectiveRootDepth(iFAOIndex, fZr_max, iThreshold) 	result(fZr_i)

  integer (kind=c_int), intent(in)    :: iFAOIndex 
	real (kind=c_float), intent(in)     :: fZr_max
	integer (kind=c_int), intent(in)    :: iThreshold ! either GDD or DOY

  ! [ RESULT ]
  real (kind=c_float) :: fZr_i

	! [ LOCALS ]
	! 0.328 feet equals 0.1 meters, which is seems to be the standard
	! initial rooting depth in the FAO-56 methodology
	real (kind=c_float), parameter :: fZr_min = 0.328

  ! if there is not much difference between the Kcb_mid and Kcb_ini, assume that
  ! we are dealing with an area such as a forest, where we assume that the rooting
  ! depths are constant year-round
	if ( fKcb_mid(iFAOIndex) - fKcb_ini(iFAOIndex) < 0.1) then

	  fZr_i = fZr_max

	elseif ( iThreshold < iL_plant(iFAOIndex) ) then

	  fZr_i = fZr_min

	else

    fZr_i = fZr_min + (fZr_max - fZr_min) * real(iThreshold - iL_plant(iFAOIndex), kind=c_float ) &
                                           / real( iL_dev(iFAOIndex) -  iL_plant(iFAOIndex), kind=c_float)

  endif

end function et_kc_CalcEffectiveRootDepth

!------------------------------------------------------------------------------

!> This function estimates Ke, the bare surface evaporation coefficient
!!
!! @note Implemented as equation 71, FAO-56, Allen and others
function et_kc_CalcSurfaceEvaporationCoefficient( iFAOIndex, fKr, fKcb )     result(fKe)

  integer (kind=c_int), intent(in)     :: iFAOIndex
  real (kind=c_float), intent(in)      :: fKr
  real (kind=c_float), intent(in)      :: fKcb
  real (kind=c_float)                  :: fKe

  fKe = fKr * ( rKcb_max(iFAOIndex) - fKcb )

end function et_kc_CalcSurfaceEvaporationCoefficient

!------------------------------------------------------------------------------

!> This subroutine updates the total available water (TAW)
!> (water within the rootzone) for a gridcell
subroutine et_kc_CalcTotalAvailableWater(fTotalAvailableWater, fReadilyAvailableWater, &
                iFAOIndex, fAvailableWaterCapacity, fCurrentRootingDepth )

  real (kind=c_float), intent(out)      :: fTotalAvailableWater
  real (kind=c_float), intent(out)      :: fReadilyAvailableWater  
  integer (kind=c_int), intent(in)      :: iFAOIndex
  real (kind=c_float), intent(in)       :: fAvailableWaterCapacity
  real (kind=c_float), intent(in)       :: fCurrentRootingDepth

  fTotalAvailableWater = fCurrentRootingDepth * fAvailableWaterCapacity
  fReadilyAvailableWater = fTotalAvailableWater * fDepletion_fraction(iFAOIndex)

  end subroutine et_kc_CalcTotalAvailableWater

!------------------------------------------------------------------------------

!> This function estimates Ks, water stress coefficient
!!
!! @note Implemented as equation 84, FAO-56, Allen and others
function et_kc_CalcWaterStressCoefficient( iFAOIndex, fDeficit, &
                      fTotalAvailableWater, fReadilyAvailableWater )      result(fKs)

  integer (kind=c_int), intent(in)     :: iFAOIndex
  real (kind=c_float), intent(in)      :: fDeficit
  real (kind=c_float), intent(in)      :: fTotalAvailableWater
  real (kind=c_float), intent(in)      :: fReadilyAvailableWater
  real (kind=c_float)                  :: fKs

  if ( fDeficit < fReadilyAvailableWater ) then
    
    fKs = 1_c_float
  
  elseif ( fDeficit < fTotalAvailableWater ) then

    fKs = ( fTotalAvailableWater - fDeficit + 1e-6_c_float ) &
             / ( (1_c_float - fDepletion_fraction(iFAOIndex) ) &
          * ( fTotalAvailableWater + 1e-6_c_float ) )

  else

    fKs = 0_c_float
  
  endif

end function et_kc_CalcWaterStressCoefficient

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
  real (kind=c_float) :: rTEW      ! Total evaporable water
  real (kind=c_float) :: rREW      ! Readily evaporable water
  real (kind=c_float) :: rKr       ! Evaporation reduction coefficient
  real (kind=c_float) :: rDeficit  ! Soil moisture deficit
  real (kind=c_float) :: r_few     ! Fraction exposed and wetted soil
  real (kind=c_float) :: rKe       ! Surface evaporation coefficient
  real (kind=c_float) :: rKs       ! Water stress coefficient
	real (kind=c_float) :: rZr_max   ! Maximum rooting depth


			 rZr_max = pConfig%ROOTING_DEPTH(cel%iLandUseIndex,cel%iSoilGroup)

       if(pIRRIGATION%lUnitsAreDOY) then

         cel%rKcb = et_kc_UpdateCropCoefficient(pIRRIGATION, pConfig%iDayOfYear)

				 if(pConfig%iDayOfYear < pIRRIGATION%iL_dev) then
				   cel%rCurrentRootingDepth = et_kc_CalcEffectiveRootDepth(pIRRIGATION, &
				     rZr_max, pConfig%iDayOfYear)
!					 cel%rSoilWaterCap = cel%rCurrentRootingDepth * cel%rSoilWaterCapInput
				 endif

       else

         cel%rKcb = et_kc_UpdateCropCoefficient(pIRRIGATION, INT(cel%rGDD, kind=c_int))

				 if(int(cel%rGDD, kind=c_int) < pIRRIGATION%iL_dev) then
				   cel%rCurrentRootingDepth = et_kc_CalcEffectiveRootDepth(pIRRIGATION, &
				     rZr_max,INT(cel%rGDD, kind=c_int))
!					 cel%rSoilWaterCap = cel%rCurrentRootingDepth * cel%rSoilWaterCapInput
				 endif

       endif

       rREW = pConfig%READILY_EVAPORABLE_WATER(cel%iLandUseIndex, cel%iSoilGroup)
       rTEW = pConfig%TOTAL_EVAPORABLE_WATER(cel%iLandUseIndex, cel%iSoilGroup)

       ! Deficit is defined in the sense of Thornthwaite and Mather
       rDeficit = MAX(rZERO, cel%rSoilWaterCap - cel%rSoilMoisture)
       ! following call updates the total available water (TAW) and
       ! readily available water (RAW) on the basis of the current
       ! plant root depth
       call et_kc_CalcTotalAvailableWater( pIRRIGATION, cel)

       ! "STANDARD" vs "NONSTANDARD": in the FAO56 publication the term
       ! "STANDARD" is used to refer to crop ET requirements under
       ! ideal conditions (i.e. plants not stressed due to scarcity
       ! of water. "NONSTANDARD" is the term used to describe ET requirements
       ! when plants are under stress, when water is scarce.

       if ( pConfig%iConfigureFAO56 == CONFIG_FAO56_TWO_FACTOR_NONSTANDARD ) then
         ! we are using the full FAO56 soil water balance approach, *INCLUDING*
				 ! the adjustments for nonstandard growing conditions (e.g. plant
				 ! stress and resulting decrease in ET during dry conditions).

         rKr = et_kc_CalcEvaporationReductionCoefficient(rTEW, rREW, rDeficit)
         r_few = et_kc_CalcFractionExposedAndWettedSoil( pIRRIGATION )
         rKe = min(et_kc_CalcSurfaceEvaporationCoefficient( pIRRIGATION, &
                 rKr ), r_few * pIRRIGATION%rKcb_mid )

         if (rKe < 0) print *, "rKe < 0: ", cel%iIrrigationTableIndex, iRow, iCol, cel%iLandUse, cel%iSoilGroup, rKe, pIRRIGATION%rKcb, pIRRIGATION%rKcb_max
         if (rKr < 0) print *, "rKr < 0: ", cel%iIrrigationTableIndex, iRow, iCol, cel%iLandUse, cel%iSoilGroup, rKr, pIRRIGATION%rKcb, pIRRIGATION%rKcb_max
         if (r_few < 0) print *, "r_few < 0: ", cel%iIrrigationTableIndex, iRow, iCol, cel%iLandUse, cel%iSoilGroup, r_few, pIRRIGATION%rKcb, pIRRIGATION%rKcb_max

         rKs = et_kc_CalcWaterStressCoefficient( pIRRIGATION, rDeficit, cel)

         cel%rBareSoilEvap = cel%rReferenceET0 * rKe
         cel%rCropETc = cel%rReferenceET0 * (cel%rKcb * rKs)

       elseif ( pConfig%iConfigureFAO56 == CONFIG_FAO56_ONE_FACTOR_NONSTANDARD ) then
         ! we are using the full FAO56 soil water balance approach, *INCLUDING*
				 ! the adjustments for nonstandard growing conditions (e.g. plant
				 ! stress and resulting decrease in ET during dry conditions).
				 ! *EXCLUDING* explicit calculation of BareSoilEvap

         rKs = et_kc_CalcWaterStressCoefficient( pIRRIGATION, rDeficit, cel)

         cel%rBareSoilEvap = rZERO
         cel%rCropETc = cel%rReferenceET0 * (cel%rKcb * rKs)

       elseif ( pConfig%iConfigureFAO56 == CONFIG_FAO56_TWO_FACTOR_STANDARD ) then

         ! if we are not using the full FAO56 soil water balance approach,
         ! we should just adjust the potential ET by the crop coefficient.
         ! The Thornthwaite-Mather soil moisture retention tables already
         ! account for the fact that water becomes more difficult to extract
         ! as the APWL increases...

				 ! NO reductions in Kc due to water availability

         rKr = et_kc_CalcEvaporationReductionCoefficient(rTEW, rREW, rDeficit)
         r_few = et_kc_CalcFractionExposedAndWettedSoil( pIRRIGATION )
         rKe = min(et_kc_CalcSurfaceEvaporationCoefficient( pIRRIGATION, &
                 rKr ), r_few * pIRRIGATION%rKcb_mid )

         cel%rBareSoilEvap = cel%rReferenceET0 * rKe
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

end module sm_FAO56
