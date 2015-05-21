!> @file
!>  Contains a single module, \ref et_crop_coefficients, which
!>  provides support for modifying reference ET through the use of
!> crop coefficients

!>  Provide support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module soil_moisture__FAO_56

  use iso_c_binding, only             : c_bool, c_short, c_int, c_float, c_double
  use constants_and_conversions, only : M_PER_FOOT
  use exceptions, only                : assert
  use parameters, only                : PARAMS
  use simulation_datetime, only       : SIM_DT 
  use string_list
  implicit none

  private

  public :: soil_moisture_FAO56_initialize, soil_moisture_FAO56_calculate

  enum, bind(c)
    enumerator :: L_INIT=1, L_DEV, L_MID, L_LATE, L_FALLOW
  end enum 

  enum, bind(c)
    enumerator :: KCB_INI=1, KCB_MID, KCB_END, KCB_MIN
  end enum 

  enum, bind(c)
    enumerator :: JAN = 1, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC
  end enum 

  ! Private, module level variables
  ! kept at a landuse code level (i.e. same value applies to all cells with same LU codes)
  integer (kind=c_int), allocatable  :: LANDUSE_CODE(:)
  real (kind=c_float), allocatable   :: REW(:,:)
  real (kind=c_float), allocatable   :: TEW(:,:)
  real (kind=c_float), allocatable   :: KCB(:,:)
  real (kind=c_float), allocatable   :: PLANTING_DATE(:)
  real (kind=c_float), allocatable   :: L_GROWTH(:,:)
  logical (kind=c_bool), allocatable :: UNITS_ARE_DOY(:)

  !real (kind=c_float), 
  real (kind=c_float), allocatable   :: DEPLETION_FRACTION(:)
  real (kind=c_float), allocatable   :: MEAN_PLANT_HEIGHT(:)

  ! variables kept for each computational cell (i.e. varies cell-by-cell)
  real (kind=c_float), allocatable    :: fKCB(:)

contains

  subroutine soil_moisture_FAO56_initialize( iNumActiveCells )

    integer (kind=c_int), intent(in)   :: iNumActiveCells

    ! [ LOCALS ]
    type (STRING_LIST_T)              :: slREW, slTEW
    type (STRING_LIST_T)              :: slList
    integer (kind=c_int), allocatable :: iTEWSeqNums(:)
    integer (kind=c_int), allocatable :: iREWSeqNums(:)
    integer (kind=c_int)              :: iNumberOfTEW, iNumberOfREW
    integer (kind=c_int)              :: iNumberOfLanduses
    integer (kind=c_int)              :: iIndex
    integer (kind=c_int)              :: iStat

    character (len=:), allocatable   :: sText

    real (kind=c_float), allocatable :: L_ini(:)
    real (kind=c_float), allocatable :: L_mid(:)
    real (kind=c_float), allocatable :: L_late(:)
    real (kind=c_float), allocatable :: Kcb_ini(:)            
    real (kind=c_float), allocatable :: Kcb_mid(:)            
    real (kind=c_float), allocatable :: Kcb_end(:)            
    real (kind=c_float), allocatable :: Kcb_min(:)
    
    real (kind=c_float), allocatable :: Kcb_jan(:)
    real (kind=c_float), allocatable :: Kcb_feb(:)
    real (kind=c_float), allocatable :: Kcb_mar(:)
    real (kind=c_float), allocatable :: Kcb_apr(:)
    real (kind=c_float), allocatable :: Kcb_may(:)                
    real (kind=c_float), allocatable :: Kcb_jun(:)
    real (kind=c_float), allocatable :: Kcb_jul(:)
    real (kind=c_float), allocatable :: Kcb_aug(:)
    real (kind=c_float), allocatable :: Kcb_sep(:)
    real (kind=c_float), allocatable :: Kcb_oct(:)
    real (kind=c_float), allocatable :: Kcb_nov(:)
    real (kind=c_float), allocatable :: Kcb_dec(:)

!    ! retrieve a string list of all keys associated with REW (i.e. "REW_1", "REW_2", "REW_3", etc)
!    slREW = PARAMS%grep_name("REW")

!    ! Convert the string list to an vector of integers; this call strips off the "REW_" part of label
!    iREWSeqNums = slREW%asInt()

!    ! count how many items are present in the vector; this should equal the number of soils groups
!    iNumberOfREW = count( iREWSeqNums > 0 )

!    ! retrieve a string list of all keys associated with TEW (i.e. "TEW_1", "TEW_2", "TEW_3", etc)
!    slTEW = PARAMS%grep_name("TEW")

!    ! Convert the string list to an vector of integers; this call strips off the "TEW_" part of label
!    iTEWSeqNums = slTEW%asInt()

!    ! count how many items are present in the vector; this should equal the number of soils groups
!    iNumberOfTEW = count( iTEWSeqNums > 0 )

   !> create string list that allows for alternate heading identifiers for the landuse code
   call slList%append("LU_Code")
   call slList%append("Landuse_Code")
   call slList%append("Landuse_Lookup_Code")

   !> Determine how many landuse codes are present
   call PARAMS%get_parameters( slKeys=slList, iValues=LANDUSE_CODE )
   iNumberOfLanduses = count( LANDUSE_CODE > 0 )

   !allocate( REW(iNumberOfLanduses, iNumberOfREW), stat=iStat )
   !call assert( iStat == 0, "Failed to allocate memory for readily evaporable water (REW) table", &
   !  __FILE__, __LINE__)

   !allocate( TEW(iNumberOfLanduses, iNumberOfTEW), stat=iStat )
   !call assert( iStat == 0, "Failed to allocate memory for total evaporable water (TEW) table", &
   !  __FILE__, __LINE__)

   !> @todo Implement thorough input error checking: 
   !! are all soils in grid included in table values?
   !> is soil suffix vector continuous?

   ! Retrieve and populate the Readily Evaporable Water (REW) table values
   CALL PARAMS%get_parameters( fValues=REW, sPrefix="REW_", iNumRows=iNumberOfLanduses )

   ! Retrieve and populate the Total Evaporable Water (TEW) table values
   CALL PARAMS%get_parameters( fValues=TEW, sPrefix="TEW_", iNumRows=iNumberOfLanduses )


   ! we should have the REW table fully filled out following this block
 !  do iIndex = 1, iNumberOfREW
 !    sText = "REW_"//asCharacter(iIndex)
 !    call PARAMS%get_parameters( sText, REW(:, iIndex) )
 !  enddo  

   ! we should have the TEW table fully filled out following this block
 !  do iIndex = 1, iNumberOfTEW
 !    sText = "TEW_"//asCharacter(iIndex)
 !    call PARAMS%get_parameters( sText, TEW(:, iIndex) )
 !  enddo  

   !> @TODO What should happen if the TEW / REW header entries do *not* fall in a 
   !!       logical sequence of values? In other words, if the user has columns named
   !!       REW_1, REW_3, REW_5, only the values associated with "REW_1" would be retrieved.
   !!       Needless to say, this would be catastrophic.

   call PARAMS%get_parameters( sKey="L_ini", fValues=L_ini )
   call PARAMS%get_parameters( sKey="L_mid", fValues=L_mid )
   call PARAMS%get_parameters( sKey="L_late", fValues=L_late )
!   call PARAMS%get_parameters( "L_min", L_min )

   call PARAMS%get_parameters( sKey="Kcb_ini", fValues=KCB_ini )
   call PARAMS%get_parameters( sKey="Kcb_mid", fValues=KCB_mid )
   call PARAMS%get_parameters( sKey="Kcb_end", fValues=KCB_end )
   call PARAMS%get_parameters( sKey="Kcb_min", fValues=KCB_min )

   call PARAMS%get_parameters( sKey="Kcb_Jan", fValues=KCB_jan )
   call PARAMS%get_parameters( sKey="Kcb_Feb", fValues=KCB_feb )
   call PARAMS%get_parameters( sKey="Kcb_Mar", fValues=KCB_mar )
   call PARAMS%get_parameters( sKey="Kcb_Apr", fValues=KCB_apr )
   call PARAMS%get_parameters( sKey="Kcb_May", fValues=KCB_may )
   call PARAMS%get_parameters( sKey="Kcb_Jun", fValues=KCB_jun )
   call PARAMS%get_parameters( sKey="Kcb_Jul", fValues=KCB_jul )
   call PARAMS%get_parameters( sKey="Kcb_Aug", fValues=KCB_aug )
   call PARAMS%get_parameters( sKey="Kcb_Sep", fValues=KCB_sep )
   call PARAMS%get_parameters( sKey="Kcb_Oct", fValues=KCB_oct )
   call PARAMS%get_parameters( sKey="Kcb_Nov", fValues=KCB_nov )
   call PARAMS%get_parameters( sKey="Kcb_Dec", fValues=KCB_dec )

   call PARAMS%get_parameters( sKey="Depletion_Fraction", fValues=DEPLETION_FRACTION )
   call PARAMS%get_parameters( sKey="Mean_Plant_Height", fValues=MEAN_PLANT_HEIGHT )

  !> @TODO Add more logic here to perform checks on the validity of this data.

  !> @TODO Need to handle missing values. WHat do we do if an entire column of values
  !!       is missing?

  end subroutine soil_moisture_FAO56_initialize

!------------------------------------------------------------------------------

 !> Update the current basal crop coefficient (Kcb) for
 !! a SINGLE irrigation table entry
 !!
 !! @param[inout] pIRRIGATION pointer to a single line of information in the irrigation file.
 !! @param[in] iThreshold either the current day of year or the number of growing degree days.
 !! @retval rKcb Basal crop coefficient given the irrigation table entries and the 
 !!         current threshold values.

 elemental function sm_FAO56_UpdateCropCoefficient( iLanduseIndex, iThreshold )  result(fKcb)

  integer (kind=c_int), intent(in)   :: iLanduseIndex
  integer (kind=c_int), intent(in)   :: iThreshold
  real (kind=c_float)                :: fKcb

  ! [ LOCALS ]
  real (kind=c_float) :: fFrac

  ! define shorthand variable names for remainder of function
  associate ( L_ini => L_GROWTH( L_INIT, iLanduseIndex ),       &
              L_mid => L_GROWTH( L_MID, iLanduseIndex ),        &
              L_late => L_GROWTH( L_LATE, iLanduseIndex ),      &
              Kcb_ini => KCB(KCB_INI, iLanduseIndex),           &
              Kcb_mid => KCB(KCB_MID, iLanduseIndex),           &
              Kcb_min => KCB(KCB_MIN, iLanduseIndex),           &
              PlantingDate => PLANTING_DATE( iLanduseIndex),    &
              Kcb_end => KCB(KCB_END, iLanduseIndex) )

    ! NOTE that L ("length") may be given in terms of DAYS or GDD increments;
    !      Therefore, the value of "iThreshold" may also be specified in terms of
    !      GDD or day of year (DOY).

    ! now calculate Kcb for the given landuse
    if( iThreshold > L_late ) then

      fKcb = Kcb_min

    elseif ( iThreshold > L_mid ) then
      
      fFrac = real(iThreshold - L_mid, kind=c_double ) / real( L_late - L_mid, kind=c_float )

      fKcb =  Kcb_mid * (1_c_float - fFrac) + Kcb_end * fFrac

    elseif ( iThreshold > L_dev ) then
      
      fKcb = Kcb_mid

    elseif ( iThreshold > L_ini ) then

      fFrac = real( iThreshold - L_ini ) / real( L_dev - L_ini )

      fKcb = Kcb_ini * (1_c_float - fFrac) + Kcb_mid * fFrac

    elseif ( iThreshold >= PlantingDate ) then
      
      fKcb = Kcb_ini
    
    else
    
      fKcb = Kcb_min
    
    endif

  end associate

end function sm_FAO56_UpdateCropCoefficient

!------------------------------------------------------------------------------

!>

elemental function calc_evaporation_reduction_coeficient(fTotalEvaporableWater,                &
                                              fReadilyEvaporableWater, fDeficit)  result(fKr)

  ! [ ARGUMENTS ]
  real (kind=c_float), intent(in) :: fTotalEvaporableWater
  real (kind=c_float), intent(in) :: fReadilyEvaporableWater
  real (kind=c_float), intent(in) :: fDeficit

  ! [ RESULT ]
  real (kind=c_float) :: fKr

  associate ( REW => fReadilyEvaporableWater,     &
              TEW => fTotalEvaporableWater,       &
              Deficit => fDeficit )

    if ( Deficit > REW .and. Deficit < TEW ) then
      fKr = (TEW - Deficit) / (TEW - REW)
    elseif ( Deficit <= REW ) then
      fKr = 1.0_c_float
    else
      fKr = 0.0_c_float
    endif

  end associate

end function calc_evaporation_reduction_coeficient

!------------------------------------------------------------------------------

!> This function estimates the fraction of the ground covered by
!! vegetation during the growing season
!!
!!@note Implemented as equation 76, FAO-56, Allen and others

elemental function calc_fraction_wetted_and_exposed_soil( iLanduseIndex, fKcb)   result (f_few)

  integer (kind=c_int), intent(in)     :: iLanduseIndex
  real (kind=c_float), intent(in)      :: fKcb
  real (kind=c_float)                  :: f_few

  ! [ LOCALS ]
  real (kind=c_float) :: f_fc
  real (kind=c_float) :: fNumerator
  real (kind=c_float) :: fDenominator
  real (kind=c_float) :: fExponent

  fNumerator = fKcb - KCB( KCB_MIN, iLanduseIndex )
  fDenominator = KCB( KCB_MID, iLanduseIndex) - KCB( KCB_MIN, iLanduseIndex )
  fExponent = 1.0_c_float + 0.5_c_float * MEAN_PLANT_HEIGHT( iLanduseIndex ) * M_PER_FOOT

  ! calculate the fraction of the ground that is currently covered
  if(fDenominator >  0.0_c_float ) then
    f_fc = ( fNumerator / fDenominator) ** fExponent
  else
    f_fc = 1.0_c_float
  endif

  ! now calculate the fraction of the ground that is EXPOSED and WETTED
  f_few = 1.0_c_float - f_fc

  if ( f_few < 0.0_c_float ) f_few = 0.0_c_float
  if ( f_few > 1.0_c_float ) f_few = 1.0_c_float

end function calc_fraction_wetted_and_exposed_soil

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

elemental function calc_effective_root_depth( iLanduseIndex, fZr_max, iThreshold ) 	result(fZr_i)

  integer (kind=c_int), intent(in)    :: iLanduseIndex 
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
	if ( KCB(KCB_MID, iLanduseIndex) - KCB( KCB_INI, iLanduseIndex ) < 0.1) then

	  fZr_i = fZr_max

	elseif ( iThreshold < PLANTING_DATE( iLanduseIndex ) ) then

	  fZr_i = fZr_min

	else

    fZr_i = fZr_min + (fZr_max - fZr_min)                                     &
            * real(iThreshold - PLANTING_DATE(iLanduseIndex), kind=c_float )     &
            / real( L_GROWTH( L_DEV, iLanduseIndex ) -  PLANTING_DATE(iLanduseIndex), kind=c_float)

  endif

end function calc_effective_root_depth

!------------------------------------------------------------------------------

!> This function estimates Ke, the bare surface evaporation coefficient
!!
!! @note Implemented as equation 71, FAO-56, Allen and others

elemental function calc_surface_evaporation_coefficient( iLanduseIndex, fKr, fKcb )     result(fKe)

  integer (kind=c_int), intent(in)     :: iLanduseIndex
  real (kind=c_float), intent(in)      :: fKr
  real (kind=c_float), intent(in)      :: fKcb
  real (kind=c_float)                  :: fKe

  fKe = fKr * ( KCB( KCB_MID, iLanduseIndex ) - fKcb )

end function calc_surface_evaporation_coefficient

!------------------------------------------------------------------------------

!> This subroutine updates the total available water (TAW)
!> (water within the rootzone) for a gridcell

elemental subroutine calc_total_available_water_TAW(fTotalAvailableWater, fReadilyAvailableWater, &
                iLanduseIndex, fAvailableWaterCapacity, fCurrentRootingDepth )

  real (kind=c_float), intent(out)      :: fTotalAvailableWater
  real (kind=c_float), intent(out)      :: fReadilyAvailableWater  
  integer (kind=c_int), intent(in)      :: iLanduseIndex
  real (kind=c_float), intent(in)       :: fAvailableWaterCapacity
  real (kind=c_float), intent(in)       :: fCurrentRootingDepth

  fTotalAvailableWater = fCurrentRootingDepth * fAvailableWaterCapacity
  fReadilyAvailableWater = fTotalAvailableWater * DEPLETION_FRACTION( iLanduseIndex )

  end subroutine calc_total_available_water_TAW

!------------------------------------------------------------------------------

!> This function estimates Ks, water stress coefficient
!!
!! @note Implemented as equation 84, FAO-56, Allen and others

elemental function calc_water_stress_coefficient_Ks( iLanduseIndex, fDeficit, &
                      fTotalAvailableWater, fReadilyAvailableWater )      result(fKs)

  integer (kind=c_int), intent(in)     :: iLanduseIndex
  real (kind=c_float), intent(in)      :: fDeficit
  real (kind=c_float), intent(in)      :: fTotalAvailableWater
  real (kind=c_float), intent(in)      :: fReadilyAvailableWater
  real (kind=c_float)                  :: fKs

  if ( fDeficit < fReadilyAvailableWater ) then
    
    fKs = 1.0_c_float
  
  elseif ( fDeficit < fTotalAvailableWater ) then

    fKs = ( fTotalAvailableWater - fDeficit + 1.0e-6_c_float ) &
             / ( (1.0_c_float - DEPLETION_FRACTION( iLanduseIndex ) ) &
          * ( fTotalAvailableWater + 1.0e-6_c_float ) )

  else

    fKs = 0.0_c_float
  
  endif

end function calc_water_stress_coefficient_Ks

!------------------------------------------------------------------------------

 elemental subroutine soil_moisture_FAO56_calculate( fSoilStorage, fActual_ET,             &
   fSoilStorage_Excess, fInfiltration, fGDD, fAvailableWaterCapacity, fReference_ET0,       &
   fRootingDepth, iLanduseIndex, iSoilGroup )

  real (kind=c_float), intent(inout)   :: fSoilStorage
  real (kind=c_float), intent(inout)   :: fActual_ET
  real (kind=c_float), intent(out)     :: fSoilStorage_Excess
  real (kind=c_float), intent(in)      :: fInfiltration
  real (kind=c_float), intent(in)      :: fGDD
  real (kind=c_float), intent(in)      :: fAvailableWaterCapacity
  real (kind=c_float), intent(in)      :: fReference_ET0
  real (kind=c_float), intent(in)      :: fRootingDepth
  integer (kind=c_int), intent(in)     :: iLanduseIndex
  integer (kind=c_int), intent(in)     :: iSoilGroup

  ! [ LOCALS ]
  real (kind=c_float) :: fKcb      ! cell's current crop coefficient
  real (kind=c_float) :: fTEW      ! Total evaporable water
  real (kind=c_float) :: fREW      ! Readily evaporable water
  real (kind=c_float) :: fTAW      ! Total Available Water
  real (kind=c_float) :: fRAW      ! Readily Available Water
  real (kind=c_float) :: fKr       ! Evaporation reduction coefficient
  real (kind=c_float) :: fDeficit  ! Soil moisture deficit
  real (kind=c_float) :: f_few     ! Fraction exposed and wetted soil
  real (kind=c_float) :: fKe       ! Surface evaporation coefficient
  real (kind=c_float) :: fKs       ! Water stress coefficient
	real (kind=c_float) :: fZr_max   ! Maximum rooting depth
  real (kind=c_float) :: fZr       ! Current rooting depth
  real (kind=c_float) :: fSoilStorage_Max
  real (kind=c_float) :: fBareSoilEvap
  real (kind=c_float) :: fCropETc

  fSoilStorage_Max = fAvailableWaterCapacity * fZr_max

  if ( UNITS_ARE_DOY( iLanduseIndex ) ) then

   fKcb = sm_FAO56_UpdateCropCoefficient( iLanduseIndex, SIM_DT%iDOY )

   if ( SIM_DT%iDOY < L_GROWTH( L_DEV, iLanduseIndex ) ) then

     fZr = calc_effective_root_depth( iLanduseIndex, &
                              fZr_max, SIM_DT%iDOY )
  !					 cel%rSoilWaterCap = cel%rCurrentRootingDepth * cel%rSoilWaterCapInput
   endif

  else

   fKcb = sm_FAO56_UpdateCropCoefficient( iLanduseIndex, INT(fGDD, kind=c_int))

   fZr = calc_effective_root_depth( iLanduseIndex, &
                              fZr_max, INT(fGDD, kind=c_int) )
  !					 cel%rSoilWaterCap = cel%rCurrentRootingDepth * cel%rSoilWaterCapInput

  endif

  fREW = REW( iLanduseIndex, iSoilGroup )
  fTEW = TEW( iLanduseIndex, iSoilGroup )

  ! following call updates the total available water (TAW) and
  ! readily available water (RAW) on the basis of the current
  ! plant root depth
  call calc_total_available_water_TAW( fTotalAvailableWater=fTAW,                       &
                                      fReadilyAvailableWater=fRAW,                     &
                                      iLanduseIndex=iLanduseIndex,                     &
                                      fAvailableWaterCapacity=fAvailableWaterCapacity, &
                                      fCurrentRootingDepth=fZr )

  ! Deficit is defined in the sense of Thornthwaite and Mather
  !
  ! ### This should be defined in terms of TAW and RAW, no?
  !
  fDeficit = MAX( 0.0_c_float, fSoilStorage_Max - fSoilStorage )


  ! "STANDARD" vs "NONSTANDARD": in the FAO56 publication the term
  ! "STANDARD" is used to refer to crop ET requirements under
  ! ideal conditions (i.e. plants not stressed due to scarcity
  ! of water. "NONSTANDARD" is the term used to describe ET requirements
  ! when plants are under stress, when water is scarce.

  ! we are using the full FAO56 soil water balance approach, *INCLUDING*
  ! the adjustments for nonstandard growing conditions (e.g. plant
  ! stress and resulting decrease in ET during dry conditions).

  fKr = calc_evaporation_reduction_coeficient(fTotalEvaporableWater=fTEW,    &
                                             fReadilyEvaporableWater=fREW,  &
                                             fDeficit=fDeficit )

  f_few = calc_fraction_wetted_and_exposed_soil( iLanduseIndex, fKcb )

  fKe = min(calc_surface_evaporation_coefficient( iLanduseIndex, fKr, fKcb ),   &
           f_few * KCB( KCB_MID, iLanduseIndex) )

  !if (fKe < 0) print *, "fKe < 0: ", cel%iIrrigationTableIndex, iRow, iCol, cel%iLandUse, cel%iSoilGroup, rKe, pIRRIGATION%rKcb, pIRRIGATION%rKcb_max
  !if (fKr < 0) print *, "fKr < 0: ", cel%iIrrigationTableIndex, iRow, iCol, cel%iLandUse, cel%iSoilGroup, rKr, pIRRIGATION%rKcb, pIRRIGATION%rKcb_max
  !if (f_few < 0) print *, "f_few < 0: ", cel%iIrrigationTableIndex, iRow, iCol, cel%iLandUse, cel%iSoilGroup, r_few, pIRRIGATION%rKcb, pIRRIGATION%rKcb_max

  fKs = calc_water_stress_coefficient_Ks( iLanduseIndex=iLanduseIndex,   &
                                         fDeficit=fDeficit,             &
                                         fTotalAvailableWater=fTAW,     &
                                         fReadilyAvailableWater=fRAW )


  fBareSoilEvap = fReference_ET0 * fKe
  fCropETc = fReference_ET0 * (fKcb * fKs)

  ! 
  fActual_ET = fCropETc + fBareSoilEvap

end subroutine soil_moisture_FAO56_calculate

end module soil_moisture__FAO_56
