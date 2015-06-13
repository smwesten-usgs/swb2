!> @file
!>  Contains a single module, \ref et_crop_coefficients, which
!>  provides support for modifying reference ET through the use of
!> crop coefficients

!>  Provide support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module soil_moisture__FAO_56

  use iso_c_binding, only             : c_bool, c_short, c_int, c_float, c_double
  use constants_and_conversions, only : M_PER_FOOT, lTRUE, lFALSE, fTINYVAL, asInt
  use datetime
  use exceptions, only                : assert, warn
  use parameters, only                : PARAMS
  use simulation_datetime, only       : SIM_DT 
  use strings, only                   : asCharacter, sQuote
  use string_list
  implicit none

  private

  public :: soil_moisture_FAO56_initialize, soil_moisture_FAO56_calculate
  public :: soil_moisture_FAO56_update_growth_stage_dates
  public :: update_crop_coefficient_date_as_threshold, update_crop_coefficient_GDD_as_threshold

  enum, bind(c)
    enumerator :: L_DOY_INI=1, L_DOY_DEV, L_DOY_MID, L_DOY_LATE
  end enum 

  enum, bind(c)
    enumerator :: GDD_PLANT=1, GDD_INI, GDD_DEV, GDD_MID, GDD_LATE
  end enum 

  enum, bind(c)
    enumerator :: PLANTING_DATE=1, ENDDATE_INI, ENDDATE_DEV, ENDDATE_MID, ENDDATE_LATE
  end enum 

  enum, bind(c)
    enumerator :: KCB_INI=13, KCB_MID, KCB_END, KCB_MIN
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
  real (kind=c_float), allocatable   :: GROWTH_STAGE_DOY(:,:)
  real (kind=c_float), allocatable   :: GROWTH_STAGE_GDD(:,:)
  type (DATETIME_T), allocatable     :: GROWTH_STAGE_DATE(:,:)
  logical (kind=c_bool), allocatable :: UNITS_ARE_DAYS(:)

  !real (kind=c_float), 
  real (kind=c_float), allocatable   :: DEPLETION_FRACTION(:)
  real (kind=c_float), allocatable   :: MEAN_PLANT_HEIGHT(:)

contains

  subroutine soil_moisture_FAO56_initialize( iNumActiveCells )

    integer (kind=c_int), intent(in)   :: iNumActiveCells

    ! [ LOCALS ]
    type (STRING_LIST_T)              :: slREW, slTEW
    type (STRING_LIST_T)              :: slList
    type (DATETIME_T)                 :: DT
    integer (kind=c_int), allocatable :: iTEWSeqNums(:)
    integer (kind=c_int), allocatable :: iREWSeqNums(:)
    integer (kind=c_int)              :: iNumberOfTEW, iNumberOfREW
    integer (kind=c_int)              :: iNumberOfLanduses
    integer (kind=c_int)              :: iIndex, iIndex2
    integer (kind=c_int)              :: iStat

    character (len=10)               :: sMMDDYYYY
    character (len=:), allocatable   :: sText

    type (STRING_LIST_T)             :: slPlantingDate

    real (kind=c_float), allocatable :: L_ini_(:)
    real (kind=c_float), allocatable :: L_dev_(:)
    real (kind=c_float), allocatable :: L_mid_(:)
    real (kind=c_float), allocatable :: L_late_(:)

    real (kind=c_float), allocatable :: GDD_plant_(:)
    real (kind=c_float), allocatable :: GDD_ini_(:)
    real (kind=c_float), allocatable :: GDD_dev_(:)
    real (kind=c_float), allocatable :: GDD_mid_(:)
    real (kind=c_float), allocatable :: GDD_late_(:)

    real (kind=c_float), allocatable :: Kcb_ini_(:)            
    real (kind=c_float), allocatable :: Kcb_mid_(:)            
    real (kind=c_float), allocatable :: Kcb_end_(:)            
    real (kind=c_float), allocatable :: Kcb_min_(:)
    
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

   !> create string list that allows for alternate heading identifiers for the landuse code
   call slList%append("LU_Code")
   call slList%append("Landuse_Code")
   call slList%append("Landuse_Lookup_Code")

   !> Determine how many landuse codes are present
   call PARAMS%get_parameters( slKeys=slList, iValues=LANDUSE_CODE )
   iNumberOfLanduses = count( LANDUSE_CODE >= 0 )

   !> @todo Implement thorough input error checking: 
   !! are all soils in grid included in table values?
   !> is soil suffix vector continuous?

   ! Retrieve and populate the Readily Evaporable Water (REW) table values
   CALL PARAMS%get_parameters( fValues=REW, sPrefix="REW_", iNumRows=iNumberOfLanduses )

   ! Retrieve and populate the Total Evaporable Water (TEW) table values
   CALL PARAMS%get_parameters( fValues=TEW, sPrefix="TEW_", iNumRows=iNumberOfLanduses )

   !> @TODO What should happen if the TEW / REW header entries do *not* fall in a 
   !!       logical sequence of values? In other words, if the user has columns named
   !!       REW_1, REW_3, REW_5, only the values associated with "REW_1" would be retrieved.
   !!       Needless to say, this would be catastrophic.

   call PARAMS%get_parameters( sKey="Planting_date", slValues=slPlantingDate, lFatal=lTRUE )

   call PARAMS%get_parameters( sKey="L_ini", fValues=L_ini_, lFatal=lTRUE )
   call PARAMS%get_parameters( sKey="L_dev", fValues=L_dev_, lFatal=lTRUE )
   call PARAMS%get_parameters( sKey="L_mid", fValues=L_mid_, lFatal=lTRUE )
   call PARAMS%get_parameters( sKey="L_late", fValues=L_late_, lFatal=lTRUE )

   call PARAMS%get_parameters( sKey="GDD_plant", fValues=GDD_plant_, lFatal=lFALSE )
   call PARAMS%get_parameters( sKey="GDD_ini", fValues=GDD_ini_, lFatal=lFALSE )
   call PARAMS%get_parameters( sKey="GDD_dev", fValues=GDD_dev_, lFatal=lFALSE )
   call PARAMS%get_parameters( sKey="GDD_mid", fValues=GDD_mid_, lFatal=lFALSE )
   call PARAMS%get_parameters( sKey="GDD_late", fValues=GDD_late_, lFatal=lFALSE )

   call PARAMS%get_parameters( sKey="Kcb_ini", fValues=KCB_ini_, lFatal=lTRUE )
   call PARAMS%get_parameters( sKey="Kcb_mid", fValues=KCB_mid_, lFatal=lTRUE )
   call PARAMS%get_parameters( sKey="Kcb_end", fValues=KCB_end_, lFatal=lTRUE )
   call PARAMS%get_parameters( sKey="Kcb_min", fValues=KCB_min_, lFatal=lTRUE )

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

   call PARAMS%get_parameters( sKey="Depletion_Fraction", fValues=DEPLETION_FRACTION, lFatal=lTRUE )
   call PARAMS%get_parameters( sKey="Mean_Plant_Height", fValues=MEAN_PLANT_HEIGHT, lFatal=lTRUE )
   call PARAMS%get_parameters( sKey="Units_Are_Days", lValues=UNITS_ARE_DAYS, lFatal=lTRUE )

    allocate( GROWTH_STAGE_DOY( 4, iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory for GROWTH_STAGE_DOY array", &
      __FILE__, __LINE__ )

    allocate( GROWTH_STAGE_GDD( 5, iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory for GROWTH_STAGE_GDD array", &
      __FILE__, __LINE__ )

    allocate( GROWTH_STAGE_DATE( 5, iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory for DATE_GROWTH array", &
      __FILE__, __LINE__ )

    allocate( KCB( 16, iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory for KCB array", &
      __FILE__, __LINE__ )

    GROWTH_STAGE_DOY = 0.0_c_float


    if ( (ubound(L_ini_,1) == iNumberOfLanduses)      &
     .and. (ubound(L_dev_,1) == iNumberOfLanduses)  &
     .and. (ubound(L_mid_,1) == iNumberOfLanduses)  & 
     .and. (ubound(L_late_,1) == iNumberOfLanduses) &
     .and. ( slPlantingDate%count == iNumberOfLanduses ) ) then

      GROWTH_STAGE_DOY( L_DOY_INI,  : ) = L_ini_
      GROWTH_STAGE_DOY( L_DOY_DEV,  : ) = L_dev_
      GROWTH_STAGE_DOY( L_DOY_MID,  : ) = L_mid_
      GROWTH_STAGE_DOY( L_DOY_LATE, : ) = L_late_

      do iIndex=1, slPlantingDate%count   

        sMMDDYYYY = trim(slPlantingDate%get( iIndex ))//"/"//asCharacter( SIM_DT%start%iYear ) 

        call GROWTH_STAGE_DATE( PLANTING_DATE, iIndex)%parsedate( sMMDDYYYY, __FILE__, __LINE__ )
   
        GROWTH_STAGE_DATE( ENDDATE_INI, iIndex ) = GROWTH_STAGE_DATE( PLANTING_DATE, iIndex ) + L_ini_( iIndex )
        GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_INI, iIndex ) + L_dev_( iIndex )
        GROWTH_STAGE_DATE( ENDDATE_MID, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex ) + L_mid_( iIndex )
        GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_MID, iIndex ) + L_late_( iIndex )

      enddo

    else

      call warn( sMessage="Problem processing planting dates and growth phase lengths; the number " &
        //"of growth phase lengths and planting dates must be equal to the number of landuses present.", &
        lFatal=lTRUE )

    endif  



    GROWTH_STAGE_GDD = 0.0_c_float

    if (ubound(GDD_plant_,1) == iNumberOfLanduses) GROWTH_STAGE_GDD( GDD_PLANT,  : ) = GDD_plant_
    if (ubound(GDD_ini_,1) == iNumberOfLanduses) GROWTH_STAGE_GDD( GDD_INI,  : ) = GDD_ini_
    if (ubound(GDD_dev_,1) == iNumberOfLanduses) GROWTH_STAGE_GDD( GDD_DEV,  : ) = GDD_dev_
    if (ubound(GDD_mid_,1) == iNumberOfLanduses) GROWTH_STAGE_GDD( GDD_MID,  : ) = GDD_mid_
    if (ubound(GDD_late_,1) == iNumberOfLanduses) GROWTH_STAGE_GDD( GDD_LATE, : ) = GDD_late_


    KCB = fTINYVAL
 
    KCB( KCB_INI, :) = KCB_ini_
    KCB( KCB_MID, :) = KCB_mid_
    KCB( KCB_END, :) = KCB_end_
    KCB( KCB_MIN, :) = KCB_min_

    if (all( KCB_jan > fTINYVAL ) ) KCB( JAN, lbound(KCB_jan, 1):ubound(KCB_jan, 1)) = KCB_jan
    if (all( KCB_feb > fTINYVAL ) ) KCB( FEB, lbound(KCB_feb, 1):ubound(KCB_feb, 1)) = KCB_feb
    if (all( KCB_mar > fTINYVAL ) ) KCB( MAR, lbound(KCB_mar, 1):ubound(KCB_mar, 1)) = KCB_mar
    if (all( KCB_apr > fTINYVAL ) ) KCB( APR, lbound(KCB_apr, 1):ubound(KCB_apr, 1)) = KCB_apr
    if (all( KCB_may > fTINYVAL ) ) KCB( MAY, lbound(KCB_may, 1):ubound(KCB_may, 1)) = KCB_may
    if (all( KCB_jun > fTINYVAL ) ) KCB( JUN, lbound(KCB_jun, 1):ubound(KCB_jun, 1)) = KCB_jun
    if (all( KCB_jul > fTINYVAL ) ) KCB( JUL, lbound(KCB_jul, 1):ubound(KCB_jul, 1)) = KCB_jul
    if (all( KCB_aug > fTINYVAL ) ) KCB( AUG, lbound(KCB_aug, 1):ubound(KCB_aug, 1)) = KCB_aug
    if (all( KCB_sep > fTINYVAL ) ) KCB( SEP, lbound(KCB_sep, 1):ubound(KCB_sep, 1)) = KCB_sep
    if (all( KCB_oct > fTINYVAL ) ) KCB( OCT, lbound(KCB_oct, 1):ubound(KCB_oct, 1)) = KCB_oct
    if (all( KCB_nov > fTINYVAL ) ) KCB( NOV, lbound(KCB_nov, 1):ubound(KCB_nov, 1)) = KCB_nov
    if (all( KCB_dec > fTINYVAL ) ) KCB( DEC, lbound(KCB_dec, 1):ubound(KCB_dec, 1)) = KCB_dec

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

 elemental function update_crop_coefficient_date_as_threshold( iLanduseIndex )           & 
                                                                          result(fKcb)

  integer (kind=c_int), intent(in)   :: iLanduseIndex
  real (kind=c_float)                :: fKcb

  ! [ LOCALS ]
  real (kind=c_float) :: fFrac

  if ( KCB( SIM_DT%curr%iMonth, iLanduseIndex ) > 0.0_c_float ) then

    fKCB = KCB( SIM_DT%curr%iMonth, iLanduseIndex )

  else  

    ! define shorthand variable names for remainder of function
    associate ( Date_ini => GROWTH_STAGE_DATE( ENDDATE_INI, iLanduseIndex ),         &
                Date_dev => GROWTH_STAGE_DATE( ENDDATE_DEV, iLanduseIndex ),         &
                Date_mid => GROWTH_STAGE_DATE( ENDDATE_MID, iLanduseIndex ),         &
                Date_late => GROWTH_STAGE_DATE( ENDDATE_LATE, iLanduseIndex ),       &
                Kcb_ini => KCB(KCB_INI, iLanduseIndex),                              &
                Kcb_mid => KCB(KCB_MID, iLanduseIndex),                              &
                Kcb_min => KCB(KCB_MIN, iLanduseIndex),                              &
                PlantingDate => GROWTH_STAGE_DATE( PLANTING_DATE, iLanduseIndex),    &
                Kcb_end => KCB(KCB_END, iLanduseIndex),                              &
                current_date => SIM_DT%curr )

      ! now calculate Kcb for the given landuse
      if( current_date > Date_late ) then

        fKcb = Kcb_min

      elseif ( current_date > Date_mid ) then
        
        fFrac = ( current_date - Date_mid ) / ( Date_late - Date_mid )

        fKcb =  Kcb_mid * (1_c_float - fFrac) + Kcb_end * fFrac

      elseif ( current_date > Date_dev ) then
        
        fKcb = Kcb_mid

      elseif ( current_date > Date_ini ) then

        fFrac = ( current_date - Date_ini ) / ( Date_dev - Date_ini )

        fKcb = Kcb_ini * (1_c_float - fFrac) + Kcb_mid * fFrac

      elseif ( current_date >= PlantingDate ) then
        
        fKcb = Kcb_ini
      
      else
      
        fKcb = Kcb_min
      
      endif

    end associate

  end if

end function update_crop_coefficient_date_as_threshold

!------------------------------------------------------------------------------

 !> Update the current basal crop coefficient (Kcb), with GDD as the threhold
 !!
 !! @param[in] fGDD current growing degree day value associated with the cell.
 !! @retval fKcb Basal crop coefficient given the irrigation table entries and the 
 !!         current threshold values.

 elemental function update_crop_coefficient_GDD_as_threshold( iLanduseIndex, fGDD )   &
                                                                         result(fKcb)

  integer (kind=c_int), intent(in)   :: iLanduseIndex
  real (kind=c_float), intent(in)    :: fGDD
  real (kind=c_float)                :: fKcb

  ! [ LOCALS ]
  real (kind=c_float) :: fFrac

  if ( KCB( SIM_DT%curr%iMonth, iLanduseIndex ) > 0.0_c_float ) then

    fKCB = KCB( SIM_DT%curr%iMonth, iLanduseIndex )

  else  

    ! define shorthand variable names for remainder of function
    associate ( GDD_ini_ => GROWTH_STAGE_GDD( GDD_INI, iLanduseIndex ),         &
                GDD_dev_ => GROWTH_STAGE_GDD( GDD_DEV, iLanduseIndex ),         &
                GDD_mid_ => GROWTH_STAGE_GDD( GDD_MID, iLanduseIndex ),         &
                GDD_late_ => GROWTH_STAGE_GDD( GDD_LATE, iLanduseIndex ),       &
                Kcb_ini => KCB(KCB_INI, iLanduseIndex),                         &
                Kcb_mid => KCB(KCB_MID, iLanduseIndex),                         &
                Kcb_min => KCB(KCB_MIN, iLanduseIndex),                         &
                GDD_plant_ => GROWTH_STAGE_GDD( GDD_PLANT, iLanduseIndex),      &
                Kcb_end => KCB(KCB_END, iLanduseIndex) )

      ! now calculate Kcb for the given landuse
      if( fGDD > GDD_late_ ) then

        fKcb = Kcb_min

      elseif ( fGDD > GDD_mid_ ) then
        
        fFrac = ( fGDD - GDD_mid_ ) / ( GDD_late_ - GDD_mid_ )

        fKcb =  Kcb_mid * (1_c_float - fFrac) + Kcb_end * fFrac

      elseif ( fGDD > GDD_dev_ ) then
        
        fKcb = Kcb_mid

      elseif ( fGDD > GDD_ini_ ) then

        fFrac = ( fGDD - GDD_ini_ ) / ( GDD_dev_ - GDD_ini_ )

        fKcb = Kcb_ini * (1_c_float - fFrac) + Kcb_mid * fFrac

      elseif ( fGDD >= GDD_plant_ ) then
        
        fKcb = Kcb_ini
      
      else
      
        fKcb = Kcb_min
      
      endif

    end associate

  end if

end function update_crop_coefficient_GDD_as_threshold

!------------------------------------------------------------------------------

subroutine soil_moisture_FAO56_update_growth_stage_dates( )

  ! [ LOCALS ]
  integer (kind=c_int) :: iIndex
  integer (kind=c_int) :: iNextPlantingYear

  do iIndex=lbound(GROWTH_STAGE_DATE,2), ubound(GROWTH_STAGE_DATE,2) 

!     print *, SIM_DT%curr%prettydate(), " | ", GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex )%prettydate()

    if ( SIM_DT%curr <= GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex ) ) cycle 

    GROWTH_STAGE_DATE( PLANTING_DATE, iIndex )%iYear = GROWTH_STAGE_DATE( PLANTING_DATE, iIndex )%iYear + 1
    call GROWTH_STAGE_DATE( PLANTING_DATE, iIndex )%calcJulianDay()

    print *, "New planting date set for LU Index ",iIndex,": ",  &
       GROWTH_STAGE_DATE( PLANTING_DATE, iIndex )%prettydate()

    GROWTH_STAGE_DATE( ENDDATE_INI, iIndex ) = GROWTH_STAGE_DATE( PLANTING_DATE, iIndex ) &
                                              + GROWTH_STAGE_DOY( L_DOY_INI, iIndex )
    GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_INI, iIndex )&
                                              + GROWTH_STAGE_DOY( L_DOY_DEV, iIndex )
    GROWTH_STAGE_DATE( ENDDATE_MID, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex )&
                                              + GROWTH_STAGE_DOY( L_DOY_MID, iIndex )
    GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_MID, iIndex )&
                                              + GROWTH_STAGE_DOY( L_DOY_LATE, iIndex )

!      GROWTH_STAGE_DATE( ENDDATE_INI, iIndex) = GROWTH_STAGE_DATE( PLANTING_DATE, iIndex) + 1.0

  enddo


end subroutine soil_moisture_FAO56_update_growth_stage_dates

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

  fNumerator = fKcb - minval(KCB( :, iLanduseIndex ) )
  fDenominator = maxval( KCB( :, iLanduseIndex) ) - minval( KCB( :, iLanduseIndex ) )
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

elemental function calc_effective_root_depth( iLanduseIndex, fZr_max, fKCB ) 	result(fZr_i)

  integer (kind=c_int), intent(in)    :: iLanduseIndex 
	real (kind=c_float), intent(in)     :: fZr_max
  real (kind=c_float), intent(in)     :: fKCB

  ! [ RESULT ]
  real (kind=c_float) :: fZr_i

	! [ LOCALS ]
	! 0.328 feet equals 0.1 meters, which is seems to be the standard
	! initial rooting depth in the FAO-56 methodology
	real (kind=c_float), parameter :: fZr_min = 0.328
  real (kind=c_float)            :: fMaxKCB
  real (kind=c_float)            :: fMinKCB

  fMaxKCB = maxval( KCB( :, iLanduseIndex ) )
  fMinKCB = minval( KCB( :, iLanduseIndex ) ) 

  ! if there is not much difference between the MAX Kcb and MIN Kcb, assume that
  ! we are dealing with an area such as a forest, where we assume that the rooting
  ! depths are constant year-round
	if ( ( fMaxKCB - fMinKCB ) < 0.1_c_float ) then

	  fZr_i = fZr_max

	elseif ( fMaxKCB > 0.0_C_float ) then

    fZr_i = fZr_min + (fZr_max - fZr_min) * fKCB / fMaxKCB 

  else

    fZr_i = fZr_min

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

  fKe = fKr * ( maxval( KCB( :, iLanduseIndex ) ) - fKcb )

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

  subroutine soil_moisture_FAO56_calculate( fSoilStorage, fActual_ET,                     &
    fSoilStorage_Excess, fInfiltration, fGDD, fAvailableWaterCapacity, fReference_ET0,    &
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

  fSoilStorage_Max = fAvailableWaterCapacity * fRootingDepth

  if ( UNITS_ARE_DAYS( iLanduseIndex ) ) then

    fKcb = update_crop_coefficient_date_as_threshold( iLanduseIndex )

  else

  !  fKcb = sm_FAO56_UpdateCropCoefficient( iLanduseIndex, INT(fGDD, kind=c_int), asInt(SIM_DT%curr%iMonth)  )

  !					 cel%rSoilWaterCap = cel%rCurrentRootingDepth * cel%rSoilWaterCapInput

  endif

  ! change from earlier coding: rooting depth is now simply keyed into the current Kcb
  fZr = calc_effective_root_depth( iLanduseIndex, fZr_max, fKcb )

  fREW = REW( iLanduseIndex, iSoilGroup )
  fTEW = TEW( iLanduseIndex, iSoilGroup )

  ! following call updates the total available water (TAW) and
  ! readily available water (RAW) on the basis of the current
  ! plant root depth
  call calc_total_available_water_TAW( fTotalAvailableWater=fTAW,                        &
                                       fReadilyAvailableWater=fRAW,                      &
                                       iLanduseIndex=iLanduseIndex,                      &
                                       fAvailableWaterCapacity=fAvailableWaterCapacity,  &
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
                                             fReadilyEvaporableWater=fREW,   &
                                             fDeficit=fDeficit )

  f_few = calc_fraction_wetted_and_exposed_soil( iLanduseIndex, fKcb )

  fKe = min(calc_surface_evaporation_coefficient( iLanduseIndex, fKr, fKcb ),   &
            f_few * KCB( KCB_MID, iLanduseIndex) )

  fKs = calc_water_stress_coefficient_Ks( iLanduseIndex=iLanduseIndex,   &
                                          fDeficit=fDeficit,             &
                                          fTotalAvailableWater=fTAW,     &
                                          fReadilyAvailableWater=fRAW )


  fBareSoilEvap = fReference_ET0 * fKe
  fCropETc = fReference_ET0 * (fKcb * fKs)

  ! 
  fActual_ET = fCropETc + fBareSoilEvap

  fSoilStorage = fSoilStorage + fInfiltration - fActual_ET

  if (fSoilStorage > fSoilStorage_Max ) then
    fSoilStorage_Excess = fSoilStorage - fSoilStorage_Max
    fSoilStorage = fSoilStorage_Max
  else
    fSoilStorage_Excess = 0.0_c_float
  endif  

end subroutine soil_moisture_FAO56_calculate

end module soil_moisture__FAO_56
