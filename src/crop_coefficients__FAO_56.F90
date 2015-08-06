!> @file
!>  Contains a single module, \ref crop_coefficients__FAO56, which
!>  provides support for modifying reference ET through the use of
!>  crop coefficients

!> Update crop coefficients for crop types in simulation.

module crop_coefficients__FAO_56

  use iso_c_binding, only             : c_bool, c_short, c_int, c_float, c_double
  use constants_and_conversions, only : M_PER_FOOT, lTRUE, lFALSE, fTINYVAL, &
                                        iTINYVAL, asInt, fZERO, in_to_mm
  use data_catalog, only              : DAT
  use data_catalog_entry, only        : DATA_CATALOG_ENTRY_T
  use datetime
  use exceptions, only                : assert, warn, die
  use parameters, only                : PARAMS
  use simulation_datetime, only       : SIM_DT 
  use strings, only                   : asCharacter, sQuote
  use string_list
  implicit none

  private

  public :: crop_coefficients_FAO56_initialize, crop_coefficients_FAO56_calculate
  public :: crop_coefficients_FAO56_update_growth_stage_dates
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

   enum, bind(c)
     enumerator :: KCB_METHOD_GDD = 1, KCB_METHOD_MONTHLY_VALUES, KCB_METHOD_FAO56
   end enum  

  ! Private, module level variables
  ! kept at a landuse code level (i.e. same value applies to all cells with same LU codes)
  integer (kind=c_int), allocatable  :: LANDUSE_CODE(:)
  real (kind=c_float), allocatable   :: REW(:,:)
  real (kind=c_float), allocatable   :: TEW(:,:)
  real (kind=c_float), allocatable   :: KCB(:,:)
  integer (kind=c_int), allocatable  :: KCB_METHOD(:)
  real (kind=c_float), allocatable   :: GROWTH_STAGE_DOY(:,:)
  real (kind=c_float), allocatable   :: GROWTH_STAGE_GDD(:,:)
  type (DATETIME_T), allocatable     :: GROWTH_STAGE_DATE(:,:)

  !real (kind=c_float), 
  real (kind=c_float), allocatable   :: DEPLETION_FRACTION(:)
  real (kind=c_float), allocatable   :: MEAN_PLANT_HEIGHT(:)

  integer (kind=c_int)               :: LU_SOILS_CSV

contains

  subroutine crop_coefficients_FAO56_initialize( fSoilStorage, iLanduseIndex, iSoilGroup, &
                                                 fAvailable_Water_Content, lActive )

    real (kind=c_float), intent(inout)   :: fSoilStorage(:)
    integer (kind=c_int), intent(in)     :: iLanduseIndex(:)
    integer (kind=c_int), intent(in)     :: iSoilGroup(:)
    real (kind=c_float), intent(in)      :: fAvailable_Water_Content(:)
    logical (kind=c_bool), intent(in)    :: lActive(:,:)

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

    real (kind=c_float)              :: fKcb_initial
    real (kind=c_float)              :: fRz_initial

    type (DATA_CATALOG_ENTRY_T), pointer :: pINITIAL_PERCENT_SOIL_MOISTURE

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

    allocate( KCB_METHOD( iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory for KCB_METHOD vector", &
      __FILE__, __LINE__ )

    KCB_METHOD = iTINYVAL
    KCB = fTINYVAL
    GROWTH_STAGE_GDD = fTINYVAL
    GROWTH_STAGE_DOY = fTINYVAL

    if ( ubound(L_ini_,1) == iNumberOfLanduses ) then
      GROWTH_STAGE_DOY( L_DOY_INI,  : ) = L_ini_
    else
      call warn(sMessage="L_ini has "//asCharacter(ubound(L_ini_,1))//" entries; there are "  &
        //asCharacter(iNumberOfLanduses)//" landuse codes.", lFatal=lTRUE)
    endif      

    if ( ubound(L_dev_,1) == iNumberOfLanduses ) then
      GROWTH_STAGE_DOY( L_DOY_DEV,  : ) = L_dev_
    else
      call warn(sMessage="L_dev has "//asCharacter(ubound(L_dev_,1))//" entries; there are "  &
        //asCharacter(iNumberOfLanduses)//" landuse codes.", lFatal=lTRUE)
    endif      

    if ( ubound(L_mid_,1) == iNumberOfLanduses ) then
      GROWTH_STAGE_DOY( L_DOY_MID,  : ) = L_mid_
    else
      call warn(sMessage="L_mid has "//asCharacter(ubound(L_mid_,1))//" entries; there are "  &
        //asCharacter(iNumberOfLanduses)//" landuse codes.", lFatal=lTRUE)
    endif      

    if ( ubound(L_late_,1) == iNumberOfLanduses ) then
      GROWTH_STAGE_DOY( L_DOY_LATE, : ) = L_late_
    else
      call warn(sMessage="L_late has "//asCharacter(ubound(L_late_,1))//" entries; there are "  &
        //asCharacter(iNumberOfLanduses)//" landuse codes.", lFatal=lTRUE)
    endif      


    if ( slPlantingDate%count == iNumberOfLanduses ) then

      do iIndex=1, slPlantingDate%count   

        sMMDDYYYY = trim(slPlantingDate%get( iIndex ))//"/"//asCharacter( SIM_DT%start%iYear ) 

        call GROWTH_STAGE_DATE( PLANTING_DATE, iIndex)%parsedate( sMMDDYYYY, __FILE__, __LINE__ )
   
        GROWTH_STAGE_DATE( ENDDATE_INI, iIndex ) = GROWTH_STAGE_DATE( PLANTING_DATE, iIndex ) + L_ini_( iIndex )
        GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_INI, iIndex ) + L_dev_( iIndex )
        GROWTH_STAGE_DATE( ENDDATE_MID, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex ) + L_mid_( iIndex )
        GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_MID, iIndex ) + L_late_( iIndex )

      enddo

    endif  

    if (ubound(GDD_plant_,1) == iNumberOfLanduses)  GROWTH_STAGE_GDD( GDD_PLANT,  : ) = GDD_plant_
    if (ubound(GDD_ini_,1) == iNumberOfLanduses)    GROWTH_STAGE_GDD( GDD_INI,  : ) = GDD_ini_
    if (ubound(GDD_dev_,1) == iNumberOfLanduses)    GROWTH_STAGE_GDD( GDD_DEV,  : ) = GDD_dev_
    if (ubound(GDD_mid_,1) == iNumberOfLanduses)    GROWTH_STAGE_GDD( GDD_MID,  : ) = GDD_mid_
    if (ubound(GDD_late_,1) == iNumberOfLanduses)   GROWTH_STAGE_GDD( GDD_LATE, : ) = GDD_late_

    if (ubound(KCB_ini_,1) == iNumberOfLanduses)  KCB( KCB_INI, :) = KCB_ini_
    if (ubound(KCB_mid_,1) == iNumberOfLanduses)  KCB( KCB_MID, :) = KCB_mid_
    if (ubound(KCB_end_,1) == iNumberOfLanduses)  KCB( KCB_END, :) = KCB_end_
    if (ubound(KCB_min_,1) == iNumberOfLanduses)  KCB( KCB_MIN, :) = KCB_min_

    if (ubound(KCB_jan,1) == iNumberOfLanduses)   KCB( JAN, :) = KCB_jan
    if (ubound(KCB_feb,1) == iNumberOfLanduses)   KCB( FEB, :) = KCB_feb
    if (ubound(KCB_mar,1) == iNumberOfLanduses)   KCB( MAR, :) = KCB_mar
    if (ubound(KCB_apr,1) == iNumberOfLanduses)   KCB( APR, :) = KCB_apr
    if (ubound(KCB_may,1) == iNumberOfLanduses)   KCB( MAY, :) = KCB_may
    if (ubound(KCB_jun,1) == iNumberOfLanduses)   KCB( JUN, :) = KCB_jun
    if (ubound(KCB_jul,1) == iNumberOfLanduses)   KCB( JUL, :) = KCB_jul
    if (ubound(KCB_aug,1) == iNumberOfLanduses)   KCB( AUG, :) = KCB_aug
    if (ubound(KCB_sep,1) == iNumberOfLanduses)   KCB( SEP, :) = KCB_sep
    if (ubound(KCB_oct,1) == iNumberOfLanduses)   KCB( OCT, :) = KCB_oct
    if (ubound(KCB_nov,1) == iNumberOfLanduses)   KCB( NOV, :) = KCB_nov
    if (ubound(KCB_dec,1) == iNumberOfLanduses)   KCB( DEC, :) = KCB_dec

    do iIndex = lbound( KCB_METHOD, 1), ubound( KCB_METHOD, 1)

      if ( all( KCB( JAN:DEC, iIndex ) >= 0.0_c_float ) ) then
        KCB_METHOD( iIndex ) = KCB_METHOD_MONTHLY_VALUES
              
      elseif ( all( GROWTH_STAGE_GDD( :, iIndex ) >= 0.0_c_float )  &
         .and. all( KCB( KCB_INI:KCB_MIN, iIndex ) >= 0.0_c_float ) ) then
        KCB_METHOD( iIndex ) = KCB_METHOD_GDD

      elseif ( all( GROWTH_STAGE_DOY( :, iIndex ) >= 0.0_c_float )  &
         .and. all( KCB( KCB_INI:KCB_MIN, iIndex ) >= 0.0_c_float ) ) then
        KCB_METHOD( iIndex ) = KCB_METHOD_FAO56
      endif

      if ( KCB_METHOD( iIndex ) < 0 )  &
        call warn("There are missing day-of-year (L_ini, L_dev, L_mid, L_late), " &
          //"growing degree-day ~(GDD_plant, GDD_ini, GDD_dev, GDD_mid, GDD_late)," &
          //" or monthly crop ~coefficients (Kcb_jan...Kcb_dec) for" &
          //" landuse "//asCharacter( LANDUSE_CODE( iIndex ) ), lFatal=lTRUE )

      if ( KCB_METHOD( iIndex ) == KCB_METHOD_FAO56 ) then
        if ( .not. ( GROWTH_STAGE_DATE( PLANTING_DATE, iIndex ) > SIM_DT%start ) )  &

        call warn("Cannot determine planting date for FAO-56 soil moisture procedure for " &
          //"landuse "//asCharacter( LANDUSE_CODE( iIndex) ), lFatal=lTRUE )

      endif

    enddo

!     do iIndex = lbound( fSoilStorage, 1 ), ubound( fSoilStorage,1 )

!       fKcb_initial = update_crop_coefficient_date_as_threshold( iLanduseIndex( iIndex ) )
!       fRz_initial = calc_effective_root_depth( iLanduseIndex=iLanduseIndex( iIndex ),                &
!                                                fZr_max=fMax_Rooting_Depths( iLanduseIndex( iIndex ), &
!                                                                             iSoilGroup( iIndex ) ),  &
!                                                fKCB=fKcb_initial )
!       fSoilStorage( iIndex ) = INITIAL_PERCENT_SOIL_MOISTURE( iIndex ) / 100.0_c_float    &
!                                * fRz_initial * fAvailable_Water_Content( iIndex )

!     enddo

  !> @TODO Add more logic here to perform checks on the validity of this data.

  !> @TODO Need to handle missing values. WHat do we do if an entire column of values
  !!       is missing?

  end subroutine crop_coefficients_FAO56_initialize

!------------------------------------------------------------------------------

 !> Update the current basal crop coefficient (Kcb) for
 !! a SINGLE irrigation table entry
 !!
 !! @param[inout] pIRRIGATION pointer to a single line of information in the irrigation file.
 !! @param[in] iThreshold either the current day of year or the number of growing degree days.
 !! @retval rKcb Basal crop coefficient given the irrigation table entries and the 
 !!         current threshold values.

 function update_crop_coefficient_date_as_threshold( iLanduseIndex )           & 
                                                                          result(fKcb)

  integer (kind=c_int), intent(in)   :: iLanduseIndex
  real (kind=c_float)                :: fKcb

  ! [ LOCALS ]
  real (kind=c_float) :: fFrac

  if ( KCB_METHOD( iLanduseIndex ) == KCB_METHOD_MONTHLY_VALUES ) then

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

 function update_crop_coefficient_GDD_as_threshold( iLanduseIndex, fGDD )   &
                                                                         result(fKcb)

  integer (kind=c_int), intent(in)   :: iLanduseIndex
  real (kind=c_float), intent(in)    :: fGDD
  real (kind=c_float)                :: fKcb

  ! [ LOCALS ]
  real (kind=c_float) :: fFrac

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

end function update_crop_coefficient_GDD_as_threshold

!------------------------------------------------------------------------------

subroutine crop_coefficients_FAO56_update_growth_stage_dates( )

  ! [ LOCALS ]
  integer (kind=c_int) :: iIndex
  integer (kind=c_int) :: iNextPlantingYear

  do iIndex=lbound(GROWTH_STAGE_DATE,2), ubound(GROWTH_STAGE_DATE,2) 

!     print *, SIM_DT%curr%prettydate(), " | ", GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex )%prettydate()

    if ( SIM_DT%curr <= GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex ) ) cycle 

    if ( KCB_METHOD( iIndex ) /= KCB_METHOD_FAO56 ) cycle

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


end subroutine crop_coefficients_FAO56_update_growth_stage_dates


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

function calc_effective_root_depth( iLanduseIndex, fZr_max, fKCB )  result(fZr_i)

  integer (kind=c_int), intent(in)    :: iLanduseIndex 
  real (kind=c_float), intent(in)     :: fZr_max
  real (kind=c_float), intent(in)     :: fKCB

  ! [ RESULT ]
  real (kind=c_float) :: fZr_i

  ! [ LOCALS ]
  ! 0.3048 feet equals 0.1 meters, which is seems to be the standard
  ! initial rooting depth in the FAO-56 methodology
  real (kind=c_float), parameter :: fZr_min = 0.3048
  real (kind=c_float)            :: fMaxKCB
  real (kind=c_float)            :: fMinKCB

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
!   if ( ( fMaxKCB - fMinKCB ) < 0.1_c_float ) then

!     fZr_i = fZr_max

!   elseif ( fMaxKCB > 0.0_C_float ) then

!     fZr_i = fZr_min + (fZr_max - fZr_min) * fKCB / fMaxKCB 

!   else

!     fZr_i = fZr_min

!   endif

  fZr_i = fZr_max

end function calc_effective_root_depth

!--------------------------------------------------------------------------------------------------

  subroutine crop_coefficients_FAO56_calculate( fSoilStorage,                                 &
    fSoilStorage_Excess, fInfiltration, fGDD, fAvailableWaterCapacity, fReference_ET0,        &
    fMaxRootingDepth, iLanduseIndex, iSoilGroup )

    real (kind=c_float), intent(inout)   :: fSoilStorage
    real (kind=c_float), intent(inout)   :: fSoilStorage_Excess
    real (kind=c_float), intent(inout)   :: fInfiltration
    real (kind=c_float), intent(inout)   :: fGDD
    real (kind=c_float), intent(inout)   :: fAvailableWaterCapacity
    real (kind=c_float), intent(inout)   :: fReference_ET0
    real (kind=c_float), intent(in)      :: fMaxRootingDepth
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

!     if (iLanduseIndex == 4) then
!       print *, "FAO56: ", fSoilStorage,                                 &
!         fSoilStorage_Excess, fInfiltration, fGDD, fAvailableWaterCapacity, fReference_ET0,    &
!         fMaxRootingDepth, iLanduseIndex, iSoilGroup
!     endif

    if ( KCB_METHOD( iLanduseIndex )  == KCB_METHOD_FAO56  &
      .or. KCB_METHOD( iLanduseIndex ) == KCB_METHOD_MONTHLY_VALUES ) then

      fKcb = update_crop_coefficient_date_as_threshold( iLanduseIndex )

    else

    !  fKcb = sm_FAO56_UpdateCropCoefficient( iLanduseIndex, INT(fGDD, kind=c_int), asInt(SIM_DT%curr%iMonth)  )

    !					 cel%rSoilWaterCap = cel%rCurrentRootingDepth * cel%rSoilWaterCapInput

    endif

    ! change from earlier coding: rooting depth is now simply keyed into the current Kcb
    fZr = calc_effective_root_depth( iLanduseIndex, fMaxRootingDepth, fKcb )

    fSoilStorage_Max = fAvailableWaterCapacity * fMaxRootingDepth

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
    ! deficit: defined in terms relative to the *current* rooting depth,
    !          in other words, relative to the maximum amount of water the
    !          roots in their current growth state could access
    fDeficit = max( 0.0_c_float, fSoilStorage_Max - fSoilStorage )

    ! "STANDARD" vs "NONSTANDARD": in the FAO56 publication the term
    ! "STANDARD" is used to refer to crop ET requirements under
    ! ideal conditions (i.e. plants not stressed due to scarcity
    ! of water. "NONSTANDARD" is the term used to describe ET requirements
    ! when plants are under stress, when water is scarce.

    ! we are using the full FAO56 soil water balance approach, *INCLUDING*
    ! the adjustments for nonstandard growing conditions (e.g. plant
    ! stress and resulting decrease in ET during dry conditions).

    fKr = calc_evaporation_reduction_coefficient(fTotalEvaporableWater=fTEW,    &
                                               fReadilyEvaporableWater=fREW,   &
                                               fDeficit=fDeficit )

    f_few = calc_fraction_wetted_and_exposed_soil( iLanduseIndex, fKcb )

    fKe = min(calc_surface_evaporation_coefficient( iLanduseIndex, fKr, fKcb ),   &
              f_few * KCB( KCB_MID, iLanduseIndex) )

    fKs = calc_water_stress_coefficient_Ks( iLanduseIndex=iLanduseIndex,   &
                                            fDeficit=fDeficit,             &
                                            fTotalAvailableWater=fTAW,     &
                                            fReadilyAvailableWater=fRAW,   &
                                            fReference_ET0=fReference_ET0 )

    
    fBareSoilEvap = fReference_ET0 * fKe
    fCropETc = fReference_ET0 * (fKcb * fKs)

    fSoilStorage = fSoilStorage + fInfiltration - fCropETc - fBareSoilEvap

    if ( fSoilStorage > fSoilStorage_Max ) then

      fSoilStorage_Excess = fSoilStorage - fSoilStorage_Max
      fSoilStorage = fSoilStorage_Max

    elseif ( fSoilStorage < fZERO ) then 
    
      fSoilStorage = fZERO
      fSoilStorage_Excess = fZERO

    else
    
      fSoilStorage_Excess = fZERO

    endif   
    
  end subroutine crop_coefficients_FAO56_calculate

end module crop_coefficients__FAO_56
