!> @file
!>  Contains a single module, \ref crop_coefficients__fao56, which
!>  provides support for modifying reference ET through the use of
!>  crop coefficients

!> Update crop coefficients for crop types in simulation.

module crop_coefficients__fao56

  use iso_c_binding, only             : c_bool, c_short, c_int, c_float, c_double
  use constants_and_conversions, only : M_PER_FOOT, TRUE, FALSE, fTINYVAL,       &
                                        iTINYVAL, asInt, asFloat, fZERO, in_to_mm, &
                                        TRUE, FALSE, clip
  use data_catalog, only              : DAT
  use data_catalog_entry, only        : DATA_CATALOG_ENTRY_T
  use datetime
  use logfiles, only                  : LOGS, LOG_ALL
  use exceptions, only                : assert, warn, die
  use parameters, only                : PARAMS
  use simulation_datetime, only       : SIM_DT
  use fstring, only                   : asCharacter, sQuote, operator(.contains.)
  use fstring_list
  implicit none

  private

  public :: crop_coefficients_FAO56_initialize, crop_coefficients_FAO56_calculate
  public :: crop_coefficients_FAO56_update_growth_stage_dates
  public :: crop_coefficients_FAO56_update_growing_season
  public :: crop_coefficients_FAO56_calculate_Kcb_Max
  public :: update_crop_coefficient_date_as_threshold, update_crop_coefficient_GDD_as_threshold
  public :: GROWTH_STAGE_DATE, PLANTING_DATE, GROWTH_STAGE_LENGTH_IN_DAYS
  public :: KCB_MIN, KCB_INI, KCB_MID, KCB_END
  public :: KCB_l, JAN, DEC, KCB_METHOD, KCB_METHOD_GDD, KCB_METHOD_FAO56
  public :: KCB_METHOD_MONTHLY_VALUES

  enum, bind(c)
    enumerator :: L_DOY_INI=1, L_DOY_DEV, L_DOY_MID, L_DOY_LATE, L_DOY_FALLOW
  end enum

  enum, bind(c)
    enumerator :: GDD_PLANT=1, GDD_INI, GDD_DEV, GDD_MID, GDD_LATE
  end enum

  enum, bind(c)
    enumerator :: PLANTING_DATE=1, ENDDATE_INI, ENDDATE_DEV, ENDDATE_MID, ENDDATE_LATE, &
                    ENDDATE_FALLOW
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
  integer (c_int), allocatable  :: LANDUSE_CODE(:)
!  real (c_float), allocatable   :: REW(:,:)
!  real (c_float), allocatable   :: TEW(:,:)
  real (c_float), allocatable   :: KCB_l(:,:)
  integer (c_int), allocatable  :: KCB_METHOD(:)
  real (c_float), allocatable   :: GROWTH_STAGE_SHIFT_DAYS(:)
  real (c_float), allocatable   :: GROWTH_STAGE_LENGTH_IN_DAYS(:,:)
  real (c_float), allocatable   :: GROWTH_STAGE_GDD(:,:)
  type (DATETIME_T), allocatable     :: GROWTH_STAGE_DATE(:,:)

  integer (c_int)               :: LU_SOILS_CSV

contains

  subroutine crop_coefficients_FAO56_initialize()

    ! [ LOCALS ]
    ! type (FSTRING_LIST_T)            :: slREW, slTEW
    type (FSTRING_LIST_T)             :: slList
    type (DATETIME_T)                 :: DT
    type (DATETIME_T)                 :: temp_date
    ! integer (c_int), allocatable :: iTEWSeqNums(:)
    ! integer (c_int), allocatable :: iREWSeqNums(:)
    integer (c_int)                   :: iNumberOfTEW, iNumberOfREW
    integer (c_int)                   :: iNumberOfLanduses
    integer (c_int)                   :: iIndex, iIndex2
    integer (c_int)                   :: iStat
    real (c_float)                    :: growing_cycle_length

    character (len=10)                :: sMMDDYYYY
    character (len=:), allocatable    :: sText

    type (FSTRING_LIST_T)             :: slPlantingDate
    type (DATETIME_T)                 :: dtPlantingDate
    character (len=:), allocatable    :: PlantingDate_str

    real (c_float), allocatable       :: L_shift_days_l(:)

    real (c_float), allocatable       :: L_ini_l(:)
    real (c_float), allocatable       :: L_dev_l(:)
    real (c_float), allocatable       :: L_mid_l(:)
    real (c_float), allocatable       :: L_late_l(:)
    real (c_float), allocatable       :: L_fallow_l(:)

    real (c_float), allocatable       :: GDD_plant_l(:)
    real (c_float), allocatable       :: GDD_ini_l(:)
    real (c_float), allocatable       :: GDD_dev_l(:)
    real (c_float), allocatable       :: GDD_mid_l(:)
    real (c_float), allocatable       :: GDD_late_l(:)

    real (c_float), allocatable       :: Kcb_MAX(:)

    real (c_float), allocatable       :: Kcb_ini_l(:)
    real (c_float), allocatable       :: Kcb_mid_l(:)
    real (c_float), allocatable       :: Kcb_end_l(:)
    real (c_float), allocatable       :: Kcb_min_l(:)

    real (c_float), allocatable       :: Kcb_jan(:)
    real (c_float), allocatable       :: Kcb_feb(:)
    real (c_float), allocatable       :: Kcb_mar(:)
    real (c_float), allocatable       :: Kcb_apr(:)
    real (c_float), allocatable       :: Kcb_may(:)
    real (c_float), allocatable       :: Kcb_jun(:)
    real (c_float), allocatable       :: Kcb_jul(:)
    real (c_float), allocatable       :: Kcb_aug(:)
    real (c_float), allocatable       :: Kcb_sep(:)
    real (c_float), allocatable       :: Kcb_oct(:)
    real (c_float), allocatable       :: Kcb_nov(:)
    real (c_float), allocatable       :: Kcb_dec(:)

    real (c_float)                    :: fKcb_initial
    real (c_float)                    :: fRz_initial

    real (c_float), parameter         :: NEAR_ZERO = 1.0e-9_c_float

    type (DATA_CATALOG_ENTRY_T), pointer :: pINITIAL_PERCENT_SOIL_MOISTURE

   !> create string list that allows for alternate heading identifiers for the landuse code
   slList = create_list("LU_Code, Landuse_Code, Landuse_Lookup_Code")

   !> Determine how many landuse codes are present
   call PARAMS%get_parameters( slKeys=slList, iValues=LANDUSE_CODE )
   iNumberOfLanduses = count( LANDUSE_CODE >= 0 )
   !> @todo Implement thorough input error checking:
   !! are all soils in grid included in table values?
   !> is soil suffix vector continuous?

   ! Retrieve and populate the Readily Evaporable Water (REW) table values
  !  CALL PARAMS%get_parameters( fValues=REW, sPrefix="REW_", iNumRows=iNumberOfLanduses )

   ! Retrieve and populate the Total Evaporable Water (TEW) table values
  !  CALL PARAMS%get_parameters( fValues=TEW, sPrefix="TEW_", iNumRows=iNumberOfLanduses )

   !> @TODO What should happen if the TEW / REW header entries do *not* fall in a
   !!       logical sequence of values? In other words, if the user has columns named
   !!       REW_1, REW_3, REW_5, only the values associated with "REW_1" would be retrieved.
   !!       Needless to say, this would be catastrophic.

   call PARAMS%get_parameters( sKey="Planting_date", slValues=slPlantingDate )

   call PARAMS%get_parameters( sKey="L_shift", fValues=L_shift_days_l)
   call PARAMS%get_parameters( sKey="L_ini", fValues=L_ini_l)
   call PARAMS%get_parameters( sKey="L_dev", fValues=L_dev_l)
   call PARAMS%get_parameters( sKey="L_mid", fValues=L_mid_l)
   call PARAMS%get_parameters( sKey="L_late", fValues=L_late_l)
   call PARAMS%get_parameters( sKey="L_fallow", fValues=L_fallow_l)

   call PARAMS%get_parameters( sKey="GDD_plant", fValues=GDD_plant_l)
   call PARAMS%get_parameters( sKey="GDD_ini", fValues=GDD_ini_l)
   call PARAMS%get_parameters( sKey="GDD_dev", fValues=GDD_dev_l)
   call PARAMS%get_parameters( sKey="GDD_mid", fValues=GDD_mid_l)
   call PARAMS%get_parameters( sKey="GDD_late", fValues=GDD_late_l)

   call PARAMS%get_parameters( sKey="Kcb_ini", fValues=KCB_ini_l)
   call PARAMS%get_parameters( sKey="Kcb_mid", fValues=KCB_mid_l)
   call PARAMS%get_parameters( sKey="Kcb_end", fValues=KCB_end_l)
   call PARAMS%get_parameters( sKey="Kcb_min", fValues=KCB_min_l)

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


    allocate( GROWTH_STAGE_LENGTH_IN_DAYS( 5, iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory for GROWTH_STAGE_LENGTH_IN_DAYS array", &
      __FILE__, __LINE__ )

    allocate( GROWTH_STAGE_GDD( 5, iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory for GROWTH_STAGE_GDD array", &
      __FILE__, __LINE__ )

    allocate( GROWTH_STAGE_DATE( 6, iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory for GROWTH_STAGE_DATE array", &
      __FILE__, __LINE__ )

    allocate( GROWTH_STAGE_SHIFT_DAYS( iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory for GROWTH_STAGE_SHIFT_DAYS array", &
      __FILE__, __LINE__ )

    allocate( KCB_l( 16, iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory for KCB_l array", &
      __FILE__, __LINE__ )

    allocate( KCB_METHOD( iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory for KCB_METHOD vector", &
      __FILE__, __LINE__ )

    KCB_METHOD = -9999
    KCB_l = -9999.
    GROWTH_STAGE_GDD = -9999.
    GROWTH_STAGE_LENGTH_IN_DAYS = 0.
    GROWTH_STAGE_SHIFT_DAYS = 0.0_c_float

    if ( ubound(L_shift_days_l,1) == iNumberOfLanduses )    &
      GROWTH_STAGE_SHIFT_DAYS = L_shift_days_l

    if ( ubound(L_ini_l,1) == iNumberOfLanduses )           &
      GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_INI,  : ) = L_ini_l

    if ( ubound(L_dev_l,1) == iNumberOfLanduses )           &
      GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_DEV,  : ) = L_dev_l

    if ( ubound(L_mid_l,1) == iNumberOfLanduses )           &
      GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_MID,  : ) = L_mid_l

    if ( ubound(L_late_l,1) == iNumberOfLanduses )          &
      GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_LATE, : ) = L_late_l

    if ( ubound(L_fallow_l,1) == iNumberOfLanduses )        &
      GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_FALLOW, : ) = L_fallow_l

    call LOGS%write(" ## Crop Kcb Curve Summary ##", iLinesAfter=1)
    call LOGS%write(" _only meaningful for landuses where the Kcb curve is defined " &
      //"in terms of days _", iLinesAfter=1)
    call LOGS%write("Landuse Code | Planting Date | End of 'ini' | End of 'dev' " &
      //"| End of 'mid' | End of 'late' | End of 'fallow' ")
    call Logs%write("-------------|---------------|--------------|--------------" &
      //"|--------------|---------------|-----------------")

    if ( slPlantingDate%count == iNumberOfLanduses .and. slPlantingDate%count > 0 ) then

      do iIndex=1, slPlantingDate%count

        PlantingDate_str = slPlantingDate%get( iIndex )

        ! if there is no planting date entry, assume user is specifying
        ! a planting GDD for this landuse code
        if ( len_trim(PlantingDate_str) == 0 ) cycle

        if ( PlantingDate_str .contains. "/" ) then

          ! append current year to the end of the user-entered planting date in mm/dd
          sMMDDYYYY = trim(PlantingDate_str)//"/"//asCharacter( SIM_DT%start%iYear )
          call GROWTH_STAGE_DATE( PLANTING_DATE, iIndex)%parsedate( sMMDDYYYY, __FILE__, __LINE__ )

          GROWTH_STAGE_DATE( PLANTING_DATE, iIndex) = GROWTH_STAGE_DATE( PLANTING_DATE, iIndex) &
                                                      + GROWTH_STAGE_SHIFT_DAYS( iIndex )

        else
          ! assume the value is a day-of-year value
          dtPlantingDate = SIM_DT%start + asFloat( PlantingDate_str) + GROWTH_STAGE_SHIFT_DAYS( iIndex )
          call dtPlantingDate%calcJulianDay()
          GROWTH_STAGE_DATE( PLANTING_DATE, iIndex) = dtPlantingDate
        endif

        ! march forward through time calculating the various dates on the Kcb curve
        ! GROWTH_STAGE_DATE( ENDDATE_INI, iIndex ) = GROWTH_STAGE_DATE( PLANTING_DATE, iIndex ) + L_ini_l( iIndex )
        ! GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_INI, iIndex ) + L_dev_l( iIndex )
        ! GROWTH_STAGE_DATE( ENDDATE_MID, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex ) + L_mid_l( iIndex )
        ! GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_MID, iIndex ) + L_late_l( iIndex )
        ! GROWTH_STAGE_DATE( ENDDATE_FALLOW, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex ) + L_fallow_l( iIndex )

        ! if any of the L_* length values is missing, a value of zero will be used, resulting in a wierd looking Kcb curve
        GROWTH_STAGE_DATE( ENDDATE_INI, iIndex ) = GROWTH_STAGE_DATE( PLANTING_DATE, iIndex )                        &
                                                   + GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_INI, iIndex )
        GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_INI, iIndex )                          &
                                                   + GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_DEV, iIndex )
        GROWTH_STAGE_DATE( ENDDATE_MID, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex )                          &
                                                   + GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_MID, iIndex )
        GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_MID, iIndex )                         &
                                                   + GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_LATE, iIndex )
        GROWTH_STAGE_DATE( ENDDATE_FALLOW, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex )                      &
                                                     + GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_FALLOW, iIndex )

        call LOGS%write( "| "//asCharacter( LANDUSE_CODE( iIndex ))//" | "                                    &
           //trim( GROWTH_STAGE_DATE( PLANTING_DATE, iIndex )%prettydate() )                                  &
             //" (doy:"//asCharacter( GROWTH_STAGE_DATE( PLANTING_DATE, iIndex )%getDayOfYear() )//") | "     &
           //trim( GROWTH_STAGE_DATE( ENDDATE_INI, iIndex )%prettydate() )//" | "                             &
             //" (doy:"//asCharacter( GROWTH_STAGE_DATE( ENDDATE_INI, iIndex )%getDayOfYear() )//") | "       &
           //trim( GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex )%prettydate() )//" | "                             &
             //" (doy:"//asCharacter( GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex )%getDayOfYear() )//") | "       &
           //trim( GROWTH_STAGE_DATE( ENDDATE_MID, iIndex )%prettydate() )//" | "                             &
             //" (doy:"//asCharacter( GROWTH_STAGE_DATE( ENDDATE_MID, iIndex )%getDayOfYear() )//") | "       &
           //trim( GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex )%prettydate() )//" | "                            &
             //" (doy:"//asCharacter( GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex )%getDayOfYear() )//") | "      &
           //trim( GROWTH_STAGE_DATE( ENDDATE_FALLOW, iIndex )%prettydate() )                                 &
             //" (doy:"//asCharacter( GROWTH_STAGE_DATE( ENDDATE_FALLOW, iIndex )%getDayOfYear() )//") | ")
      enddo

    endif

    if (ubound(GDD_plant_l,1) == iNumberOfLanduses)  GROWTH_STAGE_GDD( GDD_PLANT,  : ) = GDD_plant_l
    if (ubound(GDD_ini_l,1) == iNumberOfLanduses)    GROWTH_STAGE_GDD( GDD_INI,  : ) = GDD_ini_l
    if (ubound(GDD_dev_l,1) == iNumberOfLanduses)    GROWTH_STAGE_GDD( GDD_DEV,  : ) = GDD_dev_l
    if (ubound(GDD_mid_l,1) == iNumberOfLanduses)    GROWTH_STAGE_GDD( GDD_MID,  : ) = GDD_mid_l
    if (ubound(GDD_late_l,1) == iNumberOfLanduses)   GROWTH_STAGE_GDD( GDD_LATE, : ) = GDD_late_l

    if (ubound(KCB_ini_l,1) == iNumberOfLanduses)  KCB_l( KCB_INI, :) = KCB_ini_l
    if (ubound(KCB_mid_l,1) == iNumberOfLanduses)  KCB_l( KCB_MID, :) = KCB_mid_l
    if (ubound(KCB_end_l,1) == iNumberOfLanduses)  KCB_l( KCB_END, :) = KCB_end_l
    if (ubound(KCB_min_l,1) == iNumberOfLanduses)  KCB_l( KCB_MIN, :) = KCB_min_l

    if (ubound(KCB_jan,1) == iNumberOfLanduses)   KCB_l( JAN, :) = KCB_jan
    if (ubound(KCB_feb,1) == iNumberOfLanduses)   KCB_l( FEB, :) = KCB_feb
    if (ubound(KCB_mar,1) == iNumberOfLanduses)   KCB_l( MAR, :) = KCB_mar
    if (ubound(KCB_apr,1) == iNumberOfLanduses)   KCB_l( APR, :) = KCB_apr
    if (ubound(KCB_may,1) == iNumberOfLanduses)   KCB_l( MAY, :) = KCB_may
    if (ubound(KCB_jun,1) == iNumberOfLanduses)   KCB_l( JUN, :) = KCB_jun
    if (ubound(KCB_jul,1) == iNumberOfLanduses)   KCB_l( JUL, :) = KCB_jul
    if (ubound(KCB_aug,1) == iNumberOfLanduses)   KCB_l( AUG, :) = KCB_aug
    if (ubound(KCB_sep,1) == iNumberOfLanduses)   KCB_l( SEP, :) = KCB_sep
    if (ubound(KCB_oct,1) == iNumberOfLanduses)   KCB_l( OCT, :) = KCB_oct
    if (ubound(KCB_nov,1) == iNumberOfLanduses)   KCB_l( NOV, :) = KCB_nov
    if (ubound(KCB_dec,1) == iNumberOfLanduses)   KCB_l( DEC, :) = KCB_dec

    ! go through the table values and try to figure out how Kcb curves should be constructed:
    ! Monthly Kcb, GDD-based, or DOY-based
    do iIndex = lbound( KCB_METHOD, 1), ubound( KCB_METHOD, 1)

      if ( all( KCB_l( JAN:DEC, iIndex ) > 0.0_c_float ) ) then
        KCB_METHOD( iIndex ) = KCB_METHOD_MONTHLY_VALUES
        KCB_l( KCB_MIN, iIndex ) = minval( KCB_l(JAN:DEC, iIndex) )
        KCB_l( KCB_MID, iIndex) = minval( KCB_l(JAN:DEC, iIndex) )

      elseif ( all( GROWTH_STAGE_GDD( :, iIndex ) >= 0.0_c_float )              &
         .and. all( KCB_l( KCB_INI:KCB_MIN, iIndex ) > 0.0_c_float ) ) then
        KCB_METHOD( iIndex ) = KCB_METHOD_GDD

      elseif ( all( GROWTH_STAGE_LENGTH_IN_DAYS( PLANTING_DATE:, iIndex ) >= 0.0_c_float )              &
         .and. all( KCB_l( KCB_INI:KCB_MIN, iIndex ) > 0.0_c_float ) ) then
        KCB_METHOD( iIndex ) = KCB_METHOD_FAO56
      endif

      if ( KCB_METHOD( iIndex ) < 0 ) then
        call warn("There are missing day-of-year (L_ini, L_dev, L_mid, L_late, L_fallow), " &
          //"growing degree-day ~(GDD_plant, GDD_ini, GDD_dev, GDD_mid, GDD_late)," &
          //" or monthly crop ~coefficients (Kcb_jan...Kcb_dec) for" &
          //" landuse "//asCharacter( LANDUSE_CODE( iIndex ) ), lFatal=TRUE )
      endif

    enddo

!    do iIndex = lbound( fSoilStorage, 1 ), ubound( fSoilStorage,1 )

!      fKcb_initial = update_crop_coefficient_date_as_threshold( iLanduseIndex( iIndex ) )

      ! call calc_effective_root_depth( fRz_i=fRz_initial, iLanduseIndex=iLanduseIndex( iIndex ),    &
      !                                 fZr_max=fMax_Rooting_Depths( iLanduseIndex( iIndex ),        &
      !                                 iSoilGroup( iIndex ) ),                                      &
      !                                 Kcb=fKcb_initial )
      !
      ! fSoilStorage( iIndex ) = INITIAL_PERCENT_SOIL_MOISTURE( iIndex ) / 100.0_c_float             &
      !                          * fRz_initial * fAvailable_Water_Content( iIndex )

!    enddo

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

 impure elemental function update_crop_coefficient_date_as_threshold( iLanduseIndex )           &
                                                                          result(Kcb)

  integer (c_int), intent(in)   :: iLanduseIndex
  real (c_float)                :: Kcb

  ! [ LOCALS ]
  real (c_double) :: fFrac

  if ( KCB_METHOD( iLanduseIndex ) == KCB_METHOD_MONTHLY_VALUES ) then

    Kcb = KCB_l( SIM_DT%curr%iMonth, iLanduseIndex )

  else

    ! define shorthand variable names for remainder of function
    associate ( Date_ini => GROWTH_STAGE_DATE( ENDDATE_INI, iLanduseIndex ),           &
                Date_dev => GROWTH_STAGE_DATE( ENDDATE_DEV, iLanduseIndex ),           &
                Date_mid => GROWTH_STAGE_DATE( ENDDATE_MID, iLanduseIndex ),           &
                Date_late => GROWTH_STAGE_DATE( ENDDATE_LATE, iLanduseIndex ),         &
                Date_fallow => GROWTH_STAGE_DATE( ENDDATE_FALLOW, iLanduseIndex ),     &
                Kcb_ini => KCB_l(KCB_INI, iLanduseIndex),                              &
                Kcb_mid => KCB_l(KCB_MID, iLanduseIndex),                              &
                Kcb_min => KCB_l(KCB_MIN, iLanduseIndex),                              &
                PlantingDate => GROWTH_STAGE_DATE( PLANTING_DATE, iLanduseIndex),      &
                Kcb_end => KCB_l(KCB_END, iLanduseIndex),                              &
                current_date => SIM_DT%curr )

      ! now calculate Kcb for the given landuse

      if( current_date > Date_late ) then

        Kcb = Kcb_min

      elseif ( current_date > Date_mid ) then

        fFrac = ( current_date - Date_mid ) / ( Date_late - Date_mid )

        Kcb =  Kcb_mid * (1.0_c_double - fFrac) + Kcb_end * fFrac

      elseif ( current_date > Date_dev ) then

        Kcb = Kcb_mid

      elseif ( current_date > Date_ini ) then

        fFrac = ( current_date - Date_ini ) / ( Date_dev - Date_ini )

        Kcb = Kcb_ini * (1.0_c_double - fFrac) + Kcb_mid * fFrac

      elseif ( current_date >= PlantingDate ) then

        Kcb = Kcb_ini

      else

        Kcb = Kcb_min

      endif

    end associate

  end if

end function update_crop_coefficient_date_as_threshold

!------------------------------------------------------------------------------

pure elemental function crop_coefficients_FAO56_calculate_Kcb_Max(wind_speed_meters_per_sec,   &
                                          relative_humidity_min_pct,   &
                                          Kcb,                         & 
                                          plant_height_meters)                       result(kcb_max)

  real (c_float), intent(in) :: wind_speed_meters_per_sec
  real (c_float), intent(in) :: relative_humidity_min_pct
  real (c_float), intent(in) :: Kcb
  real (c_float), intent(in) :: plant_height_meters

  real (c_float)  :: kcb_max
  real (c_double) :: U2
  real (c_double) :: RHmin
  real (c_double) :: plant_height

  ! Limits are as suggested on page 123 of FAO-56 with respect to
  ! modifying mid-season KCB_mid values 
  RHmin = clip( relative_humidity_min_pct, minval=20., maxval=80. )
  U2 = clip(wind_speed_meters_per_sec, minval=1., maxval=6.)
  plant_height = clip(plant_height_meters, minval=1., maxval=10.)

  ! equation 72, FAO-56, p 199
  kcb_max = max(  1.2_c_double + ( (0.04_c_double * (U2 - 2._c_double)               &
                                  - 0.004_c_double * (RHmin - 45._c_double) ) )      &
                                  * (plant_height_meters/3._c_double)**0.3_c_double, &
                  Kcb + 0.05_c_double )

end function crop_coefficients_FAO56_calculate_Kcb_Max

!------------------------------------------------------------------------------

 !> Update the current basal crop coefficient (Kcb), with GDD as the threhold
 !!
 !! @param[in] fGDD current growing degree day value associated with the cell.
 !! @retval fKcb Basal crop coefficient given the irrigation table entries and the
 !!         current threshold values.

 impure elemental function update_crop_coefficient_GDD_as_threshold( iLanduseIndex, fGDD )   &
                                                                         result(fKcb)

  integer (c_int), intent(in)   :: iLanduseIndex
  real (c_float), intent(in)    :: fGDD
  real (c_float)                :: fKcb

  ! [ LOCALS ]
  real (c_double) :: fFrac

  ! define shorthand variable names for remainder of function
  associate ( GDD_ini_l => GROWTH_STAGE_GDD( GDD_INI, iLanduseIndex ),          &
              GDD_dev_l => GROWTH_STAGE_GDD( GDD_DEV, iLanduseIndex ),          &
              GDD_mid_l => GROWTH_STAGE_GDD( GDD_MID, iLanduseIndex ),          &
              GDD_late_l => GROWTH_STAGE_GDD( GDD_LATE, iLanduseIndex ),        &
              Kcb_ini => KCB_l(KCB_INI, iLanduseIndex),                         &
              Kcb_mid => KCB_l(KCB_MID, iLanduseIndex),                         &
              Kcb_min => KCB_l(KCB_MIN, iLanduseIndex),                         &
              PlantingDOY => GROWTH_STAGE_DATE( PLANTING_DATE,                  &
                                                iLanduseIndex)%getDayOfYear(),  &
              GDD_plant_l => GROWTH_STAGE_GDD( GDD_PLANT, iLanduseIndex),       &
              Kcb_end => KCB_l(KCB_END, iLanduseIndex),                         &
              current_doy => SIM_DT%curr%getDayOfYear() )

    ! now calculate Kcb for the given landuse
    if( fGDD > GDD_late_l ) then

      fKcb = Kcb_min

    elseif ( fGDD > GDD_mid_l ) then

      fFrac = ( fGDD - GDD_mid_l ) / ( GDD_late_l - GDD_mid_l )

      fKcb =  Kcb_mid * (1.0_c_double - fFrac) + Kcb_end * fFrac

    elseif ( fGDD > GDD_dev_l ) then

      fKcb = Kcb_mid

    elseif ( fGDD > GDD_ini_l ) then

      fFrac = ( fGDD - GDD_ini_l ) / ( GDD_dev_l - GDD_ini_l )

      fKcb = Kcb_ini * (1_c_double - fFrac) + Kcb_mid * fFrac

    elseif ( (PlantingDOY > 0) .and. (current_DOY >= PlantingDOY) ) then

      ! if there is a valid value for the Planting Date, use it
      fKcb = Kcb_ini

    elseif ( (GDD_plant_l >= 0.) .and. (fGDD >= GDD_plant_l) ) then

        fKcb = Kcb_ini

    else

      fKcb = Kcb_min

    endif

  end associate

end function update_crop_coefficient_GDD_as_threshold

!------------------------------------------------------------------------------

  subroutine crop_coefficients_FAO56_update_growth_stage_dates( )

    ! [ LOCALS ]
    integer (c_int) :: iIndex
    real (c_double) :: dTempDate
    type (DATETIME_T)    :: dtTempDate
    real (c_float)  :: growing_cycle_length

    do iIndex=lbound(GROWTH_STAGE_DATE,2), ubound(GROWTH_STAGE_DATE,2)

  !     print *, SIM_DT%curr%prettydate(), " | ", GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex )%prettydate()

      ! if we have not yet reached the enddate associated with the fallow period, skip
      if ( SIM_DT%curr < GROWTH_STAGE_DATE( ENDDATE_FALLOW, iIndex ) ) cycle

      if ( KCB_METHOD( iIndex ) /= KCB_METHOD_FAO56 ) cycle

      ! current date is beyond the enddate associated with fallow period;
      ! update Kcb curve and dates

!      print *, "a) ",SIM_DT%curr%prettydate(), " | ", GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex )%prettydate()

      ! it's possible that the planting date might be later in the current calendar year
      call GROWTH_STAGE_DATE( PLANTING_DATE, iIndex )%setYear( SIM_DT%curr%iYear )

!      print *, "b) ",SIM_DT%curr%prettydate(), " | ", GROWTH_STAGE_DATE( PLANTING_DATE, iIndex )%prettydate()

      ! however, if we are already past that point in the year, planting date must be
      ! next calendar year
      if ( SIM_DT%iDOY > GROWTH_STAGE_DATE( PLANTING_DATE, iIndex )%getDayOfYear() )  &
        call GROWTH_STAGE_DATE( PLANTING_DATE, iIndex )%addYear()

!      print *, "c) ",SIM_DT%curr%prettydate(), " | ", GROWTH_STAGE_DATE( PLANTING_DATE, iIndex )%prettydate()

      ! now calculate dates associated with the rest of the Kcb curve
      GROWTH_STAGE_DATE( ENDDATE_INI, iIndex ) = GROWTH_STAGE_DATE( PLANTING_DATE, iIndex ) &
                                                + GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_INI, iIndex )
      GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_INI, iIndex )&
                                                + GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_DEV, iIndex )
      GROWTH_STAGE_DATE( ENDDATE_MID, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex )&
                                                + GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_MID, iIndex )
      GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_MID, iIndex )&
                                                + GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_LATE, iIndex )
      GROWTH_STAGE_DATE( ENDDATE_FALLOW, iIndex ) = GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex )&
                                                + GROWTH_STAGE_LENGTH_IN_DAYS( L_DOY_FALLOW, iIndex )

      call LOGS%write("## Updating Kcb Date Values ##", iLinesAfter=1, lEcho=FALSE )
      call LOGS%write("Landuse Code | Planting Date | End of 'ini' | End of 'dev' " &
        //"| End of 'mid' | End of 'late' | End of 'fallow' ", lEcho=FALSE )
      call Logs%write("-------------|---------------|--------------|--------------" &
        //"|--------------|---------------|-----------------", lEcho=FALSE )

      call LOGS%write( asCharacter( LANDUSE_CODE( iIndex ))//" | "                &
         //trim( GROWTH_STAGE_DATE( PLANTING_DATE, iIndex )%prettydate() )//" | " &
         //trim( GROWTH_STAGE_DATE( ENDDATE_INI, iIndex )%prettydate() )//" | "   &
         //trim( GROWTH_STAGE_DATE( ENDDATE_DEV, iIndex )%prettydate() )//" | "   &
         //trim( GROWTH_STAGE_DATE( ENDDATE_MID, iIndex )%prettydate() )//" | "   &
         //trim( GROWTH_STAGE_DATE( ENDDATE_LATE, iIndex )%prettydate() )//" | "  &
         //trim( GROWTH_STAGE_DATE( ENDDATE_FALLOW, iIndex )%prettydate() ),      &
         lEcho=FALSE, iLogLevel=LOG_ALL )

    enddo


  end subroutine crop_coefficients_FAO56_update_growth_stage_dates

!--------------------------------------------------------------------------------------------------

  impure elemental subroutine crop_coefficients_FAO56_calculate( Kcb, landuse_index, GDD )

    real (c_float), intent(inout)          :: Kcb
    integer (c_int), intent(in)            :: landuse_index
    real (c_float), intent(in), optional   :: GDD


    if ( KCB_METHOD( landuse_index )  == KCB_METHOD_FAO56  &
      .or. KCB_METHOD( landuse_index ) == KCB_METHOD_MONTHLY_VALUES ) then

      Kcb = update_crop_coefficient_date_as_threshold( landuse_index )

    else

      Kcb = update_crop_coefficient_GDD_as_threshold( landuse_index, GDD )

    endif

  end subroutine crop_coefficients_FAO56_calculate

!--------------------------------------------------------------------------------------------------

  impure elemental subroutine crop_coefficients_FAO56_update_growing_season(   &
                                           landuse_index,                      &
                                           Kcb,                                &
                                           it_is_growing_season)

    real (c_float), intent(in)             :: Kcb
    integer (c_int), intent(in)            :: landuse_index
    logical (c_bool), intent(out)          :: it_is_growing_season

    if ( Kcb > KCB_l( KCB_MIN, landuse_index) ) then
      it_is_growing_season = TRUE
    else
      it_is_growing_season = FALSE
    endif

  end subroutine crop_coefficients_FAO56_update_growing_season

end module crop_coefficients__fao56
