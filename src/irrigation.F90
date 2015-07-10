!> @file
!>  Contains a single module, \ref irrigation, which
!>  provides support for estimating irrigation amounts

!> Provides support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module irrigation

  use iso_c_binding, only          : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog, only           : DAT
  use data_catalog_entry, only     : DATA_CATALOG_ENTRY_T
  use datetime, only               : mmdd2doy
  use exceptions, only             : warn, die, assert
  use parameters, only             : PARAMS
  use simulation_datetime, only    : SIM_DT
  use strings, only                : asCharacter
  use string_list, only            : STRING_LIST_T

  implicit none

  private 

  public :: irrigation__initialize, irrigation__calculate


  real (kind=c_float), allocatable   :: MAXIMUM_ALLOWABLE_DEPLETION_FRACTION(:)
  real (kind=c_float), allocatable   :: IRRIGATION_FROM_GROUNDWATER(:)
  real (kind=c_float), allocatable   :: IRRIGATION_FROM_SURFACE_WATER(:) 

  real (kind=c_float), allocatable   :: FRACTION_OF_IRRIGATION_FROM_GW(:)   
  real (kind=c_float), allocatable   :: IRRIGATION_EFFICIENCY(:)
  integer (kind=c_int), allocatable  :: FIRST_DAY_OF_IRRIGATION(:)
  integer (kind=c_int), allocatable  :: LAST_DAY_OF_IRRIGATION(:)

  type (DATA_CATALOG_ENTRY_T), pointer :: pIRRIGATION_MASK
  real (kind=c_float), allocatable     :: IRRIGATION_MASK(:)

  type (DATA_CATALOG_ENTRY_T), pointer :: pPERVIOUS_SURFACE_FRACTION

contains

!> Estimate the irrigation water required to sustain plant growth.
!!
!! Estimate the irrigation water required in order to
!! keep soil moisture values above the maximum allowable depletion (MAD)
!! for each gridcell.
!!
!! @param[inout] pGrd Pointer to the model grid object
!! @param[in] pConfig Pointer to the configuration data structure (type T_CONFIG).
!!

  subroutine irrigation__initialize( lActive )

    logical (kind=c_bool), intent(in)  :: lActive(:,:)

    ! [ LOCALS ]
    type (STRING_LIST_T)              :: slList
    integer (kind=c_int)              :: iNumberOfLanduses
    integer (kind=c_int), allocatable :: iLanduseTableCodes(:)
    integer (kind=c_int)              :: iNumRecs
    logical (kind=c_bool)             :: lAreLengthsEqual
    integer (kind=c_int)              :: iIndex
    integer (kind=c_int)              :: iStat
    character (len=256)               :: sBuf
    type (STRING_LIST_T)              :: slIrrigationBegin
    type (STRING_LIST_T)              :: slIrrigationEnd  

    allocate( IRRIGATION_FROM_GROUNDWATER( count( lActive ) ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory.", __FILE__, __LINE__ )

    allocate( IRRIGATION_FROM_SURFACE_WATER( count( lActive ) ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory.", __FILE__, __LINE__ )

    allocate( IRRIGATION_MASK( count( lActive ) ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory.", __FILE__, __LINE__ )

    ! create list of possible table headings to look for...
    call slList%append( "LU_Code" )
    call slList%append( "Landuse_Lookup_Code" )

    !> Determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=slList, iValues=iLanduseTableCodes )
    iNumberOfLanduses = count( iLanduseTableCodes >= 0 )

    call slList%clear()
    call slList%append("Fraction_irrigation_from_GW")
    call slList%append("Frac_irr_fm_GW")
    call slList%append("Fraction_irrigation_from_groundwater")
    call slList%append("Frac_irrigation_from_GW")
    call slList%append("Fraction_of_irrigation_from_GW")
    call slList%append("Fraction_of_irrigation_from_groundwater")

    call PARAMS%get_parameters( slKeys=slList, fValues=FRACTION_OF_IRRIGATION_FROM_GW, lFatal=lTRUE )

    call slList%clear()
    call slList%append("Max_allowable_depletion")
    call slList%append("Maximum_allowable_depletion")
    call slList%append("MAD")

    call PARAMS%get_parameters( slKeys=slList,                                  &
                                fValues=MAXIMUM_ALLOWABLE_DEPLETION_FRACTION,   &
                                lFatal=lTRUE ) 

    call slList%clear()
    call slList%append("First_day_of_irrigation")
    call slList%append("First_DOY_irrigation")
    call slList%append("Irrigation_start")

    call PARAMS%get_parameters( slKeys=slList, slValues=slIrrigationBegin, lFatal=lTRUE ) 


    call slList%clear()
    call slList%append("Irrigation_efficiency")
    call slList%append("Irrigation_application_efficiency")

    call PARAMS%get_parameters( slKeys=slList,                                  &
                                fValues=IRRIGATION_EFFICIENCY,   &
                                lFatal=lTRUE ) 

    iNumRecs = ubound(IRRIGATION_EFFICIENCY,1)
    lAreLengthsEqual = ( iNumRecs == iNumberOfLanduses ) 

    if ( .not. lAreLengthsEqual )                                                       &
      call warn( sMessage="The number of values specifying irrigation application"      &
        //" efficiency ("                                                    &
        //asCharacter( iNumRecs )//") does not match the number of landuse values ("    &
        //asCharacter( iNumberOfLanduses )//").",                                       &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )


    allocate( FIRST_DAY_OF_IRRIGATION( slIrrigationBegin%count ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )

    do iIndex = 1, slIrrigationBegin%count
      sBuf = slIrrigationBegin%get( iIndex )
      FIRST_DAY_OF_IRRIGATION( iIndex ) = mmdd2doy( sBuf )
    enddo  

    call slList%clear()
    call slList%append("Last_day_of_irrigation")
    call slList%append("Last_DOY_irrigation")
    call slList%append("Irrigation_end")

    call PARAMS%get_parameters( slKeys=slList, slValues=slIrrigationEnd, lFatal=lTRUE ) 
    call slList%clear()

    allocate( LAST_DAY_OF_IRRIGATION( slIrrigationEnd%count ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )

    do iIndex = 1, slIrrigationEnd%count
      sBuf = slIrrigationEnd%get( iIndex )
      LAST_DAY_OF_IRRIGATION( iIndex ) = mmdd2doy( sBuf )
    enddo  

    iNumRecs = ubound(FIRST_DAY_OF_IRRIGATION,1)
    lAreLengthsEqual = ( iNumRecs == iNumberOfLanduses ) 

    if ( .not. lAreLengthsEqual )                                                       &
      call warn( sMessage="The number of values specifying date of first "              &
        //"irrigation application ("                                                    &
        //asCharacter( iNumRecs )//") does not match the number of landuse values ("    &
        //asCharacter( iNumberOfLanduses )//").",                                       &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

    iNumRecs = ubound(LAST_DAY_OF_IRRIGATION,1)
    lAreLengthsEqual = ( iNumRecs == iNumberOfLanduses ) 

    if ( .not. lAreLengthsEqual )                                                       &
      call warn( sMessage="The number of values specifying date of last irrigation"     &
        //" application ("                                                              &
        //asCharacter( iNumRecs )//") does not match the number of landuse values ("    &
        //asCharacter( iNumberOfLanduses )//").",                                       &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

    iNumRecs = ubound(FRACTION_OF_IRRIGATION_FROM_GW,1)
    lAreLengthsEqual = ( iNumRecs == iNumberOfLanduses ) 

    if ( .not. lAreLengthsEqual )                                                       &
      call warn( sMessage="The number of values specifying the fraction of irrigation"  &
        //" from groundwater ("                                                         &
        //asCharacter( iNumRecs )//") does not match the number of landuse values ("    &
        //asCharacter( iNumberOfLanduses )//").",                                       &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

    iNumRecs = ubound(MAXIMUM_ALLOWABLE_DEPLETION_FRACTION,1)
    lAreLengthsEqual = ( iNumRecs == iNumberOfLanduses ) 

    if ( .not. lAreLengthsEqual )                                                       &
      call warn( sMessage="The number of values the maximum allowable depletion "       &
        //" fraction ("                                                                 &
        //asCharacter( iNumRecs )//") does not match the number of landuse values ("    &
        //asCharacter( iNumberOfLanduses )//").",                                       &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )


    ! locate the data structure associated with the gridded irrigation mask entries
    pIRRIGATION_MASK => DAT%find("IRRIGATION_MASK")
    if ( associated(pIRRIGATION_MASK) ) call pIRRIGATION_MASK%getvalues( )


    ! locate the data structure associated with the gridded pervious surface fraction values
    pPERVIOUS_SURFACE_FRACTION => DAT%find("PERVIOUS_SURFACE_FRACTION")
    if ( associated(pPERVIOUS_SURFACE_FRACTION) ) call pPERVIOUS_SURFACE_FRACTION%getvalues( )

    if (    associated(pPERVIOUS_SURFACE_FRACTION) .and. associated(pIRRIGATION_MASK) ) then

      IRRIGATION_MASK = pack( real(pIRRIGATION_MASK%pGrdBase%iData, kind=c_float)     &
                            * pPERVIOUS_SURFACE_FRACTION%pGrdBase%rData, lActive )

    elseif ( associated(pPERVIOUS_SURFACE_FRACTION) ) then

      IRRIGATION_MASK = pack( pPERVIOUS_SURFACE_FRACTION%pGrdBase%rData, lActive )

    elseif ( associated(pIRRIGATION_MASK) ) then

      IRRIGATION_MASK = pack( real(pIRRIGATION_MASK%pGrdBase%iData, kind=c_float), lActive )

    else

      IRRIGATION_MASK = 1.0_c_float

    endif

  end subroutine irrigation__initialize

!--------------------------------------------------------------------------------------------------  

  subroutine irrigation__calculate( fIrrigationAmount, iLanduseIndex, fSoilStorage, & 
                                    fSoilStorage_Max, lActive )

    real (kind=c_float), intent(inout)  :: fIrrigationAmount(:)
    integer (kind=c_int), intent(in)    :: iLanduseIndex(:)
    real (kind=c_float), intent(in)     :: fSoilStorage(:)
    real (kind=c_float), intent(in)     :: fSoilStorage_Max(:)
    logical (kind=c_bool), intent(in)   :: lActive(:,:)

    ! [ LOCALS ]
    real (kind=c_float)  :: fDepletionFraction
    real (kind=c_double) :: rDepletionAmount
    real (kind=c_double) :: rIrrigationAmount

    integer (kind=c_int)              :: iMonth
    integer (kind=c_int)              :: iDay
    integer (kind=c_int)              :: iYear
    integer (kind=c_int)              :: iJulianDay
    integer (kind=c_int)              :: iDayOfYear
    integer (kind=c_int)              :: iDaysInMonth
    integer (kind=c_int)              :: iNumDaysFromOrigin
!     integer (kind=c_int), allocatable :: iIrrigation_Mask(:)
    integer (kind=c_int)              :: iIndex
    real (kind=c_float)               :: fEfficiency

    ! zero out Irrigation term
    IRRIGATION_FROM_GROUNDWATER = rZERO
    IRRIGATION_FROM_SURFACE_WATER = rZERO

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear
      iDaysInMonth = SIM_DT%iDaysInMonth
      iDayOfYear = SIM_DT%iDOY
      iNumDaysFromOrigin = SIM_DT%iNumDaysFromOrigin

    end associate  

!     call pIRRIGATION_MASK%getvalues( iMonth, iDay, iYear, iJulianDay )
!     iIrrigation_Mask = pack( pIRRIGATION_MASK%pGrdBase%iData, lActive )

    ! for each cell, add water if soil storage zone is below the
    ! maximum allowable depletion

    ! now we run the gauntlet of tests to ensure that we really need
    ! to perform all of the irrigation calculations

    do iIndex=lbound(fSoilStorage,1), ubound(fSoilStorage,1)

      fIrrigationAmount( iIndex ) = fZERO

      fEfficiency = max( IRRIGATION_EFFICIENCY( iLanduseIndex( iIndex ) ), 0.20_c_float )

      if ( ( iDayOfYear < FIRST_DAY_OF_IRRIGATION( iLanduseIndex( iIndex ) ) ) &
        .or. ( iDayOfYear > LAST_DAY_OF_IRRIGATION( iLanduseIndex( iIndex ) ) ) )  cycle

      if ( MAXIMUM_ALLOWABLE_DEPLETION_FRACTION( iLanduseIndex( iIndex ) ) > 0.99 )  cycle
      if ( fSoilStorage_Max( iIndex ) <= fZERO ) cycle
      if ( IRRIGATION_MASK( iIndex ) < 1.0e-6_c_float ) cycle 

      if ( ( 1.0_c_float - fSoilStorage( iIndex ) / fSoilStorage_Max( iIndex ) )                           &
             > MAXIMUM_ALLOWABLE_DEPLETION_FRACTION( iLanduseIndex( iIndex ) ) ) then


      fIrrigationAmount( iIndex ) = ( fSoilStorage_Max( iIndex ) - fSoilStorage( iIndex ) ) &
                                     * IRRIGATION_MASK( iIndex ) / fEfficiency

      IRRIGATION_FROM_GROUNDWATER( iIndex ) = fIrrigationAmount( iIndex )          &
                                      * FRACTION_OF_IRRIGATION_FROM_GW( iLanduseIndex( iIndex ) )
      IRRIGATION_FROM_SURFACE_WATER( iIndex ) = fIrrigationAmount( iIndex )  &
                                                 - IRRIGATION_FROM_GROUNDWATER( iIndex )

      endif

    enddo

  end subroutine irrigation__calculate

end module irrigation
