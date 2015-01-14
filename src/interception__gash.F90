module interception__gash

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use dictionary
  use file_operations
  use parameters
  use swb_grid
  use simulation_datetime
  use strings
  use string_list
  implicit none

  private

  public interception_gash_initialize
  public interception_gash_calculate

  type (DATA_CATALOG_ENTRY_T), pointer :: pCANOPY_COVER_FRACTION
  type (DATA_CATALOG_ENTRY_T), pointer :: pSTEMFLOW_FRACTION
  type (DATA_CATALOG_ENTRY_T), pointer :: pEVAPORATION_TO_RAINFALL_RATIO

  real (kind=c_float), allocatable   :: CANOPY_COVER_FRACTION(:)
  real (kind=c_float), allocatable   :: STEMFLOW_FRACTION(:)
  real (kind=c_float), allocatable   :: EVAPORATION_TO_RAINFALL_RATIO(:)

  real (kind=c_float), allocatable   :: CANOPY_STORAGE_CAPACITY(:)
  real (kind=c_float), allocatable   :: TRUNK_STORAGE_CAPACITY(:)  

  real (kind=c_float), allocatable   :: CANOPY_STORAGE_CAPACITY_UNPACKED(:)
  real (kind=c_float), allocatable   :: TRUNK_STORAGE_CAPACITY_UNPACKED(:)  

  type (T_NETCDF4_FILE), pointer       :: pNCFILE           ! pointer to OUTPUT NetCDF file

contains

  !> Initialize the Gash interception algorithm. 
  !!
  !! Read in a canopy cover and stemflow fraction grids, 
  !! read in evaporation to rainfall ratio grid.
  !!
  subroutine interception_gash_initialize( lActive, iLandUseIndex )

    logical (kind=c_bool), intent(in)    :: lActive(:,:)
    integer (kind=c_int), intent(in)     :: iLanduseIndex(:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    type (STRING_LIST_T)                 :: slList
    integer (kind=c_int)                 :: iIndex 
    type (GENERAL_GRID_T), pointer       :: pTempGrid
    integer (kind=c_int)                 :: iCount
    integer (kind=c_int)                 :: iNumRecs
    integer (kind=c_int), allocatable    :: iLanduseTableCodes(:)
    integer (kind=c_int)                 :: iNumberOfLanduses
    logical (kind=c_bool)                :: lAreLengthsEqual

    iCount = count( lActive )

    allocate( CANOPY_COVER_FRACTION( iCount ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )
    allocate( STEMFLOW_FRACTION( iCount ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )
    allocate( EVAPORATION_TO_RAINFALL_RATIO( iCount ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )

    ! locate the data structure associated with the gridded canopy cover fraction
    pCANOPY_COVER_FRACTION => DAT%find("CANOPY_COVER_FRACTION")
    if ( .not. associated(pCANOPY_COVER_FRACTION) ) &
        call die("A CANOPY_COVER_FRACTION grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    ! locate the data structure associated with the gridded stemflow fraction
    pSTEMFLOW_FRACTION => DAT%find("STEMFLOW_FRACTION")
    if ( .not. associated(pSTEMFLOW_FRACTION) ) &
        call die("A STEMFLOW_FRACTION grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    ! locate the data structure associated with the gridded evaporation to rainfall ratio
    pEVAPORATION_TO_RAINFALL_RATIO => DAT%find("EVAPORATION_TO_RAINFALL_RATIO")
    if ( .not. associated( pEVAPORATION_TO_RAINFALL_RATIO ) ) &
        call die("A EVAPORATION_TO_RAINFALL_RATIO grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    call pCANOPY_COVER_FRACTION%getvalues()

    pTempGrid=>grid_Create( BNDS%iNumCols, BNDS%iNumRows, BNDS%fX_ll, BNDS%fY_ll, &
      BNDS%fX_ur, BNDS%fY_ur, DATATYPE_FLOAT )

    pTempGrid%rData = pCANOPY_COVER_FRACTION%pGrdBase%rData

    call grid_WriteArcGrid("Canopy_Cover_Fraction__echo.asc", pTempGrid)

    CANOPY_COVER_FRACTION = pack( pCANOPY_COVER_FRACTION%pGrdBase%rData, lActive )


    call pSTEMFLOW_FRACTION%getvalues()

    pTempGrid=>grid_Create( BNDS%iNumCols, BNDS%iNumRows, BNDS%fX_ll, BNDS%fY_ll, &
      BNDS%fX_ur, BNDS%fY_ur, DATATYPE_FLOAT )

    pTempGrid%rData = pSTEMFLOW_FRACTION%pGrdBase%rData

    call grid_WriteArcGrid("Stemflow_Fraction__echo.asc", pTempGrid)

    STEMFLOW_FRACTION = pack( pSTEMFLOW_FRACTION%pGrdBase%rData, lActive )


    call pEVAPORATION_TO_RAINFALL_RATIO%getvalues()

    pTempGrid=>grid_Create( BNDS%iNumCols, BNDS%iNumRows, BNDS%fX_ll, BNDS%fY_ll, &
      BNDS%fX_ur, BNDS%fY_ur, DATATYPE_FLOAT )

    pTempGrid%rData = pEVAPORATION_TO_RAINFALL_RATIO%pGrdBase%rData

    call grid_WriteArcGrid("Evaporation_to_Rainfall_Ratio__echo.asc", pTempGrid)

    EVAPORATION_TO_RAINFALL_RATIO = pack( pEVAPORATION_TO_RAINFALL_RATIO%pGrdBase%rData, lActive )


    !! now grab the table values needed for this module

    ! create list of possible table headings to look for...
    call slList%append( "LU_Code" )
    call slList%append( "Landuse_Lookup_Code" )

    !> Determine how many landuse codes are present
    call PARAMS%get_values( slList, iLanduseTableCodes )
    iNumberOfLanduses = count( iLanduseTableCodes >= 0 )

    call slList%deallocate()
    call slList%append("Canopy_Capacity")
    call slList%append("Canopy_Storage_Capacity")
    call PARAMS%get_values( slList, CANOPY_STORAGE_CAPACITY )

    call slList%deallocate()
    call slList%append("Trunk_Capacity")
    call slList%append("Trunk_Storage_Capacity")
    call PARAMS%get_values( slList, TRUNK_STORAGE_CAPACITY )

    iNumRecs = ubound(CANOPY_STORAGE_CAPACITY,1)
    lAreLengthsEqual = ( iNumRecs == iNumberOfLanduses ) 

    if ( .not. lAreLengthsEqual )                                                       &
      call warn( sMessage="The number of canopy storage capacity values ("              &
        //asCharacter( iNumRecs )//") does not match the number of landuse values ("    &
        //asCharacter( iNumberOfLanduses )//").",                                       &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )


    iNumRecs = ubound(TRUNK_STORAGE_CAPACITY,1)
    lAreLengthsEqual = ( iNumRecs == iNumberOfLanduses )

    if ( .not. lAreLengthsEqual )                                                        &
      call warn( sMessage="The number of trunk storage capacity values ("                &
        //asCharacter( iNumRecs )//") does not match the number of landuse values ("     &
        //asCharacter( iNumberOfLanduses )//").",                                        &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )


    allocate( CANOPY_STORAGE_CAPACITY_UNPACKED( iCount ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )

    allocate( TRUNK_STORAGE_CAPACITY_UNPACKED( iCount ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )

    do iIndex = lbound(iLandUseTableCodes,1), ubound(iLandUseTableCodes,1)
      where (iLanduseIndex == iIndex)

        CANOPY_STORAGE_CAPACITY_UNPACKED = CANOPY_STORAGE_CAPACITY( iIndex )
        TRUNK_STORAGE_CAPACITY_UNPACKED = TRUNK_STORAGE_CAPACITY( iIndex ) 

      end where  
    enddo

    deallocate(CANOPY_STORAGE_CAPACITY)
    deallocate(TRUNK_STORAGE_CAPACITY)

  end subroutine interception_gash_initialize

!--------------------------------------------------------------------------------------------------

  subroutine interception_gash_calculate( fRainfall, fFog, fInterception )

    real (kind=c_float), intent(in)        :: fRainfall(:)
    real (kind=c_float), intent(in)        :: fFog(:)
    real (kind=c_float), intent(inout)     :: fInterception(:)

    ! [ LOCALS ]
    real (kind=c_float), allocatable :: fPrecipitation_at_Saturation(:)
    real (kind=c_float)              :: coef2 = 0.0


    allocate( fPrecipitation_at_Saturation(ubound(CANOPY_STORAGE_CAPACITY_UNPACKED,1) ) )
 
    associate( Psat => fPrecipitation_at_Saturation )

      where ( CANOPY_COVER_FRACTION > 0. .and. EVAPORATION_TO_RAINFALL_RATIO  > 0. )

        Psat = - ( CANOPY_STORAGE_CAPACITY_UNPACKED / &
                  ( ( CANOPY_COVER_FRACTION ) * EVAPORATION_TO_RAINFALL_RATIO )  &
                     * log( ( 1.0 - EVAPORATION_TO_RAINFALL_RATIO ) ) &
                       / ( 1.0 - ( 1.0 - coef2 ) * EVAPORATION_TO_RAINFALL_RATIO ) )

      elsewhere
      
        Psat = 0.0

      endwhere    

!       print *, __FILE__, " : ", __LINE__, "   Psat (min,max)=", minval(Psat), maxval(Psat)

    !  where (fRainfall)

!                !! calc Precip needed to saturate canopy    
!                 Psat=-( cancap( ilu(ip) ) / ( canfrac( ip )*cerf( ip ) ) ) &
!                                 * log( ( 1 - cerf( ip ) ) / ( 1 - ( 1 - ceint2 ) * cerf( ip ) ) )
!                 if(drf+dfog.lt.Psat)then
!                    dcanint=canfrac(ip)*(drf+dfog)
!                 elseif(drf+dfog.gt.tcap(ilu(ip))/tfrac(ip))then
!                    dcanint=canfrac(ip)*Psat+canfrac(ip)*cerf(ip)*
!      1                     (drf+dfog-Psat)+tcap(ilu(ip))
!                 else
!                    dcanint=canfrac(ip)*Psat+canfrac(ip)*cerf(ip)*
!      1                     (drf+dfog-Psat)+tfrac(ip)*(drf+dfog)
!                 endif
!                 dpnet=drf+dfog-dcanint

    end associate

 
  end subroutine interception_gash_calculate

end module interception__gash