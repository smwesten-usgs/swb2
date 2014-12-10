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

  type (T_NETCDF4_FILE), pointer       :: pNCFILE           ! pointer to OUTPUT NetCDF file

contains

  !> Initialize the Gash interception algorithm. 
  !!
  !! Read in a canopy cover and stemflow fraction grids, 
  !! read in evaporation to rainfall ratio grid.
  !!
  subroutine interception_gash_initialize( lActive )

    logical (kind=c_bool)                :: lActive(:,:)


    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    type (STRING_LIST_T)                 :: slList
    integer (kind=c_int)                 :: iIndex 
    type (GENERAL_GRID_T), pointer       :: pTempGrid
    integer (kind=c_int)                 :: iCount
    integer (kind=c_int), allocatable    :: iLanduseCodes(:)
    integer (kind=c_int)                 :: iNumberOfLanduses

    iCount = count( lActive )

    allocate( CANOPY_COVER_FRACTION( iCount ), stat=iStat )
    allocate( STEMFLOW_FRACTION( iCount ), stat=iStat )
    allocate( EVAPORATION_TO_RAINFALL_RATIO( iCount ), stat=iStat )

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
    slList = "LU_Code"
    call slList%append("Landuse_Lookup_Code")

    !> Determine how many landuse codes are present
    call PARAMS%get_values( slList, iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes >= 0 )






  end subroutine interception_gash_initialize

!--------------------------------------------------------------------------------------------------

  subroutine interception_gash_calculate( fRainfall, fFog, fInterception )

    real (kind=c_float), intent(in)        :: fRainfall(:)
    real (kind=c_float), intent(in)        :: fFog(:)
    real (kind=c_float), intent(inout)     :: fInterception(:)

    ! [ LOCALS ]
    real (kind=c_float) :: fPrecipitation_at_Saturation
 
    associate( Psat => fPrecipitation_at_Saturation )

!      Psat = - ( CANOPY_COVER_FRACTION / STEMFLOW_FRACTION )

!                !! calc Precip needed to saturate canopy    
!                 Psat=-( cancap( ilu(ip) ) / ( canfrac( ip )*cerf( ip ) ) ) * log( ( 1 - cerf( ip ) ) / ( 1 - ( 1 - ceint2 ) * cerf( ip ) ) )
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