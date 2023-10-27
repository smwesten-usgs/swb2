module interception__gash

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use dictionary
  use exceptions
  use file_operations
  use parameters, only          : PARAMS
  use grid
  use netcdf4_support, only      : T_NETCDF4_FILE
  use simulation_datetime
  use fstring
  use fstring_list
  implicit none

  private

  public :: CANOPY_STORAGE_CAPACITY_TABLE_VALUES,           &
            TRUNK_STORAGE_CAPACITY_TABLE_VALUES,            &
            STEMFLOW_FRACTION_TABLE_VALUES,                 &
            EVAPORATION_TO_RAINFALL_RATIO,                  &
            P_SAT,                                          &
            GASH_INTERCEPTION_STORAGE_MAX_GROWING_SEASON,   &
            GASH_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON

  public interception_gash_initialize
  public interception_gash_calculate
  public precipitation_at_saturation

  type (DATA_CATALOG_ENTRY_T), pointer :: pEVAPORATION_TO_RAINFALL_RATIO

  real (c_float), allocatable   :: EVAPORATION_TO_RAINFALL_RATIO(:)

  real (c_float), allocatable   :: CANOPY_STORAGE_CAPACITY_TABLE_VALUES(:)
  real (c_float), allocatable   :: TRUNK_STORAGE_CAPACITY_TABLE_VALUES(:)
  real (c_float), allocatable   :: STEMFLOW_FRACTION_TABLE_VALUES(:)
  real (c_float), allocatable   :: GASH_INTERCEPTION_STORAGE_MAX_GROWING_SEASON(:)
  real (c_float), allocatable   :: GASH_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON(:)

  real (c_float), allocatable   :: P_SAT(:)

  type (T_NETCDF4_FILE), pointer     :: pNCFILE           ! pointer to OUTPUT NetCDF file

contains

  !> Initialize the Gash interception algorithm.
  !!
  !! Read in a canopy cover fraction and rainfall-to-evaporation ratio grids
  !!
  subroutine interception_gash_initialize( lActive, fCanopy_Cover_Fraction, iLandUseIndex )

    logical (c_bool), intent(in)    :: lActive(:,:)
    real (c_float), intent(in)      :: fCanopy_Cover_Fraction(:)
    integer (c_int), intent(in)     :: iLanduseIndex(:)

    ! [ LOCALS ]
    integer (c_int)                 :: iStat
    type (FSTRING_LIST_T)                 :: slList
    integer (c_int)                 :: iIndex
    integer (c_int)                 :: iCount
    integer (c_int)                 :: iNumRecs
    integer (c_int), allocatable    :: iLanduseTableCodes(:)
    integer (c_int)                 :: iNumberOfLanduses
    logical (c_bool)                :: lAreLengthsEqual
    real (c_float), allocatable     :: tempvals(:)

    iCount = count( lActive )

    allocate( EVAPORATION_TO_RAINFALL_RATIO( iCount ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )

    ! locate the data structure associated with the gridded evaporation to rainfall ratio
    pEVAPORATION_TO_RAINFALL_RATIO => DAT%find("EVAPORATION_TO_RAINFALL_RATIO")
    if ( .not. associated( pEVAPORATION_TO_RAINFALL_RATIO ) ) &
        call die("A EVAPORATION_TO_RAINFALL_RATIO grid must be supplied in order to"      &
                 //" make use of this option.", __FILE__, __LINE__)


    call pEVAPORATION_TO_RAINFALL_RATIO%getvalues()

    call grid_WriteArcGrid("Evaporation_to_Rainfall_Ratio__echo.asc", &
      pEVAPORATION_TO_RAINFALL_RATIO%pGrdBase )

    EVAPORATION_TO_RAINFALL_RATIO = pack( pEVAPORATION_TO_RAINFALL_RATIO%pGrdBase%rData, lActive )

    !! now grab the table values needed for this module

    ! create list of possible table headings to look for...
    call slList%append( "LU_Code" )
    call slList%append( "Landuse_Lookup_Code" )

    !> Determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=slList, iValues=iLanduseTableCodes )
    iNumberOfLanduses = count( iLanduseTableCodes >= 0 )

    call slList%clear()
    call slList%append("Canopy_Capacity")
    call slList%append("Canopy_Storage_Capacity")
    call PARAMS%get_parameters( slKeys=slList, fValues=CANOPY_STORAGE_CAPACITY_TABLE_VALUES )

    call slList%clear()
    call slList%append("Trunk_Capacity")
    call slList%append("Trunk_Storage_Capacity")
    call PARAMS%get_parameters( slKeys=slList, fValues=TRUNK_STORAGE_CAPACITY_TABLE_VALUES )

    call slList%clear()
    call slList%append("Stemflow_Fraction")
    call slList%append("Stemflow_Frac")
    call PARAMS%get_parameters( slKeys=slList, fValues=STEMFLOW_FRACTION_TABLE_VALUES )

    iNumRecs = ubound(CANOPY_STORAGE_CAPACITY_TABLE_VALUES,1)
    lAreLengthsEqual = ( iNumRecs == iNumberOfLanduses )

    !> retrieve interception storage max (GROWING SEASON)
    call slList%clear()
    call slList%append("interception_storage_max_growing")
    call slList%append("interception_storage_max_growing_season")
    call slList%append("interception_storage_maximum_growing")

    call PARAMS%get_parameters( slKeys=slList,                                           &
                                fValues=GASH_INTERCEPTION_STORAGE_MAX_GROWING_SEASON, &
                                lFatal=FALSE )

    iNumRecs = ubound(GASH_INTERCEPTION_STORAGE_MAX_GROWING_SEASON,1)
    lAreLengthsEqual = ( iNumRecs == iNumberOfLanduses )

    if ( .not. lAreLengthsEqual ) then
      call warn( sMessage="The number of landuses does not match the number of interception storage "      &
                          //"maximum values for the growing season"                                        &
                          //"('interception_storage_max_growing').",                                       &
                 sHints="A default value of 0.1 inches was assigned for the maximum interception storage", &
                 sModule=__FILE__, iLine=__LINE__, lFatal=FALSE )
      allocate(tempvals(iNumberOfLanduses), stat=iStat)
      tempvals = 0.1
      call move_alloc(tempvals, GASH_INTERCEPTION_STORAGE_MAX_GROWING_SEASON)
           
    endif

    !> retrieve interception storage max (NONGROWING SEASON)
    call slList%clear()
    call slList%append("interception_storage_max_nongrowing")
    call slList%append("interception_storage_max_nongrowing_season")
    call slList%append("interception_storage_maximum_nongrowing")

    call PARAMS%get_parameters( slKeys=slList,                                           &
                                fValues=GASH_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON, &
                                lFatal=FALSE )

    iNumRecs = ubound(GASH_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON,1)
    lAreLengthsEqual = ( iNumRecs == iNumberOfLanduses )

    if ( .not. lAreLengthsEqual ) then
      call warn( sMessage="The number of landuses does not match the number of interception storage "         &
                          //"maximum values for the nongrowing season"                                        &
                          //"('interception_storage_max_nongrowing').",                                       &
                 sHints="A default value of 0.1 inches was assigned for the maximum interception storage",    &
                 sModule=__FILE__, iLine=__LINE__, lFatal=FALSE )
      allocate(tempvals(iNumberOfLanduses), stat=iStat)
      tempvals = 0.1
      call move_alloc(tempvals, GASH_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON)
    endif

    !> @TODO add more guard code here to QA incoming data

    if ( .not. lAreLengthsEqual )                                                       &
      call warn( sMessage="The number of canopy storage capacity values ("              &
        //asCharacter( iNumRecs )//") does not match the number of landuse values ("    &
        //asCharacter( iNumberOfLanduses )//").",                                       &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )


    iNumRecs = ubound(TRUNK_STORAGE_CAPACITY_TABLE_VALUES,1)
    lAreLengthsEqual = ( iNumRecs == iNumberOfLanduses )

    if ( .not. lAreLengthsEqual )                                                        &
      call warn( sMessage="The number of trunk storage capacity values ("                &
        //asCharacter( iNumRecs )//") does not match the number of landuse values ("     &
        //asCharacter( iNumberOfLanduses )//").",                                        &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )


    iNumRecs = ubound(STEMFLOW_FRACTION_TABLE_VALUES,1)
    lAreLengthsEqual = ( iNumRecs == iNumberOfLanduses )

    if ( .not. lAreLengthsEqual )                                                        &
      call warn( sMessage="The number of stemflow fraction values ("                &
        //asCharacter( iNumRecs )//") does not match the number of landuse values ("     &
        //asCharacter( iNumberOfLanduses )//").",                                        &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )


    allocate( P_SAT( count( lActive ) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory for P_SAT.", __FILE__, __LINE__ )

    P_SAT = precipitation_at_saturation( EVAPORATION_TO_RAINFALL_RATIO,    &
      CANOPY_STORAGE_CAPACITY_TABLE_VALUES( iLanduseIndex ), &
      fCanopy_Cover_Fraction )

  end subroutine interception_gash_initialize

!--------------------------------------------------------------------------------------------------

  elemental function precipitation_at_saturation( E_div_P, canopy_storage_capacity, canopy_cover_fraction )  &
                                                                               result( Psat )

    real (c_float), intent(in)    :: E_div_P
    real (c_float), intent(in)    :: canopy_storage_capacity
    real (c_float), intent(in)    :: canopy_cover_fraction
    real (c_float)                :: Psat

    ! [ LOCALS ]
    real (c_float)                :: P_div_E

    if ( canopy_cover_fraction > 0.0_c_float .and. canopy_storage_capacity > 0.0_c_float ) then

       Psat = - ( canopy_storage_capacity / ( canopy_cover_fraction * E_div_P ) )      &
                     * log( 1.0_c_float - E_div_P )
    else

      Psat = 0.0_c_float

    endif

!                !! calc Precip needed to saturate canopy
!                 Psat=-( cancap( ilu(ip) ) / ( canfrac( ip )*cerf( ip ) ) ) &
!                       * log( ( 1 - cerf( ip ) ) / ( 1 - ( 1 - ceint2 ) * cerf( ip ) ) )


  end function precipitation_at_saturation

!--------------------------------------------------------------------------------------------------

  elemental subroutine interception_gash_calculate( fRainfall, fFog, fCanopy_Cover_Fraction,       &
                                                    fTrunk_Storage_Capacity, fStemflow_Fraction,   &
                                                    fEvaporation_to_Rainfall_Ratio,                &
                                                    fPrecipitation_at_Saturation, fInterception )

    real (c_float), intent(in)        :: fRainfall
    real (c_float), intent(in)        :: fFog
    real (c_float), intent(in)        :: fCanopy_Cover_Fraction
    real (c_float), intent(in)        :: fTrunk_Storage_Capacity
    real (c_float), intent(in)        :: fStemflow_Fraction
    real (c_float), intent(in)        :: fEvaporation_to_Rainfall_Ratio
    real (c_float), intent(in)        :: fPrecipitation_at_Saturation
    real (c_float), intent(inout)     :: fInterception

    ! [ LOCALS ]
    real (c_float)      :: fRainfall_plus_Fog

    fRainfall_plus_Fog = fRainfall + fFog

    if ( fPrecipitation_at_Saturation .approxequal. 0.0_c_float ) then

      fInterception = 0.0_c_float

     ! if(drf+dfog.lt.Psat)then
    else if ( fRainfall_plus_Fog < fPrecipitation_at_Saturation ) then

      !  dcanint=canfrac(ip)*(drf+dfog)
      fInterception = fCanopy_Cover_Fraction * fRainfall_plus_Fog

!       elseif(drf+dfog.gt.tcap(ilu(ip))/tfrac(ip))then
    else if ( fRainfall_plus_Fog > fTrunk_Storage_Capacity                                 &
                                   / (fStemflow_Fraction + 1.0e-6) ) then

      fInterception =  fCanopy_Cover_Fraction * fPrecipitation_at_Saturation               &
                      + fCanopy_Cover_Fraction * fEvaporation_to_Rainfall_Ratio            &
                         * ( fRainfall_plus_Fog - fPrecipitation_at_Saturation )           &
                      + fTrunk_Storage_Capacity


      ! dcanint = canfrac(ip) * Psat
      !           + canfrac(ip) * cerf(ip) * ( drf + dfog - Psat )
      !           + tcap( ilu(ip) )

    else

      fInterception =  fCanopy_Cover_Fraction * fPrecipitation_at_Saturation               &
                      + fCanopy_Cover_Fraction * fEvaporation_to_Rainfall_Ratio            &
                         * ( fRainfall_plus_Fog - fPrecipitation_at_Saturation )           &
                      + fStemflow_Fraction * fRainfall_plus_Fog

      ! dcanint = canfrac(ip) * Psat
      !           + canfrac(ip) * cerf(ip) *( drf + dfog - Psat )
      !           + tfrac(ip) * ( drf + dfog )

    end if

    fInterception = min( fInterception, fRainfall_plus_Fog )

    !! HWB code:
!                !! calc Precip needed to saturate canopy
!                 Psat=-( cancap( ilu(ip) ) / ( canfrac( ip )*cerf( ip ) ) ) &
!                                 * log( ( 1 - cerf( ip ) ) / ( 1 - ( 1 - ceint2 ) * cerf( ip ) ) )
!
!                 if ( drf + dfog .lt. Psat )then
!                    dcanint = canfrac(ip) * ( drf + dfog )
!                 elseif ( drf + dfog .gt. tcap( ilu(ip) ) / tfrac(ip) ) then
!                    dcanint = canfrac(ip) * Psat + canfrac(ip) * cerf(ip) * ( drf + dfog - Psat ) + tcap( ilu(ip) )
!                 else
!                    dcanint = canfrac(ip) * Psat + canfrac(ip) * cerf(ip) *( drf + dfog - Psat ) + tfrac(ip) * ( drf + dfog )
!                 endif
!
!                 dpnet = drf + dfog - dcanint

  end subroutine interception_gash_calculate

end module interception__gash
