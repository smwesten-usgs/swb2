!> @file
!! Contains the module \ref direct_recharge__gridded_data.

!>
!!  Module \ref direct_recharge__gridded_data
!!  provides support for adding miscellaneous source and sink terms.

module direct_recharge__gridded_data

  use iso_c_binding, only       : c_short, c_int, c_float, c_double, c_size_t, c_ptrdiff_t
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use dictionary
  use exceptions
  use file_operations
  use netcdf4_support
  use parameters, only          : PARAMS
  use simulation_datetime
  use strings
  use string_list

  implicit none

  private

  public :: direct_recharge_initialize, direct_recharge_calculate

  type (DATA_CATALOG_ENTRY_T), pointer :: pCESSPOOL
  type (DATA_CATALOG_ENTRY_T), pointer :: pDISPOSAL_WELL
  type (DATA_CATALOG_ENTRY_T), pointer :: pSTORM_DRAIN
  type (DATA_CATALOG_ENTRY_T), pointer :: pWATER_BODY_RECHARGE
  type (DATA_CATALOG_ENTRY_T), pointer :: pWATER_MAIN
  type (DATA_CATALOG_ENTRY_T), pointer :: pANNUAL_RECHARGE_RATE    

  real (kind=c_float), allocatable     :: fCESSPOOL(:)
  real (kind=c_float), allocatable     :: fDISPOSAL_WELL(:)
  real (kind=c_float), allocatable     :: fSTORM_DRAIN(:)
  real (kind=c_float), allocatable     :: fWATER_BODY_RECHARGE(:)
  real (kind=c_float), allocatable     :: fWATER_MAIN(:)
  real (kind=c_float), allocatable     :: fANNUAL_RECHARGE_RATE(:)

  real (kind=c_float), allocatable     :: fCESSPOOL_TABLE(:)
  real (kind=c_float), allocatable     :: fDISPOSAL_WELL_TABLE(:)
  real (kind=c_float), allocatable     :: fSTORM_DRAIN_TABLE(:)
  real (kind=c_float), allocatable     :: fWATER_BODY_RECHARGE_TABLE(:)
  real (kind=c_float), allocatable     :: fWATER_MAIN_TABLE(:)
  real (kind=c_float), allocatable     :: fANNUAL_RECHARGE_RATE_TABLE(:)

  type (T_NETCDF4_FILE), pointer       :: pNCFILE

contains

  !> Initialize the routine to enable input/output of arbitrary sources/sink terms. 
  !!
  !! Open gridded data file.
  !! Open a NetCDF output file to hold variable output.
  !!
  !! @param[in] lActive 2D array of active cells within the model domain.
  !! @param[in] iLanduseIndex 1D vector of indices corresponding to rows of the
  !!            landuse lookup table(s).
  !! @param[in] dX 1D vector of X coordinates associated with the model domain.
  !! @param[in] dY 1D vector of Y coordinates.
  !! @param[in] dX_lon 2D array of longitude values.
  !! @param[in] dY_lat 2D array of latitude values.

  subroutine direct_recharge_initialize( lActive, iLandUseIndex, PROJ4_string,      &
                                         dX, dY, dX_lon, dY_lat )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)
    integer (kind=c_int), intent(in)      :: iLandUseIndex(:)
    character (len=*), intent(inout)      :: PROJ4_string
    real (kind=c_double), intent(in)      :: dX(:)
    real (kind=c_double), intent(in)      :: dY(:)
    real (kind=c_double), intent(in)      :: dX_lon(:,:)
    real (kind=c_double), intent(in)      :: dY_lat(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    type (STRING_LIST_T)                 :: slString
    integer (kind=c_int)                 :: iIndex 
    integer (kind=c_int)                 :: iNX
    integer (kind=c_int)                 :: iNY
    integer (kind=c_int), allocatable    :: iLanduseCodes(:)
    integer (kind=c_int)                 :: iNumberOfLanduses
    logical (kind=c_bool)                :: lAreLengthsEqual


    !> Determine how many landuse codes are present
    call slString%append( "LU_Code" )
    call slString%append( "Landuse_Code" )

    call PARAMS%get_parameters( slKeys=slString, iValues=iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes > 0 )

    call slString%clear()
    call slString%append( "Annual_direct_recharge_rate" )
    call slString%append( "Annual_recharge_rate" )
    call slString%append( "Annual_direct_recharge" )

    call PARAMS%get_parameters( slKeys=slString , fValues=fANNUAL_RECHARGE_RATE_TABLE )

    ! attempt to find a source of GRIDDED ANNUAL DIRECT RECHARGE data
    pANNUAL_RECHARGE_RATE => DAT%find( "ANNUAL_DIRECT_RECHARGE_RATE" )


    call slString%clear()
    call slString%append( "Cesspool_direct_recharge" )
    call slString%append( "Cesspool_recharge" )
    call slString%append( "Cesspool_discharge" )
    call slString%append( "Cesspool_leakage" )    
    
    call PARAMS%get_parameters( slKeys=slString , fValues=fCESSPOOL_TABLE )

    ! attempt to find a source of GRIDDED CESSPOOL data
    pCESSPOOL => DAT%find( "CESSPOOL_LEAKAGE" )


    call slString%clear()
    call slString%append( "Storm_drain_discharge" )
    call slString%append( "Storm_drain_recharge" )
    call slString%append( "Storm_drain_leakage" )    
    
    call PARAMS%get_parameters( slKeys=slString , fValues=fSTORM_DRAIN_TABLE )

    pSTORM_DRAIN => DAT%find( "STORM_DRAIN" )


    call slString%clear()
    call slString%append( "Water_body_recharge" )
    call slString%append( "Water_body_discharge" )
    call slString%append( "Water_body_leakage" )    
    
    call PARAMS%get_parameters( slKeys=slString , fValues=fWATER_BODY_RECHARGE_TABLE )

    pWATER_BODY_RECHARGE => DAT%find( "WATER_BODY_RECHARGE" )


    call slString%clear()
    call slString%append( "Water_main_recharge" )
    call slString%append( "Water_main_discharge" )
    call slString%append( "Water_main_leakage" )
    
    call PARAMS%get_parameters( slKeys=slString , fValues=fWATER_MAIN_TABLE )

    pWATER_MAIN => DAT%find( "WATER_MAIN_LEAKAGE" )


    call slString%clear()
    call slString%append( "Disposal_well_recharge" )
    call slString%append( "Disposal_well_discharge" )
    
    call PARAMS%get_parameters( slKeys=slString , fValues=fDISPOSAL_WELL_TABLE )

    pDISPOSAL_WELL => DAT%find( "DISPOSAL_WELL_DISCHARGE" )


    if ( associated( pANNUAL_RECHARGE_RATE ) ) then

      allocate( fANNUAL_RECHARGE_RATE( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    elseif ( fANNUAL_RECHARGE_RATE_TABLE(1) > fTINYVAL ) then

      lAreLengthsEqual = ( ( ubound(fANNUAL_RECHARGE_RATE_TABLE,1) == ubound(iLanduseCodes,1) )  )

      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of annual direct"   &
          //" recharge rate values.", sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fANNUAL_RECHARGE_RATE( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

      do iIndex=lbound( iLandUseIndex, 1 ), ubound( iLandUseIndex, 1 )
        fANNUAL_RECHARGE_RATE( iIndex ) = fANNUAL_RECHARGE_RATE_TABLE( iLandUseIndex( iIndex ) )
      enddo  

     endif


    if ( associated( pCESSPOOL ) ) then

      allocate( fCESSPOOL( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    elseif ( fCESSPOOL_TABLE(1) > fTINYVAL ) then

      lAreLengthsEqual = ( ( ubound(fCESSPOOL_TABLE,1) == ubound(iLanduseCodes,1) )  )

      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of cesspool discharge/leakage values.",   &
          sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fCESSPOOL( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

      do iIndex=lbound( iLandUseIndex, 1 ), ubound( iLandUseIndex, 1 )
        fCESSPOOL( iIndex ) = fCESSPOOL_TABLE( iLandUseIndex( iIndex ) )
      enddo  

     endif


    if ( associated( pSTORM_DRAIN ) ) then

      allocate( fSTORM_DRAIN( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    elseif ( fSTORM_DRAIN_TABLE(1) > fTINYVAL ) then

      lAreLengthsEqual = ( ( ubound(fSTORM_DRAIN_TABLE,1) == ubound(iLanduseCodes,1) )  )

      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of storm drain discharge/leakage values.",   &
          sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fSTORM_DRAIN( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

      do iIndex=lbound( iLandUseIndex, 1 ), ubound( iLandUseIndex, 1 )
        fSTORM_DRAIN( iIndex ) = fSTORM_DRAIN_TABLE( iLandUseIndex( iIndex ) )
      enddo  

     endif


    if ( associated( pWATER_BODY_RECHARGE ) ) then

      allocate( fWATER_BODY_RECHARGE( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    elseif ( fWATER_BODY_RECHARGE_TABLE(1) > fTINYVAL ) then

      lAreLengthsEqual = ( ( ubound(fWATER_BODY_RECHARGE_TABLE,1) == ubound(iLanduseCodes,1) )  )

      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of water body recharge/leakage values.",   &
          sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fWATER_BODY_RECHARGE( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

      do iIndex=lbound( iLandUseIndex, 1 ), ubound( iLandUseIndex, 1 )
        fWATER_BODY_RECHARGE( iIndex ) = fWATER_BODY_RECHARGE_TABLE( iLandUseIndex( iIndex ) )
      enddo  

     endif


    if ( associated( pWATER_MAIN ) ) then

      allocate( fWATER_MAIN( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    elseif ( fWATER_MAIN_TABLE(1) > fTINYVAL ) then

      lAreLengthsEqual = ( ( ubound(fWATER_MAIN_TABLE,1) == ubound(iLanduseCodes,1) )  )

      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of water main leakage values.",   &
          sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fWATER_MAIN( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

      do iIndex=lbound( iLandUseIndex, 1 ), ubound( iLandUseIndex, 1 )
        fWATER_MAIN( iIndex ) = fWATER_MAIN_TABLE( iLandUseIndex( iIndex ) )
      enddo  

     endif


    if ( associated( pDISPOSAL_WELL ) ) then

      allocate( fDISPOSAL_WELL( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    elseif ( fDISPOSAL_WELL_TABLE(1) > fTINYVAL ) then

      lAreLengthsEqual = ( ( ubound(fDISPOSAL_WELL_TABLE,1) == ubound(iLanduseCodes,1) )  )

      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of discharge well values.",   &
          sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fDISPOSAL_WELL( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

      do iIndex=lbound( iLandUseIndex, 1 ), ubound( iLandUseIndex, 1 )
        fDISPOSAL_WELL( iIndex ) = fDISPOSAL_WELL_TABLE( iLandUseIndex( iIndex ) )
      enddo  

     endif

    !> open another netCDF file to hold total direct recharge
    iNX = ubound(lActive, 1)
    iNY = ubound(lActive, 2)

    allocate ( pNCFILE, stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    call netcdf_open_and_prepare_as_output( NCFILE=pNCFILE, sVariableName="direct_recharge",  &
      sVariableUnits="inches", iNX=iNX, iNY=iNY, sXY_units=trim( XY_UNITS_STRING ),           &
      fX=dX, fY=dY, StartDate=SIM_DT%start, EndDate=SIM_DT%end, PROJ4_string=PROJ4_string,    &
      dpLat=dY_lat, dpLon=dX_lon, fValidMin=0.0, fValidMax=2000.0   )

  end subroutine direct_recharge_initialize

!--------------------------------------------------------------------------------------------------

  subroutine direct_recharge_calculate( direct_recharge, iLanduse_Index, lActive, nodata_fill_value )

    real (kind=c_float), intent(inout)     :: direct_recharge(:)
    integer (kind=c_int), intent(in)       :: iLanduse_Index(:)
    logical (kind=c_bool), intent(in)      :: lActive(:,:)
    real (kind=c_float), intent(in)        :: nodata_fill_value(:,:)

    ! [ LOCALS ] 
    integer (kind=c_int) :: iJulianDay
    integer (kind=c_int) :: iMonth
    integer (kind=c_int) :: iDay
    integer (kind=c_int) :: iYear
    integer (kind=c_int) :: iDaysInMonth
    integer (kind=c_int) :: iNumDaysFromOrigin
    integer (kind=c_int)                 :: iNX
    integer (kind=c_int)                 :: iNY
    integer (kind=c_int) :: iIndex
    real (kind=c_float)  :: fFactor

    iNX = ubound(nodata_fill_value, 1)
    iNY = ubound(nodata_fill_value, 2)

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear
      iDaysInMonth = SIM_DT%iDaysInMonth
      iNumDaysFromOrigin = SIM_DT%iNumDaysFromOrigin

      if ( associated( pCESSPOOL ) ) then
        call pCESSPOOL%getvalues( iMonth, iDay, iYear, iJulianDay )
        if ( pCESSPOOL%lGridHasChanged ) fCESSPOOL = pack( pCESSPOOL%pGrdBase%rData, lActive )
      endif

      if ( associated( pDISPOSAL_WELL ) ) then
        call pDISPOSAL_WELL%getvalues( iMonth, iDay, iYear, iJulianDay )
        if ( pDISPOSAL_WELL%lGridHasChanged ) fDISPOSAL_WELL = pack( pDISPOSAL_WELL%pGrdBase%rData, lActive )
      endif

      if ( associated( pSTORM_DRAIN ) ) then
        call pSTORM_DRAIN%getvalues( iMonth, iDay, iYear, iJulianDay )
        if ( pSTORM_DRAIN%lGridHasChanged ) fSTORM_DRAIN = pack( pSTORM_DRAIN%pGrdBase%rData, lActive )
      endif

      if ( associated( pWATER_BODY_RECHARGE ) ) then
        call pWATER_BODY_RECHARGE%getvalues( iMonth, iDay, iYear, iJulianDay )
        if ( pWATER_BODY_RECHARGE%lGridHasChanged ) fWATER_BODY_RECHARGE = pack( pWATER_BODY_RECHARGE%pGrdBase%rData, lActive )
      endif

      if ( associated( pWATER_MAIN ) ) then
        call pWATER_MAIN%getvalues( iMonth, iDay, iYear, iJulianDay )
        if ( pWATER_MAIN%lGridHasChanged ) fWATER_MAIN = pack( pWATER_MAIN%pGrdBase%rData, lActive )
      endif      

      if ( associated( pANNUAL_RECHARGE_RATE ) ) then
        call pANNUAL_RECHARGE_RATE%getvalues( iMonth, iDay, iYear, iJulianDay )
        if ( pANNUAL_RECHARGE_RATE%lGridHasChanged ) fANNUAL_RECHARGE_RATE = pack( pANNUAL_RECHARGE_RATE%pGrdBase%rData, lActive )
      endif      


      direct_recharge = 0.0_c_float

      if ( allocated( fCESSPOOL ) )  DIRECT_RECHARGE = DIRECT_RECHARGE + fCESSPOOL

      if ( allocated( fDISPOSAL_WELL ) )  DIRECT_RECHARGE = DIRECT_RECHARGE + fDISPOSAL_WELL

      if ( allocated( fWATER_MAIN ) )  DIRECT_RECHARGE = DIRECT_RECHARGE + fWATER_MAIN

      if ( allocated( fWATER_BODY_RECHARGE ) )  DIRECT_RECHARGE = DIRECT_RECHARGE + fWATER_BODY_RECHARGE

      if ( allocated( fSTORM_DRAIN ) )  DIRECT_RECHARGE = DIRECT_RECHARGE + fSTORM_DRAIN

      if ( allocated( fANNUAL_RECHARGE_RATE) )  DIRECT_RECHARGE = DIRECT_RECHARGE + fANNUAL_RECHARGE_RATE / 365.25_c_float

      ! write timestamp to NetCDF file
      call netcdf_put_variable_vector(NCFILE=pNCFILE, &
        iVarID=pNCFILE%iVarID(NC_TIME), &
        iStart=[int( iNumDaysFromOrigin, kind=c_size_t)], &
        iCount=[1_c_size_t], &
        iStride=[1_c_ptrdiff_t], &
        dpValues=[real( iNumDaysFromOrigin, kind=c_double)])

      call netcdf_put_packed_variable_array(NCFILE=pNCFILE, &
                   iVarID=pNCFILE%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(iNY, kind=c_size_t), int(iNX, kind=c_size_t)],         &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=DIRECT_RECHARGE, lMask=lActive, rField=nodata_fill_value )


    end associate

  end subroutine direct_recharge_calculate

end module direct_recharge__gridded_data
