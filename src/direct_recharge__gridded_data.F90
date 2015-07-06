!> @file
!! Contains the module \ref direct_recharge__gridded_data.

!>
!!  Module \ref direct_recharge__gridded_data
!!  provides support for adding miscellaneous source and sink terms.

module direct_recharge__gridded_data

  use iso_c_binding, only       : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use dictionary
  use file_operations
  use netcdf4_support
  use parameters, only          : PARAMS
  use simulation_datetime
  use strings
  use string_list

  implicit none

  private

  public :: direct_recharge_initialize, direct_recharge_calculate
  public :: fCESSPOOL, fWATER_MAIN, fWATER_BODY_RECHARGE
  public :: fDISPOSAL_WELL, fSTORM_DRAIN

  type (DATA_CATALOG_ENTRY_T), pointer :: pCESSPOOL
  type (DATA_CATALOG_ENTRY_T), pointer :: pDISPOSAL_WELL
  type (DATA_CATALOG_ENTRY_T), pointer :: pSTORM_DRAIN
  type (DATA_CATALOG_ENTRY_T), pointer :: pWATER_BODY_RECHARGE
  type (DATA_CATALOG_ENTRY_T), pointer :: pWATER_MAIN

  real (kind=c_float), allocatable     :: fCESSPOOL(:)
  real (kind=c_float), allocatable     :: fDISPOSAL_WELL(:)
  real (kind=c_float), allocatable     :: fSTORM_DRAIN(:)
  real (kind=c_float), allocatable     :: fWATER_BODY_RECHARGE(:)
  real (kind=c_float), allocatable     :: fWATER_MAIN(:)

  real (kind=c_float), allocatable     :: fCESSPOOL_TABLE(:)
  real (kind=c_float), allocatable     :: fDISPOSAL_WELL_TABLE(:)
  real (kind=c_float), allocatable     :: fSTORM_DRAIN_TABLE(:)
  real (kind=c_float), allocatable     :: fWATER_BODY_RECHARGE_TABLE(:)
  real (kind=c_float), allocatable     :: fWATER_MAIN_TABLE(:)

contains

  !> Initialize the routine to enable input/output of arbitrary sources/sink terms. 
  !!
  !! Open gridded data file.
  !! Open a NetCDF output file to hold variable output.
  !!
  !! @param[in] lActive 2-D array of active cells within the model domain.
  !! @param[in] dX 1D vector of X coordinates associated with the model domain.
  !! @param[in] dY 1D vector of Y coordinates.
  !! @param[in] dX_lon 2D array of longitude values.
  !! @param[in] dY_lat 2D array of latitude values.

  subroutine direct_recharge_initialize( lActive, iLandUseIndex, dX, dY, dX_lon, dY_lat )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)
    integer (kind=c_int), intent(in)      :: iLandUseIndex(:)
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

    allocate( fCESSPOOL( count( lActive ) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( fDISPOSAL_WELL( count( lActive ) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( fWATER_MAIN( count( lActive ) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( fWATER_BODY_RECHARGE( count( lActive ) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( fSTORM_DRAIN( count( lActive ) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )
                
    fCESSPOOL = 0.0_c_float; fDISPOSAL_WELL = 0.0_c_float; fWATER_MAIN = 0.0_c_float
    fWATER_BODY_RECHARGE = 0.0_c_float; fSTORM_DRAIN = 0.0_c_float

    !> Determine how many landuse codes are present
    call slString%append( "LU_Code" )
    call slString%append( "Landuse_Code" )
    
    call PARAMS%get_parameters( slKeys=slString, iValues=iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes > 0 )

    call slString%clear()
    call slString%append( "Cesspool_direct_recharge" )
    call slString%append( "Cesspool_recharge" )
    call slString%append( "Cesspool_discharge" )
    call slString%append( "Cesspool_leakage" )    
    
    call PARAMS%get_parameters( slKeys=slString , fValues=fCESSPOOL_TABLE )

    pCESSPOOL => DAT%find( "CESSPOOL" )

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

    if ( fCESSPOOL_TABLE(1) > fTINYVAL ) then

      lAreLengthsEqual = ( ( ubound(fCESSPOOL_TABLE,1) == ubound(iLanduseCodes,1) )  )

      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of cesspool discharge/leakage values.",   &
          sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

      do iIndex=lbound( iLandUseIndex, 1 ), ubound( iLandUseIndex, 1 )
        fCESSPOOL( iIndex ) = fCESSPOOL_TABLE( iLandUseIndex( iIndex ) )
      enddo  

     endif

!     if ( fFOG_CATCH_EFFICIENCY(1) <= fTINYVAL )  &
!       call warn( "Failed to find a data column containing fog catch efficiency values.", lFATAL=lTRUE, &
!         iLogLevel=LOG_ALL )

!     lAreLengthsEqual = ( ( ubound(fFOG_CATCH_EFFICIENCY,1) == ubound(iLanduseCodes,1) )  )

!     if ( .not. lAreLengthsEqual )     &
!       call warn( sMessage="The number of landuses does not match the number of fog catch efficiency values.",   &
!         sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )


!     if ( .not. associated(pCESSPOOL) ) &
!         call die("A FOG_RATIO grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

!     allocate ( pNCFILE, stat=iStat )
!     call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )


!     !> open another netCDF file to hold fog interception
!     iNX = ubound(lActive, 1)
!     iNY = ubound(lActive, 2)

!     call netcdf_open_and_prepare_as_output( NCFILE=pNCFILE, sVariableName="fog", &
!       sVariableUnits="inches", iNX=iNX, iNY=iNY, &
!       fX=dX, fY=dY, StartDate=SIM_DT%start, EndDate=SIM_DT%end, dpLat=dY_lat, dpLon=dX_lon, &
!       fValidMin=0.0, fValidMax=2000.0   )


  end subroutine direct_recharge_initialize

!--------------------------------------------------------------------------------------------------

  subroutine direct_recharge_calculate( iLanduse_Index, lActive, fDont_Care )

    integer (kind=c_int), intent(in)       :: iLanduse_Index(:)
    logical (kind=c_bool), intent(in)      :: lActive(:,:)
    real (kind=c_float), intent(in)        :: fDont_Care(:,:)

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

    iNX = ubound(fDont_Care, 1)
    iNY = ubound(fDont_Care, 2)

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

!       ! write timestamp to NetCDF file
!       call netcdf_put_variable_vector(NCFILE=pNCFILE, &
!         iVarID=pNCFILE%iVarID(NC_TIME), &
!         iStart=[int( iNumDaysFromOrigin, kind=c_size_t)], &
!         iCount=[1_c_size_t], &
!         iStride=[1_c_ptrdiff_t], &
!         dpValues=[real( iNumDaysFromOrigin, kind=c_double)])


!       call netcdf_put_packed_variable_array(NCFILE=pNCFILE, &
!                    iVarID=pNCFILE%iVarID(NC_Z), &
!                    iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
!                    iCount=[1_c_size_t, int(iNY, kind=c_size_t), int(iNX, kind=c_size_t)],              &
!                    iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
!                    rValues=fFog, lMask=lActive, rField=fDont_Care )

    end associate

  end subroutine direct_recharge_calculate

end module direct_recharge__gridded_data
