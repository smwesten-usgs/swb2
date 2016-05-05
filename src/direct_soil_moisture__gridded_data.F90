!> @file
!! Contains the module \ref direct_soil_moisture__gridded_data.

!>
!!  Module \ref direct_soil_moisture__gridded_data
!!  provides support for adding miscellaneous source and sink terms.

module direct_soil_moisture__gridded_data

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

  public :: direct_soil_moisture_initialize, direct_soil_moisture_calculate

  type (DATA_CATALOG_ENTRY_T), pointer :: pSEPTIC_DISCHARGE
  type (DATA_CATALOG_ENTRY_T), pointer :: pANNUAL_SEPTIC_DISCHARGE

  real (kind=c_float), allocatable     :: fSEPTIC_DISCHARGE(:)
  real (kind=c_float), allocatable     :: fANNUAL_SEPTIC_DISCHARGE(:)  

  real (kind=c_float), allocatable     :: fSEPTIC_DISCHARGE_TABLE(:)
  real (kind=c_float), allocatable     :: fANNUAL_SEPTIC_DISCHARGE_TABLE(:)  

  type (T_NETCDF4_FILE), pointer       :: pNCFILE

contains

  !> Initialize the routine to enable input/output of arbitrary sources/sink terms 
  !! to be added directly to SOIL MOISTURE. 
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

  subroutine direct_soil_moisture_initialize( lActive, iLandUseIndex, PROJ4_string,      &
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
    call slString%append( "Septic_system_discharge" )
    call slString%append( "SEPTIC_DISCHARGE" )
    call slString%append( "Daily_septic_discharge" )

    call PARAMS%get_parameters( slKeys=slString , fValues=fSEPTIC_DISCHARGE_TABLE )

    ! attempt to find a source of GRIDDED SEPTIC DISCHARGE data
    pSEPTIC_DISCHARGE => DAT%find( "ANNUAL_direct_soil_moisture_RATE" )

    ! look for data in the form of a grid
    if ( associated( pSEPTIC_DISCHARGE ) ) then

      allocate( fSEPTIC_DISCHARGE( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    ! no grid? then look for a table version; values > TINYVAL indicate that 
    ! something is present
    elseif ( fSEPTIC_DISCHARGE_TABLE(1) > fTINYVAL ) then

      lAreLengthsEqual = ( ( ubound(fSEPTIC_DISCHARGE_TABLE,1) == ubound(iLanduseCodes,1) )  )

      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of annual direct"   &
          //" recharge rate values.", sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fSEPTIC_DISCHARGE( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

      ! now populate the vector of cell values
      do iIndex=lbound( iLandUseIndex, 1 ), ubound( iLandUseIndex, 1 )
        fSEPTIC_DISCHARGE( iIndex ) = fSEPTIC_DISCHARGE_TABLE( iLandUseIndex( iIndex ) )
      enddo  

     endif


    call slString%clear()
    call slString%append( "ANNUAL_septic_system_discharge" )
    call slString%append( "ANNUAL_SEPTIC_DISCHARGE" )
    call slString%append( "ANNUAL_septic_discharge" )

    call PARAMS%get_parameters( slKeys=slString , fValues=fANNUAL_SEPTIC_DISCHARGE_TABLE )

    ! attempt to find a source of GRIDDED ANNUAL SEPTIC DISCHARGE data
    pANNUAL_SEPTIC_DISCHARGE => DAT%find( "ANNUAL_SEPTIC_DISCHARGE" )

    ! look for data in the form of a grid
    if ( associated( pANNUAL_SEPTIC_DISCHARGE ) ) then

      allocate( fANNUAL_SEPTIC_DISCHARGE( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    ! no grid? then look for a table version; values > TINYVAL indicate that 
    ! something is present
    elseif ( fANNUAL_SEPTIC_DISCHARGE_TABLE(1) > fTINYVAL ) then

      lAreLengthsEqual = ( ( ubound(fANNUAL_SEPTIC_DISCHARGE_TABLE,1) == ubound(iLanduseCodes,1) )  )

      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of annual direct"   &
          //" recharge rate values.", sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fANNUAL_SEPTIC_DISCHARGE( count( lActive ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

      ! now populate the vector of cell values
      do iIndex=lbound( iLandUseIndex, 1 ), ubound( iLandUseIndex, 1 )
        fANNUAL_SEPTIC_DISCHARGE( iIndex ) = fANNUAL_SEPTIC_DISCHARGE_TABLE( iLandUseIndex( iIndex ) )
      enddo  

     endif

    !> open another netCDF file to hold total direct recharge
    iNX = ubound(lActive, 1)
    iNY = ubound(lActive, 2)

    allocate ( pNCFILE, stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    call netcdf_open_and_prepare_as_output( NCFILE=pNCFILE, sVariableName="direct_soil_moisture",  &
      sVariableUnits="inches", iNX=iNX, iNY=iNY,                                              &
      fX=dX, fY=dY, StartDate=SIM_DT%start, EndDate=SIM_DT%end, PROJ4_string=PROJ4_string,    &
      dpLat=dY_lat, dpLon=dX_lon, fValidMin=0.0, fValidMax=2000.0   )

  end subroutine direct_soil_moisture_initialize

!--------------------------------------------------------------------------------------------------

  subroutine direct_soil_moisture_calculate( direct_soil_moisture, iLanduse_Index, lActive, nodata_fill_value )

    real (kind=c_float), intent(inout)     :: direct_soil_moisture(:)
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

      if ( associated( pSEPTIC_DISCHARGE ) ) then
        call pSEPTIC_DISCHARGE%getvalues( iMonth, iDay, iYear, iJulianDay )
        if ( pSEPTIC_DISCHARGE%lGridHasChanged ) fSEPTIC_DISCHARGE = pack( pSEPTIC_DISCHARGE%pGrdBase%rData, lActive )
      endif      

      if ( associated( pANNUAL_SEPTIC_DISCHARGE ) ) then
        call pANNUAL_SEPTIC_DISCHARGE%getvalues( iMonth, iDay, iYear, iJulianDay )
        if ( pANNUAL_SEPTIC_DISCHARGE%lGridHasChanged ) fANNUAL_SEPTIC_DISCHARGE = pack( pANNUAL_SEPTIC_DISCHARGE%pGrdBase%rData, lActive )
      endif      

      direct_soil_moisture = 0.0_c_float

      if ( allocated( fSEPTIC_DISCHARGE) )  direct_soil_moisture = direct_soil_moisture + fSEPTIC_DISCHARGE
      if ( allocated( fANNUAL_SEPTIC_DISCHARGE) )  direct_soil_moisture = direct_soil_moisture + fANNUAL_SEPTIC_DISCHARGE


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
                   rValues=direct_soil_moisture, lMask=lActive, rField=nodata_fill_value )


    end associate

  end subroutine direct_soil_moisture_calculate

end module direct_soil_moisture__gridded_data
