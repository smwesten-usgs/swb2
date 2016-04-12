!> @file
!! Contains the module \ref fog__monthly_grid.

!>
!!  Module \ref fog__monthly_grid
!!  provides support for estimating fog drip given a gridded map
!!  of FOG_RATIO and FOG_CAPTURE_EFFICIENCY.

module fog__monthly_grid

  use iso_c_binding, only       : c_short, c_int, c_float, c_double, c_size_t, c_ptrdiff_t
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use dictionary
  use exceptions
  use file_operations
  use logfiles, only            : LOGS, LOG_ALL
  use netcdf4_support
  use parameters, only          : PARAMS
  use simulation_datetime
  use strings
  use string_list

  implicit none

  private

  public :: fog_monthly_grid_initialize, fog_monthly_grid_calculate, pFOG_RATIO

  type (DATA_CATALOG_ENTRY_T), pointer :: pFOG_RATIO
  real (kind=c_float), allocatable     :: fFOG_CATCH_EFFICIENCY(:)
  type (T_NETCDF4_FILE), pointer       :: pNCFILE

contains

  !> Initialize the fog drip algorithm. 
  !!
  !! Read in a fog ratio grid.
  !! Open a NetCDF output file to hold fog variable output.
  !!
  !! @param[in] lActive 2-D array of active cells within the model domain.
  !! @param[in] dX 1D vector of X coordinates associated with the model domain.
  !! @param[in] dY 1D vector of Y coordinates.
  !! @param[in] dX_lon 2D array of longitude values.
  !! @param[in] dY_lat 2D array of latitude values.

  subroutine fog_monthly_grid_initialize( lActive, dX, dY,                          &
                                          dX_lon, dY_lat, output_directory_name )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)
    real (kind=c_double), intent(in)      :: dX(:)
    real (kind=c_double), intent(in)      :: dY(:)
    real (kind=c_double), intent(in)      :: dX_lon(:,:)
    real (kind=c_double), intent(in)      :: dY_lat(:,:)
    character (len=*), intent(in)         :: output_directory_name

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    type (STRING_LIST_T)                 :: slString
    integer (kind=c_int)                 :: iIndex 
    integer (kind=c_int)                 :: iNX
    integer (kind=c_int)                 :: iNY
    integer (kind=c_int), allocatable    :: iLanduseCodes(:)
    integer (kind=c_int)                 :: iNumberOfLanduses
    logical (kind=c_bool)                :: lAreLengthsEqual

    ! locate the data structure associated with the gridded fog ratio entries
    pFOG_RATIO => DAT%find("FOG_RATIO")
    if ( .not. associated(pFOG_RATIO) ) &
        call die("A FOG_RATIO grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    allocate ( pNCFILE, stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    !> Determine how many landuse codes are present
    call slString%append("LU_Code")
    call slString%append("Landuse_Code")
    
    call PARAMS%get_parameters( slKeys=slString, iValues=iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes > 0 )

    call slString%clear()

    call slString%append("Fog_catch_eff")
    call slString%append("Fog_catch_efficiency")
    
    call PARAMS%get_parameters( slKeys=slString , fValues=fFOG_CATCH_EFFICIENCY )

    if ( fFOG_CATCH_EFFICIENCY(1) <= fTINYVAL )  &
      call warn( "Failed to find a data column containing fog catch efficiency values.", lFATAL=lTRUE, &
        iLogLevel=LOG_ALL )

    lAreLengthsEqual = ( ( ubound(fFOG_CATCH_EFFICIENCY,1) == ubound(iLanduseCodes,1) )  )

    if ( .not. lAreLengthsEqual )     &
      call warn( sMessage="The number of landuses does not match the number of fog catch efficiency values.",   &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

    !> open another netCDF file to hold fog interception
    iNX = ubound(lActive, 1)
    iNY = ubound(lActive, 2)

    call netcdf_open_and_prepare_as_output( NCFILE=pNCFILE, sVariableName="fog", &
      sVariableUnits="inches", iNX=iNX, iNY=iNY, &
      fX=dX, fY=dY, StartDate=SIM_DT%start, EndDate=SIM_DT%end, dpLat=dY_lat, dpLon=dX_lon, &
      fValidMin=0.0, fValidMax=2000.0, sDirName=output_directory_name )


  end subroutine fog_monthly_grid_initialize

!--------------------------------------------------------------------------------------------------

  subroutine fog_monthly_grid_calculate( fRainfall, fFog, iLanduse_Index, lActive, nodata_fill_value )

    real (kind=c_float), intent(in)        :: fRainfall(:)
    real (kind=c_float), intent(inout)     :: fFog(:)
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

      if ( .not. associated(pFOG_RATIO) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __FILE__, __LINE__)

      if ( .not. allocated(pFOG_RATIO%pGrdBase%rData) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of unallocated variable", __FILE__, __LINE__)

      call pFOG_RATIO%getvalues( iMonth, iDay, iYear, iJulianDay )

      ! write timestamp to NetCDF file
      call netcdf_put_variable_vector(NCFILE=pNCFILE, &
        iVarID=pNCFILE%iVarID(NC_TIME), &
        iStart=[int( iNumDaysFromOrigin, kind=c_size_t)], &
        iCount=[1_c_size_t], &
        iStride=[1_c_ptrdiff_t], &
        dpValues=[real( iNumDaysFromOrigin, kind=c_double)])

      fFog = fRainfall * pack( pFOG_RATIO%pGrdBase%rData, lActive )   &
                       * fFOG_CATCH_EFFICIENCY( iLanduse_Index )

      call netcdf_put_packed_variable_array(NCFILE=pNCFILE, &
                   iVarID=pNCFILE%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(iNY, kind=c_size_t), int(iNX, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=fFog, lMask=lActive, rField=nodata_fill_value )

    end associate

  end subroutine fog_monthly_grid_calculate

end module fog__monthly_grid
