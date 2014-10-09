!> @file
!! Contains the module \ref fog__monthly_grid.

!>
!!  Module \ref fog__monthly_grid
!!  provides support for estimating fog drip given a gridded map
!!  of FOG_ZONE and FOG_ELEVATION, and a table containing monthly
!!  fog factors. 
module fog__monthly_grid

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use dictionary
  use file_operations
  use netcdf4_support
  use simulation_datetime
  use strings
  use string_list

  implicit none

  private

  public :: fog_monthly_grid_initialize, fog_monthly_grid_calculate, pFOG_RATIO

  type (DATA_CATALOG_ENTRY_T), pointer :: pFOG_RATIO        ! data catalog object => FOG_RATIO grid
  real (kind=c_float), allocatable     :: FOG(:)           ! 
  type (T_NETCDF4_FILE), pointer       :: pNCFILE ! pointer to OUTPUT NetCDF file

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
  subroutine fog_monthly_grid_initialize( lActive, dX, dY, dX_lon, dY_lat )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)
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


    iNX = ubound(lActive, 1)
    iNY = ubound(lActive, 2)

    ! locate the data structure associated with the gridded fog ratio entries
    pFOG_RATIO => DAT%find("FOG_RATIO")
    if ( .not. associated(pFOG_RATIO) ) &
        call die("A FOG_RATIO grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    allocate ( pNCFILE, stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    call netcdf_open_and_prepare_as_output( NCFILE=pNCFILE, sVariableName="fog", &
      sVariableUnits="inches", iNX=iNX, iNY=iNY, &
      fX=dX, fY=dY, StartDate=SIM_DT%start, EndDate=SIM_DT%end, dpLat=dY_lat, dpLon=dX_lon  )


  end subroutine fog_monthly_grid_initialize

!--------------------------------------------------------------------------------------------------

  subroutine fog_monthly_grid_calculate( fRainfall, lActive, fDont_Care )

    real (kind=c_float), intent(in)        :: fRainfall(:)
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

      FOG = 0.0_c_float

      FOG = fRainfall * pack( pFOG_RATIO%pGrdBase%rData, lActive )

      call netcdf_put_packed_variable_array(NCFILE=pNCFILE, &
                   iVarID=pNCFILE%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(iNY, kind=c_size_t), int(iNX, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=FOG, lMask=lActive, rField=fDont_Care )
     
    end associate


  end subroutine fog_monthly_grid_calculate

end module fog__monthly_grid
