!> @file
!! Contains the module \ref fog__monthly_grid.

!>
!!  Module \ref fog__monthly_grid
!!  provides support for estimating fog drip given a gridded map
!!  of FOG_RATIO and FOG_CAPTURE_EFFICIENCY.

module fog__monthly_grid

  use iso_c_binding
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use dictionary
  use exceptions
  use file_operations
  use logfiles, only            : LOGS, LOG_ALL
  use parameters, only          : PARAMS
  use simulation_datetime
  use strings
  use string_list

  implicit none

  private

  public :: fog_monthly_grid_initialize, fog_monthly_grid_calculate, pFOG_RATIO

  type (DATA_CATALOG_ENTRY_T), pointer :: pFOG_RATIO
  real (c_float), allocatable     :: fFOG_CATCH_EFFICIENCY(:)

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

  subroutine fog_monthly_grid_initialize( lActive )

    logical (c_bool), intent(in)     :: lActive(:,:)

    ! [ LOCALS ]
    integer (c_int)                 :: iStat
    type (STRING_LIST_T)                 :: slString
    integer (c_int)                 :: iIndex
    integer (c_int), allocatable    :: iLanduseCodes(:)
    integer (c_int)                 :: iNumberOfLanduses
    logical (c_bool)                :: lAreLengthsEqual

    ! locate the data structure associated with the gridded fog ratio entries
    pFOG_RATIO => DAT%find("FOG_RATIO")
    if ( .not. associated(pFOG_RATIO) ) &
        call die("A FOG_RATIO grid must be supplied in order to make use of this option.", __SRCNAME__, __LINE__)

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
        sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )

  end subroutine fog_monthly_grid_initialize

!--------------------------------------------------------------------------------------------------

  subroutine fog_monthly_grid_calculate( fRainfall, fFog, iLanduse_Index, lActive, nodata_fill_value )

    real (c_float), intent(in)        :: fRainfall(:)
    real (c_float), intent(inout)     :: fFog(:)
    integer (c_int), intent(in)       :: iLanduse_Index(:)
    logical (c_bool), intent(in)      :: lActive(:,:)
    real (c_float), intent(in)        :: nodata_fill_value(:,:)

    ! [ LOCALS ]
    integer (c_int) :: iIndex
    real (c_float)  :: fFactor

    associate ( dt => SIM_DT%curr )

      if ( .not. associated(pFOG_RATIO) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)

      if ( .not. allocated(pFOG_RATIO%pGrdBase%rData) ) &
        call die("INTERNAL PROGRAMMING ERROR: attempted use of unallocated variable", __SRCNAME__, __LINE__)

      call pFOG_RATIO%getvalues( dt )

      fFog = fRainfall * pack( pFOG_RATIO%pGrdBase%rData, lActive )   &
                       * fFOG_CATCH_EFFICIENCY( iLanduse_Index )

    end associate

  end subroutine fog_monthly_grid_calculate

end module fog__monthly_grid
