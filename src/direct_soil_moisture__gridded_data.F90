!> @file
!! Contains the module \ref direct_soil_moisture__gridded_data.

!>
!!  Module \ref direct_soil_moisture__gridded_data
!!  provides support for adding miscellaneous source and sink terms.

module direct_soil_moisture__gridded_data

  use iso_c_binding, only       : c_short, c_int, c_float, c_double, c_size_t, c_ptrdiff_t
  use constants_and_conversions
  use datetime
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

  type ( DATETIME_T ), pointer         :: DATE_OF_LAST_RETRIEVAL

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

  subroutine direct_soil_moisture_initialize( is_cell_active, landuse_index )

    logical (kind=c_bool), intent(in)     :: is_cell_active(:,:)
    integer (kind=c_int), intent(in)      :: landuse_index(:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    type (STRING_LIST_T)                 :: parameter_list
    integer (kind=c_int)                 :: iIndex 
    integer (kind=c_int)                 :: iNX
    integer (kind=c_int)                 :: iNY
    integer (kind=c_int), allocatable    :: iLanduseCodes(:)
    integer (kind=c_int)                 :: iNumberOfLanduses
    logical (kind=c_bool)                :: lAreLengthsEqual


    !> Determine how many landuse codes are present
    call parameter_list%append( "LU_Code" )
    call parameter_list%append( "Landuse_Code" )

    call PARAMS%get_parameters( slKeys=parameter_list, iValues=iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes > 0 )

    call parameter_list%clear()
    call parameter_list%append( "Septic_system_discharge" )
    call parameter_list%append( "SEPTIC_DISCHARGE" )
    call parameter_list%append( "Daily_septic_discharge" )

    call PARAMS%get_parameters( slKeys=parameter_list , fValues=fSEPTIC_DISCHARGE_TABLE )

    ! attempt to find a source of GRIDDED SEPTIC DISCHARGE data
    pSEPTIC_DISCHARGE => DAT%find( "SEPTIC_DISCHARGE" )

    ! look for data in the form of a grid
    if ( associated( pSEPTIC_DISCHARGE ) ) then

      allocate( fSEPTIC_DISCHARGE( count( is_cell_active ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    ! no grid? then look for a table version; values > TINYVAL indicate that 
    ! something is present
    elseif ( fSEPTIC_DISCHARGE_TABLE(1) > fTINYVAL ) then

      lAreLengthsEqual = ( ( ubound(fSEPTIC_DISCHARGE_TABLE,1) == ubound(iLanduseCodes,1) )  )

      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of annual direct"   &
          //" recharge rate values.", sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fSEPTIC_DISCHARGE( count( is_cell_active ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

      ! now populate the vector of cell values
      do iIndex=lbound( landuse_index, 1 ), ubound( landuse_index, 1 )
        fSEPTIC_DISCHARGE( iIndex ) = fSEPTIC_DISCHARGE_TABLE( landuse_index( iIndex ) )
      enddo  

     endif

    call parameter_list%clear()
    call parameter_list%append( "ANNUAL_septic_system_discharge" )
    call parameter_list%append( "ANNUAL_SEPTIC_DISCHARGE" )
    call parameter_list%append( "ANNUAL_septic_discharge" )

    call PARAMS%get_parameters( slKeys=parameter_list , fValues=fANNUAL_SEPTIC_DISCHARGE_TABLE )

    ! attempt to find a source of GRIDDED ANNUAL SEPTIC DISCHARGE data
    pANNUAL_SEPTIC_DISCHARGE => DAT%find( "ANNUAL_SEPTIC_DISCHARGE" )

    ! look for data in the form of a grid
    if ( associated( pANNUAL_SEPTIC_DISCHARGE ) ) then

      allocate( fANNUAL_SEPTIC_DISCHARGE( count( is_cell_active ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    ! no grid? then look for a table version; values > TINYVAL indicate that 
    ! something is present
    elseif ( fANNUAL_SEPTIC_DISCHARGE_TABLE(1) > fTINYVAL ) then

      lAreLengthsEqual = ( ( ubound(fANNUAL_SEPTIC_DISCHARGE_TABLE,1) == ubound(iLanduseCodes,1) )  )

      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of annual direct"   &
          //" recharge rate values.", sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fANNUAL_SEPTIC_DISCHARGE( count( is_cell_active ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

      ! now populate the vector of cell values
      do iIndex=lbound( landuse_index, 1 ), ubound( landuse_index, 1 )
        fANNUAL_SEPTIC_DISCHARGE( iIndex ) = fANNUAL_SEPTIC_DISCHARGE_TABLE( landuse_index( iIndex ) )
      enddo  

     endif

     ! initialize last retrieval date to something implausibly low to trigger initial read
     ! in the calculate procedure
     call DATE_OF_LAST_RETRIEVAL%parseDate("01/01/1000")

  end subroutine direct_soil_moisture_initialize

!--------------------------------------------------------------------------------------------------

  subroutine direct_soil_moisture_calculate( direct_soil_moisture, is_cell_active, indx )

    real (kind=c_float), intent(inout)     :: direct_soil_moisture
    logical (kind=c_bool), intent(in)      :: is_cell_active(:,:)
    integer (kind=c_int), intent(in)       :: indx

    ! [ LOCALS ] 
    integer (kind=c_int) :: iJulianDay
    integer (kind=c_int) :: iMonth
    integer (kind=c_int) :: iDay
    integer (kind=c_int) :: iYear
    integer (kind=c_int) :: iDaysInMonth
    integer (kind=c_int) :: iNumDaysFromOrigin
    integer (kind=c_int) :: iIndex
    real (kind=c_float)  :: fFactor

    if ( .not. DATE_OF_LAST_RETRIEVAL == SIM_DT%curr ) then

      associate ( dt => SIM_DT%curr )

        iJulianDay = dt%getJulianDay()
        iMonth = asInt( dt%iMonth )
        iDay = asInt( dt%iDay )
        iYear = dt%iYear
        iDaysInMonth = SIM_DT%iDaysInMonth
        iNumDaysFromOrigin = SIM_DT%iNumDaysFromOrigin

        if ( associated( pSEPTIC_DISCHARGE ) ) then
          call pSEPTIC_DISCHARGE%getvalues( iMonth, iDay, iYear, iJulianDay )
          if ( pSEPTIC_DISCHARGE%lGridHasChanged ) fSEPTIC_DISCHARGE =                   &
               pack( pSEPTIC_DISCHARGE%pGrdBase%rData, is_cell_active )
        endif      

        if ( associated( pANNUAL_SEPTIC_DISCHARGE ) ) then
          call pANNUAL_SEPTIC_DISCHARGE%getvalues( iMonth, iDay, iYear, iJulianDay )
          if ( pANNUAL_SEPTIC_DISCHARGE%lGridHasChanged ) fANNUAL_SEPTIC_DISCHARGE =     &
              pack( pANNUAL_SEPTIC_DISCHARGE%pGrdBase%rData, is_cell_active )
        endif      

      end associate

      DATE_OF_LAST_RETRIEVAL = SIM_DT%curr

    endif

    direct_soil_moisture = 0.0_c_float

    if ( allocated( fSEPTIC_DISCHARGE) )  direct_soil_moisture = direct_soil_moisture           &
                                            + fSEPTIC_DISCHARGE( indx )
    if ( allocated( fANNUAL_SEPTIC_DISCHARGE) )  direct_soil_moisture = direct_soil_moisture    &
                                            + fANNUAL_SEPTIC_DISCHARGE( indx ) / 365.25


  end subroutine direct_soil_moisture_calculate

end module direct_soil_moisture__gridded_data
