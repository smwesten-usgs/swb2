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
  use datetime
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
  public :: DIRECT_RECHARGE_ACTIVE_FRACTION

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

  ! ****_TABLE variables: will have same number of values as there are landuses
  real (kind=c_float), allocatable     :: fCESSPOOL_TABLE(:)
  real (kind=c_float), allocatable     :: fDISPOSAL_WELL_TABLE(:)
  real (kind=c_float), allocatable     :: fSTORM_DRAIN_TABLE(:)
  real (kind=c_float), allocatable     :: fWATER_BODY_RECHARGE_TABLE(:)
  real (kind=c_float), allocatable     :: fWATER_MAIN_TABLE(:)
  real (kind=c_float), allocatable     :: fANNUAL_RECHARGE_RATE_TABLE(:)

  real (kind=c_float), allocatable     :: DIRECT_RECHARGE_ACTIVE_FRACTION_TABLE(:)
  real (kind=c_float), allocatable     :: DIRECT_RECHARGE_ACTIVE_FRACTION(:)

  type (T_NETCDF4_FILE), pointer       :: pNCFILE

  type ( DATETIME_T )                  :: DATE_OF_LAST_RETRIEVAL
  
contains

  !> Initialize the routine to enable input/output of arbitrary sources/sink terms. 
  !!
  !! Open gridded data file.
  !! Open a NetCDF output file to hold variable output.
  !!
  !! @param[in] is_cell_active 2D array of active cells within the model domain.
  !! @param[in] iLanduseIndex 1D vector of indices corresponding to rows of the
  !!            landuse lookup table(s).
  !! @param[in] dX 1D vector of X coordinates associated with the model domain.
  !! @param[in] dY 1D vector of Y coordinates.
  !! @param[in] dX_lon 2D array of longitude values.
  !! @param[in] dY_lat 2D array of latitude values.

  subroutine direct_recharge_initialize( is_cell_active, landuse_index )

    logical (kind=c_bool), intent(in)     :: is_cell_active(:,:)
    integer (kind=c_int), intent(in)      :: landuse_index(:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: status
    type (STRING_LIST_T)                 :: parameter_list
    integer (kind=c_int)                 :: indx 
    integer (kind=c_int)                 :: iNX
    integer (kind=c_int)                 :: iNY
    integer (kind=c_int), allocatable    :: landuse_codes(:)
    integer (kind=c_int)                 :: number_of_landuses
    logical (kind=c_bool)                :: are_lengths_equal


    !> Determine how many landuse codes are present
    call parameter_list%append( "LU_Code" )
    call parameter_list%append( "Landuse_Code" )

    call PARAMS%get_parameters( slKeys=parameter_list, iValues=landuse_codes )
    number_of_landuses = count( landuse_codes > 0 )

    call parameter_list%clear()
    call parameter_list%append( "Annual_direct_recharge_rate" )
    call parameter_list%append( "Annual_recharge_rate" )
    call parameter_list%append( "Annual_direct_recharge" )

    call PARAMS%get_parameters( slKeys=parameter_list , fValues=fANNUAL_RECHARGE_RATE_TABLE )

    ! attempt to find a source of GRIDDED ANNUAL DIRECT RECHARGE data
    pANNUAL_RECHARGE_RATE => DAT%find( "ANNUAL_DIRECT_RECHARGE_RATE" )


    call parameter_list%clear()
    call parameter_list%append( "Cesspool_direct_recharge" )
    call parameter_list%append( "Cesspool_recharge" )
    call parameter_list%append( "Cesspool_discharge" )
    call parameter_list%append( "Cesspool_leakage" )    
    
    call PARAMS%get_parameters( slKeys=parameter_list , fValues=fCESSPOOL_TABLE )

    ! attempt to find a source of GRIDDED CESSPOOL data
    pCESSPOOL => DAT%find( "CESSPOOL_LEAKAGE" )


    call parameter_list%clear()
    call parameter_list%append( "Storm_drain_discharge" )
    call parameter_list%append( "Storm_drain_recharge" )
    call parameter_list%append( "Storm_drain_leakage" )    
    
    call PARAMS%get_parameters( slKeys=parameter_list , fValues=fSTORM_DRAIN_TABLE )

    pSTORM_DRAIN => DAT%find( "STORM_DRAIN" )


    call parameter_list%clear()
    call parameter_list%append( "Water_body_recharge" )
    call parameter_list%append( "Water_body_discharge" )
    call parameter_list%append( "Water_body_leakage" )    
    
    call PARAMS%get_parameters( slKeys=parameter_list , fValues=fWATER_BODY_RECHARGE_TABLE )

    pWATER_BODY_RECHARGE => DAT%find( "WATER_BODY_RECHARGE" )


    call parameter_list%clear()
    call parameter_list%append( "Water_main_recharge" )
    call parameter_list%append( "Water_main_discharge" )
    call parameter_list%append( "Water_main_leakage" )
    
    call PARAMS%get_parameters( slKeys=parameter_list , fValues=fWATER_MAIN_TABLE )

    pWATER_MAIN => DAT%find( "WATER_MAIN_LEAKAGE" )


    call parameter_list%clear()
    call parameter_list%append( "Disposal_well_recharge" )
    call parameter_list%append( "Disposal_well_discharge" )
    
    call PARAMS%get_parameters( slKeys=parameter_list , fValues=fDISPOSAL_WELL_TABLE )

    pDISPOSAL_WELL => DAT%find( "DISPOSAL_WELL_DISCHARGE" )


    call PARAMS%get_parameters( sKey="Direct_recharge_active_fraction",        &
                                fValues=DIRECT_RECHARGE_ACTIVE_FRACTION_TABLE )


    if ( DIRECT_RECHARGE_ACTIVE_FRACTION_TABLE(1) > fTINYVAL ) then

      are_lengths_equal = ( ubound(DIRECT_RECHARGE_ACTIVE_FRACTION_TABLE,1)         &
                                 == ubound(landuse_codes,1) )

      if ( .not. are_lengths_equal )     &
        call warn( sMessage="The number of landuses does not match the number of direct"   &
          //" recharge active fraction values.", sModule=__SRCNAME__, iLine=__LINE__, lFatal=TRUE )

      allocate( DIRECT_RECHARGE_ACTIVE_FRACTION( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

      do indx=lbound( landuse_index, 1 ), ubound( landuse_index, 1 )
        DIRECT_RECHARGE_ACTIVE_FRACTION( indx ) = DIRECT_RECHARGE_ACTIVE_FRACTION_TABLE( landuse_index( indx ) )
      enddo  
    else

      allocate( DIRECT_RECHARGE_ACTIVE_FRACTION( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )
      DIRECT_RECHARGE_ACTIVE_FRACTION = 0.0_c_float

    endif


    if ( associated( pANNUAL_RECHARGE_RATE ) ) then

      allocate( fANNUAL_RECHARGE_RATE( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    elseif ( fANNUAL_RECHARGE_RATE_TABLE(1) > fTINYVAL ) then

      are_lengths_equal = ( ( ubound(fANNUAL_RECHARGE_RATE_TABLE,1) == ubound(landuse_codes,1) )  )

      if ( .not. are_lengths_equal )     &
        call warn( sMessage="The number of landuses does not match the number of annual direct"   &
          //" recharge rate values.", sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fANNUAL_RECHARGE_RATE( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

      do indx=lbound( landuse_index, 1 ), ubound( landuse_index, 1 )
        fANNUAL_RECHARGE_RATE( indx ) = fANNUAL_RECHARGE_RATE_TABLE( landuse_index( indx ) )
      enddo  

     endif


    if ( associated( pCESSPOOL ) ) then

      allocate( fCESSPOOL( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    elseif ( fCESSPOOL_TABLE(1) > fTINYVAL ) then

      are_lengths_equal = ( ( ubound(fCESSPOOL_TABLE,1) == ubound(landuse_codes,1) )  )

      if ( .not. are_lengths_equal )     &
        call warn( sMessage="The number of landuses does not match the number of cesspool discharge/leakage values.",   &
          sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fCESSPOOL( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

      do indx=lbound( landuse_index, 1 ), ubound( landuse_index, 1 )
        fCESSPOOL( indx ) = fCESSPOOL_TABLE( landuse_index( indx ) )
      enddo  

     endif


    if ( associated( pSTORM_DRAIN ) ) then

      allocate( fSTORM_DRAIN( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    elseif ( fSTORM_DRAIN_TABLE(1) > fTINYVAL ) then

      are_lengths_equal = ( ( ubound(fSTORM_DRAIN_TABLE,1) == ubound(landuse_codes,1) )  )

      if ( .not. are_lengths_equal )     &
        call warn( sMessage="The number of landuses does not match the number of storm drain discharge/leakage values.",   &
          sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fSTORM_DRAIN( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

      do indx=lbound( landuse_index, 1 ), ubound( landuse_index, 1 )
        fSTORM_DRAIN( indx ) = fSTORM_DRAIN_TABLE( landuse_index( indx ) )
      enddo  

     endif


    if ( associated( pWATER_BODY_RECHARGE ) ) then

      allocate( fWATER_BODY_RECHARGE( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    elseif ( fWATER_BODY_RECHARGE_TABLE(1) > fTINYVAL ) then

      are_lengths_equal = ( ( ubound(fWATER_BODY_RECHARGE_TABLE,1) == ubound(landuse_codes,1) )  )

      if ( .not. are_lengths_equal )     &
        call warn( sMessage="The number of landuses does not match the number of water body recharge/leakage values.",   &
          sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fWATER_BODY_RECHARGE( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

      do indx=lbound( landuse_index, 1 ), ubound( landuse_index, 1 )
        fWATER_BODY_RECHARGE( indx ) = fWATER_BODY_RECHARGE_TABLE( landuse_index( indx ) )
      enddo  

     endif


    if ( associated( pWATER_MAIN ) ) then

      allocate( fWATER_MAIN( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    elseif ( fWATER_MAIN_TABLE(1) > fTINYVAL ) then

      are_lengths_equal = ( ( ubound(fWATER_MAIN_TABLE,1) == ubound(landuse_codes,1) )  )

      if ( .not. are_lengths_equal )     &
        call warn( sMessage="The number of landuses does not match the number of water main leakage values.",   &
          sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fWATER_MAIN( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

      do indx=lbound( landuse_index, 1 ), ubound( landuse_index, 1 )
        fWATER_MAIN( indx ) = fWATER_MAIN_TABLE( landuse_index( indx ) )
      enddo  

     endif


    if ( associated( pDISPOSAL_WELL ) ) then

      allocate( fDISPOSAL_WELL( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    elseif ( fDISPOSAL_WELL_TABLE(1) > fTINYVAL ) then

      are_lengths_equal = ( ( ubound(fDISPOSAL_WELL_TABLE,1) == ubound(landuse_codes,1) )  )

      if ( .not. are_lengths_equal )     &
        call warn( sMessage="The number of landuses does not match the number of discharge well values.",   &
          sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )

      allocate( fDISPOSAL_WELL( count( is_cell_active ) ), stat=status )
      call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

      do indx=lbound( landuse_index, 1 ), ubound( landuse_index, 1 )
        fDISPOSAL_WELL( indx ) = fDISPOSAL_WELL_TABLE( landuse_index( indx ) )
      enddo  

     endif

    ! initialize last retrieval date to something implausibly low to trigger initial read
    ! in the calculate procedure
    call DATE_OF_LAST_RETRIEVAL%parseDate("01/01/1000", __SRCNAME__, __LINE__)

  end subroutine direct_recharge_initialize

!--------------------------------------------------------------------------------------------------

  subroutine direct_recharge_calculate( direct_recharge, indx, is_cell_active, nodata_fill_value )

    real (kind=c_float), intent(inout)     :: direct_recharge
    integer (kind=c_int), intent(in)       :: indx
    logical (kind=c_bool), intent(in)      :: is_cell_active(:,:)
    real (kind=c_float), intent(in)        :: nodata_fill_value(:,:)

    ! [ LOCALS ] 
    integer (kind=c_int) :: iJulianDay
    integer (kind=c_int) :: iMonth
    integer (kind=c_int) :: iDay
    integer (kind=c_int) :: iYear
    integer (kind=c_int) :: iDaysInMonth
    integer (kind=c_int) :: iNumDaysFromOrigin
    real (kind=c_float)  :: fFactor

    if ( .not. DATE_OF_LAST_RETRIEVAL == SIM_DT%curr ) then

      associate ( dt => SIM_DT%curr )

        iJulianDay = dt%getJulianDay()
        iMonth = asInt( dt%iMonth )
        iDay = asInt( dt%iDay )
        iYear = dt%iYear
        iDaysInMonth = SIM_DT%iDaysInMonth
        iNumDaysFromOrigin = SIM_DT%iNumDaysFromOrigin

        if ( associated( pCESSPOOL ) ) then
          call pCESSPOOL%getvalues( iMonth, iDay, iYear, iJulianDay )
          if ( pCESSPOOL%lGridHasChanged ) fCESSPOOL = pack( pCESSPOOL%pGrdBase%rData, is_cell_active )
        endif

        if ( associated( pDISPOSAL_WELL ) ) then
          call pDISPOSAL_WELL%getvalues( iMonth, iDay, iYear, iJulianDay )
          if ( pDISPOSAL_WELL%lGridHasChanged ) fDISPOSAL_WELL = pack( pDISPOSAL_WELL%pGrdBase%rData, is_cell_active )
        endif

        if ( associated( pSTORM_DRAIN ) ) then
          call pSTORM_DRAIN%getvalues( iMonth, iDay, iYear, iJulianDay )
          if ( pSTORM_DRAIN%lGridHasChanged ) fSTORM_DRAIN = pack( pSTORM_DRAIN%pGrdBase%rData, is_cell_active )
        endif

        if ( associated( pWATER_BODY_RECHARGE ) ) then
          call pWATER_BODY_RECHARGE%getvalues( iMonth, iDay, iYear, iJulianDay )
          if ( pWATER_BODY_RECHARGE%lGridHasChanged ) fWATER_BODY_RECHARGE = pack( pWATER_BODY_RECHARGE%pGrdBase%rData, is_cell_active )
        endif

        if ( associated( pWATER_MAIN ) ) then
          call pWATER_MAIN%getvalues( iMonth, iDay, iYear, iJulianDay )
          if ( pWATER_MAIN%lGridHasChanged ) fWATER_MAIN = pack( pWATER_MAIN%pGrdBase%rData, is_cell_active )
        endif      

        if ( associated( pANNUAL_RECHARGE_RATE ) ) then
          call pANNUAL_RECHARGE_RATE%getvalues( iMonth, iDay, iYear, iJulianDay )
          if ( pANNUAL_RECHARGE_RATE%lGridHasChanged ) fANNUAL_RECHARGE_RATE = pack( pANNUAL_RECHARGE_RATE%pGrdBase%rData, is_cell_active )
        endif      

        DATE_OF_LAST_RETRIEVAL = SIM_DT%curr

      end associate

    endif

    direct_recharge = 0.0_c_float

    if ( allocated( fCESSPOOL ) )  direct_recharge = direct_recharge + fCESSPOOL( indx )

    if ( allocated( fDISPOSAL_WELL ) )  direct_recharge = direct_recharge + fDISPOSAL_WELL( indx )

    if ( allocated( fWATER_MAIN ) )  direct_recharge = direct_recharge + fWATER_MAIN( indx )

    if ( allocated( fWATER_BODY_RECHARGE ) )  direct_recharge = direct_recharge + fWATER_BODY_RECHARGE( indx )

    if ( allocated( fSTORM_DRAIN ) )  direct_recharge = direct_recharge + fSTORM_DRAIN( indx )

    if ( allocated( fANNUAL_RECHARGE_RATE) )  direct_recharge = direct_recharge + fANNUAL_RECHARGE_RATE( indx ) / 365.25_c_float

  end subroutine direct_recharge_calculate

end module direct_recharge__gridded_data
