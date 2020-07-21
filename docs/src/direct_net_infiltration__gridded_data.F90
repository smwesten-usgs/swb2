!> @file
!! Contains the module \ref direct_net_infiltration__gridded_data.

!>
!!  Module \ref direct_net_infiltration__gridded_data
!!  provides support for adding miscellaneous source and sink terms.

module direct_net_infiltration__gridded_data

  use iso_c_binding
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
  use fstring
  use fstring_list

  implicit none

  private

  public :: direct_net_infiltration_initialize, direct_net_infiltration_calculate

  type (DATA_CATALOG_ENTRY_T), pointer :: pCESSPOOL
  type (DATA_CATALOG_ENTRY_T), pointer :: pDISPOSAL_WELL
  type (DATA_CATALOG_ENTRY_T), pointer :: pWATER_BODY_RECHARGE
  type (DATA_CATALOG_ENTRY_T), pointer :: pWATER_MAIN
  type (DATA_CATALOG_ENTRY_T), pointer :: pANNUAL_RECHARGE_RATE

  real (c_float), allocatable     :: fCESSPOOL(:)
  real (c_float), allocatable     :: fDISPOSAL_WELL(:)
  real (c_float), allocatable     :: fWATER_BODY_RECHARGE(:)
  real (c_float), allocatable     :: fWATER_MAIN(:)
  real (c_float), allocatable     :: fANNUAL_RECHARGE_RATE(:)

  ! ****_TABLE variables: will have same number of values as there are landuses
  real (c_float), allocatable     :: fCESSPOOL_TABLE(:)
  real (c_float), allocatable     :: fDISPOSAL_WELL_TABLE(:)
  real (c_float), allocatable     :: fWATER_BODY_RECHARGE_TABLE(:)
  real (c_float), allocatable     :: fWATER_MAIN_TABLE(:)
  real (c_float), allocatable     :: fANNUAL_RECHARGE_RATE_TABLE(:)

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

  subroutine direct_net_infiltration_initialize( is_cell_active, landuse_index )

    logical (c_bool), intent(in)     :: is_cell_active(:,:)
    integer (c_int), intent(in)      :: landuse_index(:)

    ! [ LOCALS ]
    integer (c_int)                 :: status
    type (FSTRING_LIST_T)                 :: parameter_list
    integer (c_int)                 :: indx
    integer (c_int)                 :: iNX
    integer (c_int)                 :: iNY
    integer (c_int), allocatable    :: landuse_codes(:)
    integer (c_int)                 :: number_of_landuses
    logical (c_bool)                :: are_lengths_equal


    !> Determine how many landuse codes are present
    call parameter_list%append( "LU_Code" )
    call parameter_list%append( "Landuse_Code" )
    call parameter_list%append( "Landuse_Lookup_Code" )

    call PARAMS%get_parameters( slKeys=parameter_list, iValues=landuse_codes )
    number_of_landuses = count( landuse_codes > 0 )

    call parameter_list%clear()
    call parameter_list%append( "Annual_direct_net_infiltration_rate" )
    call parameter_list%append( "Annual_direct_recharge_rate" )
    call parameter_list%append( "Annual_direct_recharge" )
    call parameter_list%append( "Annual_direct_net_infiltration" )

    call PARAMS%get_parameters( slKeys=parameter_list , fValues=fANNUAL_RECHARGE_RATE_TABLE )

    ! attempt to find a source of GRIDDED ANNUAL DIRECT RECHARGE data
    pANNUAL_RECHARGE_RATE => DAT%find( "ANNUAL_DIRECT_NET_INFILTRATION_RATE" )


    call parameter_list%clear()
    call parameter_list%append( "Cesspool_direct_net_infiltration" )
    call parameter_list%append( "Cesspool_recharge" )
    call parameter_list%append( "Cesspool_discharge" )
    call parameter_list%append( "Cesspool_leakage" )

    call PARAMS%get_parameters( slKeys=parameter_list , fValues=fCESSPOOL_TABLE )

    ! attempt to find a source of GRIDDED CESSPOOL data
    pCESSPOOL => DAT%find( "CESSPOOL_LEAKAGE" )


    ! call parameter_list%clear()
    ! call parameter_list%append( "Storm_drain_discharge" )
    ! call parameter_list%append( "Storm_drain_recharge" )
    ! call parameter_list%append( "Storm_drain_leakage" )
    !
    ! call PARAMS%get_parameters( slKeys=parameter_list , fValues=fSTORM_DRAIN_TABLE )
    !
    ! pSTORM_DRAIN => DAT%find( "STORM_DRAIN" )
    !

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

    ! if ( associated( pSTORM_DRAIN ) ) then
    !
    !   allocate( fSTORM_DRAIN( count( is_cell_active ) ), stat=status )
    !   call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )
    !
    ! elseif ( fSTORM_DRAIN_TABLE(1) > fTINYVAL ) then
    !
    !   are_lengths_equal = ( ( ubound(fSTORM_DRAIN_TABLE,1) == ubound(landuse_codes,1) )  )
    !
    !   if ( .not. are_lengths_equal )     &
    !     call warn( sMessage="The number of landuses does not match the number of storm drain discharge/leakage values.",   &
    !       sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )
    !
    !   allocate( fSTORM_DRAIN( count( is_cell_active ) ), stat=status )
    !   call assert( status==0, "Problem allocating memory", __SRCNAME__, __LINE__ )
    !
    !   do indx=lbound( landuse_index, 1 ), ubound( landuse_index, 1 )
    !     fSTORM_DRAIN( indx ) = fSTORM_DRAIN_TABLE( landuse_index( indx ) )
    !   enddo
    !
    !  endif

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

  end subroutine direct_net_infiltration_initialize

!--------------------------------------------------------------------------------------------------

  subroutine direct_net_infiltration_calculate( direct_net_infiltration, indx, is_cell_active, nodata_fill_value )

    real (c_float), intent(inout)     :: direct_net_infiltration
    integer (c_int), intent(in)       :: indx
    logical (c_bool), intent(in)      :: is_cell_active(:,:)
    real (c_float), intent(in)        :: nodata_fill_value(:,:)

    ! [ LOCALS ]
    real (c_float)  :: fFactor

    if ( .not. DATE_OF_LAST_RETRIEVAL == SIM_DT%curr ) then

      associate ( dt => SIM_DT%curr )

        if ( associated( pCESSPOOL ) ) then
          call pCESSPOOL%getvalues( dt )
          if ( pCESSPOOL%lGridHasChanged ) fCESSPOOL = pack( pCESSPOOL%pGrdBase%rData, is_cell_active )
        endif

        if ( associated( pDISPOSAL_WELL ) ) then
          call pDISPOSAL_WELL%getvalues( dt )
          if ( pDISPOSAL_WELL%lGridHasChanged ) fDISPOSAL_WELL =                                      &
               pack( pDISPOSAL_WELL%pGrdBase%rData, is_cell_active )
        endif

        ! if ( associated( pSTORM_DRAIN ) ) then
        !   call pSTORM_DRAIN%getvalues( iMonth, iDay, iYear, iJulianDay )
        !   if ( pSTORM_DRAIN%lGridHasChanged ) fSTORM_DRAIN = pack( pSTORM_DRAIN%pGrdBase%rData, is_cell_active )
        ! endif

        if ( associated( pWATER_BODY_RECHARGE ) ) then
          call pWATER_BODY_RECHARGE%getvalues( dt )
          if ( pWATER_BODY_RECHARGE%lGridHasChanged ) fWATER_BODY_RECHARGE =                          &
               pack( pWATER_BODY_RECHARGE%pGrdBase%rData, is_cell_active )
        endif

        if ( associated( pWATER_MAIN ) ) then
          call pWATER_MAIN%getvalues( dt )
          if ( pWATER_MAIN%lGridHasChanged ) fWATER_MAIN = pack( pWATER_MAIN%pGrdBase%rData, is_cell_active )
        endif

        if ( associated( pANNUAL_RECHARGE_RATE ) ) then
          call pANNUAL_RECHARGE_RATE%getvalues( dt )
          if ( pANNUAL_RECHARGE_RATE%lGridHasChanged ) fANNUAL_RECHARGE_RATE =                        &
               pack( pANNUAL_RECHARGE_RATE%pGrdBase%rData, is_cell_active )
        endif

        DATE_OF_LAST_RETRIEVAL = SIM_DT%curr

      end associate

    endif

    direct_net_infiltration = 0.0_c_float

    if ( allocated( fCESSPOOL ) )                                              &
      direct_net_infiltration = direct_net_infiltration + fCESSPOOL( indx )

    if ( allocated( fDISPOSAL_WELL ) )                                         &
      direct_net_infiltration = direct_net_infiltration + fDISPOSAL_WELL( indx )

    if ( allocated( fWATER_MAIN ) )                                            &
      direct_net_infiltration = direct_net_infiltration + fWATER_MAIN( indx )

    if ( allocated( fWATER_BODY_RECHARGE ) )                                   &
      direct_net_infiltration = direct_net_infiltration + fWATER_BODY_RECHARGE( indx )

!    if ( allocated( fSTORM_DRAIN ) )  direct_net_infiltration = direct_net_infiltration + fSTORM_DRAIN( indx )

    if ( allocated( fANNUAL_RECHARGE_RATE) )                                   &
      direct_net_infiltration = direct_net_infiltration + fANNUAL_RECHARGE_RATE( indx ) / 365.25_c_float

  end subroutine direct_net_infiltration_calculate

end module direct_net_infiltration__gridded_data
