!> @file
!! Contains the module \ref maximum_net_infiltration__gridded_data.

!>
!!  Module \ref maximum_net_infiltration__gridded_data
!!  provides support for adding miscellaneous source and sink terms.

module maximum_net_infiltration__gridded_data

  use iso_c_binding, only       : c_short, c_int, c_float, c_double, c_size_t, c_ptrdiff_t
  use constants_and_conversions
  use datetime
  use data_catalog
  use data_catalog_entry
  use dictionary
  use exceptions
  use file_operations
  use grid
  use logfiles
  use netcdf4_support
  use parameters, only          : PARAMS
  use simulation_datetime
  use strings
  use string_list

  implicit none

  private

  public :: maximum_net_infiltration_initialize, maximum_net_infiltration_calculate

  type (DATA_CATALOG_ENTRY_T), pointer :: pMAXIMUM_NET_INFILTRATION
  real (kind=c_float), allocatable     :: fMAXIMUM_NET_INFILTRATION(:)
  real (kind=c_float), allocatable     :: fMAXIMUM_NET_INFILTRATION_ARRAY(:,:)
  real (kind=c_float), allocatable     :: fMAXIMUM_NET_INFILTRATION_TABLE(:,:)
  type ( DATETIME_T ), pointer         :: DATE_OF_LAST_RETRIEVAL

contains

  !> Initialize the routine to establish maximum potential recharge rates.
  !!
  !! Open gridded data file.
  !! Open a NetCDF output file to hold variable output.
  !!
  !! @param[in] lActive 2D array of active cells within the model domain.
  !! @param[in] iLanduseIndex 1D vector of indices corresponding to rows of the
  !!            landuse lookup table(s).

  subroutine maximum_net_infiltration_initialize( is_cell_active, landuse_index )

    logical (kind=c_bool), intent(in)     :: is_cell_active(:,:)
    integer (kind=c_int), intent(in)      :: landuse_index(:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    type (STRING_LIST_T)                 :: parameter_list
    type (STRING_LIST_T)                 :: max_net_infiltration_list
    real (kind=c_float), allocatable     :: max_net_infiltration_vector(:)
    integer (kind=c_int), allocatable    :: sequence_nums(:)
    integer (kind=c_int), allocatable    :: landuse_codes(:)
    logical (kind=c_bool)                :: lAreLengthsEqual
    integer (kind=c_int)                 :: soils_indx
    integer (kind=c_int)                 :: landuse_indx
    integer (kind=c_int)                 :: number_of_landuses
    integer (kind=c_int)                 :: number_of_soils
    real (kind=c_float)                  :: value
    integer (kind=c_int)                 :: month, day,year, julian_day
    character( len=:), allocatable       :: text_str


    type (DATA_CATALOG_ENTRY_T), pointer :: pHSG
    type (DATA_CATALOG_ENTRY_T), pointer :: pLULC

    pLULC => DAT%find("LAND_USE")
    pHSG => DAT%find("HYDROLOGIC_SOILS_GROUP")

    call assert( associated( pLULC), "Possible INTERNAL PROGRAMMING ERROR -- Null pointer"          &
      //" detected for pLULC", __SRCNAME__, __LINE__ )

    call assert( associated( pLULC%pGrdBase ),   &
      "Possible INTERNAL PROGRAMMING ERROR -- Null pointer detected for pLULC%pGrdBase",            &
       __SRCNAME__, __LINE__ )

    call assert( allocated( pLULC%pGrdBase%iData ),   &
      "Possible INTERNAL PROGRAMMING ERROR -- Unallocated array detected for pLULC%pGrdBase%iData", &
      __SRCNAME__, __LINE__ )

    call assert( associated( pHSG), "Possible INTERNAL PROGRAMMING ERROR -- Null pointer"           &
      //" detected for pHSG", __SRCNAME__, __LINE__ )

    call assert( associated( pHSG%pGrdBase ),      &
      "Possible INTERNAL PROGRAMMING ERROR -- Null pointer detected for pHSG%pGrdBase",             &
       __SRCNAME__, __LINE__ )

    call assert( allocated( pHSG%pGrdBase%iData ),      &
      "Possible INTERNAL PROGRAMMING ERROR -- Unallocated array detected for pHSG%pGrdBase%iData",  &
       __SRCNAME__, __LINE__ )


    ! attempt to find a source of GRIDDED MAXIMUM_NET_INFILTRATION data
    pMAXIMUM_NET_INFILTRATION => DAT%find( "MAXIMUM_NET_INFILTRATION" )

    ! retrieve a string list of all keys associated with Max_recharge (i.e. Max_recharge_1, Max_recharge_2, etc.)
    max_net_infiltration_list = PARAMS%grep_name("max_net_infil", lFatal=TRUE )

    ! look for data in the form of a grid
    if ( associated( pMAXIMUM_NET_INFILTRATION ) ) then

      allocate( fMAXIMUM_NET_INFILTRATION( count( is_cell_active ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

      associate ( dt => SIM_DT%curr )

        if ( associated( pMAXIMUM_NET_INFILTRATION ) ) then
          ! NB: maximum potential recharge
          call pMAXIMUM_NET_INFILTRATION%getvalues( )
          if ( pMAXIMUM_NET_INFILTRATION%lGridHasChanged )     &
            fMAXIMUM_NET_INFILTRATION = pack( pMAXIMUM_NET_INFILTRATION%pGrdBase%rData, is_cell_active )
        endif

      end associate

    elseif ( max_net_infiltration_list%get(1) /= "<NA>" ) then  ! no gridded data; read from TABLE values

      call parameter_list%append("LU_Code")
      call parameter_list%append("Landuse_Code")
      call parameter_list%append("Landuse_Lookup_Code")

      !> Determine how many landuse codes are present
      call PARAMS%get_parameters( slKeys=parameter_list, iValues=landuse_codes )
      number_of_landuses = count( landuse_codes >= 0 )

      call PARAMS%get_parameters( fValues=fMAXIMUM_NET_INFILTRATION_TABLE,     &
                                  sPrefix="max_net_infil",                         &
                                  iNumRows=number_of_landuses,                 &
                                  lFatal=TRUE )

      number_of_soils = ubound( fMAXIMUM_NET_INFILTRATION_TABLE, 2 )

      call LOGS%WRITE( "Landuse Code |  Soils Code  | Number of Matches | Maximum net infiltration (in)",   &
        iLogLevel = LOG_DEBUG, lEcho = lFALSE )
      call LOGS%WRITE( "-------------|--------------|-------------------|-------------------------------- ",  &
        iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      allocate( fMAXIMUM_NET_INFILTRATION_ARRAY( ubound(is_cell_active,1),ubound(is_cell_active,2) ), stat=iStat )
      call assert( iStat == 0, "Failed to allocate memory for maximum potential recharge table", &
        __SRCNAME__, __LINE__)

      do soils_indx = 1, number_of_soils
        do landuse_indx = 1, number_of_landuses

          call LOGS%WRITE( asCharacter(landuse_codes( landuse_indx) )//" | "//asCharacter(soils_indx)//" | "//    &
              asCharacter(count( pLULC%pGrdBase%iData == landuse_codes( landuse_indx)               &
                                   .and. pHSG%pGrdBase%iData == soils_indx ) )//" | "          &
                                   //asCharacter( fMAXIMUM_NET_INFILTRATION_TABLE( landuse_indx, soils_indx) ), &
                                   iLogLevel = LOG_DEBUG, lEcho = lFALSE )

           value = fMAXIMUM_NET_INFILTRATION_TABLE( landuse_indx, soils_indx )

           where ( pLULC%pGrdBase%iData == landuse_codes( landuse_indx) .and. pHSG%pGrdBase%iData == soils_indx )

             fMAXIMUM_NET_INFILTRATION_ARRAY = value

           endwhere

        enddo

      enddo

      fMAXIMUM_NET_INFILTRATION = pack( fMAXIMUM_NET_INFILTRATION_ARRAY, is_cell_active )

      deallocate( fMAXIMUM_NET_INFILTRATION_ARRAY )
      deallocate( fMAXIMUM_NET_INFILTRATION_TABLE )

      call parameter_list%clear()

    else  ! neither TABLE nor GRIDDED Maximum Potential Recharge values were given;
          ! default to ridiculously high maximum potential recharge

      allocate( fMAXIMUM_NET_INFILTRATION( count( is_cell_active ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __SRCNAME__, __LINE__ )

      fMAXIMUM_NET_INFILTRATION = 9999.0

      call warn( "Did not find any valid maximum net infiltration rate parameters.",    &
        lFatal = lTRUE )

    endif

  end subroutine maximum_net_infiltration_initialize

!--------------------------------------------------------------------------------------------------

  elemental subroutine maximum_net_infiltration_calculate( net_infiltration, rejected_net_infiltration, indx )

    real ( kind=c_float), intent(inout)  :: net_infiltration
    real (kind=c_float), intent(inout)   :: rejected_net_infiltration
    integer (kind=c_int), intent(in)     :: indx

    if ( net_infiltration > fMAXIMUM_NET_INFILTRATION( indx ) ) then

      rejected_net_infiltration = net_infiltration - fMAXIMUM_NET_INFILTRATION( indx )
      net_infiltration = fMAXIMUM_NET_INFILTRATION( indx )

    else

      rejected_net_infiltration = 0.0_c_float

    endif

  end subroutine maximum_net_infiltration_calculate


end module maximum_net_infiltration__gridded_data
