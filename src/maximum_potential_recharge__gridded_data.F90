!> @file
!! Contains the module \ref maximum_potential_recharge__gridded_data.

!>
!!  Module \ref maximum_potential_recharge__gridded_data
!!  provides support for adding miscellaneous source and sink terms.

module maximum_potential_recharge__gridded_data

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

  public :: maximum_potential_recharge_initialize, maximum_potential_recharge_calculate

  type (DATA_CATALOG_ENTRY_T), pointer :: pMAXIMUM_POTENTIAL_RECHARGE
  real (kind=c_float), allocatable     :: fMAXIMUM_POTENTIAL_RECHARGE(:)
  real (kind=c_float), allocatable     :: fMAXIMUM_POTENTIAL_RECHARGE_ARRAY(:,:)
  real (kind=c_float), allocatable     :: fMAXIMUM_POTENTIAL_RECHARGE_TABLE(:,:)
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

  subroutine maximum_potential_recharge_initialize( is_cell_active, landuse_index )

    logical (kind=c_bool), intent(in)     :: is_cell_active(:,:)
    integer (kind=c_int), intent(in)      :: landuse_index(:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    type (STRING_LIST_T)                 :: parameter_list
    type (STRING_LIST_T)                 :: max_recharge_list
    real (kind=c_float), allocatable     :: max_recharge_vector(:)
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
    
    call assert( associated( pLULC), "Possible INTERNAL PROGRAMMING ERROR -- Null pointer detected for pLULC", &
      __FILE__, __LINE__ )

    call assert( associated( pLULC%pGrdBase ),   &
      "Possible INTERNAL PROGRAMMING ERROR -- Null pointer detected for pLULC%pGrdBase", __FILE__, __LINE__ )

    call assert( allocated( pLULC%pGrdBase%iData ),   &
      "Possible INTERNAL PROGRAMMING ERROR -- Unallocated array detected for pLULC%pGrdBase%iData", __FILE__, __LINE__ )

    call assert( associated( pHSG), "Possible INTERNAL PROGRAMMING ERROR -- Null pointer detected for pHSG", &
      __FILE__, __LINE__ )

    call assert( associated( pHSG%pGrdBase ),      & 
      "Possible INTERNAL PROGRAMMING ERROR -- Null pointer detected for pHSG%pGrdBase", __FILE__, __LINE__ )

    call assert( allocated( pHSG%pGrdBase%iData ),      & 
      "Possible INTERNAL PROGRAMMING ERROR -- Unallocated array detected for pHSG%pGrdBase%iData", __FILE__, __LINE__ )


    ! attempt to find a source of GRIDDED MAXIMUM_POTENTIAL_RECHARGE data
    pMAXIMUM_POTENTIAL_RECHARGE => DAT%find( "MAXIMUM_POTENTIAL_RECHARGE" )

    ! look for data in the form of a grid
    if ( associated( pMAXIMUM_POTENTIAL_RECHARGE ) ) then

      allocate( fMAXIMUM_POTENTIAL_RECHARGE( count( is_cell_active ) ), stat=iStat )
      call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

      associate ( dt => SIM_DT%curr )

        if ( associated( pMAXIMUM_POTENTIAL_RECHARGE ) ) then
          ! NB: maximum potential recharge 
          call pMAXIMUM_POTENTIAL_RECHARGE%getvalues( )
          if ( pMAXIMUM_POTENTIAL_RECHARGE%lGridHasChanged )     &
            fMAXIMUM_POTENTIAL_RECHARGE = pack( pMAXIMUM_POTENTIAL_RECHARGE%pGrdBase%rData, is_cell_active )
        endif      

      end associate

    else  ! no gridded data; read from TABLE values

!      iNumActiveCells = count( is_cell_active,1 )

      call parameter_list%append("LU_Code")
      call parameter_list%append("Landuse_Code")
      call parameter_list%append("Landuse_Lookup_Code")

      !> Determine how many soil groups are present

      ! retrieve a string list of all keys associated with Max_recharge (i.e. Max_recharge_1, Max_recharge_2, etc.)
      max_recharge_list = PARAMS%grep_name("Max_recharge", lFatal=FALSE )
      ! Convert the string list to an vector of integers; MODEL call strips off the "RZ_" part of label
      sequence_nums = max_recharge_list%asInt()

      ! count how many items are present in the vector; MODEL should equal the number of soils groups
      number_of_soils = count( sequence_nums > 0 )

      !> Determine how many landuse codes are present
      call PARAMS%get_parameters( slKeys=parameter_list, iValues=landuse_codes )
      number_of_landuses = count( landuse_codes >= 0 )

      allocate( fMAXIMUM_POTENTIAL_RECHARGE_TABLE(number_of_landuses, number_of_soils), stat=iStat )
      call assert( iStat == 0, "Failed to allocate memory for maximum potential recharge table", &
        __FILE__, __LINE__)

      ! we should have the max potential recharge table fully filled out following MODEL block
      do soils_indx = 1, number_of_soils
        text_str = "Max_recharge_"//asCharacter(soils_indx)
        call PARAMS%get_parameters( sKey=text_str, fValues=max_recharge_vector )
        fMAXIMUM_POTENTIAL_RECHARGE_TABLE(:, soils_indx) = max_recharge_vector
      enddo  

      call LOGS%WRITE( "Landuse Code |  Soils Code  | Number of Matches | Maximum potential recharge (in)",   &
        iLogLevel = LOG_DEBUG, lEcho = lFALSE )
      call LOGS%WRITE( "-------------|--------------|-------------------|-------------------------------- ",  &
        iLogLevel = LOG_DEBUG, lEcho = lFALSE )

      allocate( fMAXIMUM_POTENTIAL_RECHARGE_ARRAY( ubound(is_cell_active,1),ubound(is_cell_active,2) ), stat=iStat )
      call assert( iStat == 0, "Failed to allocate memory for maximum potential recharge table", &
        __FILE__, __LINE__)

      do soils_indx = 1, number_of_soils
        do landuse_indx = 1, number_of_landuses

          call LOGS%WRITE( asCharacter(landuse_codes( landuse_indx) )//" | "//asCharacter(soils_indx)//" | "//    &
              asCharacter(count( pLULC%pGrdBase%iData == landuse_codes( landuse_indx)               &
                                   .and. pHSG%pGrdBase%iData == soils_indx ) )//" | "          &
                                   //asCharacter( fMAXIMUM_POTENTIAL_RECHARGE_TABLE( landuse_indx, soils_indx) ), &
                                   iLogLevel = LOG_DEBUG, lEcho = lFALSE )

           value = fMAXIMUM_POTENTIAL_RECHARGE_TABLE( landuse_indx, soils_indx )

           where ( pLULC%pGrdBase%iData == landuse_codes( landuse_indx) .and. pHSG%pGrdBase%iData == soils_indx )

             fMAXIMUM_POTENTIAL_RECHARGE_ARRAY = value

           endwhere 

        enddo

      enddo

      fMAXIMUM_POTENTIAL_RECHARGE = pack( fMAXIMUM_POTENTIAL_RECHARGE_ARRAY, is_cell_active )

      deallocate( fMAXIMUM_POTENTIAL_RECHARGE_ARRAY )
      deallocate( fMAXIMUM_POTENTIAL_RECHARGE_TABLE )

      call parameter_list%clear()

    endif

  end subroutine maximum_potential_recharge_initialize

!--------------------------------------------------------------------------------------------------

  elemental subroutine maximum_potential_recharge_calculate( potential_recharge, rejected_potential_recharge, indx )

    real ( kind=c_float), intent(inout)  :: potential_recharge
    real (kind=c_float), intent(inout)   :: rejected_potential_recharge
    integer (kind=c_int), intent(in)     :: indx

    if ( potential_recharge > fMAXIMUM_POTENTIAL_RECHARGE( indx ) ) then

      rejected_potential_recharge = potential_recharge - fMAXIMUM_POTENTIAL_RECHARGE( indx )
      potential_recharge = fMAXIMUM_POTENTIAL_RECHARGE( indx )

    else
    
      rejected_potential_recharge = 0.0_c_float

    endif

  end subroutine maximum_potential_recharge_calculate  


end module maximum_potential_recharge__gridded_data
