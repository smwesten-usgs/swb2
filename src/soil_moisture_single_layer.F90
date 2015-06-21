module soil_moisture_storage

  use logfiles, only          : LOGS
  use routing__D8
  use strings
  use string_list


  use iso_c_binding
  implicit none

  private


contains

  subroutine soil_layer_initialize( MODEL )

    class (MODEL_DOMAIN_T), intent(inout)   :: MODEL

    ! [ LOCALS ]
    integer (kind=c_int)              :: iNumActiveCells
    integer (kind=c_int)              :: iStat
    integer (kind=c_int)              :: iNumberOfLanduses
    integer (kind=c_int)              :: iNumberOfSoilGroups
    integer (kind=c_int)              :: iSoilsIndex
    integer (kind=c_int)              :: iLUIndex
    integer (kind=c_int), allocatable :: iLanduseCodes(:)
    type (STRING_LIST_T)              :: slList
    type (STRING_LIST_T)              :: slRZ
    integer (kind=c_int), allocatable :: iRZ_SeqNums(:) 
    real (kind=c_float), allocatable  :: RZ(:)
    character (len=:), allocatable    :: sText
    real (kind=c_float), allocatable  :: water_capacity(:)
    integer (kind=c_int)              :: iIndex

    iNumActiveCells = ubound( MODEL%soil_storage_max, 1 )

    call slList%append("LU_Code")
    call slList%append("Landuse_Code")
    call slList%append("Landuse_Lookup_Code")

    !> Determine how many soil groups are present

    ! retrieve a string list of all keys associated with root zone depth (i.e. RZ_1, RZ_2, RZ_3, etc.)
    slRZ = PARAMS_DICT%grep_keys("RZ")
    ! Convert the string list to an vector of integers; MODEL call strips off the "RZ_" part of label
    iRZ_SeqNums = slRZ%asInt()
    ! count how many items are present in the vector; MODEL should equal the number of soils groups
    iNumberOfSoilGroups = count( iRZ_SeqNums > 0 )

    !> Determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=slList, iValues=iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes >= 0 )

    allocate( MAX_ROOTING_DEPTH(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for maximum rooting depth table", &
      __FILE__, __LINE__)

    ! we should have the max rooting depth table fully filled out following MODEL block
    do iSoilsIndex = 1, iNumberOfSoilGroups
      sText = "RZ_"//asCharacter(iSoilsIndex)
      call PARAMS%get_parameters( sKey=sText, fValues=RZ )
      MAX_ROOTING_DEPTH(:, iSoilsIndex) = RZ
    enddo  

!     MODEL%awc = pack(pAWC%pGrdBase%rData, MODEL%active)

    call LOGS%WRITE( "Landuse Code |  Soils Code  | Number of Matches",              &
      iLogLevel = LOG_DEBUG, lEcho = lFALSE )
    call LOGS%WRITE( "-------------|--------------|------------------- ",            &
      iLogLevel = LOG_DEBUG, lEcho = lFALSE )

    do iSoilsIndex = 1, iNumberOfSoilGroups
      do iLUIndex = 1, iNumberOfLanduses

        call LOGS%WRITE( asCharacter(iLUIndex)//" | "//asCharacter(iSoilsIndex)//" | "//  &
            asCharacter(count( MODEL%landuse_index == iLUIndex .and. MODEL%soil_group == iSoilsIndex ) ),              &
            iLogLevel = LOG_DEBUG, lEcho = lFALSE )


        do iIndex = 1, ubound(MODEL%soil_storage_max, 1)
    
          if ( MODEL%landuse_index(iIndex) == iLUIndex .and. MODEL%soil_group(iIndex) == iSoilsIndex ) then
            MODEL%soil_storage_max(iIndex) = MAX_ROOTING_DEPTH( iLUIndex, iSoilsIndex ) * MODEL%awc(iIndex)
          endif

        enddo
      enddo
    enddo

    call slList%clear()

  end subroutine soil_layer_initialize

  subroutine soil_layer_moisture_content_initialize( MODEL )

    class (MODEL_DOMAIN_T), intent(inout)   :: MODEL

    ! [ LOCALS ]
    type (STRING_LIST_T)              :: slList
    type (DATETIME_T)                 :: DT
    integer (kind=c_int)              :: iNumberOfLanduses
    integer (kind=c_int)              :: iIndex, iIndex2
    integer (kind=c_int)              :: iStat

    character (len=10)               :: sMMDDYYYY
    character (len=:), allocatable   :: sText


    type (DATA_CATALOG_ENTRY_T), pointer :: pINITIAL_PERCENT_SOIL_MOISTURE

    ! locate the data structure associated with the gridded rainfall zone entries
    pINITIAL_PERCENT_SOIL_MOISTURE => DAT%find("INITIAL_PERCENT_SOIL_MOISTURE")
    if ( .not. associated( pINITIAL_PERCENT_SOIL_MOISTURE ) ) &
        call die("A INITIAL_PERCENT_SOIL_MOISTURE grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    allocate( INITIAL_PERCENT_SOIL_MOISTURE( count( MODEL%active ) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )
 
    call pINITIAL_PERCENT_SOIL_MOISTURE%getvalues()
 
    ! map the 2D array of RAINFALL_ZONE values to the vector of active cells
    INITIAL_PERCENT_SOIL_MOISTURE = pack( pINITIAL_PERCENT_SOIL_MOISTURE%pGrdBase%rData, MODEL%active )

   !> create string list that allows for alternate heading identifiers for the landuse code
   call slList%append("LU_Code")
   call slList%append("Landuse_Code")
   call slList%append("Landuse_Lookup_Code")

   !> Determine how many landuse codes are present
   call PARAMS%get_parameters( slKeys=slList, iValues=LANDUSE_CODE )
   iNumberOfLanduses = count( LANDUSE_CODE >= 0 )

    do iIndex = lbound( fSoilStorage, 1 ), ubound( fSoilStorage,1 )

      fKcb_initial = update_crop_coefficient_date_as_threshold( iLanduseIndex( iIndex ) )
      fRz_initial = calc_effective_root_depth( iLanduseIndex=iLanduseIndex( iIndex ),                &
                                               fZr_max=fMax_Rooting_Depths( iLanduseIndex( iIndex ), &
                                                                            iSoilGroup( iIndex ) ),  &
                                               fKCB=fKcb_initial )

      MODEL%soil_storage( iIndex ) = INITIAL_PERCENT_SOIL_MOISTURE( iIndex )    &
                               * fRz_initial * MODEL%awc( iIndex )

    enddo

  !> @TODO Add more logic here to perform checks on the validity of this data.

  !> @TODO Need to handle missing values. WHat do we do if an entire column of values
  !!       is missing?

  end subroutine soil_layer_moisture_content_initialize



  subroutine soil_layer_mass_balance_calculate( MODEL )

    class (MODEL_DOMAIN_T), intent(inout)   :: MODEL

    ! [ LOCALS ]
    integer (kind=c_int) :: index
    integer (kind=c_int) :: orderindex
    integer (kind=c_int) :: targetindex

     if ( associated(MODEL%calc_routing) ) then

      MODEL%runon = 0.0_c_float

      do index=lbound(MODEL%runon,1), ubound(MODEL%runon,1)

        orderindex = ORDER_INDEX( index )
        targetindex = TARGET_INDEX( index )

        MODEL%inflow( orderindex ) =   MODEL%runon( orderindex )                      &
                                     + MODEL%gross_precip( orderindex )               &
                                     + MODEL%fog( orderindex )                        &
                                     + MODEL%snowmelt( orderindex )                   &
                                     - MODEL%interception( orderindex )                      
                             
        call MODEL%calc_runoff( orderindex )
 
        MODEL%infiltration( orderindex ) = MODEL%inflow( orderindex ) - MODEL%runoff( orderindex )
    
        if ( targetindex > 0) then
          
          MODEL%runon( targetindex ) = MODEL%runoff( orderindex )

        endif 

        call MODEL%calc_soil_moisture( orderindex )

      enddo  

     else

       MODEL%runon = 0.0_c_float
       MODEL%inflow = MODEL%gross_precip + MODEL%fog - MODEL%interception + MODEL%snowmelt
       call MODEL%calc_runoff()
       MODEL%infiltration = MODEL%inflow - MODEL%runoff
       call MODEL%calc_soil_moisture()

     endif
   
  end subroutine soil_layer_mass_balance_calculate

end module soil_moisture_storage