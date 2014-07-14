module soil_moisture_storage

  use iso_c_binding
  implicit iNumberOfLanduses

  private


contains

    subroutine initialize_soil_layers( fSoilStorage_Max, iLanduseIndex, iSoilsGroup )

    real (kind=c_float), intent(inout)    :: fSoilStorage_Max(:)
    integer (kind=c_int), intent(in)      :: iLanduseIndex(:)
    integer (kind=c_int), intent(in)      :: iSoilsGroup(:)

    ! [ LOCALS ]
    integer (kind=c_int)              :: iNumActiveCells
    integer (kind=c_int)              :: iStat
    integer (kind=c_int)              :: iNumberOfLanduses
    integer (kind=c_int)              :: iNumberOfSoilGroups
    integer (kind=c_int)              :: iSoilsIndex
    integer (kind=c_int)              :: iLUIndex
    type (STRING_LIST_T)              :: slList
    type (STRING_LIST_T)              :: slRZ
    integer (kind=c_int), allocatable :: iRZ_SeqNums(:) 
    real (kind=c_float), allocatable  :: RZ(:)
    character (len=:), allocatable    :: sText


    iNumActiveCells = ubound(fSoilStorage_Max,1)

    call slList%append("LU_Code")
    call slList%append("Landuse_Lookup_Code")

    !> Determine how many soil groups are present

    ! retrieve a string list of all keys associated with root zone depth (i.e. RZ_1, RZ_2, RZ_3, etc.)
    slRZ = PARAMS%grep_keys("RZ")
    ! Convert the string list to an vector of integers; this call strips off the "RZ_" part of label
    iRZ_SeqNums = slRZ%asInt()
    ! count how many items are present in the vector; this should equal the number of soils groups
    iNumberOfSoilGroups = count( iRZ_SeqNums > 0 )

    !> Determine how many landuse codes are present
    call PARAMS%get_values( slList, iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes > 0 )

    allocate( ROOTING_DEPTH(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for maximum rooting depth table", &
      __FILE__, __LINE__)

    ! we should have the max rooting depth table fully filled out following this block
    do iSoilsIndex = 1, iNumberOfSoilGroups
      sText = "RZ_"//asCharacter(iSoilsIndex)
      call PARAMS%get_values( sText, RZ )
      ROOTING_DEPTH(:, iSoilsIndex) = RZ
    enddo  

    do iSoilsIndex = 1, iNumberOfSoilGroups
      do iLUIndex = 1, iNumberOfLanduses

        where ( iLanduseIndex == iLUIndex .and. iSoilsGroup == iSoilsIndex )

          fSoilStorage_Max = ROOTING_DEPTH( iLUIndex, iSoilsIndex ) * 2.

        end where

      enddo
    enddo

  end subroutine initialize_soil_layers






end module soil_moisture_storage