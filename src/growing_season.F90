module growing_season

  use iso_c_binding
  use constants_and_conversions, only : TRUE, FALSE, asInt, fTINYVAL
  use datetime, only                  : mmdd2doy
  use exceptions
  use parameters, only                : PARAMS
  use simulation_datetime, only       : SIM_DT
  use strings
  use string_list, only               : STRING_LIST_T
  implicit none

  private

  public :: growing_season_initialize
  public :: growing_season_update

  integer (kind=c_int), allocatable :: iLanduseCodes(:)
  real (kind=c_float), allocatable  :: FIRST_DAY_OF_GROWING_SEASON(:)
  real (kind=c_float), allocatable  :: LAST_DAY_OF_GROWING_SEASON(:)
  real (kind=c_float), allocatable  :: GDD_FIRST_DAY_OF_GROWING_SEASON(:)
  real (kind=c_float), allocatable  :: KILLING_FROST_TEMP_LAST_DAY_OF_GROWING_SEASON(:)

  character( len=2 ), parameter     :: DATE_DELIMS = "/-"
  real (kind=c_float), parameter    :: NODATA_VALUE = -9999._c_float

contains

  subroutine growing_season_initialize( )

    ! [ LOCALS ]
    integer (kind=c_int)              :: iNumberOfLanduses
    logical (kind=c_bool)             :: lAreLengthsEqual
    character (len=:), allocatable    :: sTemp
    type (STRING_LIST_T)              :: sl_temp_list
    type (STRING_LIST_T)              :: sl_growing_season_begin
    type (STRING_LIST_T)              :: sl_growing_season_end
    character (len=32)                :: str_buffer
    real (kind=c_float), allocatable  :: temp_values(:)
    integer (kind=c_int)              :: indx
    integer (kind=c_int)              :: status

    !> Determine how many landuse codes are present
    sTemp = "LU_Code"
    call PARAMS%get_parameters( sKey=sTemp,                                            &
                                iValues=iLanduseCodes )

    iNumberOfLanduses = count( iLanduseCodes >= 0 )

    !> retrieve first day of growing season
    call sl_temp_list%clear()
    call sl_temp_list%append("First_day_of_growing_season")
    call sl_temp_list%append("First_DOY_growing_season")
    call sl_temp_list%append("Growing_season_start")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                                  &
                                slValues=sl_growing_season_begin,                     &
                                lFatal=.true._c_bool )

    !> process first day of growing season. retrieved as a list of strings;
    !! must convert the strings from mm/dd to DOY
    allocate( FIRST_DAY_OF_GROWING_SEASON( iNumberOfLanduses ), stat=status )
    call assert( status==0, "Problem allocating memory.", __SRCNAME__, __LINE__ )

    if ( sl_growing_season_begin%count == iNumberOfLanduses                &
         .and. sl_growing_season_begin%countmatching("<NA>") == 0 ) then

      do indx = 1, sl_growing_season_begin%count
        str_buffer = sl_growing_season_begin%get( indx )

        if ( scan( str_buffer, DATE_DELIMS ) > 0 ) then
          FIRST_DAY_OF_GROWING_SEASON( indx ) = mmdd2doy( str_buffer, "FIRST_DAY_OF_GROWING_SEASON" )
        else
          FIRST_DAY_OF_GROWING_SEASON( indx ) = asInt( str_buffer )
        endif
      enddo

    else

      FIRST_DAY_OF_GROWING_SEASON = NODATA_VALUE

    endif

    !> retrieve last day of growing season
    call sl_temp_list%clear()
    call sl_temp_list%append("Last_day_of_growing_season")
    call sl_temp_list%append("Last_DOY_growing_season")
    call sl_temp_list%append("Growing_season_end")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                                  &
                                slValues=sl_growing_season_end,                       &
                                lFatal=.true._c_bool )

    !> process last day of growing season. retrieved as a list of strings;
    !! must convert the strings from mm/dd to DOY
    allocate( LAST_DAY_OF_GROWING_SEASON( iNumberOfLanduses ), stat=status )
    call assert( status==0, "Problem allocating memory.", __SRCNAME__, __LINE__ )

    if ( sl_growing_season_end%count == iNumberOfLanduses                  &
         .and. sl_growing_season_end%countmatching("<NA>") == 0 ) then

      do indx = 1, sl_growing_season_end%count
        str_buffer = sl_growing_season_end%get( indx )
        if ( scan( str_buffer, DATE_DELIMS ) > 0 ) then
          LAST_DAY_OF_GROWING_SEASON( indx ) = mmdd2doy( str_buffer, "LAST_DAY_OF_GROWING_SEASON" )
        else
          LAST_DAY_OF_GROWING_SEASON( indx ) = asInt( str_buffer )
        endif
      enddo

    else

      LAST_DAY_OF_GROWING_SEASON = NODATA_VALUE

    endif

    !> GDD for first day of growing season
    call sl_temp_list%clear()
    call sl_temp_list%append("GDD_first_day_of_growing_season")
    call sl_temp_list%append("GDD_start_of_growing_season")

    call PARAMS%get_parameters( slKeys=sl_temp_list,          &
                                fValues=temp_values,          &
                                lFatal=FALSE )

    lAreLengthsEqual = ( ubound(temp_values,1) == ubound(iLanduseCodes,1) )

    if ( lAreLengthsEqual ) then

      call move_alloc( temp_values, GDD_FIRST_DAY_OF_GROWING_SEASON )

    else

      call warn( sMessage="The number of landuses does not match the number of GDD values "  &
                        //"specified for defining the beginning of the growing season.",     &
                 sModule=__SRCNAME__, iLine=__LINE__, lFatal=FALSE )

      allocate( GDD_FIRST_DAY_OF_GROWING_SEASON( ubound( iLanduseCodes, 1) ), stat=status )
      call assert( status==0, "Problem allocating memory.", __SRCNAME__, __LINE__)

      GDD_FIRST_DAY_OF_GROWING_SEASON = NODATA_VALUE

    endif

    !> Air temperature defining last day of growing season
    call sl_temp_list%clear()
    call sl_temp_list%append("Killing_frost_temperature")
    call sl_temp_list%append("Air_temperature_end_of_growing_season")

    call PARAMS%get_parameters( slKeys=sl_temp_list,          &
                                fValues=temp_values,          &
                                lFatal=FALSE )

    lAreLengthsEqual = ( ubound(temp_values,1) == ubound(iLanduseCodes,1) )

    if ( lAreLengthsEqual ) then

      call move_alloc( temp_values, KILLING_FROST_TEMP_LAST_DAY_OF_GROWING_SEASON )

    else

      call warn( sMessage="The number of landuses does not match the number of killing frost values "  &
                        //"specified to define the end of the growing season.",                        &
                 sModule=__SRCNAME__, iLine=__LINE__, lFatal=FALSE )

      allocate( KILLING_FROST_TEMP_LAST_DAY_OF_GROWING_SEASON( ubound( iLanduseCodes, 1) ), stat=status )
      call assert( status==0, "Problem allocating memory.", __SRCNAME__, __LINE__)

      KILLING_FROST_TEMP_LAST_DAY_OF_GROWING_SEASON = NODATA_VALUE

    endif


    if (    all( KILLING_FROST_TEMP_LAST_DAY_OF_GROWING_SEASON  == NODATA_VALUE ) &
      .and. all( GDD_FIRST_DAY_OF_GROWING_SEASON  == NODATA_VALUE )               &
      .and. all( FIRST_DAY_OF_GROWING_SEASON == NODATA_VALUE )                    &
      .and. all( LAST_DAY_OF_GROWING_SEASON == NODATA_VALUE ) )                   &
        call warn( sMessage="No entries found to assist in defining growing season " &
          //"(e.g. 'Growing_season_start', 'Growing_season_end').",                  &
          sHints="Make sure you have the first and last day of the growing season "  &
          //"specified in one of your lookup tables.",                               &
          sModule=__SRCNAME__, iLine=__LINE__, lFatal=TRUE )


  end subroutine growing_season_initialize

!--------------------------------------------------------------------------------------------------

  elemental subroutine growing_season_update( landuse_index,                   &
                                              GDD,                             &
                                              mean_air_temp,                   &
                                              it_is_growing_season )

    integer (kind=c_int), intent(in)      :: landuse_index
    real (kind=c_float), intent(in)       :: GDD
    real (kind=c_float), intent(in)       :: mean_air_temp
    logical (kind=c_bool), intent(inout)  :: it_is_growing_season

    ! first growing season day > last if we're growing a winter crop, winter wheat for example
    if ( FIRST_DAY_OF_GROWING_SEASON(landuse_index) > LAST_DAY_OF_GROWING_SEASON (landuse_index) ) then

      if ( it_is_growing_season ) then

        if ( KILLING_FROST_TEMP_LAST_DAY_OF_GROWING_SEASON( landuse_index ) > NODATA_VALUE ) then

          if ( mean_air_temp <= KILLING_FROST_TEMP_LAST_DAY_OF_GROWING_SEASON( landuse_index ) ) &
                 it_is_growing_season = FALSE
        elseif (    SIM_DT%iDOY > LAST_DAY_OF_GROWING_SEASON( landuse_index )  ) then
                 it_is_growing_season = FALSE
        endif

      else  ! not growing season; should it be?

        if ( GDD_FIRST_DAY_OF_GROWING_SEASON( landuse_index ) > 0. ) then

          if ( GDD >= GDD_FIRST_DAY_OF_GROWING_SEASON( landuse_index ) ) &
               it_is_growing_season = TRUE
        elseif ( ( SIM_DT%iDOY <= LAST_DAY_OF_GROWING_SEASON( landuse_index ) )          &
            .or. ( SIM_DT%iDOY >= FIRST_DAY_OF_GROWING_SEASON( landuse_index ) ) )  then
               it_is_growing_season = TRUE
        endif

      endif

    else

      if ( it_is_growing_season ) then

        if ( KILLING_FROST_TEMP_LAST_DAY_OF_GROWING_SEASON( landuse_index ) > NODATA_VALUE ) then

          if ( mean_air_temp <= KILLING_FROST_TEMP_LAST_DAY_OF_GROWING_SEASON( landuse_index ) ) &
               it_is_growing_season = FALSE
        elseif (    SIM_DT%iDOY > LAST_DAY_OF_GROWING_SEASON( landuse_index ) )  then
               it_is_growing_season = FALSE
        endif

      else  ! not growing season; should it be?

        if ( GDD_FIRST_DAY_OF_GROWING_SEASON( landuse_index ) > 0. ) then
          if ( GDD >= GDD_FIRST_DAY_OF_GROWING_SEASON( landuse_index ) ) &
               it_is_growing_season = TRUE
        elseif (    SIM_DT%iDOY <= LAST_DAY_OF_GROWING_SEASON( landuse_index )          &
            .and. SIM_DT%iDOY >= FIRST_DAY_OF_GROWING_SEASON( landuse_index )  )  then
               it_is_growing_season = TRUE

        endif

      endif

    endif

  end subroutine growing_season_update

end module growing_season
