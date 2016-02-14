module interception__bucket

  use iso_c_binding
  use exceptions
  use parameters, only     : PARAMS
  use strings
  implicit none

  private

  public interception_bucket_initialize
  public interception_bucket_calculate

  logical (kind=c_bool) :: GROWING_SEASON = .true._c_bool

  integer (kind=c_int), allocatable :: iLanduseCodes(:)
  real (kind=c_float), allocatable  :: fInterceptionValue_GrowingSeason(:)
  real (kind=c_float), allocatable  :: fInterceptionValue_DormantSeason(:)

contains   

  subroutine interception_bucket_initialize()

    ! [ LOCALS ]
    integer (kind=c_int)              :: iNumberOfLanduses
    logical (kind=c_bool)             :: lAreLengthsEqual
    character (len=:), allocatable    :: sTemp
!    type (STRING_LIST_T)              :: sl_temp_list



    !> Determine how many landuse codes are present
    sTemp = "LU_Code"
    call PARAMS%get_parameters( sKey=sTemp, iValues=iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes > 0 )

    ! !> retrieve growing season interception amount
    ! call sl_temp_list%clear()
    ! call sl_temp_list%append("Growing_season_interception")
    ! call sl_temp_list%append("Interception_growing")

    ! call PARAMS%get_parameters( slKeys=sl_temp_list, fValues=fInterceptionValue_GrowingSeason, lFatal=lTRUE ) 
    
    ! !> retrieve growing season interception amount
    ! call sl_temp_list%clear()
    ! call sl_temp_list%append("Nongrowing_season_interception")
    ! call sl_temp_list%append("Interception_nongrowing")
    ! call sl_temp_list%append("Interception_dormant")    

    ! call PARAMS%get_parameters( slKeys=sl_temp_list, fValues=fInterceptionValue_DormantSeason, lFatal=lTRUE ) 

    ! lAreLengthsEqual = ( ( ubound(fInterceptionValue_GrowingSeason,1) == ubound(iLanduseCodes,1) )  &
    !               .and. ( ubound(fInterceptionValue_DormantSeason,1) == ubound(iLanduseCodes,1) )    )

    ! if ( .not. lAreLengthsEqual )     &
    !   call warn( sMessage="The number of landuses does not match the number of interception values.",   &
    !     sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

    ! !> retrieve first day of growing season
    ! call sl_temp_list%clear()
    ! call sl_temp_list%append("First_day_of_growing_season")
    ! call sl_temp_list%append("First_DOY_growing_season")
    ! call sl_temp_list%append("Growing_season_start")

    ! call PARAMS%get_parameters( slKeys=sl_temp_list, slValues=sl_growing_seas_begin, lFatal=lTRUE ) 

    ! !> retrieve last day of growing season
    ! call sl_temp_list%clear()
    ! call sl_temp_list%append("Last_day_of_growing_season")
    ! call sl_temp_list%append("Last_DOY_growing_season")
    ! call sl_temp_list%append("Growing_season_end")

    ! call PARAMS%get_parameters( slKeys=sl_temp_list, slValues=sl_growing_seas_end, lFatal=lTRUE ) 
    ! call sl_temp_list%clear()

    ! !> process first day of growing season. retrieved as a list of strings; 
    ! !! must convert the strings from mm/dd to DOY
    ! allocate( FIRST_DAY_OF_GROWING_SEAS( sl_growing_seas_begin%count ), stat=status )
    ! call assert( status==0, "Problem allocating memory.", __FILE__, __LINE__ )

    ! do index = 1, sl_growing_seas_begin%count
    !   str_buffer = sl_growing_seas_begin%get( index )
    !   FIRST_DAY_OF_GROWING_SEAS( index ) = mmdd2doy( str_buffer )
    ! enddo  

    ! !> process last day of growing season. retrieved as a list of strings; 
    ! !! must convert the strings from mm/dd to DOY
    ! allocate( LAST_DAY_OF_IRRIGATION( sl_irrigation_end%count ), stat=status )
    ! call assert( status==0, "Problem allocating memory.", __FILE__, __LINE__ )

    ! do index = 1, sl_growing_seas_end%count
    !   str_buffer = sl_growing_seas_end%get( index )
    !   LAST_DAY_OF_GROWING_SEAS( index ) = mmdd2doy( str_buffer )
    ! enddo  

  end subroutine interception_bucket_initialize

!--------------------------------------------------------------------------------------------------

  elemental function interception_bucket_calculate( iLanduseIndex, fPrecip, fFog, &
                                                          fCanopy_Cover_Fraction )   result( fInterception )

    integer (kind=c_int), intent(in) :: iLanduseIndex
    real (kind=c_float), intent(in)  :: fPrecip
    real (kind=c_float), intent(in)  :: fFog
    real (kind=c_float), intent(in)  :: fCanopy_Cover_Fraction
    real (kind=c_float)              :: fInterception

    !!! Need to come up with a module that provides the current day of year, month, day, year, and growing season

    ! [ LOCALS ]
    real (kind=c_float) :: fPotentialInterception

    if (GROWING_SEASON) then

      fPotentialInterception = fInterceptionValue_GrowingSeason(iLanduseIndex)

    else

      fPotentialInterception = fInterceptionValue_DormantSeason(iLanduseIndex)

    endif

    fInterception = min( fPotentialInterception, fPrecip + fFog ) * fCanopy_Cover_Fraction

 
  end function interception_bucket_calculate

end module interception__bucket