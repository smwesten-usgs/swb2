module interception__bucket

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

  public :: interception_bucket_initialize, interception_bucket_calculate
  public :: interception_bucket_update_growing_season
  public :: IS_GROWING_SEASON

  logical (kind=c_bool) :: GROWING_SEASON = .true._c_bool

  integer (kind=c_int), allocatable :: iLanduseCodes(:)
  real (kind=c_float), allocatable  :: INTERCEPTION_A_VALUE_GROWING_SEASON(:)
  real (kind=c_float), allocatable  :: INTERCEPTION_B_VALUE_GROWING_SEASON(:)
  real (kind=c_float), allocatable  :: INTERCEPTION_N_VALUE_GROWING_SEASON(:)  
  real (kind=c_float), allocatable  :: INTERCEPTION_A_VALUE_NONGROWING_SEASON(:)
  real (kind=c_float), allocatable  :: INTERCEPTION_B_VALUE_NONGROWING_SEASON(:)
  real (kind=c_float), allocatable  :: INTERCEPTION_N_VALUE_NONGROWING_SEASON(:) 
  real (kind=c_float), allocatable  :: FIRST_DAY_OF_GROWING_SEASON(:)
  real (kind=c_float), allocatable  :: LAST_DAY_OF_GROWING_SEASON(:)
  real (kind=c_float), allocatable  :: GDD_FIRST_DAY_OF_GROWING_SEASON(:)
  real (kind=c_float), allocatable  :: KILLING_FROST_TEMP_LAST_DAY_OF_GROWING_SEASON(:) 

  logical (kind=c_bool), allocatable :: IS_GROWING_SEASON(:)

  character( len=2 ), parameter     :: DATE_DELIMS = "/-"
  real (kind=c_float), parameter    :: NODATA_VALUE = -9999._c_float

contains   

  subroutine interception_bucket_initialize( active_cells )

    logical (kind=c_bool), intent(in)   :: active_cells(:,:)

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

    iNumberOfLanduses = count( iLanduseCodes > 0 )

    !> retrieve growing season interception amount: 'a' term
    call sl_temp_list%clear()
    call sl_temp_list%append("Growing_season_interception")
    call sl_temp_list%append("Interception_growing")
    call sl_temp_list%append("Interception_a_term_growing_season")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                                  &
                                fValues=temp_values,                                  &
                                lFatal=TRUE )

    if (all( temp_values > fTINYVAL) ) then

      lAreLengthsEqual = ( ubound(temp_values,1) == ubound(iLanduseCodes,1) )
      
      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of interception values "  &
                          //"specified for the 'a' term for use during the growing season.",              &
                   sModule=__FILE__, iLine=__LINE__, lFatal=TRUE )

      call move_alloc( temp_values, INTERCEPTION_A_VALUE_GROWING_SEASON )

    endif  
    
    !> retrieve growing season interception amount: 'b' term
    call sl_temp_list%clear()
    call sl_temp_list%append("Growing_season_interception_b")
    call sl_temp_list%append("Interception_growing_b_term")
    call sl_temp_list%append("Interception_b_term_growing_season")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                       &
                                fValues=temp_values,                       &
                                lFatal=FALSE ) 

    if (all( temp_values <= fTINYVAL) ) then

      allocate(INTERCEPTION_B_VALUE_GROWING_SEASON( ubound( iLanduseCodes, 1) ), stat=status )
      INTERCEPTION_B_VALUE_GROWING_SEASON = 0.0_c_float

    else

      lAreLengthsEqual = ( ubound(temp_values,1) == ubound(iLanduseCodes,1) )
      
      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of interception values "  &
                          //"specified for the 'b' term for use during the growing season.",              &
                   sModule=__FILE__, iLine=__LINE__, lFatal=FALSE )

      call move_alloc( temp_values, INTERCEPTION_B_VALUE_GROWING_SEASON )

    endif  

    !> retrieve growing season interception amount: 'n' term
    call sl_temp_list%clear()
    call sl_temp_list%append("Growing_season_interception_n")
    call sl_temp_list%append("Interception_growing_n_term")
    call sl_temp_list%append("Interception_n_term_growing_season")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                                  &
                                fValues=temp_values,                                  &
                                lFatal=FALSE ) 

    if (all( temp_values <= fTINYVAL) ) then

      allocate(INTERCEPTION_N_VALUE_GROWING_SEASON( ubound( iLanduseCodes, 1) ), stat=status )
      INTERCEPTION_N_VALUE_GROWING_SEASON = 1.0_c_float

    else

      lAreLengthsEqual = ( ubound(temp_values,1) == ubound(iLanduseCodes,1) )
      
      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of interception values "  &
                          //"specified for the 'n' term for use during the growing season.",              &
                   sModule=__FILE__, iLine=__LINE__, lFatal=TRUE )

      call move_alloc( temp_values, INTERCEPTION_N_VALUE_GROWING_SEASON )

    endif  

    !> retrieve nongrowing season interception amount: 'a' term
    call sl_temp_list%clear()
    call sl_temp_list%append("Nongrowing_season_interception")
    call sl_temp_list%append("Interception_nongrowing")
    call sl_temp_list%append("Interception_a_term_nongrowing_season")

    call PARAMS%get_parameters( slKeys=sl_temp_list,          &
                                fValues=temp_values,          &
                                lFatal=TRUE ) 

    if (all( temp_values > fTINYVAL) ) then

      lAreLengthsEqual = ( ubound(temp_values,1) == ubound(iLanduseCodes,1) )
      
      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of interception values "  &
                          //"specified for the 'a' term for use during the nongrowing season.",              &
                   sModule=__FILE__, iLine=__LINE__, lFatal=FALSE )

      call move_alloc( temp_values, INTERCEPTION_A_VALUE_NONGROWING_SEASON )

    endif  

    !> retrieve nongrowing season interception amount: 'b' term
    call sl_temp_list%clear()
    call sl_temp_list%append("Nongrowing_season_interception_b")
    call sl_temp_list%append("Interception_nongrowing_b_term")
    call sl_temp_list%append("Interception_b_term_nongrowing_season")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                       &
                                fValues=temp_values,                       &
                                lFatal=FALSE ) 

    if (all( temp_values <= fTINYVAL) ) then

      allocate(INTERCEPTION_B_VALUE_NONGROWING_SEASON( ubound( iLanduseCodes, 1) ), stat=status )
      INTERCEPTION_B_VALUE_NONGROWING_SEASON = 0.0_c_float

    else

      lAreLengthsEqual = ( ubound(temp_values,1) == ubound(iLanduseCodes,1) )
      
      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of interception values "  &
                          //"specified for the 'b' term for use during the nongrowing season.",              &
                   sModule=__FILE__, iLine=__LINE__, lFatal=FALSE )

      call move_alloc( temp_values, INTERCEPTION_B_VALUE_NONGROWING_SEASON )

    endif  

    !> retrieve nongrowing season interception amount: 'n' term
    call sl_temp_list%clear()
    call sl_temp_list%append("Nongrowing_season_interception_n")
    call sl_temp_list%append("Interception_nongrowing_n_term")
    call sl_temp_list%append("Interception_n_term_nongrowing_season")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                                  &
                                fValues=temp_values,                                  &
                                lFatal=FALSE ) 

    if (all( temp_values <= fTINYVAL) ) then

      allocate(INTERCEPTION_N_VALUE_NONGROWING_SEASON( ubound( iLanduseCodes, 1) ), stat=status )
      INTERCEPTION_N_VALUE_NONGROWING_SEASON = 1.0_c_float

    else

      lAreLengthsEqual = ( ubound(temp_values,1) == ubound(iLanduseCodes,1) )
      
      if ( .not. lAreLengthsEqual )     &
        call warn( sMessage="The number of landuses does not match the number of interception values "  &
                          //"specified for the 'n' term for use during the nongrowing season.",              &
                   sModule=__FILE__, iLine=__LINE__, lFatal=FALSE )

      call move_alloc( temp_values, INTERCEPTION_N_VALUE_NONGROWING_SEASON )

    endif  


    !> retrieve first day of growing season
    call sl_temp_list%clear()
    call sl_temp_list%append("First_day_of_growing_season")
    call sl_temp_list%append("First_DOY_growing_season")
    call sl_temp_list%append("Growing_season_start")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                                  &
                                slValues=sl_growing_season_begin,                     &
                                lFatal=.true._c_bool ) 

    !> retrieve last day of growing season
    call sl_temp_list%clear()
    call sl_temp_list%append("Last_day_of_growing_season")
    call sl_temp_list%append("Last_DOY_growing_season")
    call sl_temp_list%append("Growing_season_end")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                                  &
                                slValues=sl_growing_season_end,                       &
                                lFatal=.true._c_bool ) 

    call sl_temp_list%clear()

    !> process first day of growing season. retrieved as a list of strings; 
    !! must convert the strings from mm/dd to DOY
    allocate( FIRST_DAY_OF_GROWING_SEASON( sl_growing_season_begin%count ), stat=status )
    call assert( status==0, "Problem allocating memory.", __FILE__, __LINE__ )

    do indx = 1, sl_growing_season_begin%count
      str_buffer = sl_growing_season_begin%get( indx )
      if ( scan( str_buffer, DATE_DELIMS ) > 0 ) then
        FIRST_DAY_OF_GROWING_SEASON( indx ) = mmdd2doy( str_buffer )
      else
        FIRST_DAY_OF_GROWING_SEASON( indx ) = asInt( str_buffer )
      endif  
    enddo  

    !> process last day of growing season. retrieved as a list of strings; 
    !! must convert the strings from mm/dd to DOY
    allocate( LAST_DAY_OF_GROWING_SEASON( sl_growing_season_end%count ), stat=status )
    call assert( status==0, "Problem allocating memory.", __FILE__, __LINE__ )

    do indx = 1, sl_growing_season_end%count
      str_buffer = sl_growing_season_end%get( indx )
      if ( scan( str_buffer, DATE_DELIMS ) > 0 ) then
        LAST_DAY_OF_GROWING_SEASON( indx ) = mmdd2doy( str_buffer )
      else
        LAST_DAY_OF_GROWING_SEASON( indx ) = asInt( str_buffer )  
      endif  
    enddo  



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
                 sModule=__FILE__, iLine=__LINE__, lFatal=FALSE )

      allocate( GDD_FIRST_DAY_OF_GROWING_SEASON( ubound( iLanduseCodes, 1) ), stat=status )
      call assert( status==0, "Problem allocating memory.", __FILE__, __LINE__)

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
                 sModule=__FILE__, iLine=__LINE__, lFatal=FALSE )

      allocate( KILLING_FROST_TEMP_LAST_DAY_OF_GROWING_SEASON( ubound( iLanduseCodes, 1) ), stat=status )
      call assert( status==0, "Problem allocating memory.", __FILE__, __LINE__)

      KILLING_FROST_TEMP_LAST_DAY_OF_GROWING_SEASON = NODATA_VALUE

    endif


    allocate( IS_GROWING_SEASON( count( active_cells ) ), stat=status )
    call assert( status==0, "Problem allocating memory.", __FILE__, __LINE__ )

    IS_GROWING_SEASON = FALSE

  end subroutine interception_bucket_initialize

!--------------------------------------------------------------------------------------------------

  elemental subroutine interception_bucket_update_growing_season( landuse_index,         &
                                                                  GDD,                   &
                                                                  mean_air_temp,         &
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

  end subroutine interception_bucket_update_growing_season

!--------------------------------------------------------------------------------------------------  

  elemental function interception_bucket_calculate( iLanduseIndex,                                     &
                                                    fPrecip,                                           &
                                                    fFog,                                              &
                                                    fCanopy_Cover_Fraction,                            &
                                                    it_is_growing_season )     result( fInterception )

    integer (kind=c_int), intent(in)   :: iLanduseIndex
    real (kind=c_float), intent(in)    :: fPrecip
    real (kind=c_float), intent(in)    :: fFog
    real (kind=c_float), intent(in)    :: fCanopy_Cover_Fraction
    logical (kind=c_bool), intent(in)  :: it_is_growing_season
    real (kind=c_float)                :: fInterception

    ! [ LOCALS ]
    real (kind=c_float) :: fPotentialInterception
    real (kind=c_float) :: precip_plus_fog

    precip_plus_fog = fPrecip + fFog

    if ( it_is_growing_season ) then

      fPotentialInterception =   INTERCEPTION_A_VALUE_GROWING_SEASON( iLanduseIndex )    &
                               + INTERCEPTION_B_VALUE_GROWING_SEASON( iLanduseIndex )    &
                               * precip_plus_fog                                         &
                               ** INTERCEPTION_N_VALUE_GROWING_SEASON( iLanduseIndex )

    else

      fPotentialInterception =   INTERCEPTION_A_VALUE_NONGROWING_SEASON( iLanduseIndex )    &
                               + INTERCEPTION_B_VALUE_NONGROWING_SEASON( iLanduseIndex )    &
                               * precip_plus_fog                                            &
                               ** INTERCEPTION_N_VALUE_NONGROWING_SEASON( iLanduseIndex )

    endif

    fInterception = min( fPotentialInterception, fPrecip + fFog )  * fCanopy_Cover_Fraction

 
  end function interception_bucket_calculate

end module interception__bucket