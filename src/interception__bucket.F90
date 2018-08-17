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
!  public :: IS_GROWING_SEASON

!  logical (kind=c_bool) :: GROWING_SEASON = .true._c_bool

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

  !> Form of the bucket interception: I = A + P*B^n

!  logical (kind=c_bool), allocatable :: IS_GROWING_SEASON(:)

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
    call sl_temp_list%append("Growing_season_interception_a")
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
                   sModule=__SRCNAME__, iLine=__LINE__, lFatal=TRUE )

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
                   sModule=__SRCNAME__, iLine=__LINE__, lFatal=FALSE )

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
                   sModule=__SRCNAME__, iLine=__LINE__, lFatal=TRUE )

      call move_alloc( temp_values, INTERCEPTION_N_VALUE_GROWING_SEASON )

    endif

    !> retrieve nongrowing season interception amount: 'a' term
    call sl_temp_list%clear()
    call sl_temp_list%append("Nongrowing_season_interception")
    call sl_temp_list%append("Nongrowing_season_interception_a")
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
                   sModule=__SRCNAME__, iLine=__LINE__, lFatal=FALSE )

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
                   sModule=__SRCNAME__, iLine=__LINE__, lFatal=FALSE )

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
                   sModule=__SRCNAME__, iLine=__LINE__, lFatal=FALSE )

      call move_alloc( temp_values, INTERCEPTION_N_VALUE_NONGROWING_SEASON )

    endif

  end subroutine interception_bucket_initialize

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

      fPotentialInterception =   INTERCEPTION_A_VALUE_GROWING_SEASON( iLanduseIndex )   ! &
!                               + INTERCEPTION_B_VALUE_GROWING_SEASON( iLanduseIndex )    &
!                               * precip_plus_fog                                         &
!                               ** INTERCEPTION_N_VALUE_GROWING_SEASON( iLanduseIndex )

    else

      fPotentialInterception =   INTERCEPTION_A_VALUE_NONGROWING_SEASON( iLanduseIndex )   ! &
 !                              + INTERCEPTION_B_VALUE_NONGROWING_SEASON( iLanduseIndex )    &
 !                              * precip_plus_fog                                            &
 !                              ** INTERCEPTION_N_VALUE_NONGROWING_SEASON( iLanduseIndex )

    endif

    fInterception = min( fPotentialInterception, fPrecip + fFog )  * fCanopy_Cover_Fraction


  end function interception_bucket_calculate

end module interception__bucket
