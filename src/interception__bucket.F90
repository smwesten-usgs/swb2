module interception__bucket

  use iso_c_binding
  use constants_and_conversions, only : TRUE, FALSE, asInt, fTINYVAL
  use datetime, only                  : mmdd2doy
  use exceptions
  use parameters, only                : PARAMS
  use simulation_datetime, only       : SIM_DT
  use fstring
  use fstring_list, only               : FSTRING_LIST_T
  implicit none

  private

  public :: interception_bucket_initialize, interception_bucket_calculate
  public :: BUCKET_INTERCEPTION_STORAGE_MAX_GROWING_SEASON
  public :: BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON
!  public :: IS_GROWING_SEASON

!  logical (c_bool) :: GROWING_SEASON = .true._c_bool

  integer (c_int), allocatable :: iLanduseCodes(:)
  real (c_float), allocatable  :: INTERCEPTION_A_VALUE_GROWING_SEASON(:)
  real (c_float), allocatable  :: INTERCEPTION_B_VALUE_GROWING_SEASON(:)
  real (c_float), allocatable  :: INTERCEPTION_N_VALUE_GROWING_SEASON(:)
  real (c_float), allocatable  :: INTERCEPTION_A_VALUE_NONGROWING_SEASON(:)
  real (c_float), allocatable  :: INTERCEPTION_B_VALUE_NONGROWING_SEASON(:)
  real (c_float), allocatable  :: INTERCEPTION_N_VALUE_NONGROWING_SEASON(:)
  real (c_float), allocatable  :: BUCKET_INTERCEPTION_STORAGE_MAX_GROWING_SEASON(:)
  real (c_float), allocatable  :: BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON(:)

  !> Form of the bucket interception: I = A + P*B^n

!  logical (c_bool), allocatable :: IS_GROWING_SEASON(:)

  character( len=2 ), parameter     :: DATE_DELIMS = "/-"
  real (c_float), parameter    :: NODATA_VALUE = -9999._c_float

contains

  subroutine interception_bucket_initialize( active_cells )

    logical (c_bool), intent(in)   :: active_cells(:,:)

    ! [ LOCALS ]
    integer (c_int)              :: iNumberOfLanduses
    logical (c_bool)             :: lAreLengthsEqual
    character (len=:), allocatable    :: sTemp
    type (FSTRING_LIST_T)              :: sl_temp_list
    type (FSTRING_LIST_T)              :: sl_growing_season_begin
    type (FSTRING_LIST_T)              :: sl_growing_season_end
    character (len=32)                :: str_buffer
    real (c_float), allocatable  :: temp_values(:)
    integer (c_int)              :: indx
    integer (c_int)              :: status

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

    !> retrieve interception storage max (NONGROWING)
    call sl_temp_list%clear()
    call sl_temp_list%append("Interception_storage_max_nongrowing")
    call sl_temp_list%append("Interception_storage_max_nongrowing_season")
    call sl_temp_list%append("Interception_Storage_Maximum_nongrowing")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                                           &
                                fValues=BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON,     &
                                lFatal=FALSE )

    lAreLengthsEqual = ( ubound(BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON,1) == ubound(iLanduseCodes,1) )

    if ( .not. lAreLengthsEqual ) then
      call warn( sMessage="The number of landuses does not match the number of interception storage "        &
                         //"maximum values for the NONGROWING season "                                       &
                         //"('interception_storage_max_nongrowing').",                                       &
                 sHints="A default value equal to the 'Growing_season_interception_a' was assigned for the"  &
                         //" maximum interception storage",   &
                 sModule=__FILE__, iLine=__LINE__, lFatal=FALSE )
      allocate(temp_values(iNumberOfLanduses), stat=status)
      temp_values = INTERCEPTION_A_VALUE_NONGROWING_SEASON
      call move_alloc(temp_values, BUCKET_INTERCEPTION_STORAGE_MAX_NONGROWING_SEASON)
    endif

    !> retrieve interception storage max (GROWING)
    call sl_temp_list%clear()
    call sl_temp_list%append("Interception_storage_max_growing")
    call sl_temp_list%append("Interception_storage_max_growing_season")
    call sl_temp_list%append("Interception_Storage_Maximum_growing")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                                              &
                                fValues=BUCKET_INTERCEPTION_STORAGE_MAX_GROWING_SEASON,           &
                                lFatal=FALSE )

    lAreLengthsEqual = ( ubound(BUCKET_INTERCEPTION_STORAGE_MAX_GROWING_SEASON,1) == ubound(iLanduseCodes,1) )

    if ( .not. lAreLengthsEqual ) then
      call warn( sMessage="The number of landuses does not match the number of interception storage "      &
                         //"maximum values for the GROWING season "                                        &
                         //"('interception_storage_max_growing').",                                        &
                         sHints="A default value equal to the 'Nongrowing_season_interception_a' was"      &
                         //" assigned for the maximum interception storage",                               &
                 sModule=__FILE__, iLine=__LINE__, lFatal=FALSE )
      allocate(temp_values(iNumberOfLanduses), stat=status)
      temp_values = INTERCEPTION_A_VALUE_GROWING_SEASON
      call move_alloc(temp_values, BUCKET_INTERCEPTION_STORAGE_MAX_GROWING_SEASON)
    endif

  end subroutine interception_bucket_initialize

!--------------------------------------------------------------------------------------------------

  elemental subroutine interception_bucket_calculate( iLanduseIndex,                                     &
                                                      fPrecip,                                           &
                                                      fFog,                                              &
                                                      fCanopy_Cover_Fraction,                            &
                                                      it_is_growing_season,                              &
                                                      fInterception )

    integer (c_int), intent(in)   :: iLanduseIndex
    real (c_float), intent(in)    :: fPrecip
    real (c_float), intent(in)    :: fFog
    real (c_float), intent(in)    :: fCanopy_Cover_Fraction
    logical (c_bool), intent(in)  :: it_is_growing_season
    real (c_float), intent(out)   :: fInterception

    ! [ LOCALS ]
    real (c_float) :: fPotentialInterception
    real (c_float) :: precip_plus_fog

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

  end subroutine interception_bucket_calculate

end module interception__bucket
