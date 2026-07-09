!> @file
!> Contains the unified phenology provider module for SWB2.
!>
!> This module determines vegetation growth state (growing/dormant, growth stage,
!> growth fraction) for all active cells at each daily timestep. It replaces the
!> former growing_season.F90 module with explicit method selection and unified
!> parameter naming.
!>
!> Supported methods:
!>   - DOY_BASED: binary on/off from Growing_season_start_date / Growing_season_end_date
!>   - GDD_THRESHOLD: binary on/off from Growing_season_start_GDD / Killing_frost_temperature
!>   - FAO56_DATES: (Phase 2) continuous growth_fraction from date-based stage lengths
!>   - FAO56_GDD: (Phase 3) continuous growth_fraction from GDD-based stage lengths

module phenology

  use iso_c_binding, only : c_int, c_float, c_bool
  use constants_and_conversions, only : TRUE, FALSE
  use exceptions, only : assert, warn
  use datetime, only : mmdd2doy
  use parameters, only : PARAMS
  use fstring_list, only : FSTRING_LIST_T, NA_FLOAT
  implicit none

  private

  ! --- Public routines ---
  public :: phenology_initialize
  public :: phenology_update
  public :: phenology_update_doy_based
  public :: phenology_update_gdd_threshold

  ! --- Public constants (growth stages) ---
  public :: GROWTH_STAGE_DORMANT, GROWTH_STAGE_MID

  ! --- Public constants (method indices) ---
  public :: PHENOLOGY_NONE, PHENOLOGY_DOY_BASED, PHENOLOGY_GDD_THRESHOLD

  ! --- Public module data (per-landuse arrays, populated by phenology_initialize) ---
  public :: GROWING_SEASON_START_DOY, GROWING_SEASON_END_DOY
  public :: GROWING_SEASON_START_GDD, KILLING_FROST_TEMP
  public :: PHENOLOGY_METHOD_INDEX

  ! --- Growth stage constants ---
  integer(c_int), parameter :: GROWTH_STAGE_DORMANT = 0
  integer(c_int), parameter :: GROWTH_STAGE_INI     = 1
  integer(c_int), parameter :: GROWTH_STAGE_DEV     = 2
  integer(c_int), parameter :: GROWTH_STAGE_MID     = 3
  integer(c_int), parameter :: GROWTH_STAGE_LATE    = 4

  ! --- Phenology method constants ---
  integer(c_int), parameter :: PHENOLOGY_NONE          = 0
  integer(c_int), parameter :: PHENOLOGY_DOY_BASED     = 1
  integer(c_int), parameter :: PHENOLOGY_GDD_THRESHOLD = 2
  integer(c_int), parameter :: PHENOLOGY_FAO56_DATES   = 3
  integer(c_int), parameter :: PHENOLOGY_FAO56_GDD     = 4

  ! --- Module-level per-landuse arrays ---
  integer(c_int), allocatable :: GROWING_SEASON_START_DOY(:)
  integer(c_int), allocatable :: GROWING_SEASON_END_DOY(:)
  real(c_float), allocatable  :: GROWING_SEASON_START_GDD(:)
  real(c_float), allocatable  :: KILLING_FROST_TEMP(:)
  integer(c_int), allocatable :: PHENOLOGY_METHOD_INDEX(:)

  ! --- Private constants ---
  character(len=2), parameter  :: DATE_DELIMS = "/-"
  integer(c_int), parameter    :: NODATA_INT = -9999

contains

  !---------------------------------------------------------------------------
  !> @brief Initialize the phenology module from lookup table parameters.
  !!
  !! Reads Growing_season_start_date, Growing_season_end_date,
  !! Growing_season_start_GDD, and Killing_frost_temperature from the
  !! PARAMS lookup table. Populates module-level per-landuse arrays.
  !!
  !! Growing_season_start_date and Growing_season_end_date accept either
  !! mm/dd format (e.g., "03/17", "4-01") or integer DOY (e.g., "91").
  !!
  !! Columns that are absent or contain "<NA>" are filled with NODATA values.
  !---------------------------------------------------------------------------
  subroutine phenology_initialize()

    ! [ LOCALS ]
    integer(c_int)                     :: number_of_landuses
    integer(c_int)                     :: indx, status
    integer(c_int), allocatable        :: landuse_codes(:)
    type(FSTRING_LIST_T)               :: sl_start_date_strings
    type(FSTRING_LIST_T)               :: sl_end_date_strings
    real(c_float), allocatable         :: temp_float_values(:)
    character(len=32)                  :: str_buffer

    !> Determine number of landuse codes
    call PARAMS%get_parameters( sKey="LU_Code", iValues=landuse_codes )
    number_of_landuses = size( landuse_codes )

    ! --- Growing_season_start_date (mm/dd or DOY) ---
    call PARAMS%get_parameters( sKey="Growing_season_start_date", &
                                slValues=sl_start_date_strings,   &
                                lFatal=FALSE )

    allocate( GROWING_SEASON_START_DOY( number_of_landuses ), stat=status )
    call assert( status == 0, "phenology_initialize: allocation failed", &
                 __FILE__, __LINE__ )

    if ( sl_start_date_strings%count == number_of_landuses ) then

      do indx = 1, number_of_landuses
        str_buffer = sl_start_date_strings%get( indx )
        if ( str_buffer == "<NA>" .or. str_buffer == "" ) then
          GROWING_SEASON_START_DOY( indx ) = NODATA_INT
        else if ( scan( str_buffer, DATE_DELIMS ) > 0 ) then
          GROWING_SEASON_START_DOY( indx ) = mmdd2doy( str_buffer, &
            "Growing_season_start_date" )
        else
          read( str_buffer, *, iostat=status ) GROWING_SEASON_START_DOY( indx )
          if ( status /= 0 ) GROWING_SEASON_START_DOY( indx ) = NODATA_INT
        end if
      end do

    else
      GROWING_SEASON_START_DOY = NODATA_INT
    end if

    ! --- Growing_season_end_date (mm/dd or DOY) ---
    call PARAMS%get_parameters( sKey="Growing_season_end_date", &
                                slValues=sl_end_date_strings,   &
                                lFatal=FALSE )

    allocate( GROWING_SEASON_END_DOY( number_of_landuses ), stat=status )
    call assert( status == 0, "phenology_initialize: allocation failed", &
                 __FILE__, __LINE__ )

    if ( sl_end_date_strings%count == number_of_landuses ) then

      do indx = 1, number_of_landuses
        str_buffer = sl_end_date_strings%get( indx )
        if ( str_buffer == "<NA>" .or. str_buffer == "" ) then
          GROWING_SEASON_END_DOY( indx ) = NODATA_INT
        else if ( scan( str_buffer, DATE_DELIMS ) > 0 ) then
          GROWING_SEASON_END_DOY( indx ) = mmdd2doy( str_buffer, &
            "Growing_season_end_date" )
        else
          read( str_buffer, *, iostat=status ) GROWING_SEASON_END_DOY( indx )
          if ( status /= 0 ) GROWING_SEASON_END_DOY( indx ) = NODATA_INT
        end if
      end do

    else
      GROWING_SEASON_END_DOY = NODATA_INT
    end if

    ! --- Growing_season_start_GDD (float) ---
    call PARAMS%get_parameters( sKey="Growing_season_start_GDD", &
                                fValues=temp_float_values,       &
                                lFatal=FALSE )

    if ( allocated( temp_float_values ) &
         .and. size( temp_float_values ) == number_of_landuses ) then
      call move_alloc( temp_float_values, GROWING_SEASON_START_GDD )
    else
      allocate( GROWING_SEASON_START_GDD( number_of_landuses ), stat=status )
      call assert( status == 0, "phenology_initialize: allocation failed", &
                   __FILE__, __LINE__ )
      GROWING_SEASON_START_GDD = NA_FLOAT
    end if

    ! --- Killing_frost_temperature (float) ---
    call PARAMS%get_parameters( sKey="Killing_frost_temperature", &
                                fValues=temp_float_values,        &
                                lFatal=FALSE )

    if ( allocated( temp_float_values ) &
         .and. size( temp_float_values ) == number_of_landuses ) then
      call move_alloc( temp_float_values, KILLING_FROST_TEMP )
    else
      allocate( KILLING_FROST_TEMP( number_of_landuses ), stat=status )
      call assert( status == 0, "phenology_initialize: allocation failed", &
                   __FILE__, __LINE__ )
      KILLING_FROST_TEMP = NA_FLOAT
    end if

    ! --- Determine method per landuse based on which columns have data ---
    allocate( PHENOLOGY_METHOD_INDEX( number_of_landuses ), stat=status )
    call assert( status == 0, "phenology_initialize: allocation failed", &
                 __FILE__, __LINE__ )

    do indx = 1, number_of_landuses
      if ( GROWING_SEASON_START_DOY(indx) /= NODATA_INT ) then
        PHENOLOGY_METHOD_INDEX(indx) = PHENOLOGY_DOY_BASED
      else if ( GROWING_SEASON_START_GDD(indx) /= NA_FLOAT ) then
        PHENOLOGY_METHOD_INDEX(indx) = PHENOLOGY_GDD_THRESHOLD
      else
        PHENOLOGY_METHOD_INDEX(indx) = PHENOLOGY_NONE
      end if
    end do

  end subroutine phenology_initialize

  !---------------------------------------------------------------------------
  !> @brief Dispatch phenology update to the appropriate method for a landuse.
  !!
  !! Reads PHENOLOGY_METHOD_INDEX for the given landuse and calls the
  !! corresponding update subroutine. All possible inputs are passed;
  !! each method uses only what it needs.
  !!
  !! @param[in]    landuse_index            Index into per-landuse arrays (1-based)
  !! @param[in]    current_doy              Current day of year (1-366)
  !! @param[in]    current_gdd              Accumulated GDD for current year
  !! @param[in]    mean_air_temperature     Current mean daily air temperature
  !! @param[in]    it_is_growing_season_in  Growing season state from previous day
  !! @param[out]   growth_fraction          0.0 (dormant) to 1.0 (full growth)
  !! @param[out]   it_is_growing_season     Updated growing season state
  !! @param[out]   growth_stage             Current growth stage enum value
  !---------------------------------------------------------------------------
  subroutine phenology_update( landuse_index,            &
                               current_doy,              &
                               current_gdd,              &
                               mean_air_temperature,     &
                               it_is_growing_season_in,  &
                               growth_fraction,          &
                               it_is_growing_season,     &
                               growth_stage )

    integer(c_int), intent(in)   :: landuse_index
    integer(c_int), intent(in)   :: current_doy
    real(c_float), intent(in)    :: current_gdd
    real(c_float), intent(in)    :: mean_air_temperature
    logical(c_bool), intent(in)  :: it_is_growing_season_in
    real(c_float), intent(out)   :: growth_fraction
    logical(c_bool), intent(out) :: it_is_growing_season
    integer(c_int), intent(out)  :: growth_stage

    select case ( PHENOLOGY_METHOD_INDEX(landuse_index) )

      case ( PHENOLOGY_DOY_BASED )
        call phenology_update_doy_based(              &
          current_doy=current_doy,                    &
          growing_season_start_doy=GROWING_SEASON_START_DOY(landuse_index), &
          growing_season_end_doy=GROWING_SEASON_END_DOY(landuse_index),     &
          growth_fraction=growth_fraction,            &
          it_is_growing_season=it_is_growing_season,  &
          growth_stage=growth_stage )

      case ( PHENOLOGY_GDD_THRESHOLD )
        call phenology_update_gdd_threshold(          &
          current_gdd=current_gdd,                    &
          mean_air_temperature=mean_air_temperature,  &
          growing_season_start_gdd=GROWING_SEASON_START_GDD(landuse_index), &
          killing_frost_temperature=KILLING_FROST_TEMP(landuse_index),      &
          it_is_growing_season_in=it_is_growing_season_in,                  &
          growth_fraction=growth_fraction,            &
          it_is_growing_season=it_is_growing_season,  &
          growth_stage=growth_stage )

      case default
        ! PHENOLOGY_NONE: no growing season defined — always dormant
        growth_fraction      = 0.0_c_float
        it_is_growing_season = FALSE
        growth_stage         = GROWTH_STAGE_DORMANT

    end select

  end subroutine phenology_update

  !---------------------------------------------------------------------------
  !> @brief Determine phenology state for DOY_BASED method.
  !!
  !! Binary on/off: growth_fraction is 1.0 within the growing season and
  !! 0.0 outside. Handles winter crops where start DOY > end DOY (season
  !! wraps across the year boundary).
  !!
  !! @param[in]  current_doy               Current day of year (1-366)
  !! @param[in]  growing_season_start_doy  First day of growing season (1-366)
  !! @param[in]  growing_season_end_doy    Last day of growing season (1-366, inclusive)
  !! @param[out] growth_fraction           0.0 (dormant) or 1.0 (growing)
  !! @param[out] it_is_growing_season      .true. if within growing season
  !! @param[out] growth_stage              GROWTH_STAGE_DORMANT or GROWTH_STAGE_MID
  !---------------------------------------------------------------------------
  pure subroutine phenology_update_doy_based( current_doy,              &
                                              growing_season_start_doy, &
                                              growing_season_end_doy,   &
                                              growth_fraction,          &
                                              it_is_growing_season,     &
                                              growth_stage )

    integer(c_int), intent(in)   :: current_doy
    integer(c_int), intent(in)   :: growing_season_start_doy
    integer(c_int), intent(in)   :: growing_season_end_doy
    real(c_float), intent(out)   :: growth_fraction
    logical(c_bool), intent(out) :: it_is_growing_season
    integer(c_int), intent(out)  :: growth_stage

    ! [ LOCALS ]
    logical :: is_growing

    if ( growing_season_start_doy <= growing_season_end_doy ) then

      ! Normal season: start and end within the same calendar year
      is_growing = ( current_doy >= growing_season_start_doy ) &
             .and. ( current_doy <= growing_season_end_doy )

    else

      ! Winter crop: season wraps across year boundary (e.g., start=275, end=120)
      is_growing = ( current_doy >= growing_season_start_doy ) &
              .or. ( current_doy <= growing_season_end_doy )

    end if

    if ( is_growing ) then
      growth_fraction      = 1.0_c_float
      it_is_growing_season = TRUE
      growth_stage         = GROWTH_STAGE_MID
    else
      growth_fraction      = 0.0_c_float
      it_is_growing_season = FALSE
      growth_stage         = GROWTH_STAGE_DORMANT
    end if

  end subroutine phenology_update_doy_based

  !---------------------------------------------------------------------------
  !> @brief Determine phenology state for GDD_THRESHOLD method.
  !!
  !! Binary on/off driven by thermal accumulation. Growing season starts when
  !! accumulated GDD reaches the threshold. Season ends when mean air temperature
  !! drops to or below the killing frost temperature.
  !!
  !! This subroutine is stateful in that it requires the current growing-season
  !! state as input: if already growing, it checks for killing frost; if dormant,
  !! it checks whether the GDD threshold has been reached.
  !!
  !! @param[in]  current_gdd               Accumulated GDD for current year (degree-days)
  !! @param[in]  mean_air_temperature      Current mean daily air temperature
  !! @param[in]  growing_season_start_gdd  GDD threshold for growing season start
  !! @param[in]  killing_frost_temperature Temperature at or below which season ends
  !! @param[in]  it_is_growing_season_in   Growing season state from previous timestep
  !! @param[out] growth_fraction           0.0 (dormant) or 1.0 (growing)
  !! @param[out] it_is_growing_season      Updated growing season state
  !! @param[out] growth_stage              GROWTH_STAGE_DORMANT or GROWTH_STAGE_MID
  !---------------------------------------------------------------------------
  pure subroutine phenology_update_gdd_threshold( current_gdd,               &
                                                  mean_air_temperature,       &
                                                  growing_season_start_gdd,   &
                                                  killing_frost_temperature,  &
                                                  it_is_growing_season_in,    &
                                                  growth_fraction,            &
                                                  it_is_growing_season,       &
                                                  growth_stage )

    real(c_float), intent(in)    :: current_gdd
    real(c_float), intent(in)    :: mean_air_temperature
    real(c_float), intent(in)    :: growing_season_start_gdd
    real(c_float), intent(in)    :: killing_frost_temperature
    logical(c_bool), intent(in)  :: it_is_growing_season_in
    real(c_float), intent(out)   :: growth_fraction
    logical(c_bool), intent(out) :: it_is_growing_season
    integer(c_int), intent(out)  :: growth_stage

    ! [ LOCALS ]
    logical :: is_growing

    if ( it_is_growing_season_in ) then

      ! Already growing — check for killing frost
      if ( mean_air_temperature <= killing_frost_temperature ) then
        is_growing = .false.
      else
        is_growing = .true.
      end if

    else

      ! Dormant — check whether GDD threshold has been reached
      if ( current_gdd >= growing_season_start_gdd ) then
        is_growing = .true.
      else
        is_growing = .false.
      end if

    end if

    if ( is_growing ) then
      growth_fraction      = 1.0_c_float
      it_is_growing_season = TRUE
      growth_stage         = GROWTH_STAGE_MID
    else
      growth_fraction      = 0.0_c_float
      it_is_growing_season = FALSE
      growth_stage         = GROWTH_STAGE_DORMANT
    end if

  end subroutine phenology_update_gdd_threshold

end module phenology
