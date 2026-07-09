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
  implicit none

  private

  ! --- Public routines ---
  public :: phenology_update_doy_based
  public :: phenology_update_gdd_threshold

  ! --- Public constants (growth stages) ---
  public :: GROWTH_STAGE_DORMANT, GROWTH_STAGE_MID

  ! --- Growth stage constants ---
  integer(c_int), parameter :: GROWTH_STAGE_DORMANT = 0
  integer(c_int), parameter :: GROWTH_STAGE_INI     = 1
  integer(c_int), parameter :: GROWTH_STAGE_DEV     = 2
  integer(c_int), parameter :: GROWTH_STAGE_MID     = 3
  integer(c_int), parameter :: GROWTH_STAGE_LATE    = 4

contains

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
