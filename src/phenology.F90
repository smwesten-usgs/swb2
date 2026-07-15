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
!>   - FAO56_DATES: continuous growth_fraction from date-based stage lengths
!>   - FAO56_GDD: continuous growth_fraction from GDD-based stage lengths
!>
!> ## growth_fraction semantics
!>
!> growth_fraction (0.0–1.0) represents the structural development of the plant —
!> how much of its full physical form (canopy, root system) is in place. It is NOT
!> a measure of physiological activity or transpiration rate.
!>
!> Trajectory for FAO56 methods:
!>   - DORMANT: 0.0 (bare soil / no leaves / post-harvest)
!>   - INI:     0.0 → 0.1 (emergence, seedling establishment)
!>   - DEV:     0.1 → 1.0 (rapid canopy and root development)
!>   - MID:     1.0 (fully developed)
!>   - LATE:    1.0 (structure remains intact during senescence)
!>   - DORMANT: 0.0 (harvest / leaf-off / frost kill — abrupt transition)
!>
!> For DOY_BASED and GDD_THRESHOLD methods: strictly 0.0 or 1.0 (binary).
!>
!> ## Intended downstream consumers of growth_fraction
!>
!>   - interception__bucket.F90: interpolate storage capacity between nongrowing
!>     and growing-season maxima
!>   - rooting_depth__FAO56.F90: (future) scale Zr between Zr_min and Zr_max
!>   - any module needing a continuous 0–1 signal for vegetation presence
!>
!> Note: Kcb interpolation does NOT use growth_fraction. It uses growth_stage +
!> stage_fraction, which captures the late-season Kcb decline independently.
!> Rooting depth and plant height currently derive from Kcb directly (FAO-56
!> methodology); growth_fraction is an alternative for modules that need a
!> structural signal without coupling to the crop coefficient method.

module phenology

  use iso_c_binding, only : c_int, c_float, c_bool
  use constants_and_conversions, only : TRUE, FALSE
  use exceptions, only : assert, warn
  use datetime, only : mmdd2doy
  use logfiles, only : LOGS, LOG_ALL
  use parameters, only : PARAMETERS_T
  use fstring, only : asCharacter
  use fstring_list, only : FSTRING_LIST_T, NA_FLOAT
  implicit none

  private

  ! --- Public routines ---
  public :: phenology_initialize
  public :: phenology_update
  public :: phenology_update_doy_based
  public :: phenology_update_gdd_threshold
  public :: phenology_update_fao56_dates
  public :: phenology_update_fao56_gdd

  ! --- Public constants (growth stages) ---
  public :: GROWTH_STAGE_DORMANT, GROWTH_STAGE_INI, GROWTH_STAGE_DEV
  public :: GROWTH_STAGE_MID, GROWTH_STAGE_LATE

  ! --- Public constants (method indices) ---
  public :: PHENOLOGY_NONE, PHENOLOGY_DOY_BASED, PHENOLOGY_GDD_THRESHOLD
  public :: PHENOLOGY_FAO56_DATES, PHENOLOGY_FAO56_GDD

  ! --- Public module data (per-landuse arrays, populated by phenology_initialize) ---
  public :: GROWING_SEASON_START_DOY, GROWING_SEASON_END_DOY
  public :: GROWING_SEASON_START_GDD, KILLING_FROST_TEMP
  public :: PHENOLOGY_METHOD_INDEX
  public :: L_INI_DAYS, L_DEV_DAYS, L_MID_DAYS, L_LATE_DAYS
  public :: GDD_INI, GDD_DEV, GDD_MID, GDD_LATE

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

  ! --- FAO56 date-based stage lengths (days) ---
  integer(c_int), allocatable :: L_INI_DAYS(:)
  integer(c_int), allocatable :: L_DEV_DAYS(:)
  integer(c_int), allocatable :: L_MID_DAYS(:)
  integer(c_int), allocatable :: L_LATE_DAYS(:)

  ! --- FAO56 GDD-based stage lengths (degree-days) ---
  real(c_float), allocatable  :: GDD_INI(:)
  real(c_float), allocatable  :: GDD_DEV(:)
  real(c_float), allocatable  :: GDD_MID(:)
  real(c_float), allocatable  :: GDD_LATE(:)

  ! --- Private constants ---
  character(len=2), parameter  :: DATE_DELIMS = "/-"
  integer(c_int), parameter    :: NODATA_INT = -9999

contains

  !---------------------------------------------------------------------------
  !> @brief Initialize the phenology module from lookup table parameters.
  !!
  !! Reads Growing_season_start_date, Growing_season_end_date,
  !! Growing_season_start_GDD, and Killing_frost_temperature from the
  !! Reads Growing_season_start_date, Growing_season_end_date,
  !! Growing_season_start_GDD, and Killing_frost_temperature from the
  !! provided PARAMETERS_T instance. Populates module-level per-landuse arrays.
  !!
  !! Growing_season_start_date and Growing_season_end_date accept either
  !! mm/dd format (e.g., "03/17", "4-01") or integer DOY (e.g., "91").
  !!
  !! Columns that are absent or contain "<NA>" are filled with NODATA values.
  !!
  !! @param[inout] params  PARAMETERS_T instance containing lookup table data.
  !---------------------------------------------------------------------------
  subroutine phenology_initialize(params)

    type(PARAMETERS_T), intent(inout) :: params

    ! [ LOCALS ]
    integer(c_int)                     :: number_of_landuses
    integer(c_int)                     :: indx, status
    integer(c_int), allocatable        :: landuse_codes(:)
    type(FSTRING_LIST_T)               :: sl_start_date_strings
    type(FSTRING_LIST_T)               :: sl_end_date_strings
    real(c_float), allocatable         :: temp_float_values(:)
    character(len=32)                  :: str_buffer

    !> Determine number of landuse codes
    call params%get_parameters( sKey="LU_Code", iValues=landuse_codes )
    number_of_landuses = size( landuse_codes )

    ! --- Growing_season_start_date (mm/dd or DOY) ---
    call params%get_parameters( sKey="Growing_season_start_date", &
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
    call params%get_parameters( sKey="Growing_season_end_date", &
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
    call params%get_parameters( sKey="Growing_season_start_GDD", &
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
    call params%get_parameters( sKey="Killing_frost_temperature", &
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

    ! --- FAO56 date-based stage lengths (L_ini, L_dev, L_mid, L_late) ---
    call params%get_parameters( sKey="L_ini", fValues=temp_float_values, lFatal=FALSE )
    allocate( L_INI_DAYS( number_of_landuses ), stat=status )
    call assert( status == 0, "phenology_initialize: allocation failed", __FILE__, __LINE__ )
    if ( allocated( temp_float_values ) &
         .and. size( temp_float_values ) == number_of_landuses ) then
      L_INI_DAYS = int( temp_float_values, c_int )
      deallocate( temp_float_values )
    else
      L_INI_DAYS = NODATA_INT
    end if

    call params%get_parameters( sKey="L_dev", fValues=temp_float_values, lFatal=FALSE )
    allocate( L_DEV_DAYS( number_of_landuses ), stat=status )
    call assert( status == 0, "phenology_initialize: allocation failed", __FILE__, __LINE__ )
    if ( allocated( temp_float_values ) &
         .and. size( temp_float_values ) == number_of_landuses ) then
      L_DEV_DAYS = int( temp_float_values, c_int )
      deallocate( temp_float_values )
    else
      L_DEV_DAYS = NODATA_INT
    end if

    call params%get_parameters( sKey="L_mid", fValues=temp_float_values, lFatal=FALSE )
    allocate( L_MID_DAYS( number_of_landuses ), stat=status )
    call assert( status == 0, "phenology_initialize: allocation failed", __FILE__, __LINE__ )
    if ( allocated( temp_float_values ) &
         .and. size( temp_float_values ) == number_of_landuses ) then
      L_MID_DAYS = int( temp_float_values, c_int )
      deallocate( temp_float_values )
    else
      L_MID_DAYS = NODATA_INT
    end if

    call params%get_parameters( sKey="L_late", fValues=temp_float_values, lFatal=FALSE )
    allocate( L_LATE_DAYS( number_of_landuses ), stat=status )
    call assert( status == 0, "phenology_initialize: allocation failed", __FILE__, __LINE__ )
    if ( allocated( temp_float_values ) &
         .and. size( temp_float_values ) == number_of_landuses ) then
      L_LATE_DAYS = int( temp_float_values, c_int )
      deallocate( temp_float_values )
    else
      L_LATE_DAYS = NODATA_INT
    end if

    ! --- FAO56 GDD-based stage lengths (GDD_ini, GDD_dev, GDD_mid, GDD_late) ---
    call params%get_parameters( sKey="GDD_ini", fValues=temp_float_values, lFatal=FALSE )
    if ( allocated( temp_float_values ) &
         .and. size( temp_float_values ) == number_of_landuses ) then
      call move_alloc( temp_float_values, GDD_INI )
    else
      allocate( GDD_INI( number_of_landuses ), stat=status )
      call assert( status == 0, "phenology_initialize: allocation failed", __FILE__, __LINE__ )
      GDD_INI = NA_FLOAT
    end if

    call params%get_parameters( sKey="GDD_dev", fValues=temp_float_values, lFatal=FALSE )
    if ( allocated( temp_float_values ) &
         .and. size( temp_float_values ) == number_of_landuses ) then
      call move_alloc( temp_float_values, GDD_DEV )
    else
      allocate( GDD_DEV( number_of_landuses ), stat=status )
      call assert( status == 0, "phenology_initialize: allocation failed", __FILE__, __LINE__ )
      GDD_DEV = NA_FLOAT
    end if

    call params%get_parameters( sKey="GDD_mid", fValues=temp_float_values, lFatal=FALSE )
    if ( allocated( temp_float_values ) &
         .and. size( temp_float_values ) == number_of_landuses ) then
      call move_alloc( temp_float_values, GDD_MID )
    else
      allocate( GDD_MID( number_of_landuses ), stat=status )
      call assert( status == 0, "phenology_initialize: allocation failed", __FILE__, __LINE__ )
      GDD_MID = NA_FLOAT
    end if

    call params%get_parameters( sKey="GDD_late", fValues=temp_float_values, lFatal=FALSE )
    if ( allocated( temp_float_values ) &
         .and. size( temp_float_values ) == number_of_landuses ) then
      call move_alloc( temp_float_values, GDD_LATE )
    else
      allocate( GDD_LATE( number_of_landuses ), stat=status )
      call assert( status == 0, "phenology_initialize: allocation failed", __FILE__, __LINE__ )
      GDD_LATE = NA_FLOAT
    end if

    ! --- Assign method per landuse based on which columns have data ---
    ! Priority: FAO56_DATES > FAO56_GDD > DOY_BASED > GDD_THRESHOLD > NONE
    do indx = 1, number_of_landuses

      if ( GROWING_SEASON_START_DOY(indx) /= NODATA_INT   &
           .and. L_INI_DAYS(indx) /= NODATA_INT           &
           .and. L_DEV_DAYS(indx) /= NODATA_INT           &
           .and. L_MID_DAYS(indx) /= NODATA_INT           &
           .and. L_LATE_DAYS(indx) /= NODATA_INT ) then
        PHENOLOGY_METHOD_INDEX(indx) = PHENOLOGY_FAO56_DATES

      else if ( GROWING_SEASON_START_GDD(indx) > NA_FLOAT &
                .and. GDD_INI(indx) > NA_FLOAT            &
                .and. GDD_DEV(indx) > NA_FLOAT            &
                .and. GDD_MID(indx) > NA_FLOAT            &
                .and. GDD_LATE(indx) > NA_FLOAT ) then
        PHENOLOGY_METHOD_INDEX(indx) = PHENOLOGY_FAO56_GDD

      else if ( GROWING_SEASON_START_DOY(indx) /= NODATA_INT ) then
        PHENOLOGY_METHOD_INDEX(indx) = PHENOLOGY_DOY_BASED

      else if ( GROWING_SEASON_START_GDD(indx) > NA_FLOAT ) then
        PHENOLOGY_METHOD_INDEX(indx) = PHENOLOGY_GDD_THRESHOLD

      else
        PHENOLOGY_METHOD_INDEX(indx) = PHENOLOGY_NONE
      end if

    end do

    ! --- Write summary table to logfile ---
    call LOGS%write( "", iLinesBefore=1 )
    call LOGS%write( "## Phenology Method Summary ##", iLogLevel=LOG_ALL, lEcho=FALSE )
    call LOGS%write( "LU_Code | Method          | Start        | End/Parameters", &
                     iLogLevel=LOG_ALL, lEcho=FALSE )
    call LOGS%write( "--------|-----------------|--------------|-----------------------------", &
                     iLogLevel=LOG_ALL, lEcho=FALSE )

    do indx = 1, number_of_landuses

      select case ( PHENOLOGY_METHOD_INDEX(indx) )

        case ( PHENOLOGY_FAO56_DATES )
          call LOGS%write( "  "//asCharacter(landuse_codes(indx))        &
            //"   | FAO56_DATES     | DOY "                              &
            //asCharacter(GROWING_SEASON_START_DOY(indx))                &
            //"     | L: "//asCharacter(L_INI_DAYS(indx))//"/"           &
            //asCharacter(L_DEV_DAYS(indx))//"/"                         &
            //asCharacter(L_MID_DAYS(indx))//"/"                         &
            //asCharacter(L_LATE_DAYS(indx))//" days",                   &
            iLogLevel=LOG_ALL, lEcho=FALSE )

        case ( PHENOLOGY_FAO56_GDD )
          call LOGS%write( "  "//asCharacter(landuse_codes(indx))        &
            //"   | FAO56_GDD       | GDD "                              &
            //asCharacter(int(GROWING_SEASON_START_GDD(indx)))           &
            //"     | GDD: "//asCharacter(int(GDD_INI(indx)))//"/"       &
            //asCharacter(int(GDD_DEV(indx)))//"/"                       &
            //asCharacter(int(GDD_MID(indx)))//"/"                       &
            //asCharacter(int(GDD_LATE(indx)))//" | frost: "             &
            //asCharacter(int(KILLING_FROST_TEMP(indx)))//"F",           &
            iLogLevel=LOG_ALL, lEcho=FALSE )

        case ( PHENOLOGY_DOY_BASED )
          call LOGS%write( "  "//asCharacter(landuse_codes(indx))        &
            //"   | DOY_BASED       | DOY "                              &
            //asCharacter(GROWING_SEASON_START_DOY(indx))                &
            //"     | DOY "                                              &
            //asCharacter(GROWING_SEASON_END_DOY(indx)),                 &
            iLogLevel=LOG_ALL, lEcho=FALSE )

        case ( PHENOLOGY_GDD_THRESHOLD )
          call LOGS%write( "  "//asCharacter(landuse_codes(indx))        &
            //"   | GDD_THRESHOLD   | GDD "                              &
            //asCharacter(int(GROWING_SEASON_START_GDD(indx)))           &
            //"     | frost: "                                           &
            //asCharacter(int(KILLING_FROST_TEMP(indx)))//"F",           &
            iLogLevel=LOG_ALL, lEcho=FALSE )

        case default
          call LOGS%write( "  "//asCharacter(landuse_codes(indx))        &
            //"   | NONE            | --           | --",                 &
            iLogLevel=LOG_ALL, lEcho=FALSE )

      end select

    end do

    call LOGS%write( "", iLinesAfter=1 )

    ! --- Warn if monthly Kcb columns exist but no phenology method is defined ---
    call validate_monthly_kcb_has_phenology(params, landuse_codes, number_of_landuses)

    ! --- Detect legacy column names and emit fatal error with migration guide ---
    call detect_legacy_column_names(params)

  end subroutine phenology_initialize

  !---------------------------------------------------------------------------
  !> @brief Warn when monthly Kcb columns are present but no phenology method is defined.
  !!
  !! If a land use has monthly Kcb values (Kcb_Jan...Kcb_Dec) but PHENOLOGY_NONE,
  !! growth_fraction will be 0.0 permanently, which means interception capacity
  !! will be stuck at the nongrowing value. This is almost certainly unintended.
  !!
  !! @param[inout] params              PARAMETERS_T instance to probe
  !! @param[in]    landuse_codes       Array of land use code values
  !! @param[in]    number_of_landuses  Number of land uses in the table
  !---------------------------------------------------------------------------
  subroutine validate_monthly_kcb_has_phenology(params, landuse_codes, number_of_landuses)

    type(PARAMETERS_T), intent(inout) :: params
    integer(c_int), intent(in)        :: landuse_codes(:)
    integer(c_int), intent(in)        :: number_of_landuses

    ! [ LOCALS ]
    real(c_float), allocatable :: kcb_jan_vals(:)
    integer(c_int) :: indx

    ! Probe for Kcb_Jan — if present, monthly Kcb is in use
    call params%get_parameters( sKey="Kcb_Jan", fValues=kcb_jan_vals, lFatal=FALSE )

    if ( .not. allocated( kcb_jan_vals ) ) return
    if ( size( kcb_jan_vals ) /= number_of_landuses ) return

    do indx = 1, number_of_landuses

      if ( kcb_jan_vals(indx) > 0.0_c_float &
           .and. PHENOLOGY_METHOD_INDEX(indx) == PHENOLOGY_NONE ) then

        call warn( "Land use "//asCharacter( landuse_codes(indx) )          &
          //" has monthly Kcb values but no phenology method."               &
          //"~ growth_fraction will be 0.0 (dormant) permanently,"           &
          //" which affects interception capacity."                           &
          //"~ Add Growing_season_start_date and Growing_season_end_date"     &
          //" columns (e.g. 01/01 and 12/31 for evergreen land uses)"        &
          //" to enable DOY_BASED phenology.",                               &
          lFatal=TRUE )

      end if

    end do

  end subroutine validate_monthly_kcb_has_phenology

  !---------------------------------------------------------------------------
  !> @brief Detect legacy column names and emit fatal errors with migration guide.
  !!
  !! Probes params for known legacy column names that the old growing_season.F90
  !! and crop_coefficients__fao56.F90 modules used. If found, emits a fatal
  !! warning telling the user exactly what to rename.
  !!
  !! @param[inout] params  PARAMETERS_T instance to probe for legacy names.
  !---------------------------------------------------------------------------
  subroutine detect_legacy_column_names(params)

    type(PARAMETERS_T), intent(inout) :: params

    ! [ LOCALS ]
    type(FSTRING_LIST_T) :: sl_test
    integer(c_int) :: num_found

    ! --- Legacy names and their replacements ---
    ! Each check: probe PARAMS for the old name. If data comes back,
    ! the user has the old name in their table.

    ! Planting_date → Growing_season_start_date
    call params%get_parameters( sKey="Planting_date", slValues=sl_test, lFatal=FALSE )
    if ( sl_test%count > 0 .and. sl_test%count_matching("<NA>") < sl_test%count ) then
      call warn( "Legacy column name 'Planting_date' detected in lookup table."         &
        //"~ Please rename to 'Growing_season_start_date'.",                             &
        lFatal=TRUE )
    end if

    ! First_day_of_growing_season → Growing_season_start_date
    call params%get_parameters( sKey="First_day_of_growing_season", slValues=sl_test, lFatal=FALSE )
    if ( sl_test%count > 0 .and. sl_test%count_matching("<NA>") < sl_test%count ) then
      call warn( "Legacy column name 'First_day_of_growing_season' detected."           &
        //"~ Please rename to 'Growing_season_start_date'.",                             &
        lFatal=TRUE )
    end if

    ! Last_day_of_growing_season → Growing_season_end_date
    call params%get_parameters( sKey="Last_day_of_growing_season", slValues=sl_test, lFatal=FALSE )
    if ( sl_test%count > 0 .and. sl_test%count_matching("<NA>") < sl_test%count ) then
      call warn( "Legacy column name 'Last_day_of_growing_season' detected."            &
        //"~ Please rename to 'Growing_season_end_date'.",                               &
        lFatal=TRUE )
    end if

    ! GDD_plant → Growing_season_start_GDD
    call params%get_parameters( sKey="GDD_plant", slValues=sl_test, lFatal=FALSE )
    if ( sl_test%count > 0 .and. sl_test%count_matching("<NA>") < sl_test%count ) then
      call warn( "Legacy column name 'GDD_plant' detected."                             &
        //"~ Please rename to 'Growing_season_start_GDD'.",                              &
        lFatal=TRUE )
    end if

    ! GDD_first_day_of_growing_season → Growing_season_start_GDD
    call params%get_parameters( sKey="GDD_first_day_of_growing_season", slValues=sl_test, lFatal=FALSE )
    if ( sl_test%count > 0 .and. sl_test%count_matching("<NA>") < sl_test%count ) then
      call warn( "Legacy column name 'GDD_first_day_of_growing_season' detected."       &
        //"~ Please rename to 'Growing_season_start_GDD'.",                              &
        lFatal=TRUE )
    end if

  end subroutine detect_legacy_column_names

  !---------------------------------------------------------------------------
  !> @brief Dispatch phenology update to the appropriate method for a landuse.
  !!
  !! Reads PHENOLOGY_METHOD_INDEX for the given landuse and calls the
  !! corresponding update subroutine. All possible inputs are passed;
  !! each method uses only what it needs.
  !!
  !! @param[in]    landuse_index            Index into per-landuse arrays (1-based)
  !! @param[in]    current_doy              Current day of year (1-366)
  !! @param[in]    days_in_year             Days in current year (365 or 366)
  !! @param[in]    current_gdd              Accumulated GDD for current year
  !! @param[in]    mean_air_temperature     Current mean daily air temperature
  !! @param[in]    it_is_growing_season_in  Growing season state from previous day
  !! @param[inout] frost_killed_season      TRUE if frost already ended season this year
  !! @param[out]   growth_fraction          0.0 (dormant) to 1.0 (fully developed structure)
  !! @param[out]   it_is_growing_season     Updated growing season state
  !! @param[out]   growth_stage             Current growth stage enum value
  !! @param[out]   stage_fraction           0.0–1.0 position within current stage
  !---------------------------------------------------------------------------
  subroutine phenology_update( landuse_index,            &
                               current_doy,              &
                               days_in_year,             &
                               current_gdd,              &
                               mean_air_temperature,     &
                               it_is_growing_season_in,  &
                               frost_killed_season,      &
                               growth_fraction,          &
                               it_is_growing_season,     &
                               growth_stage,             &
                               stage_fraction )

    integer(c_int), intent(in)     :: landuse_index
    integer(c_int), intent(in)     :: current_doy
    integer(c_int), intent(in)     :: days_in_year
    real(c_float), intent(in)      :: current_gdd
    real(c_float), intent(in)      :: mean_air_temperature
    logical(c_bool), intent(in)    :: it_is_growing_season_in
    logical(c_bool), intent(inout) :: frost_killed_season
    real(c_float), intent(out)     :: growth_fraction
    logical(c_bool), intent(out)   :: it_is_growing_season
    integer(c_int), intent(out)    :: growth_stage
    real(c_float), intent(out)     :: stage_fraction

    select case ( PHENOLOGY_METHOD_INDEX(landuse_index) )

      case ( PHENOLOGY_DOY_BASED )
        call phenology_update_doy_based(              &
          current_doy=current_doy,                    &
          growing_season_start_doy=GROWING_SEASON_START_DOY(landuse_index), &
          growing_season_end_doy=GROWING_SEASON_END_DOY(landuse_index),     &
          growth_fraction=growth_fraction,            &
          it_is_growing_season=it_is_growing_season,  &
          growth_stage=growth_stage )
        stage_fraction = growth_fraction  ! binary: 0 or 1

      case ( PHENOLOGY_GDD_THRESHOLD )
        call phenology_update_gdd_threshold(          &
          current_gdd=current_gdd,                    &
          mean_air_temperature=mean_air_temperature,  &
          growing_season_start_gdd=GROWING_SEASON_START_GDD(landuse_index), &
          killing_frost_temperature=KILLING_FROST_TEMP(landuse_index),      &
          it_is_growing_season_in=it_is_growing_season_in,                  &
          frost_killed_season=frost_killed_season,    &
          growth_fraction=growth_fraction,            &
          it_is_growing_season=it_is_growing_season,  &
          growth_stage=growth_stage )
        stage_fraction = growth_fraction  ! binary: 0 or 1

      case ( PHENOLOGY_FAO56_DATES )
        call phenology_update_fao56_dates(            &
          current_doy=current_doy,                    &
          growing_season_start_doy=GROWING_SEASON_START_DOY(landuse_index), &
          l_ini=L_INI_DAYS(landuse_index),            &
          l_dev=L_DEV_DAYS(landuse_index),            &
          l_mid=L_MID_DAYS(landuse_index),            &
          l_late=L_LATE_DAYS(landuse_index),          &
          days_in_year=days_in_year,                  &
          growth_stage=growth_stage,                  &
          stage_fraction=stage_fraction,              &
          growth_fraction=growth_fraction,            &
          it_is_growing_season=it_is_growing_season )

      case ( PHENOLOGY_FAO56_GDD )
        call phenology_update_fao56_gdd(              &
          current_gdd=current_gdd,                    &
          mean_air_temperature=mean_air_temperature,  &
          growing_season_start_gdd=GROWING_SEASON_START_GDD(landuse_index), &
          killing_frost_temperature=KILLING_FROST_TEMP(landuse_index),      &
          gdd_ini=GDD_INI(landuse_index),             &
          gdd_dev=GDD_DEV(landuse_index),             &
          gdd_mid=GDD_MID(landuse_index),             &
          gdd_late=GDD_LATE(landuse_index),           &
          frost_killed_season=frost_killed_season,    &
          growth_stage=growth_stage,                  &
          stage_fraction=stage_fraction,              &
          growth_fraction=growth_fraction,            &
          it_is_growing_season=it_is_growing_season )

      case default
        ! PHENOLOGY_NONE: no growing season defined — always dormant
        growth_fraction      = 0.0_c_float
        it_is_growing_season = FALSE
        growth_stage         = GROWTH_STAGE_DORMANT
        stage_fraction       = 0.0_c_float

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
  !! accumulated GDD reaches the threshold AND temperature is above the killing
  !! frost threshold. Season ends when mean air temperature drops to or below
  !! the killing frost temperature.
  !!
  !! Once a killing frost terminates the growing season, it cannot restart
  !! until the next calendar year (hard latch via frost_killed_season flag).
  !!
  !! @param[in]     current_gdd               Accumulated GDD for current year (degree-days)
  !! @param[in]     mean_air_temperature      Current mean daily air temperature
  !! @param[in]     growing_season_start_gdd  GDD threshold for growing season start
  !! @param[in]     killing_frost_temperature Temperature at or below which season ends
  !! @param[in]     it_is_growing_season_in   Growing season state from previous timestep
  !! @param[inout]  frost_killed_season       TRUE if frost already ended the season this year
  !! @param[out]    growth_fraction           0.0 (dormant) or 1.0 (growing)
  !! @param[out]    it_is_growing_season      Updated growing season state
  !! @param[out]    growth_stage              GROWTH_STAGE_DORMANT or GROWTH_STAGE_MID
  !---------------------------------------------------------------------------
  pure subroutine phenology_update_gdd_threshold( current_gdd,               &
                                                  mean_air_temperature,       &
                                                  growing_season_start_gdd,   &
                                                  killing_frost_temperature,  &
                                                  it_is_growing_season_in,    &
                                                  frost_killed_season,        &
                                                  growth_fraction,            &
                                                  it_is_growing_season,       &
                                                  growth_stage )

    real(c_float), intent(in)    :: current_gdd
    real(c_float), intent(in)    :: mean_air_temperature
    real(c_float), intent(in)    :: growing_season_start_gdd
    real(c_float), intent(in)    :: killing_frost_temperature
    logical(c_bool), intent(in)  :: it_is_growing_season_in
    logical(c_bool), intent(inout) :: frost_killed_season
    real(c_float), intent(out)   :: growth_fraction
    logical(c_bool), intent(out) :: it_is_growing_season
    integer(c_int), intent(out)  :: growth_stage

    ! [ LOCALS ]
    logical :: is_growing

    if ( frost_killed_season ) then

      ! Hard latch: once frost has killed the season this year, stay dormant
      ! until GDD resets (next calendar year)
      is_growing = .false.

    else if ( it_is_growing_season_in ) then

      ! Already growing — check for killing frost
      if ( mean_air_temperature <= killing_frost_temperature ) then
        is_growing = .false.
        frost_killed_season = TRUE
      else
        is_growing = .true.
      end if

    else

      ! Dormant, no prior frost kill this year — check whether GDD threshold
      ! has been reached AND temperature is above frost threshold
      if ( current_gdd >= growing_season_start_gdd &
           .and. mean_air_temperature > killing_frost_temperature ) then
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

  !---------------------------------------------------------------------------
  !> @brief Determine phenology state for FAO56_DATES method.
  !!
  !! Structural development driven by calendar days since planting.
  !! Growth stages are determined by cumulative stage lengths (L_ini, L_dev,
  !! L_mid, L_late) counted from the planting date. growth_fraction ramps
  !! from 0 to 1 over INI+DEV and holds at 1.0 through MID and LATE
  !! (plant structure remains intact during senescence). Handles year-wrapping
  !! for winter crops (planting DOY > harvest DOY wraps into next year).
  !!
  !! @param[in]  current_doy               Current day of year (1-366)
  !! @param[in]  growing_season_start_doy  Planting date as DOY
  !! @param[in]  l_ini                     Length of initial stage (days)
  !! @param[in]  l_dev                     Length of development stage (days)
  !! @param[in]  l_mid                     Length of mid-season stage (days)
  !! @param[in]  l_late                    Length of late-season stage (days)
  !! @param[in]  days_in_year              Number of days in current year (365 or 366)
  !! @param[out] growth_stage              DORMANT, INI, DEV, MID, or LATE
  !! @param[out] stage_fraction            0.0–1.0 position within current stage
  !! @param[out] growth_fraction           0.0–1.0 structural development (holds at 1.0 through LATE)
  !! @param[out] it_is_growing_season      TRUE when growth_stage > DORMANT
  !---------------------------------------------------------------------------
  pure subroutine phenology_update_fao56_dates( current_doy,              &
                                                growing_season_start_doy, &
                                                l_ini,                    &
                                                l_dev,                    &
                                                l_mid,                    &
                                                l_late,                   &
                                                days_in_year,             &
                                                growth_stage,             &
                                                stage_fraction,           &
                                                growth_fraction,          &
                                                it_is_growing_season )

    integer(c_int), intent(in)   :: current_doy
    integer(c_int), intent(in)   :: growing_season_start_doy
    integer(c_int), intent(in)   :: l_ini
    integer(c_int), intent(in)   :: l_dev
    integer(c_int), intent(in)   :: l_mid
    integer(c_int), intent(in)   :: l_late
    integer(c_int), intent(in)   :: days_in_year
    integer(c_int), intent(out)  :: growth_stage
    real(c_float), intent(out)   :: stage_fraction
    real(c_float), intent(out)   :: growth_fraction
    logical(c_bool), intent(out) :: it_is_growing_season

    ! [ LOCALS ]
    integer(c_int) :: days_since_planting
    integer(c_int) :: total_season_length
    integer(c_int) :: end_ini, end_dev, end_mid, end_late

    ! Cumulative stage boundaries (days since planting)
    end_ini  = l_ini
    end_dev  = l_ini + l_dev
    end_mid  = l_ini + l_dev + l_mid
    end_late = l_ini + l_dev + l_mid + l_late
    total_season_length = end_late

    ! Calculate days since planting, handling year wrap
    days_since_planting = current_doy - growing_season_start_doy
    if ( days_since_planting < 0 ) then
      days_since_planting = days_since_planting + days_in_year
    end if

    ! Determine growth stage and stage_fraction from days since planting
    if ( days_since_planting >= total_season_length ) then

      ! Past end of growing season — dormant
      growth_stage         = GROWTH_STAGE_DORMANT
      stage_fraction       = 0.0_c_float
      growth_fraction      = 0.0_c_float
      it_is_growing_season = FALSE

    else if ( days_since_planting < end_ini ) then

      ! Initial stage
      growth_stage   = GROWTH_STAGE_INI
      if ( l_ini > 0 ) then
        stage_fraction = real( days_since_planting, c_float ) / real( l_ini, c_float )
      else
        stage_fraction = 1.0_c_float
      end if
      growth_fraction      = stage_fraction * 0.1_c_float
      it_is_growing_season = TRUE

    else if ( days_since_planting < end_dev ) then

      ! Development stage — Kcb ramps from ini to mid
      growth_stage   = GROWTH_STAGE_DEV
      if ( l_dev > 0 ) then
        stage_fraction = real( days_since_planting - end_ini, c_float ) &
                       / real( l_dev, c_float )
      else
        stage_fraction = 1.0_c_float
      end if
      growth_fraction      = 0.1_c_float + stage_fraction * 0.9_c_float
      it_is_growing_season = TRUE

    else if ( days_since_planting < end_mid ) then

      ! Mid-season stage — full maturity
      growth_stage   = GROWTH_STAGE_MID
      if ( l_mid > 0 ) then
        stage_fraction = real( days_since_planting - end_dev, c_float ) &
                       / real( l_mid, c_float )
      else
        stage_fraction = 1.0_c_float
      end if
      growth_fraction      = 1.0_c_float
      it_is_growing_season = TRUE

    else

      ! Late season stage — structure remains fully developed
      growth_stage   = GROWTH_STAGE_LATE
      if ( l_late > 0 ) then
        stage_fraction = real( days_since_planting - end_mid, c_float ) &
                       / real( l_late, c_float )
      else
        stage_fraction = 1.0_c_float
      end if
      growth_fraction      = 1.0_c_float
      it_is_growing_season = TRUE

    end if

  end subroutine phenology_update_fao56_dates

  !---------------------------------------------------------------------------
  !> @brief Determine phenology state for FAO56_GDD method.
  !!
  !! Structural development driven by growing degree day accumulation.
  !! Growing season starts when GDD reaches the planting threshold. Growth
  !! stages are determined by cumulative GDD thresholds (GDD_ini, GDD_dev,
  !! GDD_mid, GDD_late) counted from the planting GDD. growth_fraction ramps
  !! from 0 to 1 over INI+DEV and holds at 1.0 through MID and LATE.
  !! Season ends on killing frost (hard latch for remainder of year).
  !!
  !! @param[in]     current_gdd               Accumulated GDD for current year
  !! @param[in]     mean_air_temperature      Current mean daily air temperature
  !! @param[in]     growing_season_start_gdd  GDD threshold to begin growth
  !! @param[in]     killing_frost_temperature Frost threshold to end season
  !! @param[in]     gdd_ini                   GDD accumulation for initial stage
  !! @param[in]     gdd_dev                   GDD accumulation for development stage
  !! @param[in]     gdd_mid                   GDD accumulation for mid-season stage
  !! @param[in]     gdd_late                  GDD accumulation for late-season stage
  !! @param[inout]  frost_killed_season       Hard latch: TRUE after killing frost
  !! @param[out]    growth_stage              DORMANT, INI, DEV, MID, or LATE
  !! @param[out]    stage_fraction            0.0–1.0 position within current stage
  !! @param[out]    growth_fraction           0.0–1.0 structural development (holds at 1.0 through LATE)
  !! @param[out]    it_is_growing_season      TRUE when growth_stage > DORMANT
  !---------------------------------------------------------------------------
  pure subroutine phenology_update_fao56_gdd( current_gdd,               &
                                              mean_air_temperature,       &
                                              growing_season_start_gdd,   &
                                              killing_frost_temperature,  &
                                              gdd_ini,                    &
                                              gdd_dev,                    &
                                              gdd_mid,                    &
                                              gdd_late,                   &
                                              frost_killed_season,        &
                                              growth_stage,               &
                                              stage_fraction,             &
                                              growth_fraction,            &
                                              it_is_growing_season )

    real(c_float), intent(in)      :: current_gdd
    real(c_float), intent(in)      :: mean_air_temperature
    real(c_float), intent(in)      :: growing_season_start_gdd
    real(c_float), intent(in)      :: killing_frost_temperature
    real(c_float), intent(in)      :: gdd_ini
    real(c_float), intent(in)      :: gdd_dev
    real(c_float), intent(in)      :: gdd_mid
    real(c_float), intent(in)      :: gdd_late
    logical(c_bool), intent(inout) :: frost_killed_season
    integer(c_int), intent(out)    :: growth_stage
    real(c_float), intent(out)     :: stage_fraction
    real(c_float), intent(out)     :: growth_fraction
    logical(c_bool), intent(out)   :: it_is_growing_season

    ! [ LOCALS ]
    real(c_float) :: gdd_since_planting
    real(c_float) :: end_ini, end_dev, end_mid, end_late

    ! Cumulative GDD boundaries (since planting GDD threshold)
    end_ini  = gdd_ini
    end_dev  = gdd_ini + gdd_dev
    end_mid  = gdd_ini + gdd_dev + gdd_mid
    end_late = gdd_ini + gdd_dev + gdd_mid + gdd_late

    ! Check for frost hard latch
    if ( frost_killed_season ) then
      growth_stage         = GROWTH_STAGE_DORMANT
      stage_fraction       = 0.0_c_float
      growth_fraction      = 0.0_c_float
      it_is_growing_season = FALSE
      return
    end if

    ! Check for killing frost (only meaningful once season has started)
    if ( current_gdd >= growing_season_start_gdd &
         .and. mean_air_temperature <= killing_frost_temperature ) then
      frost_killed_season  = TRUE
      growth_stage         = GROWTH_STAGE_DORMANT
      stage_fraction       = 0.0_c_float
      growth_fraction      = 0.0_c_float
      it_is_growing_season = FALSE
      return
    end if

    ! GDD since planting threshold was crossed
    gdd_since_planting = current_gdd - growing_season_start_gdd

    ! Not yet planted
    if ( gdd_since_planting < 0.0_c_float ) then
      growth_stage         = GROWTH_STAGE_DORMANT
      stage_fraction       = 0.0_c_float
      growth_fraction      = 0.0_c_float
      it_is_growing_season = FALSE
      return
    end if

    ! Past end of growing season (all GDD stages completed)
    if ( gdd_since_planting >= end_late ) then
      growth_stage         = GROWTH_STAGE_DORMANT
      stage_fraction       = 0.0_c_float
      growth_fraction      = 0.0_c_float
      it_is_growing_season = FALSE
      return
    end if

    ! Determine stage from GDD position
    if ( gdd_since_planting < end_ini ) then

      growth_stage = GROWTH_STAGE_INI
      if ( gdd_ini > 0.0_c_float ) then
        stage_fraction = gdd_since_planting / gdd_ini
      else
        stage_fraction = 1.0_c_float
      end if
      growth_fraction      = stage_fraction * 0.1_c_float
      it_is_growing_season = TRUE

    else if ( gdd_since_planting < end_dev ) then

      growth_stage = GROWTH_STAGE_DEV
      if ( gdd_dev > 0.0_c_float ) then
        stage_fraction = ( gdd_since_planting - end_ini ) / gdd_dev
      else
        stage_fraction = 1.0_c_float
      end if
      growth_fraction      = 0.1_c_float + stage_fraction * 0.9_c_float
      it_is_growing_season = TRUE

    else if ( gdd_since_planting < end_mid ) then

      growth_stage = GROWTH_STAGE_MID
      if ( gdd_mid > 0.0_c_float ) then
        stage_fraction = ( gdd_since_planting - end_dev ) / gdd_mid
      else
        stage_fraction = 1.0_c_float
      end if
      growth_fraction      = 1.0_c_float
      it_is_growing_season = TRUE

    else

      growth_stage = GROWTH_STAGE_LATE
      if ( gdd_late > 0.0_c_float ) then
        stage_fraction = ( gdd_since_planting - end_mid ) / gdd_late
      else
        stage_fraction = 1.0_c_float
      end if
      growth_fraction      = 1.0_c_float
      it_is_growing_season = TRUE

    end if

  end subroutine phenology_update_fao56_gdd

end module phenology
