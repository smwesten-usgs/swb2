!> @file
!>  Contains a single module, \ref irrigation, which
!>  provides support for estimating irrigation amounts

!> Provides support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module irrigation

  use iso_c_binding, only          : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog, only           : DAT
  use data_catalog_entry, only     : DATA_CATALOG_ENTRY_T
  use datetime, only               : mmdd2doy
  use exceptions, only             : warn, die, assert
  use logfiles, only               : LOGS, LOG_ALL
  use parameters, only             : PARAMS
  use simulation_datetime, only    : SIM_DT
  use fstring, only                : asCharacter, operator( .contains. )
  use fstring_list, only            : FSTRING_LIST_T

  implicit none

  private

  public :: irrigation__initialize, irrigation__calculate, irrigation__output_schedule_values

  enum, bind(c)
    enumerator :: APP_NONE=0, APP_FIELD_CAPACITY, APP_DEFINED_DEFICIT, APP_CONSTANT_AMOUNT, &
                  APP_HWB_DEMAND_BASED
  end enum

  character (len=35), parameter :: APP_OPTION_NAME(0:4) = &
     [ "Apply nothing                      ", &
       "Apply to field capacity            ", &
       "Apply to specified deficit fraction", &
       "Apply constant amount              ", &
       "Apply proportional to (PE + RO - R)"   ]

  real (c_float), allocatable     :: MAXIMUM_ALLOWABLE_DEPLETION_FRACTION(:)
  real (c_float), allocatable     :: IRRIGATION_FROM_GROUNDWATER(:)
  real (c_float), allocatable     :: IRRIGATION_FROM_SURFACE_WATER(:)

  real (c_float), allocatable     :: FRACTION_OF_IRRIGATION_FROM_GW(:)
  real (c_float), allocatable     :: IRRIGATION_EFFICIENCY(:)
  integer (c_int), allocatable    :: NUM_DAYS_OF_IRRIGATION(:)
  integer (c_int), allocatable    :: FIRST_DAY_OF_IRRIGATION(:)
  integer (c_int), allocatable    :: LAST_DAY_OF_IRRIGATION(:)
  integer (c_int), allocatable    :: APPLICATION_METHOD_CODE(:)
  integer (c_short), allocatable  :: MONTHLY_IRRIGATION_SCHEDULE(:,:)
  real (c_float), allocatable     :: APPLICATION_AMOUNT(:)

  type (DATA_CATALOG_ENTRY_T), pointer :: pIRRIGATION_MASK

contains

!> Estimate the irrigation water required to sustain plant growth.
!!
!! Estimate the irrigation water required in order to
!! keep soil moisture values above the maximum allowable depletion (MAD)
!! for each gridcell.
!!
!! @param[inout] pGrd Pointer to the model grid object
!! @param[in] pConfig Pointer to the configuration data structure (type T_CONFIG).
!!

  subroutine irrigation__initialize( is_active)

    logical (c_bool), intent(in)    :: is_active(:,:)

    ! [ LOCALS ]
    type (FSTRING_LIST_T)              :: sl_temp_list
    integer (c_int)              :: number_of_landuse_codes
    integer (c_int), allocatable :: landuse_table_codes(:)
    integer (c_int)              :: num_records
    logical (c_bool)             :: are_lengths_unequal
    integer (c_int)              :: index
    integer (c_int)              :: i
    integer (c_int)              :: status
    character (len=256)               :: str_buffer
    type (FSTRING_LIST_T)              :: sl_irrigation_days
    type (FSTRING_LIST_T)              :: sl_irrigation_begin
    type (FSTRING_LIST_T)              :: sl_irrigation_end
    type (FSTRING_LIST_T)              :: sl_application_method
    type (FSTRING_LIST_T)              :: sl_monthly_irrigation_schedule
    character (len=31)                :: temp_str

    allocate( IRRIGATION_FROM_GROUNDWATER( count( is_active ) ), stat=status )
    call assert( status==0, "Failed to allocate memory.", __SRCNAME__, __LINE__ )

    allocate( IRRIGATION_FROM_SURFACE_WATER( count( is_active ) ), stat=status )
    call assert( status==0, "Failed to allocate memory.", __SRCNAME__, __LINE__ )

    ! create list of possible table headings to look for...
    call sl_temp_list%append( "LU_Code" )
    call sl_temp_list%append( "Landuse_Lookup_Code" )

    !> determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=sl_temp_list, iValues=landuse_table_codes )
    number_of_landuse_codes = count( landuse_table_codes >= 0 )

    ! create list of possible table headings to look for...
    call sl_temp_list%clear()
    call sl_temp_list%append( "Monthly_Irrigation_Schedule" )
    call sl_temp_list%append( "Monthly_Irr_Schedule" )
    call sl_temp_list%append( "Irrigation_Application_Schedule" )
    call sl_temp_list%append( "Monthly_Application_Schedule" )

    !> determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=sl_temp_list, slValues=sl_monthly_irrigation_schedule )

    !> retrieve list of fraction of irrigation from groundwater
    call sl_temp_list%clear()
    call sl_temp_list%append("Fraction_irrigation_from_GW")
    call sl_temp_list%append("Frac_irr_fm_GW")
    call sl_temp_list%append("Fraction_irrigation_from_groundwater")
    call sl_temp_list%append("Frac_irrigation_from_GW")
    call sl_temp_list%append("Fraction_of_irrigation_from_GW")
    call sl_temp_list%append("Fraction_of_irrigation_from_groundwater")

    call PARAMS%get_parameters( slKeys=sl_temp_list, fValues=FRACTION_OF_IRRIGATION_FROM_GW, lFatal=TRUE )

    !> retrieve maximum allowable depletion
    call sl_temp_list%clear()
    call sl_temp_list%append("Max_allowable_depletion")
    call sl_temp_list%append("Maximum_allowable_depletion")
    call sl_temp_list%append("MAD")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                            &
                                fValues=MAXIMUM_ALLOWABLE_DEPLETION_FRACTION,   &
                                lFatal=TRUE )


    !> retrieve length (in days since planting) of irrigation
    call sl_temp_list%clear()
    call sl_temp_list%append("Irrigation_length")
    call sl_temp_list%append("Irrigation_days")
    call sl_temp_list%append("Irrigation_days_since_planting")

    call PARAMS%get_parameters( slKeys=sl_temp_list, slValues=sl_irrigation_days, lFatal=FALSE )


    !> retrieve first day of irrigation
    call sl_temp_list%clear()
    call sl_temp_list%append("First_day_of_irrigation")
    call sl_temp_list%append("First_DOY_irrigation")
    call sl_temp_list%append("Irrigation_start")
    call sl_temp_list%append("Irrigation_start_date")

    call PARAMS%get_parameters( slKeys=sl_temp_list, slValues=sl_irrigation_begin, lFatal=TRUE )


    !> retrieve last day of irrigation
    call sl_temp_list%clear()
    call sl_temp_list%append("Last_day_of_irrigation")
    call sl_temp_list%append("Last_DOY_irrigation")
    call sl_temp_list%append("Irrigation_end")
    call sl_temp_list%append("Irrigation_end_date")

    call PARAMS%get_parameters( slKeys=sl_temp_list, slValues=sl_irrigation_end, lFatal=TRUE )
    call sl_temp_list%clear()


    !> retrieve irrigation efficiency
    call sl_temp_list%clear()
    call sl_temp_list%append("Irrigation_efficiency")
    call sl_temp_list%append("Irrigation_application_efficiency")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                     &
                                fValues=IRRIGATION_EFFICIENCY,           &
                                lFatal=TRUE )

    !> check: number of irrigation efficiency values == number of landuse codes?
    num_records = ubound(IRRIGATION_EFFICIENCY,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes )

    if ( are_lengths_unequal )                                                             &
      call warn( sMessage="The number of values specifying irrigation application"         &
        //" efficiency ("                                                                  &
        //asCharacter( num_records )//") does not match the number of landuse values ("    &
        //asCharacter( number_of_landuse_codes )//").",                                    &
        sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )


    !> process first day of irrigation. retrieved as a list of strings;
    !! must convert the strings from mm/dd to DOY
    allocate( FIRST_DAY_OF_IRRIGATION( sl_irrigation_begin%count ), stat=status )
    call assert( status==0, "Problem allocating memory.", __SRCNAME__, __LINE__ )

    if ( sl_irrigation_begin%count_matching("<NA>") == 0 ) then

      do index = 1, sl_irrigation_begin%count
        str_buffer = sl_irrigation_begin%get( index )
        if ( scan(str_buffer, "/-") /= 0 ) then
          FIRST_DAY_OF_IRRIGATION( index ) = mmdd2doy( str_buffer, "FIRST_DAY_OF_IRRIGATION" )
        else
          FIRST_DAY_OF_IRRIGATION( index ) = asInt( str_buffer )
        endif
      enddo

    else

      FIRST_DAY_OF_IRRIGATION = 90
      call warn("No value was found to define the first day of irrigation.",      &
           sHints="Make sure there is a lookup table with the column name "       &
           //"'First_day_of_irrigation'.", lFatal=TRUE )
    endif

    !> process last day of irrigation. retrieved as a list of strings;
    !! must convert the strings from mm/dd to DOY
    allocate( LAST_DAY_OF_IRRIGATION( sl_irrigation_end%count ), stat=status )
    call assert( status==0, "Problem allocating memory.", __SRCNAME__, __LINE__ )

    if ( sl_irrigation_end%count_matching("<NA>") == 0 ) then

      do index = 1, sl_irrigation_end%count
        str_buffer = sl_irrigation_end%get( index )
        if ( scan(str_buffer, "/-") /= 0 ) then
          LAST_DAY_OF_IRRIGATION( index ) = mmdd2doy( str_buffer, "LAST_DAY_OF_IRRIGATION" )
        else
          LAST_DAY_OF_IRRIGATION( index ) = asInt( str_buffer )
        endif
      enddo

    else

      LAST_DAY_OF_IRRIGATION = 90
      call warn("No value was found to define the last day of irrigation.",      &
           sHints="Make sure there is a lookup table with the column name "       &
           //"'Last_day_of_irrigation'.", lFatal=TRUE )
    endif

    !> process number of days of irrigation. retrieved as a list of strings
    allocate( NUM_DAYS_OF_IRRIGATION( sl_irrigation_days%count ), stat=status )
    call assert( status==0, "Problem allocating memory.", __SRCNAME__, __LINE__ )

    NUM_DAYS_OF_IRRIGATION = sl_irrigation_days%get_integer()

    if ( sl_irrigation_days%count_matching("<NA>") == 0 ) then

      where ( NUM_DAYS_OF_IRRIGATION == 0 )
        NUM_DAYS_OF_IRRIGATION = 999999
      end where

    else

      NUM_DAYS_OF_IRRIGATION = 999999

    endif

    !> retrieve application option (i.e. to field capacity, to defined deficit amount, as constant amount)
    call sl_temp_list%clear()
    call sl_temp_list%append("Irrigation_application_method")
    call sl_temp_list%append("Irrigation_application_scheme")
    call sl_temp_list%append("Irrigation_application_option")
    call sl_temp_list%append("Application_method")
    call sl_temp_list%append("Application_scheme")
    call sl_temp_list%append("Application_option")

    call PARAMS%get_parameters( slKeys=sl_temp_list, slValues=sl_application_method, lFatal=TRUE )
    call sl_temp_list%clear()

    allocate( APPLICATION_METHOD_CODE( sl_application_method%count ), stat=status )
    call assert( status==0, "Problem allocating memory.", __SRCNAME__, __LINE__ )

    !> retrieve application amount. not used if "field capacity" option is active.
    !! value represents deficit fraction or constant amount depending on application option active.
    call sl_temp_list%clear()
    call sl_temp_list%append("Application_amount")
    call sl_temp_list%append("Irrigation_amount")

    call PARAMS%get_parameters( slKeys=sl_temp_list, fValues=APPLICATION_AMOUNT, lFatal=TRUE )
    call sl_temp_list%clear()

    !> basic checks to see whether the number of parameters equals the number of lu codes
    num_records = ubound(FIRST_DAY_OF_IRRIGATION,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes )

    if ( are_lengths_unequal )                                                            &
      call warn( sMessage="The number of values specifying date of first "                &
        //"irrigation application ("                                                      &
        //asCharacter( num_records )//") does not match the number of landuse values ("   &
        //asCharacter( number_of_landuse_codes )//").",                                   &
        sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )

    num_records = ubound(LAST_DAY_OF_IRRIGATION,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes )

    if ( are_lengths_unequal )                                                            &
      call warn( sMessage="The number of values specifying date of last irrigation"       &
        //" application ("                                                                &
        //asCharacter( num_records )//") does not match the number of landuse values ("   &
        //asCharacter( number_of_landuse_codes )//").",                                   &
        sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )

    num_records = ubound(FRACTION_OF_IRRIGATION_FROM_GW,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes )

    if ( are_lengths_unequal )                                                            &
      call warn( sMessage="The number of values specifying the fraction of irrigation"    &
        //" from groundwater ("                                                           &
        //asCharacter( num_records )//") does not match the number of landuse values ("   &
        //asCharacter( number_of_landuse_codes )//").",                                   &
        sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )

    num_records = ubound(MAXIMUM_ALLOWABLE_DEPLETION_FRACTION,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes )

    if ( are_lengths_unequal )                                                            &
      call warn( sMessage="The number of values for the maximum allowable depletion "     &
        //" fraction ("                                                                   &
        //asCharacter( num_records )//") does not match the number of landuse values ("   &
        //asCharacter( number_of_landuse_codes )//").",                                   &
        sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )


    num_records = sl_monthly_irrigation_schedule%count
    are_lengths_unequal = ( num_records /= number_of_landuse_codes )

    allocate( MONTHLY_IRRIGATION_SCHEDULE( number_of_landuse_codes, 31 ), stat=status )
    call assert( status==0, "Problem allocating memory")

    if ( are_lengths_unequal ) then
      call warn( sMessage="The number of values defining monthly irrigation application"  &
        //" timing ("//asCharacter( num_records )//")~does not match the number of"       &
        //" landuse codes ("//asCharacter( number_of_landuse_codes )//"). ~Assuming"      &
        //" that irrigation is applied *every* day [default].",                           &
        sModule=__SRCNAME__, iLine=__LINE__, lEcho=.true._c_bool, iLogLevel=LOG_ALL )

      MONTHLY_IRRIGATION_SCHEDULE = 1_c_int

    else

      MONTHLY_IRRIGATION_SCHEDULE = 0_c_int

      do index=1, number_of_landuse_codes
        temp_str = sl_monthly_irrigation_schedule%get( index )
        if ( is_numeric( temp_str ) ) then
          do i=1, ubound(MONTHLY_IRRIGATION_SCHEDULE, 2)
            MONTHLY_IRRIGATION_SCHEDULE( index, i ) = asInt( temp_str(i:i) )
          enddo
        endif
      enddo
    endif

    num_records = ubound(APPLICATION_METHOD_CODE,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes )

    if ( are_lengths_unequal )                                                            &
      call warn( sMessage="The number of values for the irrigation application option ("  &
        //asCharacter( num_records )//") does not match the number of landuse values ("   &
        //asCharacter( number_of_landuse_codes )//").",                                   &
        sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )


    num_records = ubound(APPLICATION_AMOUNT,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes )

    if ( are_lengths_unequal )                                                            &
      call warn( sMessage="The number of values for the irrigation application amount ("  &
        //asCharacter( num_records )//") does not match the number of landuse values ("   &
        //asCharacter( number_of_landuse_codes )//").",                                   &
        sModule=__SRCNAME__, iLine=__LINE__, lFatal=.true._c_bool )

   ! locate the data structure associated with the gridded irrigation mask entries
    ! pIRRIGATION_MASK => DAT%find("IRRIGATION_MASK")
    ! if ( associated(pIRRIGATION_MASK) ) call pIRRIGATION_MASK%getvalues( )

    ! if ( associated(pIRRIGATION_MASK) ) then

    !   irrigation_mask = pack( real(pIRRIGATION_MASK%pGrdBase%iData, c_float), is_active )

    ! else

    !   irrigation_mask = 1.0_c_float

    ! endif

    call LOGS%write(" ## Initializing irrigation application rules and application schedules ##", iLinesBefore=1, &
      iLinesAfter=1, iLogLevel=LOG_ALL )

    do index = 1, sl_application_method%count

      str_buffer = sl_application_method%get( index )

      if ( str_buffer .contains. "capacity") then
        APPLICATION_METHOD_CODE( index ) = APP_FIELD_CAPACITY
      elseif ( str_buffer .contains. "deficit") then
        APPLICATION_METHOD_CODE( index ) = APP_DEFINED_DEFICIT
      elseif ( str_buffer .contains. "constant") then
        APPLICATION_METHOD_CODE( index ) = APP_CONSTANT_AMOUNT
      elseif ( str_buffer .contains. "demand") then
        APPLICATION_METHOD_CODE( index ) = APP_HWB_DEMAND_BASED
      else
        APPLICATION_METHOD_CODE( index ) = APP_NONE
      endif


      call LOGS%write("  landuse "//asCharacter( landuse_table_codes( index ) )//": " &
        //trim(APP_OPTION_NAME( APPLICATION_METHOD_CODE( index ) ) ), iLogLevel=LOG_ALL )

    enddo

    IRRIGATION_FROM_GROUNDWATER(:)    = 0.0_c_float
    IRRIGATION_FROM_SURFACE_WATER(:)  = 0.0_c_float

  end subroutine irrigation__initialize

!--------------------------------------------------------------------------------------------------

  function irrigation__output_schedule_values( landuse_index )  result( values )

    integer (c_int), intent(in)   :: landuse_index
    integer (c_int)               :: values(31)

    values = MONTHLY_IRRIGATION_SCHEDULE( landuse_index, : )

  end function irrigation__output_schedule_values

!--------------------------------------------------------------------------------------------------

  impure elemental subroutine irrigation__calculate( irrigation_amount,     &
                                              landuse_index,                &
                                              soil_storage,                 &
                                              soil_storage_max,             &
                                              total_available_water,        &
                                              rainfall,                     &
                                              runoff,                       &
                                              crop_etc,                     &
                                              irrigation_mask,              &
                                              num_days_since_planting,      &
                                              monthly_rainfall,             &
                                              monthly_runoff                &
                                                )

    real (c_float), intent(inout)          :: irrigation_amount
    integer (c_int), intent(in)            :: landuse_index
    real (c_double), intent(in)            :: soil_storage
    real (c_float), intent(in)             :: soil_storage_max
    real (c_double), intent(in)            :: total_available_water
    real (c_float), intent(in)             :: rainfall
    real (c_float), intent(in)             :: runoff
    real (c_float), intent(in)             :: crop_etc
    real (c_float), intent(in)             :: irrigation_mask
    integer (c_int), intent(in)            :: num_days_since_planting
    real (c_float), intent(in), optional   :: monthly_rainfall
    real (c_float), intent(in), optional   :: monthly_runoff

    ! [ LOCALS ]
    real (c_float)        :: depletion_fraction

    integer (c_int)       :: month
    integer (c_int)       :: day
    integer (c_int)       :: year
    integer (c_int)       :: julian_day
    integer (c_int)       :: day_of_year
    integer (c_int)       :: days_in_month
    integer (c_int)       :: num_days_from_origin
    integer (c_int)       :: index
    character (len=31)         :: irrigation_day
    integer (c_int)       :: irrigation_days_per_month
    real (c_float)        :: efficiency
    real (c_float)        :: interim_irrigation_amount
    integer (c_int)       :: option

    ! zero out Irrigation term
!    IRRIGATION_FROM_GROUNDWATER = rZERO
!    IRRIGATION_FROM_SURFACE_WATER = rZERO

    associate ( dt => SIM_DT%curr )

      julian_day = dt%getJulianDay()
      month = asInt( dt%iMonth )
      day = asInt( dt%iDay )
      year = dt%iYear
      days_in_month = SIM_DT%iDaysInMonth
      day_of_year = SIM_DT%iDOY
      num_days_from_origin = SIM_DT%iNumDaysFromOrigin

    end associate

!     call pIRRIGATION_MASK%getvalues( month, day, year, julian_day )
!     iIrrigation_Mask = pack( pIRRIGATION_MASK%pGrdBase%iData, is_active )

    ! for each cell, add water if soil storage zone is below the
    ! maximum allowable depletion

    ! now we run the gauntlet of tests to ensure that we really need
    ! to perform all of the irrigation calculations

    do

      irrigation_amount         = 0.0_c_float
      interim_irrigation_amount = 0.0_c_float
      depletion_fraction        = 0.0_c_float

      if ( ( day_of_year < FIRST_DAY_OF_IRRIGATION( landuse_index ) ) &
        .or. ( day_of_year > LAST_DAY_OF_IRRIGATION( landuse_index ) ) )  exit

!      if ( MAXIMUM_ALLOWABLE_DEPLETION_FRACTION( landuse_index ) > 0.99 )  exit
      if ( soil_storage_max <= 0.0_c_float ) exit
      if ( IRRIGATION_MASK < 1.0e-6_c_float ) exit
      if ( num_days_since_planting > NUM_DAYS_OF_IRRIGATION( landuse_index ) ) exit

      ! total_available_water is calculated only by the fao56_two_stage module, but
      ! not by any other modules; need to just calculate depletion fraction based on
      ! total soil moisture storage capacity in this case
      if ( total_available_water > 0.0_c_float ) then
        depletion_fraction = min( ( soil_storage_max - soil_storage ) / total_available_water, 1.0_c_float )
      else
        depletion_fraction = min( ( soil_storage_max - soil_storage ) / soil_storage_max, 1.0_c_float )
      endif

      option = APPLICATION_METHOD_CODE( landuse_index )

      select case ( option )

        case ( APP_FIELD_CAPACITY )

          if ( depletion_fraction >= MAXIMUM_ALLOWABLE_DEPLETION_FRACTION( landuse_index ) )      &
            interim_irrigation_amount = max( 0.0_c_float, soil_storage_max - soil_storage )

        case ( APP_DEFINED_DEFICIT )

          ! @TODO check this calculation

          if ( depletion_fraction >= MAXIMUM_ALLOWABLE_DEPLETION_FRACTION( landuse_index ) )      &
            interim_irrigation_amount = max( 0.0_c_float, APPLICATION_AMOUNT( landuse_index )     &
                                        * soil_storage_max - soil_storage )

        case ( APP_CONSTANT_AMOUNT )

          if ( depletion_fraction >= MAXIMUM_ALLOWABLE_DEPLETION_FRACTION( landuse_index ) )      &
            interim_irrigation_amount = APPLICATION_AMOUNT( landuse_index )

        case ( APP_HWB_DEMAND_BASED )

          if ( MONTHLY_IRRIGATION_SCHEDULE( landuse_index, day ) == 0 ) exit

          if (present( monthly_runoff ) .and. present( monthly_rainfall ) ) then

            irrigation_days_per_month = count( MONTHLY_IRRIGATION_SCHEDULE( landuse_index, 1:days_in_month ) == 1 )

            if ( irrigation_days_per_month <= 0 ) then
              interim_irrigation_amount = 0.0_c_float
            else
              interim_irrigation_amount = max( 0.0_c_float,    &
              ( crop_etc * real( days_in_month, c_float) + monthly_runoff - monthly_rainfall ) )  &
                / real( irrigation_days_per_month, c_float )
            endif

          else

            interim_irrigation_amount = max( 0.0_c_float, crop_etc + runoff - rainfall )

          endif

        case default

          interim_irrigation_amount = 0.0_c_float

      end select

      efficiency = max( IRRIGATION_EFFICIENCY( landuse_index ), 0.20_c_float )

      irrigation_amount =  interim_irrigation_amount                                    &
                                     * irrigation_mask                                  &
                                     / efficiency

      exit

    enddo

  end subroutine irrigation__calculate

end module irrigation
