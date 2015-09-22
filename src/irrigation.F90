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
  use strings, only                : asCharacter, operator( .contains. )
  use string_list, only            : STRING_LIST_T

  implicit none

  private 

  public :: irrigation__initialize, irrigation__calculate

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

  real (kind=c_float), allocatable     :: MAXIMUM_ALLOWABLE_DEPLETION_FRACTION(:)
  real (kind=c_float), allocatable     :: IRRIGATION_FROM_GROUNDWATER(:)
  real (kind=c_float), allocatable     :: IRRIGATION_FROM_SURFACE_WATER(:) 
  
  real (kind=c_float), allocatable     :: FRACTION_OF_IRRIGATION_FROM_GW(:)   
  real (kind=c_float), allocatable     :: IRRIGATION_EFFICIENCY(:)
  integer (kind=c_int), allocatable    :: FIRST_DAY_OF_IRRIGATION(:)
  integer (kind=c_int), allocatable    :: LAST_DAY_OF_IRRIGATION(:)
  integer (kind=c_int), allocatable    :: APPLICATION_METHOD_CODE(:)
  integer (kind=c_short), allocatable  :: MONTHLY_IRRIGATION_SCHEDULE(:,:)
  real (kind=c_float), allocatable     :: APPLICATION_AMOUNT(:)

  type (DATA_CATALOG_ENTRY_T), pointer :: pIRRIGATION_MASK
  real (kind=c_float), allocatable     :: IRRIGATION_MASK(:)

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

  subroutine irrigation__initialize( irrigation_mask, is_active)

    real (kind=c_float), intent(inout)   :: irrigation_mask(:)
    logical (kind=c_bool), intent(in)    :: is_active(:,:)

    ! [ LOCALS ]
    type (STRING_LIST_T)              :: sl_temp_list
    integer (kind=c_int)              :: number_of_landuse_codes
    integer (kind=c_int), allocatable :: landuse_table_codes(:)
    integer (kind=c_int)              :: num_records
    logical (kind=c_bool)             :: are_lengths_unequal
    integer (kind=c_int)              :: index
    integer (kind=c_int)              :: i
    integer (kind=c_int)              :: status
    character (len=256)               :: str_buffer
    type (STRING_LIST_T)              :: sl_irrigation_begin
    type (STRING_LIST_T)              :: sl_irrigation_end  
    type (STRING_LIST_T)              :: sl_application_method 
    type (STRING_LIST_T)              :: sl_monthly_irrigation_schedule 
    character (len=31)                :: temp_str    

    allocate( IRRIGATION_FROM_GROUNDWATER( count( is_active ) ), stat=status )
    call assert( status==0, "Failed to allocate memory.", __FILE__, __LINE__ )

    allocate( IRRIGATION_FROM_SURFACE_WATER( count( is_active ) ), stat=status )
    call assert( status==0, "Failed to allocate memory.", __FILE__, __LINE__ )

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

    call PARAMS%get_parameters( slKeys=sl_temp_list, fValues=FRACTION_OF_IRRIGATION_FROM_GW, lFatal=lTRUE )

    !> retrieve maximum allowable depletion
    call sl_temp_list%clear()
    call sl_temp_list%append("Max_allowable_depletion")
    call sl_temp_list%append("Maximum_allowable_depletion")
    call sl_temp_list%append("MAD")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                            &
                                fValues=MAXIMUM_ALLOWABLE_DEPLETION_FRACTION,   &
                                lFatal=lTRUE ) 

    !> retrieve first day of irrigation
    call sl_temp_list%clear()
    call sl_temp_list%append("First_day_of_irrigation")
    call sl_temp_list%append("First_DOY_irrigation")
    call sl_temp_list%append("Irrigation_start")

    call PARAMS%get_parameters( slKeys=sl_temp_list, slValues=sl_irrigation_begin, lFatal=lTRUE ) 


    !> retrieve last day of irrigation
    call sl_temp_list%clear()
    call sl_temp_list%append("Last_day_of_irrigation")
    call sl_temp_list%append("Last_DOY_irrigation")
    call sl_temp_list%append("Irrigation_end")

    call PARAMS%get_parameters( slKeys=sl_temp_list, slValues=sl_irrigation_end, lFatal=lTRUE ) 
    call sl_temp_list%clear()


    !> retrieve irrigation efficiency
    call sl_temp_list%clear()
    call sl_temp_list%append("Irrigation_efficiency")
    call sl_temp_list%append("Irrigation_application_efficiency")

    call PARAMS%get_parameters( slKeys=sl_temp_list,                     &
                                fValues=IRRIGATION_EFFICIENCY,           &
                                lFatal=lTRUE ) 

    !> check: number of irrigation efficiency values == number of landuse codes?
    num_records = ubound(IRRIGATION_EFFICIENCY,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes ) 

    if ( are_lengths_unequal )                                                             &
      call warn( sMessage="The number of values specifying irrigation application"         &
        //" efficiency ("                                                                  &
        //asCharacter( num_records )//") does not match the number of landuse values ("    &
        //asCharacter( number_of_landuse_codes )//").",                                    &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )


    !> process first day of irrigation. retrieved as a list of strings; 
    !! must convert the strings from mm/dd to DOY
    allocate( FIRST_DAY_OF_IRRIGATION( sl_irrigation_begin%count ), stat=status )
    call assert( status==0, "Problem allocating memory.", __FILE__, __LINE__ )

    do index = 1, sl_irrigation_begin%count
      str_buffer = sl_irrigation_begin%get( index )
      FIRST_DAY_OF_IRRIGATION = mmdd2doy( str_buffer )
    enddo  

    !> process last day of irrigation. retrieved as a list of strings; 
    !! must convert the strings from mm/dd to DOY
    allocate( LAST_DAY_OF_IRRIGATION( sl_irrigation_end%count ), stat=status )
    call assert( status==0, "Problem allocating memory.", __FILE__, __LINE__ )

    do index = 1, sl_irrigation_end%count
      str_buffer = sl_irrigation_end%get( index )
      LAST_DAY_OF_IRRIGATION = mmdd2doy( str_buffer )
    enddo  


    !> retrieve application option (i.e. to field capacity, to defined deficit amount, as constant amount)
    call sl_temp_list%clear()
    call sl_temp_list%append("Irrigation_application_method")
    call sl_temp_list%append("Irrigation_application_scheme")
    call sl_temp_list%append("Irrigation_application_option")    
    call sl_temp_list%append("Application_method")
    call sl_temp_list%append("Application_scheme")
    call sl_temp_list%append("Application_option")        

    call PARAMS%get_parameters( slKeys=sl_temp_list, slValues=sl_application_method, lFatal=lTRUE ) 
    call sl_temp_list%clear()

    allocate( APPLICATION_METHOD_CODE( sl_application_method%count ), stat=status )
    call assert( status==0, "Problem allocating memory.", __FILE__, __LINE__ )

    call LOGS%write(" ## Initializing irrigation application rules ##", iLinesBefore=1, &
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

 
    !> retrieve application amount. not used if "field capacity" option is active.
    !! value represents deficit fraction or constant amount depending on application option active.
    call sl_temp_list%clear()
    call sl_temp_list%append("Application_amount")
    call sl_temp_list%append("Irrigation_amount")

    call PARAMS%get_parameters( slKeys=sl_temp_list, fValues=APPLICATION_AMOUNT, lFatal=lTRUE ) 
    call sl_temp_list%clear()

    !> basic checks to see whether the number of parameters equals the number of lu codes
    num_records = ubound(FIRST_DAY_OF_IRRIGATION,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes ) 

    if ( are_lengths_unequal )                                                            &
      call warn( sMessage="The number of values specifying date of first "                &
        //"irrigation application ("                                                      &
        //asCharacter( num_records )//") does not match the number of landuse values ("   &
        //asCharacter( number_of_landuse_codes )//").",                                   &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

    num_records = ubound(LAST_DAY_OF_IRRIGATION,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes ) 

    if ( are_lengths_unequal )                                                            &
      call warn( sMessage="The number of values specifying date of last irrigation"       &
        //" application ("                                                                &
        //asCharacter( num_records )//") does not match the number of landuse values ("   &
        //asCharacter( number_of_landuse_codes )//").",                                   &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

    num_records = ubound(FRACTION_OF_IRRIGATION_FROM_GW,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes ) 

    if ( are_lengths_unequal )                                                            &
      call warn( sMessage="The number of values specifying the fraction of irrigation"    &
        //" from groundwater ("                                                           &
        //asCharacter( num_records )//") does not match the number of landuse values ("   &
        //asCharacter( number_of_landuse_codes )//").",                                   &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

    num_records = ubound(MAXIMUM_ALLOWABLE_DEPLETION_FRACTION,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes ) 

    if ( are_lengths_unequal )                                                            &
      call warn( sMessage="The number of values for the maximum allowable depletion "     &
        //" fraction ("                                                                   &
        //asCharacter( num_records )//") does not match the number of landuse values ("   &
        //asCharacter( number_of_landuse_codes )//").",                                   &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )


    num_records = sl_monthly_irrigation_schedule%count
    are_lengths_unequal = ( num_records /= number_of_landuse_codes ) 

    allocate( MONTHLY_IRRIGATION_SCHEDULE( number_of_landuse_codes, 31 ), stat=status )
    call assert( status==0, "Problem allocating memory")

    if ( are_lengths_unequal ) then
      call warn( sMessage="The number of values defining monthly irrigation application"  &
        //" timing ("//asCharacter( num_records )//")~does not match the number of"       &
        //" landuse codes ("//asCharacter( number_of_landuse_codes )//"). ~Assuming"      &
        //" that irrigation is applied *every* day [default].",                           &
        sModule=__FILE__, iLine=__LINE__, lEcho=.true._c_bool, iLogLevel=LOG_ALL )

      MONTHLY_IRRIGATION_SCHEDULE = 1

    else
      do index=1, number_of_landuse_codes
        temp_str = sl_monthly_irrigation_schedule%get( index )
        do i=1, ubound(MONTHLY_IRRIGATION_SCHEDULE, 2)
          MONTHLY_IRRIGATION_SCHEDULE( index, i ) = asInt( temp_str(i:i) )
        enddo  
      enddo  
    endif  

    num_records = ubound(APPLICATION_METHOD_CODE,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes ) 

    if ( are_lengths_unequal )                                                            &
      call warn( sMessage="The number of values for the irrigation application option ("  &
        //asCharacter( num_records )//") does not match the number of landuse values ("   &
        //asCharacter( number_of_landuse_codes )//").",                                   &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )


    num_records = ubound(APPLICATION_AMOUNT,1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes ) 

    if ( are_lengths_unequal )                                                            &
      call warn( sMessage="The number of values for the irrigation application amount ("  &
        //asCharacter( num_records )//") does not match the number of landuse values ("   &
        //asCharacter( number_of_landuse_codes )//").",                                   &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )


    ! locate the data structure associated with the gridded irrigation mask entries
    pIRRIGATION_MASK => DAT%find("IRRIGATION_MASK")
    if ( associated(pIRRIGATION_MASK) ) call pIRRIGATION_MASK%getvalues( )


    if ( associated(pIRRIGATION_MASK) ) then

      IRRIGATION_MASK = pack( real(pIRRIGATION_MASK%pGrdBase%iData, kind=c_float), is_active )

    else

      IRRIGATION_MASK = 1.0_c_float

    endif

  end subroutine irrigation__initialize

!--------------------------------------------------------------------------------------------------  

  elemental subroutine irrigation__calculate( irrigation_amount,            &
                                              landuse_index,                &
                                              soil_storage,                 & 
                                              soil_storage_max,             &
                                              rainfall,                     &
                                              runoff,                       &
                                              crop_etc,                     &
                                              irrigation_mask,              &
                                              monthly_rainfall,             &
                                              monthly_runoff                &
                                                )

    real (kind=c_float), intent(inout)          :: irrigation_amount
    integer (kind=c_int), intent(in)            :: landuse_index
    real (kind=c_float), intent(in)             :: soil_storage
    real (kind=c_float), intent(in)             :: soil_storage_max
    real (kind=c_float), intent(in)             :: rainfall
    real (kind=c_float), intent(in)             :: runoff
    real (kind=c_float), intent(in)             :: crop_etc
    real (kind=c_float), intent(in)             :: irrigation_mask
    real (kind=c_float), intent(in), optional   :: monthly_rainfall
    real (kind=c_float), intent(in), optional   :: monthly_runoff    

    ! [ LOCALS ]
    real (kind=c_float)        :: depletion_fraction
    real (kind=c_double)       :: depletion_amount

    integer (kind=c_int)       :: month
    integer (kind=c_int)       :: day
    integer (kind=c_int)       :: year
    integer (kind=c_int)       :: julian_day
    integer (kind=c_int)       :: day_of_year
    integer (kind=c_int)       :: days_in_month
    integer (kind=c_int)       :: num_days_from_origin
    integer (kind=c_int)       :: index
    character (len=31)         :: irrigation_day
    integer (kind=c_int)       :: irrigation_days_per_month
    real (kind=c_float)        :: efficiency
    real (kind=c_float)        :: interim_irrigation_amount
    integer (kind=c_int)       :: option

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

      irrigation_amount = 0.0_c_float

      if ( MONTHLY_IRRIGATION_SCHEDULE( landuse_index, day ) == 0 ) exit

      if ( ( day_of_year < FIRST_DAY_OF_IRRIGATION( landuse_index ) ) &
        .or. ( day_of_year > LAST_DAY_OF_IRRIGATION( landuse_index ) ) )  exit

      if ( MAXIMUM_ALLOWABLE_DEPLETION_FRACTION( landuse_index ) > 0.99 )  exit
      if ( soil_storage_max <= 0.0_c_float ) exit
      if ( IRRIGATION_MASK < 1.0e-6_c_float ) exit 

      depletion_fraction = 1.0_c_float - soil_storage / soil_storage_max

      option = APPLICATION_METHOD_CODE( landuse_index )

      efficiency = max( IRRIGATION_EFFICIENCY( landuse_index ), 0.20_c_float )

      if ( depletion_fraction >= MAXIMUM_ALLOWABLE_DEPLETION_FRACTION( landuse_index ) ) then

        select case ( option )

          case ( APP_FIELD_CAPACITY )

            interim_irrigation_amount = max( 0.0_c_float, soil_storage_max - soil_storage )

          case ( APP_DEFINED_DEFICIT )
          
            interim_irrigation_amount = max( 0.0_c_float, APPLICATION_AMOUNT( landuse_index )                                   &
                                          * soil_storage_max - soil_storage )  

          case ( APP_CONSTANT_AMOUNT )
          
            interim_irrigation_amount = APPLICATION_AMOUNT( landuse_index )


          case ( APP_HWB_DEMAND_BASED )

            if (present( monthly_runoff ) .and. present( monthly_rainfall ) ) then

              irrigation_days_per_month = sum( MONTHLY_IRRIGATION_SCHEDULE( landuse_index, : ) )

              if ( irrigation_days_per_month <= 0 ) then
                interim_irrigation_amount = 0.0_c_float
              else  
                interim_irrigation_amount = max( 0.0_c_float,    &
                ( crop_etc * real( days_in_month, kind=c_float) + monthly_runoff - monthly_rainfall ) )  &
                  / irrigation_days_per_month
              endif

            else
            
              interim_irrigation_amount = max( 0.0_c_float, crop_etc + runoff - rainfall )

            endif  

          case default

            interim_irrigation_amount = 0.0_c_float

        end select  

        irrigation_amount =  interim_irrigation_amount                                    &
                                       * IRRIGATION_MASK                                  &
                                       / efficiency 

      endif

      exit

    enddo

  end subroutine irrigation__calculate

end module irrigation
