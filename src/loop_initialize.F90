module loop_initialize

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use constants_and_conversions, only : lTRUE, lFALSE, asFloat, BNDS
  use datetime
  use data_catalog
  use data_catalog_entry
  use dictionary
  use exceptions
  use file_operations
!  use loop_iterate
  use model_domain
  use parameters
  use precipitation__method_of_fragments
  use simulation_datetime
  use strings
  use string_list  
  implicit none

  private

  public :: read_control_file, initialize_options
  public :: check_for_fatal_warnings

  type GRIDDED_DATASETS_T
    character (len=29)     :: sName
    logical (kind=c_bool)  :: lOptional
    integer (kind=c_int)   :: iDataType 
  end type GRIDDED_DATASETS_T

  type METHODS_LIST_T
    character (len=23)     :: sName
  end type METHODS_LIST_T

  type (GRIDDED_DATASETS_T), parameter  :: KNOWN_GRIDS(18) = &

    [ GRIDDED_DATASETS_T("PRECIPITATION                ", lFALSE, DATATYPE_FLOAT ),     &
      GRIDDED_DATASETS_T("TMIN                         ", lFALSE, DATATYPE_FLOAT ),     &
      GRIDDED_DATASETS_T("TMAX                         ", lFALSE, DATATYPE_FLOAT ),     &
      GRIDDED_DATASETS_T("AVAILABLE_WATER_CONTENT      ", lFALSE, DATATYPE_FLOAT ),     &
      GRIDDED_DATASETS_T("POTENTIAL_ET                 ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("SOLAR_RADIATION              ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("WIND_SPEED                   ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("RAINFALL_ZONE                ", lTRUE, DATATYPE_INT ),        &
      GRIDDED_DATASETS_T("FLOW_DIRECTION               ", lTRUE, DATATYPE_INT),         &
      GRIDDED_DATASETS_T("FOG_RATIO                    ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("LAND_USE                     ", lFALSE, DATATYPE_INT ),       &
      GRIDDED_DATASETS_T("SOILS_GROUP                  ", lFALSE, DATATYPE_INT ),       &
      GRIDDED_DATASETS_T("CANOPY_COVER_FRACTION        ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("STEMFLOW_FRACTION            ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("EVAPORATION_TO_RAINFALL_RATIO", lTRUE, DATATYPE_FLOAT ),      & 
      GRIDDED_DATASETS_T("RAINFALL_ADJUST_FACTOR       ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("RUNOFF_ZONE                  ", lTRUE, DATATYPE_INT ),        &                 
      GRIDDED_DATASETS_T("RELATIVE_HUMIDITY            ", lTRUE, DATATYPE_FLOAT )   ]

  type (METHODS_LIST_T), parameter  :: KNOWN_METHODS(7) =   &
    [ METHODS_LIST_T("INTERCEPTION           "),            &
      METHODS_LIST_T("EVAPOTRANSPIRATION     "),            &
      METHODS_LIST_T("RUNOFF                 "),            &
      METHODS_LIST_T("PRECIPITATION          "),            &
      METHODS_LIST_T("FOG                    "),            &
      METHODS_LIST_T("SOIL_MOISTURE          "),            &
      METHODS_LIST_T("FLOW_ROUTING           ")    ]

contains

  subroutine read_control_file( sFilename ) 

    character (len=*), intent(in)    :: sFilename

    ! [ LOCALS ]
    character (len=256)   :: sRecord, sSubstring
    integer (kind=c_int)  :: iStat
    type (ASCII_FILE_T) :: CF

    ! open the control file and define the comment characters and delimiters to be used in 
    ! parsing the ASCII text
    call CF%open( sFilename = sFilename, &
                  sCommentChars = "#%!", &
                  sDelimiters = "WHITESPACE", &
                  lHasHeader = .false._c_bool )

    do 

      ! read in next line of the control file
      sRecord = CF%readLine()

      if ( CF%isEOF() )  exit

      ! create and allocate memory for a single dictionary entry
      CF_ENTRY => null()
      allocate( CF_ENTRY, stat=iStat )
      call assert(iStat == 0, "Failed to allocate memory for dictionary object", &
            __FILE__, __LINE__ )

      ! break off key value for the current record
      call chomp(sRecord, sSubstring, CF%sDelimiters )

      if ( len_trim( sSubstring ) > 0 ) then

        ! first add the key value to the directory entry data structure
        call CF_ENTRY%add_key( sSubstring )

        ! break off first directive for the current record
        call chomp( sRecord, sSubstring, CF%sDelimiters )

        do while ( len_trim( sSubString ) > 0 )
          
          ! add the next directive snippet to dictionary entry data structure
          call CF_ENTRY%add_string( sSubstring )

          ! break off next directive for the current record
          call chomp( sRecord, sSubstring, CF%sDelimiters )

        enddo  

        ! add the final entry to the dictionary data structure
        call CF_DICT%add_entry( CF_ENTRY )

      endif  
      
    enddo

    ! close the control file
    call CF%close()

  end subroutine read_control_file

!--------------------------------------------------------------------------------------------------  

  subroutine initialize_options()

    integer (kind=c_int) :: iIndex

    ! define SWB project boundary and geographic projection
    call initialize_grid_options()

    ! define the start and end date for the simulation
    call initialize_start_and_end_dates()

    ! read in and munge all tables that have been defined in the control file as ***_LOOKUP_TABLE
    call initialize_parameter_tables()
    
    ! scan input file entries for keywords associated with known gridded datasets
    ! (e.g. PRECIPITATION, TMIN, TMAX, FOG_ZONE, etc.)
    
    ! if the grid is mentioned in the control file, call the bound "initialize" method that 
    ! will wire in the type of data file. all associated methods will also be acted upon if 
    ! present in the control file (e.g. TMAX_ADD_OFFSET, TMAX_NETCDF_X_VAR, etc.)
    do iIndex = 1, ubound(KNOWN_GRIDS, 1)

      call initialize_generic_grid( sKey=KNOWN_GRIDS(iIndex)%sName, &
         lOptional=KNOWN_GRIDS(iIndex)%lOptional,                   &
         iDataType=KNOWN_GRIDS(iIndex)%iDataType )
   
    enddo

    ! scan the control file input for method specifications
    ! (e.g. EVAPOTRANSPIRATION_METHOD HARGREAVES-SAMANI )
    do iIndex = 1, ubound(KNOWN_METHODS, 1)

      call initialize_generic_method( sKey=KNOWN_METHODS(iIndex)%sName )
   
    enddo
    
    ! bring in soils (HSG, AWC), landuse, and flow direction data from native grids
    ! and pack that data into vectors for active grid cells only
    call initialize_soils_landuse_awc_flowdir_values()

    ! temporary diagnostic: dump statistics on each of the state variables
!    call MODEL%summarize()

    ! check to see that there are no NULL method pointers
    call MODEL%preflight_check_method_pointers()

    ! call each of the initialization routines associated with the chosen methods
    call MODEL%initialize_methods()

    ! open and prepare NetCDF files for output
    call MODEL%initialize_netcdf_output()

  end subroutine initialize_options

!--------------------------------------------------------------------------------------------------



  !> Generic routine to handle intake of gridded data.
  !! 
  !! @param[in]  sKey        Name for the data type being processed.
  !! @param[in]  lOptional   If lOptional is TRUE, kill model run eventually IF sKey
  !!                         does not match a known data type.
  !! @param[in]  iDataType   Datatype as defined in @ref constants_and-conversion.F90
  !!
  !! This routine accepts a data grid type, for example, "PRECIPITATION", and attempts to
  !! handle all related control file directives associated with this data type. In this way,
  !! a new gridded data type may be added simply by extending the list of known data types
  !! to the list defined in the module variable @ref KNOWN_TYPES.
  !!

  subroutine initialize_generic_grid(sKey, lOptional, iDataType )

    character (len=*), intent(in)      :: sKey
    logical (kind=c_bool), intent(in)  :: lOptional
    integer (kind=c_int), intent(in)   :: iDataType

    ! [ LOCALS ]
    type (STRING_LIST_T)                 :: myDirectives
    type (STRING_LIST_T)                 :: myOptions  
    integer (kind=c_int)                 :: iIndex
    character (len=:), allocatable       :: sCmdText
    character (len=:), allocatable       :: sArgText
    character (len=:), allocatable       :: sArgText_1
    character (len=:), allocatable       :: sArgText_2
    integer (kind=c_int)                 :: iStat
    type (DATA_CATALOG_ENTRY_T), pointer :: pENTRY
    logical (kind=c_bool)             :: lGridPresent

    pENTRY => null()
    lGridPresent = lFALSE

    ! obtain a string list of directives that contain the keyword
    ! (e.g. if the key is "TMIN", this might return:
    ! "TMIN ARC_ASCII input/mygrid.asc"
    ! "TMIN_PROJECTION_DEFINITION =Proj=latlon +datum=WGS84"
    ! "TMIN_MINIMUM_ALLOWED_VALUE -60.0" )
    ! the loop below then handles the specific directives in turn
    myDirectives = CF_DICT%grep_keys( sKey )

    ! call myDirectives%print

    if ( myDirectives%count == 0 ) then
    
      call LOGS%write("Your control file is missing gridded data relating to "//dquote(sKey)//".", &
        iLogLevel=LOG_ALL, lEcho=lFALSE )
    
      if (.not. lOptional) then
        call warn("Your control file is missing gridded data relating to "//dquote(sKey)//".", &
          lFatal = lTRUE )

      endif

    else  
    
      ! allocate memory for a generic data_catalog_entry
      allocate(pENTRY, stat=iStat)
      call assert( iStat == 0, "Failed to allocate memory for the "//dquote(sKey)//" data structure", &
        __FILE__, __LINE__ )

      ! process all known directives associated with key word
      do iIndex = 1, myDirectives%count

        ! myDirectives is a string list of all SWB directives that contain sKey
        ! sCmdText contains an individual directive
        sCmdText = myDirectives%get(iIndex)

        ! For this directive, obtain the associated dictionary entries
        call CF_DICT%get_values(sCmdText, myOptions )

        ! dictionary entries are initially space-delimited; sArgText_1 contains
        ! all dictionary entries present, concatenated, with a space between entries
        sArgText = myOptions%get(1, myOptions%count )

        ! echo the original directive and dictionary entries to the logfile
        call LOGS%write(">> "//sCmdText//" "//sArgText, iLogLevel=LOG_GENERAL, iTab=4)

        ! most of the time, we only care about the first dictionary entry, obtained below
        sArgText_1 = myOptions%get(1)
        sArgText_2 = myOptions%get(2)

        ! first option is that the key value and directive are the same 
        ! (.e.g. "PRECIPITATION"; no trailing underscores or modifiers )
        ! this is a grid definition directive
        if ( sCmdText .strequal. sKey ) then

          pENTRY%sVariableName_z = asLowercase( sKey )

          ! determine the type of grid and act appropriately
          if (sArgText_1 .strequal. "CONSTANT" ) then

              call pENTRY%initialize(            &
                sDescription=trim(sCmdText),     &
                rConstant=asFloat(sArgText_2)  ) 
              lGridPresent = lTRUE           

            elseif ( (sArgText_1 .strequal. "ARC_ASCII")              &
                .or. (sArgText_1 .strequal. "SURFER")                 &
                .or. (sArgText_1 .strequal. "ARC_GRID") ) then

              call pENTRY%initialize(           &
                sDescription=trim(sCmdText),    &
                sFileType=trim(sArgText_1),     &
                sFilename=trim(sArgText_2),     &
                iDataType=iDataType )
              lGridPresent = lTRUE

            elseif ( sArgText_1 .strequal. "NETCDF" ) then
              
              call pENTRY%initialize_netcdf(    &
                sDescription=trim(sCmdText),    &
                sFilename = trim(sArgText_2),   &
                iDataType=iDataType )
              lGridPresent = lTRUE
 
            else

              call warn( "Did not find a valid "//dquote(sKey)//" option. Value supplied was: "//dquote(sArgText_1), &
                lFatal = lTRUE, sHints="Valid options include "//dquote("ARC_ASCII")//", "//dquote("ARC_GRID") &
                //", "//dquote("SURFER")//", or "//dquote("NETCDF") )

            endif  

        elseif ( index( string=sCmdText, substring="_SCALE" ) > 0 ) then

          call pENTRY%set_scale(asFloat(sArgText_1))

        elseif ( index( string=sCmdText, substring="_OFFSET" ) > 0 ) then

          call pENTRY%set_offset(asFloat(sArgText_1))

        elseif ( index( string=sCmdText, substring="NETCDF_X_VAR" ) > 0 ) then

          pENTRY%sVariableName_x = trim(sArgText_1)

        elseif ( index( string=sCmdText, substring="NETCDF_Y_VAR" ) > 0 ) then

          pENTRY%sVariableName_y = trim(sArgText_1)

        elseif ( index( string=sCmdText, substring="NETCDF_Z_VAR" ) > 0 ) then

          pENTRY%sVariableName_z = trim(sArgText_1)

        elseif ( index( string=sCmdText, substring="NETCDF_TIME_VAR" ) > 0 ) then

          pENTRY%sVariableName_time = trim(sArgText_1)

        elseif ( index( string=sCmdText, substring="NETCDF_VARIABLE_ORDER" ) > 0 ) then

          call pENTRY%set_variable_order( asLowercase(sArgText_1) )

        elseif ( index( string=sCmdText, substring="NETCDF_FLIP_VERTICAL" ) > 0 ) then

          call pENTRY%set_grid_flip_vertical()

        elseif ( index( string=sCmdText, substring="NETCDF_FLIP_HORIZONTAL" ) > 0 ) then

          call pENTRY%set_grid_flip_horizontal()

        elseif ( index( string=sCmdText, substring="NETCDF_MAKE_LOCAL_ARCHIVE" ) > 0 ) then

          call pENTRY%set_make_local_archive(lTRUE)

        elseif ( index( string=sCmdText, substring="_PROJECTION_DEFINITION" ) > 0 ) then 

          call pENTRY%set_PROJ4( trim(sArgText) )

        elseif ( index( string=sCmdText, substring="_MINIMUM_ALLOWED_VALUE" ) > 0 ) then

          pENTRY%rMinAllowedValue = asFloat(sArgText_1)

        elseif ( index( string=sCmdText, substring="_MAXIMUM_ALLOWED_VALUE" ) > 0 ) then

          pENTRY%rMaxAllowedValue = asFloat(sArgText_1)

        elseif ( index( string=sCmdText, substring="_MISSING_VALUES_CODE" ) > 0 ) then

          pENTRY%rMissingValuesCode = asFloat(sArgText_1)

        elseif ( index( string=sCmdText, substring="_MISSING_VALUES_OPERATOR" ) > 0 ) then

          pENTRY%sMissingValuesOperator = trim(sArgText_1)

        elseif ( index( string=sCmdText, substring= "_MISSING_VALUES_ACTION") > 0 ) then
          
          if (sArgText_1 .strequal. "ZERO") then

            pENTRY%iMissingValuesAction = MISSING_VALUES_ZERO_OUT
          
          elseif (sArgText_1 .strequal. "MEAN" ) then
          
            pENTRY%iMissingValuesAction = MISSING_VALUES_REPLACE_WITH_MEAN
          
          else
          
            call warn("Unknown missing value action supplied for " &
              //dquote(sKey)//" data: "//dquote(sArgText_1) )
          
          endif

        elseif ( index( string=sCmdText, substring="_METHOD") > 0 ) then

          ! no operation; just keep SWB quiet about this and it will be included in the
          ! methods initialization section

        else

          call warn("Unknown directive detected in code at line "//asCharacter(__LINE__)//", file "//__FILE__ &
            //". ~Ignoring. Directive is: "//dquote(sCmdText), iLogLevel=LOG_DEBUG )
 
        endif

      enddo

      ! if an unadorned grid specification directive was processed, then we can add the key and
      ! the data_catalog_entry to the data_catalog
      if ( lGridPresent )call DAT%add( key=sKey, data=pENTRY )

      pENTRY => null()

    endif

  end subroutine initialize_generic_grid

!--------------------------------------------------------------------------------------------------

  subroutine initialize_grid_options()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myOptions  
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat
    real (kind=c_double)             :: rX0, rX1, rY0, rY1, rGridCellSize
    integer (kind=c_int)             :: iNX, iNY
    real (kind=c_float)              :: fTempVal


    ! For this directive, obtain the associated dictionary entries
    call CF_DICT%get_values( "GRID", myOptions )

    ! dictionary entries are initially space-delimited; sArgText contains
    ! all dictionary entries present, concatenated, with a space between entries
    sArgText = myOptions%get(1, myOptions%count )

    ! echo the original directive and dictionary entries to the logfile
    call LOGS%write(">> "//"GRID "//sArgText)

    iNX = asInt( myOptions%get(1) )
    iNY = asInt( myOptions%get(2) )
    rX0 = asDouble( myOptions%get(3) )
    rY0 = asDouble( myOptions%get(4) )
    
    if ( myOptions%count == 5 ) then

      rGridCellSize = asDouble( myOptions%get(5) )

      call MODEL%initialize_grid(iNX, iNY, rX0, rY0, rGridCellSize)

      rX1 = rX0 + rGridCellSize * real(iNX, kind=c_double)
      rY1 = rY0 + rGridCellSize * real(iNY, kind=c_double)

    elseif ( myOptions%count == 7 ) then

      rX1 = asDouble( myOptions%get(5) )
      rY1 = asDouble( myOptions%get(6) )
      rGridCellSize = asDouble( myOptions%get(7) )

      fTempVal = ( rX1 - rX0 ) / real(iNX, kind=c_double)

      call MODEL%initialize_grid(iNX, iNY, rX0, rY0, rGridCellSize)

    else

      call warn("Grid specification is flawed or missing.", lFatal=lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

    endif

    call myOptions%clear()

    ! For this directive, obtain the associated dictionary entries
    call CF_DICT%get_values( "BASE_PROJECTION_DEFINITION", myOptions )

    ! dictionary entries are initially space-delimited; sArgText contains
    ! all dictionary entries present, concatenated, with a space between entries
    sArgText = myOptions%get(1, myOptions%count )

    ! echo the original directive and dictionary entries to the logfile
    call LOGS%write(">> "//"BASE_PROJECTION_DEFINITION "//sArgText)

    ! BNDS is a module-level data structure that will be used in other modules to 
    ! supply bounding box information for the SWB project area
    BNDS%iNumCols = iNX
    BNDS%iNumRows = iNY
    BNDS%fX_ll = rX0
    BNDS%fY_ll = rY0
    BNDS%fY_ur = rY1
    BNDS%fX_ur = rX1
    BNDS%fGridCellSize = rGridCellSize
    BNDS%sPROJ4_string = trim(sArgText)

    MODEL%PROJ4_string = trim(sArgText)

  end subroutine initialize_grid_options

!--------------------------------------------------------------------------------------------------

  subroutine initialize_soils_landuse_awc_flowdir_values()

!! this shouldnt be here....move to model_domain
! 

      call MODEL%get_land_use()

      call MODEL%get_available_water_content()

      call MODEL%get_soil_groups()

      call MODEL%set_inactive_cells()

      call MODEL%initialize_arrays()

      call MODEL%initialize_latitude()

      call MODEL%initialize_soil_groups()

      call MODEL%initialize_landuse()

      call MODEL%initialize_available_water_content()

      call MODEL%initialize_soil_layers()

  end subroutine initialize_soils_landuse_awc_flowdir_values

!--------------------------------------------------------------------------------------------------

  subroutine initialize_start_and_end_dates()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions  
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sCmdText
    character (len=:), allocatable   :: sOptionText
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat


    myDirectives = CF_DICT%grep_keys("DATE")
      
    if ( myDirectives%count < 2 ) then

      call warn("Your control file seems to be missing START_DATE and/or END_DATE", &
        lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

    else  
    
      call LOGS%set_loglevel( LOG_ALL )
      call LOGS%set_echo( lFALSE )

      do iIndex = 1, myDirectives%count

        ! myDirectives is a string list of all SWB directives that contain the phrase "LANDUSE"
        ! sCmdText contains an individual directive
        sCmdText = myDirectives%get(iIndex)

        ! For this directive, obtain the associated dictionary entries
        call CF_DICT%get_values(sCmdText, myOptions )

        ! dictionary entries are initially space-delimited; sArgText contains
        ! all dictionary entries present, concatenated, with a space between entries
        sArgText = myOptions%get(1, myOptions%count )

        ! echo the original directive and dictionary entries to the logfile
        call LOGS%write(">> "//sCmdText//" "//sArgText)

        ! most of the time, we only care about the first dictionary entry, obtained below
        sOptionText = myOptions%get(1)

        select case ( sCmdText )

          case ( "START_DATE", "STARTDATE" )

            call SIM_DT%start%parseDate( sOptionText )
            call SIM_DT%start%calcJulianDay()

          case ( "END_DATE", "ENDDATE" )

            call SIM_DT%end%parseDate( sOptionText )
            call SIM_DT%end%calcJulianDay()

          case default

            call warn("Unknown directive present, line "//asCharacter(__LINE__)//", file "//__FILE__ &
              //". Ignoring. Directive is: "//dquote(sCmdText), iLogLevel=LOG_DEBUG )
 
        end select

      enddo

      SIM_DT%curr = SIM_DT%start
      SIM_DT%iDOY = day_of_year( SIM_DT%curr%getJulianDay() )

      SIM_DT%iDaysInMonth = SIM_DT%curr%dayspermonth()
      SIM_DT%iDaysInYear = SIM_DT%curr%daysperyear()
      SIM_DT%lIsLeapYear = SIM_DT%curr%isLeapYear()

    endif

  end subroutine initialize_start_and_end_dates 

!--------------------------------------------------------------------------------------------------

  !> Find any parameter tables specified in the control file; process and store contents.
  !!
  !! Any control file entry that contains the text "LOOKUP_TABLE" is assumed to specify a parameter table
  !! that needs to be read in and processed. 
  !!
  !! @note The entries given in the files are implicitly assumed to be in sorted order. For example, if the parameters
  !! pertain to landuse codes, it is assumed that all table values are given in order from lowest to highest 
  !! landuse code.

  subroutine initialize_parameter_tables()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions
    type (STRING_LIST_T)             :: slString  
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sCmdText
    character (len=:), allocatable   :: sOptionText
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat
    type (PARAMETERS_T)         :: PARAMS
    integer (kind=c_int)             :: iCount


    iCount = 0

    myDirectives = CF_DICT%grep_keys("LOOKUP_TABLE")
      
    if ( myDirectives%count == 0 ) then

      call warn("Your control file seems to be missing the required lookup table(s).", &
        lFatal = lTRUE )

    else  
    
      call LOGS%set_loglevel( LOG_ALL )
      call LOGS%set_echo( lFALSE )

      do iIndex = 1, myDirectives%count

        ! myDirectives is a string list of all SWB directives that contain the phrase "LOOKUP_TABLE"
        ! sCmdText contains an individual directive
        sCmdText = myDirectives%get(iIndex)

        ! For this directive, obtain the associated dictionary entries
        call CF_DICT%get_values(sCmdText, myOptions )

        ! dictionary entries are initially space-delimited; sArgText contains
        ! all dictionary entries present, concatenated, with a space between entries
        sArgText = myOptions%get(1, myOptions%count )

        ! echo the original directive and dictionary entries to the logfile
        call LOGS%write(">> "//sCmdText//" "//sArgText)

        ! most of the time, we only care about the first dictionary entry, obtained below
        sOptionText = myOptions%get(1)

        if ( index(string=sCmdText, substring="LOOKUP_TABLE" ) > 0 ) then

            call PARAMS%add_file( sOptionText )
            iCount = iCount + 1

        else

            call warn("Unknown directive present, line "//asCharacter(__LINE__)//", file "//__FILE__ &
              //". Ignoring. Directive is: "//dquote(sCmdText), iLogLevel=LOG_DEBUG )
        
        endif

      enddo  

      if ( iCount > 0 ) then

        call PARAMS%munge_file()
        ! call PARAMS%print_all()       

      endif

    endif

  end subroutine initialize_parameter_tables

!--------------------------------------------------------------------------------------------------

subroutine initialize_generic_method( sKey )

  character (len=*), intent(in)    :: sKey

  ! [ LOCALS ]
  type (STRING_LIST_T)             :: myDirectives
  type (STRING_LIST_T)             :: myOptions  
  integer (kind=c_int)             :: iIndex
  character (len=:), allocatable   :: sCmdText
  character (len=:), allocatable   :: sOptionText
  character (len=:), allocatable   :: sArgText
  integer (kind=c_int)             :: iStat

  ! obtain a list of control file directives whose key values contain the string sKey
  myDirectives = CF_DICT%grep_keys( trim(sKey) )
    
  if ( myDirectives%count == 0 ) then

    call warn("Your control file is missing any of the required directives relating to "//dquote(sKey)//" method.", &
      lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

  else  
  
    call LOGS%set_loglevel( LOG_ALL )
    call LOGS%set_echo( lFALSE )

    ! repeat this process for each control file directive in list
    do iIndex = 1, myDirectives%count

      ! sCmdText contains an individual directive (e.g. TMAX NETCDF input/tmax_%0m_%0d_%0Y.nc )
      sCmdText = myDirectives%get(iIndex)

      ! For this directive, obtain the associated dictionary entries
      call CF_DICT%get_values(sCmdText, myOptions )

      ! dictionary entries are initially space-delimited; sArgText contains
      ! all dictionary entries present, concatenated, with a space between entries
      sArgText = myOptions%get(1, myOptions%count )

      ! echo the original directive and dictionary entries to the logfile
      call LOGS%write(">> "//sCmdText//" "//sArgText)

      ! most of the time, we only care about the first dictionary entry, obtained below
      sOptionText = myOptions%get(1)

      if ( index(string=sCmdText, substring="METHOD" ) > 0 ) then

        call MODEL%set_method( trim(sCmdText), trim(sOptionText) )

      endif
      
    enddo
    
  endif

end subroutine initialize_generic_method

!--------------------------------------------------------------------------------------------------

  subroutine check_for_fatal_warnings()

    call check_warnings()

  end subroutine check_for_fatal_warnings


end module loop_initialize