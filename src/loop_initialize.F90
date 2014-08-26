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

  type (ASCII_FILE_T) :: CF

  type (DICT_T), public             :: CF_DICT 
  type (DICT_ENTRY_T), pointer      :: CF_ENTRY

  type GRIDDED_DATASETS_T
    character (len=23)     :: sName
    logical (kind=c_bool)  :: lOptional
    integer (kind=c_int)   :: iDataType 
  end type GRIDDED_DATASETS_T

  type METHODS_LIST_T
    character (len=23)     :: sName
  end type METHODS_LIST_T

  type (GRIDDED_DATASETS_T), parameter  :: KNOWN_GRIDS(11) = &

    [ GRIDDED_DATASETS_T("PRECIPITATION          ", lFALSE, DATATYPE_FLOAT ),     &
      GRIDDED_DATASETS_T("TMIN                   ", lFALSE, DATATYPE_FLOAT ),     &
      GRIDDED_DATASETS_T("TMAX                   ", lFALSE, DATATYPE_FLOAT ),     &
      GRIDDED_DATASETS_T("AVAILABLE_WATER_CONTENT", lFALSE, DATATYPE_FLOAT ),     &
      GRIDDED_DATASETS_T("POTENTIAL_ET           ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("SOLAR_RADIATION        ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("WIND_SPEED             ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("FOG_ZONE               ", lTRUE, DATATYPE_INT ),        &
      GRIDDED_DATASETS_T("LAND_USE               ", lFALSE, DATATYPE_INT ),       &
      GRIDDED_DATASETS_T("SOILS_GROUP            ", lFALSE, DATATYPE_INT ),       &
      GRIDDED_DATASETS_T("RELATIVE_HUMIDITY      ", lTRUE, DATATYPE_FLOAT )   ]

  type (METHODS_LIST_T)  :: KNOWN_METHODS(3) =     &
    [ METHODS_LIST_T("INTERCEPTION           "),   &
      METHODS_LIST_T("EVAPOTRANSPIRATION     "),   &
      METHODS_LIST_T("INFILTRATION           ")  ]

contains

  subroutine read_control_file( sFilename ) 

    character (len=*), intent(in)    :: sFilename

    ! [ LOCALS ]
    character (len=256)   :: sRecord, sSubstring
    integer (kind=c_int)  :: iStat

    call CF%open( sFilename = sFilename, &
                  sCommentChars = "#%!", &
                  sDelimiters = "WHITESPACE", &
                  lHasHeader = .false._c_bool )

    do 

      ! read in next line of file
      sRecord = CF%readLine()

      if ( CF%isEOF() )  exit

      ! create and allocate memory for dictionary entry
      CF_ENTRY => null()
      allocate( CF_ENTRY, stat=iStat )
      call assert(iStat == 0, "Failed to allocate memory for dictionary object", &
            __FILE__, __LINE__ )

      ! break off key value for the current record
      call chomp(sRecord, sSubstring, CF%sDelimiters )

      if ( len_trim(sSubstring) > 0 ) then

        call CF_ENTRY%add_key( sSubstring )

        ! break off first directive for the current record
        call chomp(sRecord, sSubstring, CF%sDelimiters )

        do while ( len_trim(sSubString) > 0 )

          call CF_ENTRY%add_string( sSubstring )

          ! break off next directive for the current record
          call chomp(sRecord, sSubstring, CF%sDelimiters )

        enddo  

        call CF_DICT%add_entry(CF_ENTRY)

      endif  
      
    enddo

    call CF%close()

  end subroutine read_control_file

!--------------------------------------------------------------------------------------------------  

  subroutine initialize_options()

    integer (kind=c_int) :: iIndex

    call initialize_grid_options()

    call initialize_start_and_end_dates()
    
    call initialize_parameter_tables()
    
    do iIndex = 1, ubound(KNOWN_GRIDS, 1)

      call initialize_generic_grid( sKey=KNOWN_GRIDS(iIndex)%sName, &
         lOptional=KNOWN_GRIDS(iIndex)%lOptional,                   &
         iDataType=KNOWN_GRIDS(iIndex)%iDataType )
   
    enddo

    call initialize_interception_method()
    call initialize_evapotranspiration_method()
    call initialize_infiltration_method()
    call MODEL%set_inactive_cells()
    call MODEL%initialize_arrays()
    call initialize_soils_landuse_awc_flowdir_values()
    call MODEL%preflight_check_method_pointers()

    call MODEL%initialize_methods()

    call MODEL%initialize_netcdf_output()

  end subroutine initialize_options

!--------------------------------------------------------------------------------------------------

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


    pENTRY => null()

    myDirectives = CF_DICT%grep_keys( sKey )

    call myDirectives%print

    if ( myDirectives%count == 0 ) then
      
      if (.not. lOptional) then

        call LOGS%write("Your control file seems to be missing any of the required directives relating to "//dquote(sKey), &
            iLogLevel=LOG_ALL, lEcho=lTRUE )
        call warn("Your control file is missing required directives. See the logfile and fix this before running again.", &
          lFatal = lTRUE )

      endif

    else  
    
      allocate(pENTRY, stat=iStat)
      call assert( iStat == 0, "Failed to allocate memory for the "//dquote(sKey)//" data structure", &
        __FILE__, __LINE__ )

      do iIndex = 1, myDirectives%count

        ! myDirectives is a string list of all SWB directives that contain sKey
        ! sCmdText contains an individual directive
        sCmdText = myDirectives%get(iIndex)

        ! For this directive, obtain the associated dictionary entries
        call CF_DICT%get_values(sCmdText, myOptions )

        call myOptions%print()

        ! dictionary entries are initially space-delimited; sArgText_1 contains
        ! all dictionary entries present, concatenated, with a space between entries
        sArgText = myOptions%get(1, myOptions%count )

        ! echo the original directive and dictionary entries to the logfile
        call LOGS%write(">> "//sCmdText//" "//sArgText, iLogLevel=LOG_GENERAL)

        ! most of the time, we only care about the first dictionary entry, obtained below
        sArgText_1 = myOptions%get(1)
        sArgText_2 = myOptions%get(2)

!        call CF_DICT%get_values(sCmdText, myOptions )

        if ( sCmdText .strequal. sKey ) then

          pENTRY%sVariableName_z = asLowercase( sKey )

          if (sArgText_1 .strequal. "CONSTANT" ) then

              call pENTRY%initialize(            &
                sDescription=trim(sCmdText),     &
                rConstant=asFloat(sArgText_2)  )            

            elseif ( (sArgText_1 .strequal. "ARC_ASCII")              &
                .or. (sArgText_1 .strequal. "SURFER")                 &
                .or. (sArgText_1 .strequal. "ARC_GRID") ) then

              call pENTRY%initialize(           &
                sDescription=trim(sCmdText),    &
                sFileType=trim(sArgText_1),     &
                sFilename=trim(sArgText_2),     &
                iDataType=iDataType )

            elseif ( sArgText_1 .strequal. "NETCDF" ) then
              
              call pENTRY%initialize_netcdf(    &
                sDescription=trim(sCmdText),    &
                sFilename = trim(sArgText_2),     &
                iDataType=iDataType )
 
            else

              call warn( "Did not find a valid "//dquote(sKey)//" option. Value supplied was: "//dquote(sArgText_1), &
                lFatal = lTRUE )

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

        else

          call warn("Unknown directive present, line "//asCharacter(__LINE__)//", file "//__FILE__ &
            //". Ignoring. Directive is: "//dquote(sCmdText), iLogLevel=LOG_DEBUG )
 
        endif

      enddo

      call DAT%add( key=sKey, data=pENTRY )

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

      !pGrd => grid_Create(iNX, iNY, rX0, rY0, rGridCellSize, GRID_DATATYPE_ALL) 
      call MODEL%initialize_grid(iNX, iNY, rX0, rY0, rGridCellSize)

      rX1 = rX0 + rGridCellSize * real(iNX, kind=c_double)
      rY1 = rY0 + rGridCellSize * real(iNY, kind=c_double)

    elseif ( myOptions%count == 7 ) then

      rX1 = asDouble( myOptions%get(5) )
      rY1 = asDouble( myOptions%get(6) )
      rGridCellSize = asDouble( myOptions%get(7) )

      fTempVal = ( rX1 - rX0 ) / real(iNX, kind=c_double)
      

      !pGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, GRID_DATATYPE_ALL)
      call MODEL%initialize_grid(iNX, iNY, rX0, rY0, rGridCellSize)

    else

      call warn("Grid specification is flawed or missing.", lFatal=lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

    endif

    call myOptions%deallocate()

    ! For this directive, obtain the associated dictionary entries
    call CF_DICT%get_values( "BASE_PROJECTION_DEFINITION", myOptions )

    ! dictionary entries are initially space-delimited; sArgText contains
    ! all dictionary entries present, concatenated, with a space between entries
    sArgText = myOptions%get(1, myOptions%count )

    ! echo the original directive and dictionary entries to the logfile
    call LOGS%write(">> "//"BASE_PROJECTION_DEFINITION "//sArgText)

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

      call MODEL%initialize_latitude()

      call MODEL%initialize_soil_groups()

      call MODEL%initialize_landuse()

      call MODEL%initialize_available_water_content()

      call MODEL%initialize_soil_layers()

  end subroutine initialize_soils_landuse_awc_flowdir_values





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
    type (PARAMETER_FILES_T)         :: PARAM_FILES
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

            call PARAM_FILES%add( sOptionText )
            iCount = iCount + 1

        else

            call warn("Unknown directive present, line "//asCharacter(__LINE__)//", file "//__FILE__ &
              //". Ignoring. Directive is: "//dquote(sCmdText), iLogLevel=LOG_DEBUG )
        
        endif

      enddo  

      if ( iCount > 0 ) then

        call PARAM_FILES%munge()
        call PARAMS%print_all()       

      endif

    endif

  end subroutine initialize_parameter_tables

!--------------------------------------------------------------------------------------------------

  subroutine initialize_interception_method()

  use interception__bucket

  ! [ LOCALS ]
  type (STRING_LIST_T)             :: myDirectives
  type (STRING_LIST_T)             :: myOptions  
  integer (kind=c_int)             :: iIndex
  character (len=:), allocatable   :: sCmdText
  character (len=:), allocatable   :: sOptionText
  character (len=:), allocatable   :: sArgText
  integer (kind=c_int)             :: iStat


  myDirectives = CF_DICT%grep_keys("INTERCEPTION")
    
  if ( myDirectives%count == 0 ) then

    call warn("Your control file seems to be missing any of the required directives relating to INTERCEPTION method.", &
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

        case ( "INTERCEPTION_METHOD" )

          sArgText = myOptions%get(2)

          select case (sOptionText)

            case ( "BUCKET" )

              call MODEL%set_interception("BUCKET")

          end select
          
        case default
        
          call warn("Unknown interception method was specified: "//dquote(sArgText), iLogLevel=LOG_ALL )

      end select
      
    enddo
    
  endif

end subroutine initialize_interception_method


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


  myDirectives = CF_DICT%grep_keys( trim(sKey) )
    
  if ( myDirectives%count == 0 ) then

    call warn("Your control file seems to be missing any of the required directives relating to "//dquote(sKey)//" method.", &
      lFatal = lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

  else  
  
    call LOGS%set_loglevel( LOG_ALL )
    call LOGS%set_echo( lFALSE )

    do iIndex = 1, myDirectives%count

      ! myDirectives is a string list of all SWB directives that contain the phrase given in sKey
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

      if ( index(string=sCmdText, substring="METHOD" ) > 0 ) then

        sArgText = myOptions%get(2)

        call MODEL%set_method( trim(sCmdText), trim(aArgText) )

      endif
      
    enddo
    
  endif

end subroutine initialize_generic_method

!--------------------------------------------------------------------------------------------------

subroutine initialize_evapotranspiration_method()

  ! [ LOCALS ]
  type (STRING_LIST_T)             :: myDirectives
  type (STRING_LIST_T)             :: myOptions  
  integer (kind=c_int)             :: iIndex
  character (len=:), allocatable   :: sCmdText
  character (len=:), allocatable   :: sOptionText
  character (len=:), allocatable   :: sArgText
  integer (kind=c_int)             :: iStat


  myDirectives = CF_DICT%grep_keys("EVAPOTRANSPIRATION")
    
  if ( myDirectives%count == 0 ) then

    call warn("Your control file seems to be missing any of the required directives relating to EVAPOTRANSPIRATION method.", &
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

        case ( "EVAPOTRANSPIRATION_METHOD", "ET_METHOD" )

          sArgText = myOptions%get(2)

          select case (sOptionText)

            case ( "HARGREAVES", "HARGREAVES-SAMANI" )

              call MODEL%set_evapotranspiration("HARGREAVES")

            case ( "J-H", "JENSEN-HAISE" )

              call MODEL%set_evapotranspiration("JENSEN-HAISE")

            end select
            
          case default
          
            call warn("Unknown evapotranspiration method was specified: "//dquote(sArgText), iLogLevel=LOG_ALL )

        end select
        
      enddo
      
    endif

  end subroutine initialize_evapotranspiration_method

!--------------------------------------------------------------------------------------------------  

  subroutine initialize_infiltration_method()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions  
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sCmdText
    character (len=:), allocatable   :: sOptionText
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat


    myDirectives = CF_DICT%grep_keys("INFILTRATION")
      
    if ( myDirectives%count == 0 ) then

      call warn("Your control file seems to be missing any of the required directives relating to INFILTRATION method.", &
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

          case ( "INFILTRATION_METHOD", "RUNOFF_METHOD" )

            sArgText = myOptions%get(2)

            select case (sOptionText)

              case ( "C-N", "CURVE_NUMBER" )

                call MODEL%set_infiltration_method("CURVE_NUMBER")

              case ( "G-A", "GREEN_AMPT", "GREEN-AMPT" )

                call MODEL%set_infiltration_method("GREEN_AMPT")

            end select
            
          case default
          
            call warn("Unknown infiltration method was specified: "//dquote(sArgText), iLogLevel=LOG_ALL )

        end select
        
      enddo
      
    endif

  end subroutine initialize_infiltration_method

!--------------------------------------------------------------------------------------------------

  subroutine check_for_fatal_warnings()

    call check_warnings()

  end subroutine check_for_fatal_warnings


end module loop_initialize