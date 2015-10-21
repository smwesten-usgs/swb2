module model_initialize

  use iso_c_binding, only                : c_int, c_float, c_double, c_bool
  use constants_and_conversions
  use datetime
  use data_catalog
  use data_catalog_entry
  use dictionary
  use exceptions
  use logfiles, only                     : LOGS, LOG_ALL, LOG_DEBUG
  use file_operations
  use grid
  use output, only                       : initialize_output
  use model_domain
  use simulation_datetime, only : SIM_DT
  use strings
  use string_list  
  implicit none

  private

  public :: read_control_file, initialize_all
  public :: check_for_fatal_warnings

  type GRIDDED_DATASETS_T
    character (len=29)     :: sName
    integer (kind=c_int)   :: iDataType 
  end type GRIDDED_DATASETS_T

  type (GRIDDED_DATASETS_T), parameter  :: KNOWN_GRIDS(12) = &

    [ GRIDDED_DATASETS_T("GRIDFILE_001                 ", DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("GRIDFILE_002                 ", DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("GRIDFILE_003                 ", DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("GRIDFILE_004                 ", DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("GRIDFILE_005                 ", DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("GRIDFILE_006                 ", DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("GRIDFILE_007                 ", DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("GRIDFILE_008                 ", DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("GRIDFILE_009                 ", DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("GRIDFILE_010                 ", DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("GRIDFILE_011                 ", DATATYPE_FLOAT ),      &                
      GRIDDED_DATASETS_T("GRIDFILE_012                 ", DATATYPE_FLOAT )   ]

  type (GENERAL_GRID_T), pointer    :: pCOORD_GRD

contains

  subroutine initialize_all()

    ! [ LOCALS ]
    integer (kind=c_int)            :: iIndex
    type (STRING_LIST_T)            :: slList
    character (len=:), allocatable  :: sBuf

    ! define SWB project boundary and geographic projection
    call initialize_grid_options()

    ! define the start and end date for the simulation
    call initialize_start_and_end_dates()
        
    do iIndex = 1, ubound(KNOWN_GRIDS, 1)

      call initialize_generic_grid( sKey=KNOWN_GRIDS(iIndex)%sName,            &
                                    lOptional=lTRUE,                           &
                                    iDataType=KNOWN_GRIDS(iIndex)%iDataType )
   
    enddo

    call initialize_latitude()

  end subroutine initialize_all

!--------------------------------------------------------------------------------------------------

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



  !> Generic routine to handle intake of gridded data.
  !! 
  !! @param[in]  sKey        Name for the data type being processed.
  !! @param[in]  lOptional   If lOptional is TRUE, kill model run eventually IF sKey
  !!                         does not match a known data type.
  !! @param[in]  iDataType   Datatype as defined in @ref constants_and-conversion.F90
  !!
  !! This routine accepts a data grid type, for example, "PRECIPITATION", and attempts to
  !! handle all related control file directives associated with MODEL data type. In MODEL way,
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
    character (len=:), allocatable       :: sArgText_3
    integer (kind=c_int)                 :: iStat
    logical (kind=c_bool)             :: lGridPresent
    type (DATA_CATALOG_ENTRY_T), pointer :: pENTRY

    lGridPresent = lFALSE
    pENTRY => null()

    ! obtain a string list of directives that contain the keyword
    ! (e.g. if the key is "TMIN", MODEL might return:
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

        ! For MODEL directive, obtain the associated dictionary entries
        call CF_DICT%get_values(sCmdText, myOptions )

        ! dictionary entries are initially space-delimited; sArgText_1 contains
        ! all dictionary entries present, concatenated, with a space between entries
        sArgText = myOptions%get(1, myOptions%count )

        ! echo the original directive and dictionary entries to the logfile
        call LOGS%write("> "//sCmdText//" "//sArgText, iLinesBefore=1 )

        ! most of the time, we only care about the first dictionary entry, obtained below
        sArgText_1 = myOptions%get(1)
        sArgText_2 = myOptions%get(2)

        ! first option is that the key value and directive are the same 
        ! (.e.g. "PRECIPITATION"; no trailing underscores or modifiers )
        ! MODEL is a grid definition directive
 
        if ( sCmdText .strequal. sKey ) then

          pENTRY%sVariableName_z = asLowercase( sKey )

          ! override the usual checks to see whether the source data
          ! completely covers the target (i.e. base grid)
          call pENTRY%set_complete_spatial_coverage_flag( lFALSE )

          ! determine the type of grid and act appropriately
          if (sArgText_1 .strequal. "CONSTANT" ) then

            select case ( iDataType )

              case ( DATATYPE_FLOAT )

              call pENTRY%initialize(            &
                sDescription=trim(sCmdText),     &
                rConstant=asFloat(sArgText_2)  ) 
              lGridPresent = lTRUE   

            case ( DATATYPE_INT )

              call pENTRY%initialize(            &
                sDescription=trim(sCmdText),     &
                iConstant=asInt(sArgText_2)  ) 
              lGridPresent = lTRUE   

            case default

              call die( "INTERNAL PROGRAMMING ERROR: Unhandled data type selected.", &
                __FILE__, __LINE__ )

            end select
            
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


        elseif ( index( string=sCmdText, substring="_USE_MAJORITY_FILTER" ) > 0 ) then

          call pENTRY%set_majority_filter_flag( lTRUE )

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

          ! no operation; just keep SWB quiet about METHOD and it will be included in the
          ! methods initialization section. 

        else

          call warn("Unknown directive detected in code at line "//asCharacter(__LINE__)//", file "//__FILE__ &
            //". ~Ignoring. Directive is: "//dquote(sCmdText), iLogLevel=LOG_DEBUG )
 
        endif

      enddo

      ! if an unadorned grid specification directive was processed, then we can add the key and
      ! the data_catalog_entry to the data_catalog
      if ( lGridPresent ) then
        call DAT%add( key=sKey, data=pENTRY )
        call pENTRY%dump_data_structure()
      endif  

    endif

    call myDirectives%clear()
    call myOptions%clear()

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


    ! For MODEL directive, obtain the associated dictionary entries
    call CF_DICT%get_values( "GRID", myOptions )

    ! dictionary entries are initially space-delimited; sArgText contains
    ! all dictionary entries present, concatenated, with a space between entries
    sArgText = myOptions%get(1, myOptions%count )

    ! echo the original directive and dictionary entries to the logfile
    call LOGS%write("> GRID "//sArgText, iLinesBefore=1 )

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

    ! For MODEL directive, obtain the associated dictionary entries
    call CF_DICT%get_values( "BASE_PROJECTION_DEFINITION", myOptions )

    ! dictionary entries are initially space-delimited; sArgText contains
    ! all dictionary entries present, concatenated, with a space between entries
    sArgText = myOptions%get(1, myOptions%count )

    ! echo the original directive and dictionary entries to the logfile
    call LOGS%write("> BASE_PROJECTION_DEFINITION "//sArgText, iLinesBefore=1)

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

  subroutine initialize_start_and_end_dates()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions  
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sCmdText
    character (len=:), allocatable   :: sOptionText
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat
    logical (kind=c_bool)            :: lHaveStartDate
    logical (kind=c_bool)            :: lHaveEndDate

    lHaveStartDate = lFALSE
    lHaveEndDate = lFALSE

    myDirectives = CF_DICT%grep_keys("DATE")
      
!     if ( myDirectives%count < 2 ) then

!       call warn(sMessage="Your control file seems to be missing START_DATE and/or END_DATE", &
!         sHints="Add a START_DATE and/or END_DATE directive to your control file. Date "      &
!         //"~should be specified as mm/dd/yyyy.", lFatal = lTRUE, iLogLevel = LOG_ALL,        &
!         lEcho = lTRUE )

  
    call LOGS%set_loglevel( LOG_ALL )
    call LOGS%set_echo( lFALSE )

    do iIndex = 1, myDirectives%count

      ! myDirectives is a string list of all SWB directives that contain the string "DATE"
      ! sCmdText contains an individual directive
      sCmdText = myDirectives%get(iIndex)

      ! For MODEL directive, obtain the associated dictionary entries
      call CF_DICT%get_values(sCmdText, myOptions )

      ! dictionary entries are initially space-delimited; sArgText contains
      ! all dictionary entries present, concatenated, with a space between entries
      sArgText = myOptions%get(1, myOptions%count )

      ! echo the original directive and dictionary entries to the logfile
      call LOGS%write("> "//sCmdText//" "//sArgText, iLinesBefore=1 )

      ! most of the time, we only care about the first dictionary entry, obtained below
      sOptionText = myOptions%get(1)

      select case ( sCmdText )

        case ( "START_DATE", "STARTDATE", "BEGIN_DATE" )

          lHaveStartDate = lTRUE
          call SIM_DT%start%parseDate( sOptionText )
          call SIM_DT%start%calcJulianDay()

        case ( "END_DATE", "ENDDATE", "STOP_DATE" )

          lHaveEndDate = lTRUE
          call SIM_DT%end%parseDate( sOptionText )
          call SIM_DT%end%calcJulianDay()

        case default

          call warn("Unknown directive present, line "//asCharacter(__LINE__)//", file "//__FILE__ &
            //". Ignoring. Directive is: "//dquote(sCmdText), iLogLevel=LOG_DEBUG )

      end select

    enddo

    if ( lHaveStartDate .and. lHaveEndDate ) then

      SIM_DT%curr = SIM_DT%start
      SIM_DT%iDOY = day_of_year( SIM_DT%curr%getJulianDay() )

      SIM_DT%iDaysInMonth = SIM_DT%curr%dayspermonth()
      SIM_DT%iDaysInYear = SIM_DT%curr%daysperyear()
      SIM_DT%lIsLeapYear = SIM_DT%curr%isLeapYear()

      call LOGS%write("Model run start date set to: "//SIM_DT%start%prettydate(), iTab=4)
      call LOGS%write("Model run end date set to:   "//SIM_DT%end%prettydate(), iTab=4)

    else
    
      call warn(sMessage="Your control file seems to be missing START_DATE and/or END_DATE", &
        sHints="Add a START_DATE and/or END_DATE directive to your control file. Date "      &
        //"~should be specified as mm/dd/yyyy.", lFatal = lTRUE, iLogLevel = LOG_ALL,        &
        lEcho = lTRUE )
  
    endif

  end subroutine initialize_start_and_end_dates 

!--------------------------------------------------------------------------------------------------

  subroutine initialize_latitude()

    ! [ LOCALS ]
    integer (kind=c_int)  :: iIndex

    pCOORD_GRD => grid_Create( iNX=MODEL%number_of_columns, iNY=MODEL%number_of_rows, &
        rX0=MODEL%X_ll, rY0=MODEL%Y_ll, &
        rGridCellSize=MODEL%gridcellsize, iDataType=GRID_DATATYPE_REAL )  

    allocate ( MODEL%X(MODEL%number_of_columns ) )
    allocate ( MODEL%Y(MODEL%number_of_rows ) )

    ! call the grid routine to populate the X and Y values
    call grid_PopulateXY( pCOORD_GRD )

    ! populating these in order to have them available later for use in writing results to NetCDF
    MODEL%X = pCOORD_GRD%rX( :, 1 )
    MODEL%Y = pCOORD_GRD%rY( 1, : ) 

    ! transform to unprojected (lat/lon) coordinate system
    call grid_Transform(pGrd=pCOORD_GRD, sFromPROJ4=MODEL%PROJ4_string, &
        sToPROJ4="+proj=lonlat +ellps=GRS80 +datum=WGS84 +no_defs" )
    
    MODEL%latitude = pack( pCOORD_GRD%rY, MODEL%active )

    MODEL%X_lon = pCOORD_GRD%rX
    MODEL%Y_lat = pCOORD_GRD%rY

    pCOORD_GRD%rData=pCOORD_GRD%rX
    call grid_WriteArcGrid( sFilename="Longitude__calculated.asc", pGrd=pCOORD_GRD )
    pCOORD_GRD%rData=pCOORD_GRD%rY
    call grid_WriteArcGrid( sFilename="Latitude__calculated.asc", pGrd=pCOORD_GRD )

    call grid_Destroy( pCOORD_GRD )

  end subroutine initialize_latitude

end module model_initialize