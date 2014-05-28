module loop_initialize

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use constants_and_conversions, only : lTRUE, lFALSE, asFloat
  use data_catalog_entry
  use exceptions
  use model_domain, only : pGrd
  use strings
  use string_list
  use file_operations
  use dictionary
  implicit none

  private

  public :: read_control_file, initialize_options
  public :: check_for_fatal_warnings

  type (ASCII_FILE_T) :: CF

  type (DICT_T), public             :: CF_DICT 
  type (DICT_ENTRY_T), pointer      :: CF_ENTRY

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

    do while ( .not. CF%isEOF() )

      ! read in next line of file
      sRecord = CF%readLine()

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

  subroutine initialize_options()

    call initialize_grid_options()
    call initialize_precipitation_options()
    call initialize_tmax_options()
    call initialize_tmin_options()
    call initialize_flow_direction_options()
    call initialize_water_capacity_options()
    call initialize_soils_group_options()
    call initialize_landuse_options()

  end subroutine initialize_options


  subroutine initialize_precipitation_options()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions  
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sCmdText
    character (len=:), allocatable   :: sOptionText
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat


    myDirectives = CF_DICT%grep_keys("PRECIP")

    if ( myDirectives%count == 0 ) then
      call warn("Your control file seems to be missing any of the required directives relating to PRECIPITATION", &
        lFatal = lTRUE )
    else  
    
      allocate(PRCP, stat=iStat)
      call assert( iStat == 0, "Failed to allocate memory for the precipitation (PRCP) data structure", &
        __FILE__, __LINE__ )

      call LOGS%set_loglevel( LOG_ALL )
      call LOGS%set_echo( lFALSE )

      do iIndex = 1, myDirectives%count

        ! myDirectives is a string list of all SWB directives that contain the phrase "PRECIP"
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

          case ( "PRECIPITATION" )

              if (.not. associated(PRCP))  allocate(PRCP, stat=iStat)
                call assert(iStat==0, "Problem allocating memory for the precipitation (PRCP) data structure",   &
                  __FILE__, __LINE__)

              PRCP%sVariableName_z = "prcp"

              sArgText = myOptions%get(2)

              select case (sOptionText)

                case ("ARC_ASCII", "SURFER")

                  call PRCP%initialize(sDescription=trim(sCmdText), &
                    sFileType=trim(sOptionText), &
                    sFilenameTemplate=trim(sArgText), &
                    iDataType=DATATYPE_REAL )

                case ("NETCDF")
                  
                  call PRCP%initialize_netcdf( &
                    sDescription=trim(sCmdText), &
                    sFilenameTemplate = trim(sArgText), &
                    iDataType=DATATYPE_REAL, &
                    pGrdBase=pGrd)
     
                case default

                  call warn( "Did not find a valid PRECIPITATION option. Value supplied was: "//dquote(sOptionText), &
                    lFatal = lTRUE )

                end select  

          case ( "PRECIPITATION_SCALE" )

            call PRCP%set_scale(asFloat(sOptionText))

          case ( "PRECIPITATION_OFFSET" )
  
            call PRCP%set_offset(asFloat(sOptionText))

          case ( "PRECIPITATION_CONVERSION_FACTOR" )
            
            call PRCP%set_conversion_factor(asFloat(sOptionText))

          case ( "NETCDF_PRECIP_X_VAR" )

            PRCP%sVariableName_x = trim(sOptionText)

          case ( "NETCDF_PRECIP_Y_VAR" )

            PRCP%sVariableName_y = trim(sOptionText)

          case ( "NETCDF_PRECIP_Z_VAR" )

            PRCP%sVariableName_z = trim(sOptionText)

          case ( "NETCDF_PRECIP_TIME_VAR" )

            PRCP%sVariableName_time = trim(sOptionText)

          case ( "NETCDF_PRECIP_VARIABLE_ORDER" )

            call PRCP%set_variable_order( asLowercase(sOptionText) )

          case ( "NETCDF_PRECIP_FLIP_VERTICAL" )

            call PRCP%set_grid_flip_vertical()

          case ( "NETCDF_PRECIP_FLIP_HORIZONTAL" )

            call PRCP%set_grid_flip_horizontal()

          case ( "NETCDF_PRECIP_MAKE_LOCAL_ARCHIVE" )

            call PRCP%set_make_local_archive(lTRUE)

          case ( "PRECIPITATION_GRID_PROJECTION_DEFINITION" )

            sArgText = myOptions%get(2, myOptions%count )
            call PRCP%set_PROJ4( trim(sArgText) )

          case ( "PRECIPITATION_MINIMUM_ALLOWED_VALUE" )

            PRCP%rMinAllowedValue = asFloat(sOptionText)

          case ( "PRECIPITATION_MAXIMUM_ALLOWED_VALUE" )

            PRCP%rMaxAllowedValue = asFloat(sOptionText)

          case ( "PRECIPITATION_MISSING_VALUES_CODE" )

            PRCP%rMissingValuesCode = asFloat(sOptionText)

          case ( "PRECIPITATION_MISSING_VALUES_OPERATOR" ) 

            PRCP%sMissingValuesOperator = trim(sOptionText)

          case ( "PRECIPITATION_MISSING_VALUES_ACTION")

            if (sOptionText == "ZERO") then
              PRCP%iMissingValuesAction = MISSING_VALUES_ZERO_OUT
            elseif (sOptionText == "MEAN" ) then
              PRCP%iMissingValuesAction = MISSING_VALUES_REPLACE_WITH_MEAN
            else
              call assert(lFALSE, "Unknown missing value action supplied for" &
                //" precipitation data: "//dquote(sOptionText) )
            endif



          case default

          call warn("Unknown directive present. Ignoring. Directive is: "//dquote(sCmdText) )

        end select



      enddo

    endif



  end subroutine initialize_precipitation_options


  subroutine initialize_tmax_options()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions  
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sCmdText
    character (len=:), allocatable   :: sOptionText
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat


    myDirectives = CF_DICT%grep_keys("TMAX")

    if ( myDirectives%count == 0 ) then
      call warn("Your control file seems to be missing any of the required directives relating to TMAX", &
        lFatal = lTRUE )
    else  
    
      allocate(TMAX, stat=iStat)
      call assert( iStat == 0, "Failed to allocate memory for the maximum air temperature (TMAX) data structure", &
        __FILE__, __LINE__ )

      do iIndex = 1, myDirectives%count

        ! myDirectives is a string list of all SWB directives that contain the phrase "PRECIP"
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

        call CF_DICT%get_values("TMAX", myOptions )

        select case ( sCmdText )

          case ( "TMAX" )

            if (.not. associated(TMAX))  allocate(TMAX, stat=iStat)
              call assert(iStat==0, "Problem allocating memory for the maximum air temperature (TMAX) data structure",   &
                __FILE__, __LINE__)

            TMAX%sVariableName_z = "tmax"

            sArgText = myOptions%get(2)

            select case (sOptionText)

              case ("ARC_ASCII", "SURFER")

                call TMAX%initialize(sDescription=trim(sCmdText), &
                  sFileType=trim(sOptionText), &
                  sFilenameTemplate=trim(sArgText), &
                  iDataType=DATATYPE_REAL )

              case ("NETCDF")
                
                call TMAX%initialize_netcdf( &
                  sDescription=trim(sCmdText), &
                  sFilenameTemplate = trim(sArgText), &
                  iDataType=DATATYPE_REAL, &
                  pGrdBase=pGrd)
   
              case default

                call warn( "Did not find a valid TMAX option. Value supplied was: "//dquote(sOptionText), &
                  lFatal = lTRUE )

              end select  

          case ( "TMAX_SCALE" )

            call TMAX%set_scale(asFloat(sOptionText))

          case ( "TMAX_OFFSET" )
  
            call TMAX%set_offset(asFloat(sOptionText))

          case ( "TMAX_CONVERSION_FACTOR" )
            
            call TMAX%set_conversion_factor(asFloat(sOptionText))

          case ( "NETCDF_TMAX_X_VAR" )

            TMAX%sVariableName_x = trim(sOptionText)

          case ( "NETCDF_TMAX_Y_VAR" )

            TMAX%sVariableName_y = trim(sOptionText)

          case ( "NETCDF_TMAX_Z_VAR" )

            TMAX%sVariableName_z = trim(sOptionText)

          case ( "NETCDF_TMAX_TIME_VAR" )

            TMAX%sVariableName_time = trim(sOptionText)

          case ( "NETCDF_TMAX_VARIABLE_ORDER" )

            call TMAX%set_variable_order( asLowercase(sOptionText) )

          case ( "NETCDF_TMAX_FLIP_VERTICAL" )

            call TMAX%set_grid_flip_vertical()

          case ( "NETCDF_TMAX_FLIP_HORIZONTAL" )

            call TMAX%set_grid_flip_horizontal()

          case ( "NETCDF_TMAX_MAKE_LOCAL_ARCHIVE" )

            call TMAX%set_make_local_archive(lTRUE)

          case ( "TMAX_GRID_PROJECTION_DEFINITION" )

            sArgText = myOptions%get(2, myOptions%count )
            call TMAX%set_PROJ4( trim(sArgText) )

          case ( "TMAX_MINIMUM_ALLOWED_VALUE" )

            TMAX%rMinAllowedValue = asFloat(sOptionText)

          case ( "TMAX_MAXIMUM_ALLOWED_VALUE" )

            TMAX%rMaxAllowedValue = asFloat(sOptionText)

          case ( "TMAX_MISSING_VALUES_CODE" )

            TMAX%rMissingValuesCode = asFloat(sOptionText)

          case ( "TMAX_MISSING_VALUES_OPERATOR" ) 

            TMAX%sMissingValuesOperator = trim(sOptionText)

          case ( "TMAX_MISSING_VALUES_ACTION")
            
            if (sOptionText == "ZERO") then
              TMAX%iMissingValuesAction = MISSING_VALUES_ZERO_OUT
            elseif (sOptionText == "MEAN" ) then
              TMAX%iMissingValuesAction = MISSING_VALUES_REPLACE_WITH_MEAN
            else
              call assert(lFALSE, "Unknown missing value action supplied for" &
                //" TMAX data: "//dquote(sOptionText) )
            endif

          case default

            call warn("Unknown directive present. Ignoring. Directive is: "//dquote(sCmdText) )
 
        end select

      enddo

    endif

  end subroutine initialize_tmax_options



  subroutine initialize_tmin_options()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions  
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sCmdText
    character (len=:), allocatable   :: sOptionText
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat


    myDirectives = CF_DICT%grep_keys("TMIN")

    if ( myDirectives%count == 0 ) then
      call warn("Your control file seems to be missing any of the required directives relating to TMIN", &
        lFatal = lTRUE )
    else  
    
      allocate(TMIN, stat=iStat)
      call assert( iStat == 0, "Failed to allocate memory for the maximum air temperature (TMIN) data structure", &
        __FILE__, __LINE__ )

      do iIndex = 1, myDirectives%count

        ! myDirectives is a string list of all SWB directives that contain the phrase "PRECIP"
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

        call CF_DICT%get_values("TMIN", myOptions )

        select case ( sCmdText )

          case ( "TMIN" )

            if (.not. associated(TMIN))  allocate(TMIN, stat=iStat)
              call assert(iStat==0, "Problem allocating memory for the maximum air temperature (TMIN) data structure",   &
                __FILE__, __LINE__)

            TMIN%sVariableName_z = "tmin"

            sArgText = myOptions%get(2)

            select case (sOptionText)

              case ("ARC_ASCII", "SURFER")

                call TMIN%initialize(sDescription=trim(sCmdText), &
                  sFileType=trim(sOptionText), &
                  sFilenameTemplate=trim(sArgText), &
                  iDataType=DATATYPE_REAL )

              case ("NETCDF")
                
                call TMIN%initialize_netcdf( &
                  sDescription=trim(sCmdText), &
                  sFilenameTemplate = trim(sArgText), &
                  iDataType=DATATYPE_REAL, &
                  pGrdBase=pGrd)
   
              case default

                call warn( "Did not find a valid TMIN option. Value supplied was: "//dquote(sOptionText), &
                  lFatal = lTRUE )

              end select  

          case ( "TMIN_SCALE" )

            call TMIN%set_scale(asFloat(sOptionText))

          case ( "TMIN_OFFSET" )
  
            call TMIN%set_offset(asFloat(sOptionText))

          case ( "TMIN_CONVERSION_FACTOR" )
            
            call TMIN%set_conversion_factor(asFloat(sOptionText))

          case ( "NETCDF_TMIN_X_VAR" )

            TMIN%sVariableName_x = trim(sOptionText)

          case ( "NETCDF_TMIN_Y_VAR" )

            TMIN%sVariableName_y = trim(sOptionText)

          case ( "NETCDF_TMIN_Z_VAR" )

            TMIN%sVariableName_z = trim(sOptionText)

          case ( "NETCDF_TMIN_TIME_VAR" )

            TMIN%sVariableName_time = trim(sOptionText)

          case ( "NETCDF_TMIN_VARIABLE_ORDER" )

            call TMIN%set_variable_order( asLowercase(sOptionText) )

          case ( "NETCDF_TMIN_FLIP_VERTICAL" )

            call TMIN%set_grid_flip_vertical()

          case ( "NETCDF_TMIN_FLIP_HORIZONTAL" )

            call TMIN%set_grid_flip_horizontal()

          case ( "NETCDF_TMIN_MAKE_LOCAL_ARCHIVE" )

            call TMIN%set_make_local_archive(lTRUE)

          case ( "TMIN_GRID_PROJECTION_DEFINITION" )

            sArgText = myOptions%get(2, myOptions%count )
            call TMIN%set_PROJ4( trim(sArgText) )

          case ( "TMIN_MINIMUM_ALLOWED_VALUE" )

            TMIN%rMinAllowedValue = asFloat(sOptionText)

          case ( "TMIN_MAXIMUM_ALLOWED_VALUE" )

            TMIN%rMaxAllowedValue = asFloat(sOptionText)

          case ( "TMIN_MISSING_VALUES_CODE" )

            TMIN%rMissingValuesCode = asFloat(sOptionText)

          case ( "TMIN_MISSING_VALUES_OPERATOR" ) 

            TMIN%sMissingValuesOperator = trim(sOptionText)

          case ( "TMIN_MISSING_VALUES_ACTION")
            
            if (sOptionText == "ZERO") then
              TMIN%iMissingValuesAction = MISSING_VALUES_ZERO_OUT
            elseif (sOptionText == "MEAN" ) then
              TMIN%iMissingValuesAction = MISSING_VALUES_REPLACE_WITH_MEAN
            else
              call assert(lFALSE, "Unknown missing value action supplied for" &
                //" TMIN data: "//dquote(sOptionText) )
            endif

          case default

            call warn("Unknown directive present. Ignoring. Directive is: "//dquote(sCmdText) )
 
        end select

      enddo

    endif

  end subroutine initialize_tmin_options






  subroutine initialize_grid_options()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myOptions  
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat
    real (kind=c_double)             :: rX0, rX1, rY0, rY1, rGridCellSize
    integer (kind=c_int)             :: iNX, iNY


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

      pGrd => grid_Create(iNX, iNY, rX0, rY0, rGridCellSize, GRID_DATATYPE_ALL) 

    elseif ( myOptions%count == 7 ) then

      rX0 = asDouble( myOptions%get(5) )
      rY0 = asDouble( myOptions%get(6) )
      rGridCellSize = asDouble( myOptions%get(7) )

      pGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, GRID_DATATYPE_ALL)

    else

      call warn("Wrong number of entries present for grid specification", lFatal=lTRUE)

    endif


  end subroutine initialize_grid_options




  subroutine initialize_flow_direction_options()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions  
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sCmdText
    character (len=:), allocatable   :: sOptionText
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat


    myDirectives = CF_DICT%grep_keys("FLOW_DIRECTION")

    if ( myDirectives%count == 0 ) then
      call warn("Your control file seems to be missing any of the required directives relating to FLOW_DIRECTION", &
        lFatal = lTRUE )
    else  
    
      allocate(FLOWDIR, stat=iStat)
      call assert( iStat == 0, "Failed to allocate memory for the flow direction (FLOWDIR) data structure", &
        __FILE__, __LINE__ )

      call LOGS%set_loglevel( LOG_ALL )
      call LOGS%set_echo( lFALSE )

      do iIndex = 1, myDirectives%count

        ! myDirectives is a string list of all SWB directives that contain the phrase "FLOW_DIRECTION"
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

          case ( "FLOW_DIRECTION" )

            if (.not. associated(PRCP))  allocate(PRCP, stat=iStat)
              call assert(iStat==0, "Problem allocating memory for the flow direction (FLOWDIR) data structure",   &
                __FILE__, __LINE__)

            FLOWDIR%sVariableName_z = "flowdir"

            sArgText = myOptions%get(2)

            select case (sOptionText)

              case ( "ARC_ASCII", "SURFER", "ARC_GRID" )

                call FLOWDIR%initialize(sDescription=trim(sCmdText), &
                  sFileType=trim(sOptionText), &
                  sFilenameTemplate=trim(sArgText), &
                  iDataType=DATATYPE_REAL )

              case ( "NETCDF" )
                  
                call FLOWDIR%initialize_netcdf( &
                  sDescription=trim(sCmdText), &
                  sFilenameTemplate = trim(sArgText), &
                  iDataType=DATATYPE_REAL, &
                  pGrdBase=pGrd)
     
              case default

                call warn( "Did not find a valid FLOW_DIRECTION option. Value supplied was: "//dquote(sOptionText), &
                    lFatal = lTRUE )

            end select  

          case ( "FLOW_DIRECTION_PROJECTION_DEFINITION" )

            sArgText = myOptions%get(2, myOptions%count )
            call FLOWDIR%set_PROJ4( trim(sArgText) )

          case default

            call warn("Unknown directive present. Ignoring. Directive is: "//dquote(sCmdText) )
 
        end select

      enddo

    endif

  end subroutine initialize_flow_direction_options  






    subroutine initialize_water_capacity_options()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions  
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sCmdText
    character (len=:), allocatable   :: sOptionText
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat


    myDirectives = CF_DICT%grep_keys("WATER_CAPACITY")

    if ( myDirectives%count == 0 ) then
      call warn("Your control file seems to be missing any of the required directives relating to WATER_CAPACITY", &
        lFatal = lTRUE )
    else  
    
      allocate(AWC, stat=iStat)
      call assert( iStat == 0, "Failed to allocate memory for the water capacity (AWC) data structure", &
        __FILE__, __LINE__ )

      call LOGS%set_loglevel( LOG_ALL )
      call LOGS%set_echo( lFALSE )

      do iIndex = 1, myDirectives%count

        ! myDirectives is a string list of all SWB directives that contain the phrase "WATER_CAPACITY"
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

          case ( "WATER_CAPACITY" )

            if (.not. associated(AWC))  allocate(AWC, stat=iStat)
              call assert(iStat==0, "Problem allocating memory for the water capacity (AWC) data structure",   &
                __FILE__, __LINE__)

            FLOWDIR%sVariableName_z = "awc"

            sArgText = myOptions%get(2)

            select case (sOptionText)

              case ( "ARC_ASCII", "SURFER", "ARC_GRID" )

                call AWC%initialize(sDescription=trim(sCmdText), &
                  sFileType=trim(sOptionText), &
                  sFilenameTemplate=trim(sArgText), &
                  iDataType=DATATYPE_REAL )

              case ( "NETCDF" )
                  
                call AWC%initialize_netcdf( &
                  sDescription=trim(sCmdText), &
                  sFilenameTemplate = trim(sArgText), &
                  iDataType=DATATYPE_REAL, &
                  pGrdBase=pGrd)
     
              case default

                call warn( "Did not find a valid WATER_CAPACITY option. Value supplied was: "//dquote(sOptionText), &
                    lFatal = lTRUE )

            end select  

          case ( "WATER_CAPACITY_PROJECTION_DEFINITION" )

            sArgText = myOptions%get(2, myOptions%count )
            call AWC%set_PROJ4( trim(sArgText) )

          case default

            call warn("Unknown directive present. Ignoring. Directive is: "//dquote(sCmdText) )
 
        end select

      enddo

    endif

  end subroutine initialize_water_capacity_options  






  subroutine initialize_soils_group_options()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions  
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sCmdText
    character (len=:), allocatable   :: sOptionText
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat


    myDirectives = CF_DICT%grep_keys("SOILS_GROUP")
    if (myDirectives%count == 0) then
      call myDirectives%deallocate()      
      myDirectives = CF_DICT%grep_keys("SOIL_GROUP")
    endif
      
    if ( myDirectives%count == 0 ) then

      call warn("Your control file seems to be missing any of the required directives relating to SOILS_GROUP", &
        lFatal = lTRUE )

    else  
    
      allocate(HSG, stat=iStat)
      call assert( iStat == 0, "Failed to allocate memory for the soils group (HSG) data structure", &
        __FILE__, __LINE__ )

      call LOGS%set_loglevel( LOG_ALL )
      call LOGS%set_echo( lFALSE )

      do iIndex = 1, myDirectives%count

        ! myDirectives is a string list of all SWB directives that contain the phrase "WATER_CAPACITY"
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

          case ( "SOILS_GROUP", "SOIL_GROUP" )

            if (.not. associated(HSG))  allocate(HSG, stat=iStat)
              call assert(iStat==0, "Problem allocating memory for the soils group (HSG) data structure",   &
                __FILE__, __LINE__)

            HSG%sVariableName_z = "hsg"

            sArgText = myOptions%get(2)

            select case (sOptionText)

              case ( "ARC_ASCII", "SURFER", "ARC_GRID" )

                call HSG%initialize(sDescription=trim(sCmdText), &
                  sFileType=trim(sOptionText), &
                  sFilenameTemplate=trim(sArgText), &
                  iDataType=DATATYPE_REAL )

              case ( "NETCDF" )
                  
                call HSG%initialize_netcdf( &
                  sDescription=trim(sCmdText), &
                  sFilenameTemplate = trim(sArgText), &
                  iDataType=DATATYPE_REAL, &
                  pGrdBase=pGrd)
     
              case default

                call warn( "Did not find a valid SOILS_GROUP option. Value supplied was: "//dquote(sOptionText), &
                    lFatal = lTRUE )

            end select  

          case ( "SOILS_GROUP_PROJECTION_DEFINITION", "SOIL_GROUP_PROJECTION_DEFINITION" )

            sArgText = myOptions%get(2, myOptions%count )
            call HSG%set_PROJ4( trim(sArgText) )

          case default

            call warn("Unknown directive present. Ignoring. Directive is: "//dquote(sCmdText) )
 
        end select

      enddo

    endif

  end subroutine initialize_soils_group_options  




  subroutine initialize_landuse_options()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions  
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sCmdText
    character (len=:), allocatable   :: sOptionText
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat


    myDirectives = CF_DICT%grep_keys("LAND_USE")
    if (myDirectives%count == 0) then
      call myDirectives%deallocate()      
      myDirectives = CF_DICT%grep_keys("LANDUSE")
    endif
      
    if ( myDirectives%count == 0 ) then

      call warn("Your control file seems to be missing any of the required directives relating to LANDUSE", &
        lFatal = lTRUE )

    else  
    
      allocate(HSG, stat=iStat)
      call assert( iStat == 0, "Failed to allocate memory for the landuse (LULC) data structure", &
        __FILE__, __LINE__ )

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

          case ( "LANDUSE", "LAND_USE" )

            if (.not. associated(LULC))  allocate(LULC, stat=iStat)
              call assert(iStat==0, "Problem allocating memory for the soils group (LULC) data structure",   &
                __FILE__, __LINE__)

            LULC%sVariableName_z = "lulc"

            sArgText = myOptions%get(2)

            select case (sOptionText)

              case ( "ARC_ASCII", "SURFER", "ARC_GRID" )

                call LULC%initialize(sDescription=trim(sCmdText), &
                  sFileType=trim(sOptionText), &
                  sFilenameTemplate=trim(sArgText), &
                  iDataType=DATATYPE_REAL )

              case ( "NETCDF" )
                  
                call LULC%initialize_netcdf( &
                  sDescription=trim(sCmdText), &
                  sFilenameTemplate = trim(sArgText), &
                  iDataType=DATATYPE_REAL, &
                  pGrdBase=pGrd)
     
              case default

                call warn( "Did not find a valid LANDUSE option. Value supplied was: "//dquote(sOptionText), &
                    lFatal = lTRUE )

            end select  

          case ( "LANDUSE_PROJECTION_DEFINITION", "LAND_USE_PROJECTION_DEFINITION" )

            sArgText = myOptions%get(2, myOptions%count )
            call LULC%set_PROJ4( trim(sArgText) )

          case default

            call warn("Unknown directive present. Ignoring. Directive is: "//dquote(sCmdText) )
 
        end select

      enddo

    endif

  end subroutine initialize_landuse_options  




!--------------------------------------------------------------------------------------------------  

  subroutine check_for_fatal_warnings()

    call check_warnings()

  end subroutine check_for_fatal_warnings


end module loop_initialize