module loop_initialize

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use constants_and_conversions, only : lTRUE, lFALSE, asFloat
  use data_catalog_entry
  use exceptions
  use strings
  use string_list
  use file_operations
  use dictionary
  implicit none

  private

  public :: read_control_file, initialize_precipitation_options

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

      do iIndex = 1, myDirectives%count

        sCmdText = myDirectives%get(iIndex)


        call CF_DICT%get_values("PRECIPITATION", myOptions )

        select case ( sCmdText )

          case ( "PRECIPITATION" )

            call CF_DICT%get_values("PRECIPITATION", myOptions )
            sOptionText = myOptions%get(1)

              if (.not. associated(PRCP))  allocate(PRCP, stat=iStat)
                call assert(iStat==0, "Problem allocating memory for the precipitation (PRCP) data structure",   &
                  __FILE__, __LINE__)

              PRCP%sVariableName_z = "prcp"

              select case (sOptionText)

                case ("ARC_ASCII")

                case ("SURFER")

                case ("NETCDF")

                case default

                  call warn( "Did not find a valid PRECIPITATION option. Value supplied was: "//dquote(sOptionText), &
                    lFatal = lTRUE )

                end select  

          case ( "PRECIPITATION_SCALE" )

            call CF_DICT%get_values("PRECIPITATION_SCALE", myOptions )
            sOptionText = myOptions%get(1)
            call PRCP%set_scale(asFloat(sOptionText))

          case ( "PRECIPITATION_OFFSET" )
  
            call CF_DICT%get_values("PRECIPITATION_OFFSET", myOptions )
            sOptionText = myOptions%get(1)
            call PRCP%set_offset(asFloat(sOptionText))

          case ( "PRECIPITATION_CONVERSION_FACTOR" )
            
            call CF_DICT%get_values("PRECIPITATION_CONVERSION_FACTOR", myOptions )
            sOptionText = myOptions%get(1)
            call PRCP%set_conversion_factor(asFloat(sOptionText))

          case ( "NETCDF_PRECIP_X_VAR" )

            call CF_DICT%get_values("NETCDF_PRECIP_X_VAR", myOptions )
            sOptionText = myOptions%get(1)
            PRCP%sVariableName_x = trim(sOptionText)

          case ( "NETCDF_PRECIP_Y_VAR" )

            call CF_DICT%get_values("NETCDF_PRECIP_Y_VAR", myOptions )
            sOptionText = myOptions%get(1)
            PRCP%sVariableName_y = trim(sOptionText)

          case ( "NETCDF_PRECIP_Z_VAR" )

            call CF_DICT%get_values("NETCDF_PRECIP_Z_VAR", myOptions )
            sOptionText = myOptions%get(1)
            PRCP%sVariableName_z = trim(sOptionText)

          case ( "NETCDF_PRECIP_TIME_VAR" )

            call CF_DICT%get_values("NETCDF_PRECIP_TIME_VAR", myOptions )
            sOptionText = myOptions%get(1)
            PRCP%sVariableName_time = trim(sOptionText)

          case ( "NETCDF_PRECIP_VARIABLE_ORDER" )

            call CF_DICT%get_values("NETCDF_PRECIP_VARIABLE_ORDER", myOptions )
            sOptionText = myOptions%get(1)
            call PRCP%set_variable_order( asLowercase(sOptionText) )

          case ( "NETCDF_PRECIP_FLIP_VERTICAL" )

            call PRCP%set_grid_flip_vertical()

          case ( "NETCDF_PRECIP_FLIP_HORIZONTAL" )

            call PRCP%set_grid_flip_horizontal()

          case ( "NETCDF_PRECIP_MAKE_LOCAL_ARCHIVE" )

            call PRCP%set_make_local_archive(lTRUE)

          case ( "PRECIPITATION_GRID_PROJECTION_DEFINITION" )

            call CF_DICT%get_values("PRECIPITATION_GRID_PROJECTION_DEFINITION", myOptions )
            sOptionText = myOptions%get(1)
            sArgText = myOptions%get(2, myOptions%count )
            call PRCP%set_PROJ4( trim(sArgText) )

          case ( "PRECIPITATION_MINIMUM_ALLOWED_VALUE" )

            call CF_DICT%get_values("PRECIPITATION_MINIMUM_ALLOWED_VALUE", myOptions )
            sOptionText = myOptions%get(1)
            PRCP%rMinAllowedValue = asFloat(sOptionText)

          case ( "PRECIPITATION_MAXIMUM_ALLOWED_VALUE" )

            call CF_DICT%get_values("PRECIPITATION_MAXIMUM_ALLOWED_VALUE", myOptions )
            sOptionText = myOptions%get(1)
            PRCP%rMaxAllowedValue = asFloat(sOptionText)

          case ( "PRECIPITATION_MISSING_VALUES_CODE" )

            call CF_DICT%get_values("PRECIPITATION_MISSING_VALUES_CODE", myOptions )
            sOptionText = myOptions%get(1)
            PRCP%rMissingValuesCode = asFloat(sOptionText)

          case ( "PRECIPITATION_MISSING_VALUES_OPERATOR" ) 

            call CF_DICT%get_values("PRECIPITATION_MISSING_VALUES_OPERATOR", myOptions )
            sOptionText = myOptions%get(1)
            PRCP%sMissingValuesOperator = trim(sOptionText)

          case ( "PRECIPITATION_MISSING_VALUES_ACTION")
            
            call CF_DICT%get_values("PRECIPITATION_MISSING_VALUES_ACTION", myOptions )
            sOptionText = myOptions%get(1)

            if (sOptionText == "ZERO") then
              PRCP%iMissingValuesAction = MISSING_VALUES_ZERO_OUT
            elseif (sOptionText == "MEAN" ) then
              PRCP%iMissingValuesAction = MISSING_VALUES_REPLACE_WITH_MEAN
            else
              call assert(lFALSE, "Unknown missing value action supplied for" &
                //" precipitation data: "//dquote(sOptionText) )
            endif



          case default

        end select



      enddo

    endif



  end subroutine initialize_precipitation_options


end module loop_initialize