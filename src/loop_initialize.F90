module loop_initialize

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use constants_and_conversions, only : lTRUE, lFALSE
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

        select case(sCmdText)
        
          case ( "PRECIPITATION" )

            call CF_DICT%get_values("PRECIPITATION", myOptions )

              sOptionText = myOptions%get(1)

              select case (sOptionText)

                case ("ARC_ASCII")

                case ("SURFER")

                case ("NETCDF")

                case default

                  call warn( "Did not find a valid PRECIPITATION option. Value supplied was: "//dquote(sOptionText), &
                    lFatal = lTRUE )

                end select  

          case ( "PRECIPITATION_GRID_PROJECTION_DEFINITION" )

          case default

        end select



      enddo

    endif



  end subroutine initialize_precipitation_options


end module loop_initialize