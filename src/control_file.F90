module control_file

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use exceptions
  use strings
  use file_operations
  use dictionary
  implicit none

  private

  public :: read_control_file

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



end module control_file