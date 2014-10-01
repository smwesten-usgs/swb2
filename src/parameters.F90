module parameters

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use exceptions
  use file_operations
  use logfiles
  use strings
  use string_list
  use dictionary
  use constants_and_conversions
  implicit none

  private

  !! module to provide a single POC for storage and retrieval of parameter scalars and vectors
  !! need to provide a method for storing, finding, and retrieving parameters.
  !! dictionary: keyword, string_list
  
  !! first create list of files. parse through each file, adding to the param dictionary.
  !! once complete, allow other modules to interrogate the dictionary. return matches in the
  !! data type required for the parameter. once all params are in place, the data structures can be
  !! deallocated. 

  type, public :: PARAMETER_FILES_T

    type (STRING_LIST_T)               :: filenames
    type (STRING_LIST_T)               :: delimiters
    type (STRING_LIST_T)               :: comment_chars
    integer (kind=c_int)               :: count           = 0

  contains

    procedure, private   :: add_filename_to_list_sub
    generic              :: add => add_filename_to_list_sub

    procedure, private   :: munge_files_and_add_to_param_list_sub
    generic              :: munge => munge_files_and_add_to_param_list_sub

  end type PARAMETER_FILES_T    

  type (DICT_T), public :: PARAMS

  integer (kind=c_int), parameter :: MAX_TABLE_RECORD_LEN = 512

contains

  subroutine add_filename_to_list_sub(this, sFilename, sDelimiters, sCommentChars )

    class (PARAMETER_FILES_T)                     :: this
    character (len=*), intent(in)                 :: sFilename
    character (len=*), intent(in), optional       :: sDelimiters
    character (len=*), intent(in), optional       :: sCommentChars

    ! [ LOCALS ]
    character (len=:), allocatable  :: sDelimiters_
    character (len=:), allocatable  :: sCommentChars_
    
    if (present(sDelimiters) ) then
      sDelimiters_ = sDelimiters
    else 
      sDelimiters_ = sTAB
    endif

    if ( present(sCommentChars) ) then
      sCommentChars_ = sCommentChars
    else
      sCommentChars_ = "#!"
    endif  

    this%count = this%count + 1
    
    call this%filenames%append(sFilename)
    call this%delimiters%append(sDelimiters_)
    call this%comment_chars%append(sCommentChars_)
    
  end subroutine add_filename_to_list_sub

!--------------------------------------------------------------------------------------------------

  subroutine munge_files_and_add_to_param_list_sub(this)

    class (PARAMETER_FILES_T)    :: this

    ! [ LOCALS ]
    integer (kind=c_int)         :: iFileIndex, iColIndex
    integer (kind=c_int)         :: iStat
    type (ASCII_FILE_T)          :: DF
    type (DICT_ENTRY_T), pointer :: pDict
    type (DICT_ENTRY_T), pointer :: pCurrentDict
    integer (kind=c_int)         :: iNumberOfHeaderLines
    character (len=:), allocatable :: sNumberOfHeaderLines
    character (len=MAX_TABLE_RECORD_LEN) :: sRecord, sItem

    if ( this%count > 0 ) then

      ! iterate over the list of files
      do iFileIndex = 1, this%filenames%count

       ! open the file associated with current file index value
        call DF%open(sFilename = this%filenames%get(iFileIndex),             &
                     sCommentChars = this%comment_chars%get(iFileIndex),     &
                     sDelimiters = this%delimiters%get(iFileIndex) )

        ! obtain the headers from the file
        DF%slColNames = DF%readHeader()

        call LOGS%write( "Number of columns in file: "//asCharacter( DF%slColNames%count ), iTab=35 )

        ! loop over each column header
        do iColIndex = 1, DF%slColNames%count

          ! create and allocate memory for dictionary entry
          pDict => null()
          allocate( pDict, stat=iStat )
          call assert(iStat == 0, "Failed to allocate memory for dictionary object", &
              __FILE__, __LINE__ )

          ! add dictionary entry to dictionary
          call pDict%add_key( DF%slColNames%get(iColIndex) )
          call PARAMS%add_entry(pDict)

        enddo  

        ! now read in the remainder of the file
        do while ( .not. DF%isEOF() )

          ! read in next line of file
          sRecord = DF%readLine()

          ! skip blank lines
          if ( len_trim(sRecord) == 0 ) cycle

          ! loop over each column header
          do iColIndex = 1, DF%slColNames%count

            ! find pointer associated with header name
            ! (inefficient, but should be OK for small # of columns)
            pCurrentDict => PARAMS%get_entry( DF%slColNames%get(iColIndex) )

            ! break off next column of data for the current record
            call chomp(sRecord, sItem, this%delimiters%get(iFileIndex) )

            ! must avoid manipulating null pointers at all costs
            if ( associated(pCurrentDict)) then
            
              ! if not null, it means that we were able to return a pointer
              ! associated with the current column heading
              call pCurrentDict%add_string(sItem)
            
            else
            
              call warn("Internal programming error: null pointer detected" &
                //" -- was trying to find pointer associated with column"//dquote(DF%slColNames%get(iColIndex)), &
                __FILE__, __LINE__)  
            
            endif 
          
          enddo  
        
        enddo

        call DF%close()

      enddo
    endif

  end subroutine munge_files_and_add_to_param_list_sub

!--------------------------------------------------------------------------------------------------

end module parameters