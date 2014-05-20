module parameters

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use data_file
  use strings
  use string_list
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

    type (STRING_LIST_T)  :: filenames
    type (STRING_LIST_T)  :: delimiters
    type (STRING_LIST_T)  :: comment_chars
    integer (kind=c_int)  :: count           = 0

  contains

    procedure, private   :: add_filename_to_list_sub
    generic              :: add => add_filename_to_list_sub

    procedure, private   :: munge_files_and_add_to_param_list_sub
    generic              :: munge => munge_files_and_add_to_param_list_sub

  end type PARAMETER_FILES_T    


contains

  subroutine add_filename_to_list_sub(this, sFilename, sDelimiters, sCommentChars )

    class (STRING_LIST_T)             :: this
    character (len=*), intent(in)     :: sFilename
    character (len=*), intent(in), optional :: sDelimiters
    character (len=*), intent(in), optional :: sCommentChars

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

    call this%filenames%append(sFilename)
    call this%delimiters%append(sDelimiters_)
    call this%comment_chars%append(sCommentChars_)

    this%count = this%count + 1

  end subroutine add_filename_to_list_sub

!--------------------------------------------------------------------------------------------------

  subroutine munge_files_and_add_to_param_list_sub(this)

    class (PARAMETER_FILES_T)    :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    type (DATA_FILE_T)   :: DF

    if ( this%count > 0 ) then

      ! iterate over the list of files
      do iIndex = 1, this%filenames%count

       ! open the file
        call DF%open(sFilename = this%filenames%get(iIndex),           &
                     sCommentChars = this%delimiters%get(iIndex),      &
                     sDelimiters = this%comment_chars%get(iIndex) )

        ! obtain the headers from the file
        DF%slColNames = DF%readHeader()

      enddo

    endif

  end subroutine munge_files_and_add_to_param_list_sub

!--------------------------------------------------------------------------------------------------

 




end module parameters