module data_file

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use iso_fortran_env, only : IOSTAT_END
  use types_new
  use exceptions
  use constants_and_conversions
  use strings
  use string_list
  implicit none

  private

  integer (kind=c_int), parameter :: MAX_STR_LEN = 256
  
  type, public :: DATA_FILE_T

    character (len=:), allocatable  :: sFilename
    character (len=:), allocatable  :: sDelimiters
    character (len=:), allocatable  :: sCommentChars
    type (STRING_LIST_T)            :: slColNames
    integer (kind=c_int)            :: iCurrentLinenum = 0
    integer (kind=c_int)            :: iNumberOfLines = 0
    integer (kind=c_int)            :: iNumberOfRecords = 0
    logical (kind=c_bool)           :: lIsOpen = lFALSE
    integer (kind=c_int)            :: iUnitNum
    integer (kind=c_int)            :: iStat
    character (len=MAX_STR_LEN)     :: sBuf
    character (len=:), allocatable  :: stMissingValue

  contains

    procedure, private :: open_file_char_sub
    generic :: open => open_file_char_sub

    procedure, private :: close_file_sub
    generic :: close => close_file_sub

    procedure, private :: is_file_open_fn
    generic :: isOpen => is_file_open_fn

    procedure, private :: does_file_exist_fn
    generic :: exists => does_file_exist_fn

    procedure, private :: is_current_line_a_comment_fn
    generic :: isComment => is_current_line_a_comment_fn

    procedure, private :: count_number_of_lines_sub
    generic :: countLines => count_number_of_lines_sub

    procedure, private :: return_num_lines_fn
    generic :: numLines => return_num_lines_fn

    procedure, private :: return_num_records_fn
    generic :: numRecords => return_num_records_fn

    procedure, private :: return_current_linenum_fn
    generic :: currentLineNum => return_current_linenum_fn

! procedure :: readHeaderMultiline => read_multiline_header_sub
! procedure :: numRows => return_num_rows_fn
! procedure :: numCols => return_num_cols_fn
! procedure :: countRows => count_rows_sub
! procedure :: countColumns => cound_columns_sub

    procedure, private :: read_header_fn
    generic, public    :: readHeader => read_header_fn

    procedure, private :: read_line_of_data_fn
    generic, public    :: readLine => read_line_of_data_fn

! procedure :: putRow => put_row_of_data_sub
! procedure :: getNext => get_next_data_item_fn

  end type DATA_FILE_T


contains

  function return_num_lines_fn(this)    result(iNumLines)

    class (DATA_FILE_T)   :: this
    integer (kind=c_int) :: iNumLines

    iNumLines = this%iNumberOfLines

  end function return_num_lines_fn


  function return_num_records_fn(this)    result(iNumRecords)

    class (DATA_FILE_T)   :: this
    integer (kind=c_int) :: iNumRecords

    iNumRecords = this%iNumberOfRecords

  end function return_num_records_fn


  function return_current_linenum_fn(this)    result(iCurrentLinenum)

    class (DATA_FILE_T)   :: this
    integer (kind=c_int) :: iCurrentLinenum

    iCurrentLinenum = this%iCurrentLinenum

  end function return_current_linenum_fn

  function is_current_line_a_comment_fn(this)      result(lIsComment)

    class (DATA_FILE_T)     :: this
    logical (kind=c_bool)   :: lIsComment

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iLen
    integer (kind=c_int) :: iLeadingSpace
    character (len=32)   :: sBufTemp

    iLen = len_trim( this%sBuf ) 

    if ( iLen < 32 ) then
      sBufTemp = this%sBuf(1:iLen)
    else
      sBufTemp = this%sBuf(1:32)  
    endif  
    
    iIndex = scan( adjustl(sBufTemp) , this%sCommentChars )
    
    lIsComment = lFALSE

    if (iIndex == 1 ) lIsComment = lTRUE
        
  end function is_current_line_a_comment_fn  




  subroutine open_file_char_sub(this, sFilename, sCommentChars, sDelimiters)

    class (DATA_FILE_T), intent(inout) :: this
    character (len=*), intent(in) :: sFilename
    character (len=*), intent(in), optional :: sCommentChars
    character (len=*), intent(in), optional :: sDelimiters

    if (present(sCommentChars)) then
      this%sCommentChars = sCommentChars
    else
      this%sCommentChars = sCOMMENT_CHARS
    endif

    if (present(sDelimiters)) then
      this%sDelimiters = sDelimiters
    else
      this%sDelimiters = sWHITESPACE
    endif

    if (.not. this%isOpen() ) then

      this%lIsOpen = lFALSE

      open(newunit=this%iUnitNum, file=sFilename, iostat=this%iStat)
      call assert(this%iStat == 0, "Failed to open file.", __FILE__, __LINE__)

      if (this%iStat == 0) this%lIsOpen = lTRUE

      call this%countLines()

      open(newunit=this%iUnitNum, file=sFilename, iostat=this%iStat)
      call assert(this%iStat == 0, "Failed to open file.", __FILE__, __LINE__)

      if (this%iStat == 0) this%lIsOpen = lTRUE

      write(*, fmt="(/,10x, a)") "Opened file "//dquote(sFilename)
      write(*, fmt="(a50, i8)") "Number of lines in file: ", this%numLines()
      write(*, fmt="(a50, i8)") "Number of lines excluding blanks and comments: ", this%numRecords()

    else

      print *, "Failed to open file "//dquote(sFilename)

    endif

  end subroutine open_file_char_sub


  subroutine close_file_sub(this)

    class (DATA_FILE_T) :: this

    close(unit=this%iUnitNum, iostat=this%iStat)
    this%lIsOpen = lFALSE

  end subroutine close_file_sub



  function does_file_exist_fn(this, sFilename) result(lExists)

    class (DATA_FILE_T)              :: this
    character (len=*), intent(in)    :: sFilename
    logical(kind=c_bool)             :: lExists

    inquire(file=sFilename, exist=lExists)

  end function does_file_exist_fn



  function is_file_open_fn(this) result(lIsOpen)

    class (DATA_FILE_T) :: this
    logical(kind=c_bool) :: lIsOpen

    lIsOpen = this%lIsOpen

  end function is_file_open_fn


  subroutine count_number_of_lines_sub(this)

    class (DATA_FILE_T), intent(inout) :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iNumLines
    integer (kind=c_int) :: iNumRecords
    integer (kind=c_int) :: iIndex    

    iNumLines = 0
    iNumRecords = 0
    iStat = 0

    if ( this%isOpen() ) then

      do

        read (unit = this%iUnitNum, fmt="(a)", iostat = iStat)  this%sBuf

        print *, this%iUnitNum, iNumLines, iStat
        print *, dquote( trim(this%sBuf) ) 

        iNumLines = iNumLines + 1

        if ( .not. this%isComment() )   iNumRecords = iNumRecords + 1

        if (iStat == IOSTAT_END) exit

      enddo

      call this%close()

      this%iNumberOfLines= iNumLines
      this%iNumberOfRecords = iNumRecords

    endif

  end subroutine count_number_of_lines_sub



  function read_header_fn(this) result (stList)

    class (DATA_FILE_T), intent(inout) :: this
    type (STRING_LIST_T) :: stList

    ! [ LOCALS ]
    character (len=MAX_STR_LEN)           :: sString 
    character (len=MAX_STR_LEN)           :: sSubString
    character (len=:), allocatable        :: sSubStringClean
    logical (kind=c_bool)                 :: lIsComment
    
    lIsComment = lTRUE

    do while( lIsComment )

      this%sBuf = this%readline()
      this%iCurrentLinenum = this%iCurrentLinenum + 1
      lIsComment = this%isComment()

    enddo

    do while ( len_trim( this%sBuf ) > 0)

      call chomp( this%sBuf, sSubString, this%sDelimiters )

      call replace(sSubString, " ", "_")
      call replace(sSubString, ".", "_")
      sSubStringClean = trim( clean( sSubString, sDOUBLE_QUOTE ) )
      call stList%append( trim( adjustl( sSubStringClean ) ) )
    enddo

  end function read_header_fn




  function read_line_of_data_fn(this) result(sText)

    class (DATA_FILE_T), intent(inout) :: this
    character (len=:), allocatable     :: sText

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    if (this%isOpen() ) then

      read (unit = this%iUnitNum, fmt = "(a)", iostat = iStat) this%sBuf
      sText = trim(this%sBuf)

      this%iCurrentLinenum = this%iCurrentLinenum + 1

      if (iStat == IOSTAT_END) then
        call this%close()
      endif

    endif

  end function read_line_of_data_fn



end module data_file
