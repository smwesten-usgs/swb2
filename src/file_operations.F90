module file_operations

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use iso_fortran_env, only : IOSTAT_END
  use logfiles
  use exceptions
  use constants_and_conversions
  use strings
  use string_list
  implicit none

  private
  
  integer (kind=c_int), parameter :: MAX_STR_LEN = 8192
  
  type, public :: ASCII_FILE_T

    character (len=:), allocatable  :: sFilename
    character (len=:), allocatable  :: sDelimiters
    character (len=:), allocatable  :: sCommentChars
    type (STRING_LIST_T)            :: slColNames
    integer (kind=c_int)            :: iCurrentLinenum = 0
    integer (kind=c_int)            :: iNumberOfLines = 0
    integer (kind=c_int)            :: iNumberOfRecords = 0
    integer (kind=c_int)            :: iNumberOfHeaderLines = 1
    logical (kind=c_bool)           :: lIsOpen = lFALSE
    logical (kind=c_bool)           :: lEOF = lFALSE
    integer (kind=c_int)            :: iUnitNum
    integer (kind=c_int)            :: iStat
    character (len=MAX_STR_LEN)     :: sBuf
    character (len=:), allocatable  :: stMissingValue

  contains

    procedure, private :: open_file_read_access_sub
    procedure, private :: open_file_write_access_sub
    generic :: open => open_file_read_access_sub, &
                       open_file_write_access_sub

    procedure, private :: close_file_sub
    generic :: close => close_file_sub

    procedure, private :: is_file_open_fn
    generic :: isOpen => is_file_open_fn

    procedure, private :: have_we_reached_the_EOF_fn
    generic :: isEOF => have_we_reached_the_EOF_fn

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

    procedure, private :: read_header_fn
    generic, public    :: readHeader => read_header_fn

    procedure, private :: read_line_of_data_fn
    generic, public    :: readLine => read_line_of_data_fn

  end type ASCII_FILE_T

contains

  function return_num_lines_fn(this)    result(iNumLines)

    class (ASCII_FILE_T)   :: this
    integer (kind=c_int) :: iNumLines

    iNumLines = this%iNumberOfLines

  end function return_num_lines_fn

!--------------------------------------------------------------------------------------------------

  function return_num_records_fn(this)                 result(iNumRecords)

    class (ASCII_FILE_T)   :: this
    integer (kind=c_int) :: iNumRecords

    iNumRecords = this%iNumberOfRecords

  end function return_num_records_fn

!--------------------------------------------------------------------------------------------------

  function return_current_linenum_fn(this)          result(iCurrentLinenum)

    class (ASCII_FILE_T)   :: this
    integer (kind=c_int) :: iCurrentLinenum

    iCurrentLinenum = this%iCurrentLinenum

  end function return_current_linenum_fn

!--------------------------------------------------------------------------------------------------

  function have_we_reached_the_EOF_fn(this)           result(lIsEOF)

    class (ASCII_FILE_T)     :: this
    logical (kind=c_bool)   :: lIsEOF

    lIsEOF = this%lEOF

  end function have_we_reached_the_EOF_fn  

!--------------------------------------------------------------------------------------------------

  function is_current_line_a_comment_fn(this)         result(lIsComment)

    class (ASCII_FILE_T)     :: this
    logical (kind=c_bool)   :: lIsComment

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iLen
    character (len=1)   :: sBufTemp

    iLen = len_trim( this%sBuf ) 

    sBufTemp = adjustl(this%sBuf)

    iIndex = verify( sBufTemp , this%sCommentChars )
    
    lIsComment = lFALSE

    if ( iIndex == 0 .or. len_trim(this%sBuf) == 0 ) lIsComment = lTRUE
        
  end function is_current_line_a_comment_fn  

!--------------------------------------------------------------------------------------------------

  subroutine open_file_read_access_sub(this, sFilename, sCommentChars, sDelimiters, lHasHeader )

    class (ASCII_FILE_T), intent(inout)         :: this
    character (len=*), intent(in)               :: sFilename
    character (len=*), intent(in)               :: sCommentChars
    character (len=*), intent(in)               :: sDelimiters
    logical (kind=c_bool), intent(in), optional :: lHasHeader
    
    this%sCommentChars = sCommentChars
    this%sDelimiters = sDelimiters

    if (present( lHasHeader ) ) then
      if (.not. lHasHeader ) this%iNumberOfHeaderLines = 0
    endif

    if (.not. this%isOpen() ) then

      open(newunit=this%iUnitNum, file=sFilename, iostat=this%iStat, action='READ')
      call assert(this%iStat == 0, "Failed to open file "//dquote(sFilename)//".", __FILE__, __LINE__)

      if (this%iStat == 0) this%lIsOpen = lTRUE
      this%lEOF = lFALSE

      call this%countLines()

      call LOGS%write( sMessage="Opened file "//dquote(sFilename), iTab=20, iLinesBefore=1, iLogLevel=LOG_ALL )
      call LOGS%write( "Comment characters: "//dquote(sCommentChars), iTab=40 )
      call LOGS%write( "Number of lines in file: "//asCharacter( this%numLines() ), iTab=35 )
      call LOGS%write( "Number of lines excluding blanks, headers and comments: " &
           //asCharacter( this%numRecords() ), iTab=4, iLinesAfter=1 )

    else

      call die( "Failed to open file "//dquote(sFilename)//" with read access." )

    endif

  end subroutine open_file_read_access_sub

!--------------------------------------------------------------------------------------------------

  subroutine open_file_write_access_sub(this, sFilename)

    class (ASCII_FILE_T), intent(inout) :: this
    character (len=*), intent(in) :: sFilename

    if (.not. this%isOpen() ) then

      open(newunit=this%iUnitNum, file=sFilename, iostat=this%iStat, action='WRITE')
      call assert(this%iStat == 0, "Failed to open file "//dquote(sFilename)//".", __FILE__, __LINE__)

      if (this%iStat == 0) this%lIsOpen = lTRUE

      this%lEOF = lFALSE

      call LOGS%write( "Opened file "//dquote(sFilename) )
   
    else

      call LOGS%write( "Failed to open file "//dquote(sFilename)//" with WRITE access" )

    endif

  end subroutine open_file_write_access_sub

!--------------------------------------------------------------------------------------------------

  subroutine close_file_sub(this)

    class (ASCII_FILE_T) :: this

    close(unit=this%iUnitNum, iostat=this%iStat)
    this%lIsOpen = lFALSE

  end subroutine close_file_sub

!--------------------------------------------------------------------------------------------------

  function does_file_exist_fn(this, sFilename) result(lExists)

    class (ASCII_FILE_T)              :: this
    character (len=*), intent(in)    :: sFilename
    logical(kind=c_bool)             :: lExists

    inquire(file=sFilename, exist=lExists)

  end function does_file_exist_fn

!--------------------------------------------------------------------------------------------------

  function is_file_open_fn(this) result(lIsOpen)

    class (ASCII_FILE_T) :: this
    logical(kind=c_bool) :: lIsOpen

    lIsOpen = this%lIsOpen

  end function is_file_open_fn

!--------------------------------------------------------------------------------------------------

  subroutine count_number_of_lines_sub(this)

    class (ASCII_FILE_T), intent(inout) :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iNumLines
    integer (kind=c_int) :: iNumRecords
    integer (kind=c_int) :: iIndex    

    iNumLines = 0
    iNumRecords = 0
    iStat = 0

    if ( this%isOpen() ) then

      rewind( unit = this%iUnitNum )

      do

        read (unit = this%iUnitNum, fmt="(a)", iostat = iStat)  this%sBuf

        if (iStat == IOSTAT_END) exit

        iNumLines = iNumLines + 1

        if ( .not. this%isComment() )   iNumRecords = iNumRecords + 1

      enddo

      rewind( unit = this%iUnitNum )

      this%iNumberOfLines= iNumLines
      this%iNumberOfRecords = iNumRecords - this%iNumberOfHeaderLines

    endif

  end subroutine count_number_of_lines_sub

!--------------------------------------------------------------------------------------------------

  function read_header_fn(this) result (stList)

    class (ASCII_FILE_T), intent(inout) :: this
    type (STRING_LIST_T) :: stList

    ! [ LOCALS ]
    character (len=MAX_STR_LEN)           :: sString 
    character (len=MAX_STR_LEN)           :: sSubString
    character (len=:), allocatable        :: sSubStringClean


    this%sBuf = this%readline()

    do while ( len_trim( this%sBuf ) > 0)

      call chomp( this%sBuf, sSubString, this%sDelimiters )

      call replace(sSubString, " ", "_")
      call replace(sSubString, ".", "_")
      sSubStringClean = trim( clean( sSubString, sDOUBLE_QUOTE ) )
      call stList%append( trim( adjustl( sSubStringClean ) ) )
    enddo

  end function read_header_fn

!--------------------------------------------------------------------------------------------------

  function read_line_of_data_fn(this) result(sText)

    class (ASCII_FILE_T), intent(inout) :: this
    character (len=:), allocatable     :: sText

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    logical (kind=c_bool)                 :: lIsComment
    
    lIsComment = lTRUE

    do while ( lIsComment .and. this%isOpen() )

      if (this%isOpen() ) then

        read (unit = this%iUnitNum, fmt = "(a)", iostat = iStat) this%sBuf

        if (iStat == IOSTAT_END) then
          this%lEOF = lTRUE
          sText = ""
          call this%close()
        else
          sText = trim(this%sBuf)
          this%iCurrentLinenum = this%iCurrentLinenum + 1
        endif

        lIsComment = this%isComment()

      endif

    enddo

  end function read_line_of_data_fn

end module file_operations
