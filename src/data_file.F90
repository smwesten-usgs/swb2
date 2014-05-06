module data_file

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use iso_fortran_env, only : IOSTAT_END
  use types_new
  use constants_and_conversions
  use strings
  use string_list
  implicit none

  private

  character (len=8192) :: sBuf

  type, public :: T_DATA_FILE

    character (len=:), allocatable  :: sFilename
    character (len=:), allocatable  :: sDelimiters
    character (len=:), allocatable  :: sCommentChars
    integer (kind=c_int)            :: iCurrentLinenum = 0
    integer (kind=c_int)            :: iNumberOfLines = 0
    integer (kind=c_int)            :: iNumberOfRecords = 0
    logical (kind=c_bool)           :: lIsOpen = lFALSE
    integer (kind=c_int)            :: iUnitNum
    integer (kind=c_int)            :: iStat
    character (len=:), allocatable  :: stMissingValue

  contains

    !procedure, private :: open_file_char_sub
    procedure, public :: open => open_file_char_sub

    !procedure, private :: close_file_sub
    procedure, public :: close => close_file_sub

    procedure, private :: is_file_open_fn
    generic, public :: isOpen => is_file_open_fn

    procedure, private :: does_file_exist_fn
    generic, public    :: exists => does_file_exist_fn


    procedure, private :: count_number_of_lines_sub
    generic, public    :: countLines => count_number_of_lines_sub

    procedure, private :: return_num_lines_fn
    generic, public    :: numLines => return_num_lines_fn

    procedure, private :: return_num_records_fn
    generic, public    :: numRecords => return_num_records_fn

    procedure, private :: return_current_linenum_fn
    generic, public    :: currentLineNum => return_current_linenum_fn

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

  end type T_DATA_FILE


contains

  function return_num_lines_fn(this)    result(iNumLines)

    class (T_DATA_FILE)   :: this
    integer (kind=c_int) :: iNumLines

    iNumLines = this%iNumberOfLines

  end function return_num_lines_fn


  function return_num_records_fn(this)    result(iNumRecords)

    class (T_DATA_FILE)   :: this
    integer (kind=c_int) :: iNumRecords

    iNumRecords = this%iNumberOfRecords

  end function return_num_records_fn


  function return_current_linenum_fn(this)    result(iCurrentLinenum)

    class (T_DATA_FILE)   :: this
    integer (kind=c_int) :: iCurrentLinenum

    iCurrentLinenum = this%iCurrentLinenum

  end function return_current_linenum_fn



  subroutine open_file_char_sub(this, sFilename, sCommentChars, sDelimiters)

    class (T_DATA_FILE), intent(inout) :: this
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

      write(*, fmt="(a, i8, a)") "Opened file. ", this%numlines(), " records present."

    else

      print *, "Failed to open file "//dquote(sFilename)

    endif

  end subroutine open_file_char_sub


  subroutine close_file_sub(this)

    class (T_DATA_FILE) :: this

    close(unit=this%iUnitNum, iostat=this%iStat)
    this%lIsOpen = lFALSE

  end subroutine close_file_sub



  function does_file_exist_fn(this, sFilename) result(lExists)

    class (T_DATA_FILE)              :: this
    character (len=*), intent(in)    :: sFilename
    logical(kind=c_bool)             :: lExists

    inquire(file=sFilename, exist=lExists)

  end function does_file_exist_fn



  function is_file_open_fn(this) result(lIsOpen)

    class (T_DATA_FILE) :: this
    logical(kind=c_bool) :: lIsOpen

    lIsOpen = this%lIsOpen

  end function is_file_open_fn


  subroutine count_number_of_lines_sub(this)

    class (T_DATA_FILE), intent(inout) :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iNumLines
    integer (kind=c_int) :: iNumRecords
    integer (kind=c_int) :: iIndex
    character (len=16)   :: sBuf

    iNumLines = 0
    iNumRecords = 0
    iStat = 0

    if ( this%isOpen() ) then

      do

        read (unit = this%iUnitNum, fmt=*, iostat = iStat)  sBuf
        iNumLines = iNumLines + 1

        iIndex = scan(string=adjustl(sBuf), &
                      set=this%sCommentChars )

        ! if there are valid characters and first character
        ! is not in the list of comment characters, count as a
        ! valid record
        if (len_trim(sBuf) /= 0 .and. iIndex /= 1) &
          iNumRecords = iNumRecords + 1

        if (iStat == IOSTAT_END) exit

      enddo

      call this%close()

      this%iNumberOfLines= iNumLines
      this%iNumberOfRecords = iNumRecords

    endif

  end subroutine count_number_of_lines_sub



  function read_header_fn(this) result (stList)

    class (T_DATA_FILE), intent(inout) :: this
    type (STRING_LIST_T) :: stList

    ! [ LOCALS ]
    character (len=:), allocatable :: sString 
    character (len=:), allocatable :: sSubString
    sString = this%readline()

    this%iCurrentLinenum = this%iCurrentLinenum + 1

    do while ( len_trim(sString) > 0)

      call chomp( sString, sSubString, ",")
      call replace(sSubString, " ", "_")
      call replace(sSubString, ".", "_")
      call stList%append(sSubString)

    enddo

  end function read_header_fn




  function read_line_of_data_fn(this) result(sText)

    class (T_DATA_FILE), intent(inout) :: this
    character (len=:), allocatable     :: sText

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    if (this%isOpen() ) then

      read (unit = this%iUnitNum, fmt = "(a)", iostat = iStat) sBuf
      sText = trim(sBuf)

      this%iCurrentLinenum = this%iCurrentLinenum + 1

      if (iStat == IOSTAT_END) then
        call this%close()
      endif

    endif

  end function read_line_of_data_fn



end module data_file
