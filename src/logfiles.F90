module logfiles

  use iso_c_binding, only : c_bool, c_int
  implicit none

  private

  public :: LOGFILE_T

  type LOGFILE_T
    character (len=:), allocatable  :: sFilePrefix
    character (len=:), allocatable  :: sFilename
    integer (kind=c_int)            :: iCurrentLinenum = 0
    logical (kind=c_bool)           :: lIsOpen = lFALSE
    integer (kind=c_int)            :: iUnitNum
    integer (kind=c_int)            :: iStat
 
  contains

    procedure, private :: open_file_write_access_sub
    generic :: open => open_file_write_access_sub

    procedure, private :: close_file_sub
    generic :: close => close_file_sub

  end type LOGFILE_T

  enum, bind(c)
    enumerator :: DT_YEAR = 1, DT_MONTH = 2, DT_DAY = 3, DT_DIFF_FM_UTC = 4, &
                  DT_HOUR = 5, DT_MINUTES = 6, DT_SECONDS = 7, DT_MILLISECONDS = 8
  end enum

contains

  subroutine open_file_write_access_sub(this, sFilename)

    class (LOGFILE_T), intent(inout) :: this
    character (len=*), intent(in) :: sFilename
    character (len=*), intent(in), optional :: sCommentChars
    character (len=*), intent(in), optional :: sDelimiters

    if (.not. this%isOpen() ) then

      open(newunit=this%iUnitNum, file=sFilename, iostat=this%iStat, access='WRITE')
      call assert(this%iStat == 0, "Failed to open file.", __FILE__, __LINE__)

      if (this%iStat == 0) this%lIsOpen = lTRUE

      write(*, fmt="(/,10x, a)") "Opened file "//dquote(sFilename)
   
    else

      print *, "Failed to open file "//dquote(sFilename)

    endif

  end subroutine open_file_write_access_sub

  subroutine make_filename_sub(this)

    class (LOGFILE_T)     :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iValues(8)
    character (len=2)    :: sHour
    character (len=2)    :: sMinutes
    character (len=2)    :: sSeconds
    
    character (len=2)    :: sDay
    character (len=2)    :: sMonth
    character (len=4)    :: sYear

    call DATE_AND_TIME( VALUES = iValues )

    write(sHour, fmt="(i0.2)") iValues( DT_HOUR )
    write(sMinutes, fmt="(i0.2)") iValues( DT_MINUTES )
    write(sSeconds, fmt="(i0.2)") iValues( DT_SECONDS )
    write(sMonth, fmt="(i0.2)") iValues( DT_MONTH )
    write(sDay, fmt="(i0.2)") iValues( DT_DAY )
    write(sYear, fmt="(i0.4)") iValues( DT_YEAR )

    this%sFilename = this%sFilePrefix//"_LOGFILE__"//sYear//"_"//sMonth//"_"//sDay//"__"  &
                                                   //sHour//"-"//sMinutes//"-"//sSeconds//".txt"


  end subroutine make_filename_sub


  subroutine write_to_logfile_sub(this, sMessage)

    class (LOGFILE_T)                  :: this
    character (len=*), intent(in)      :: sMessage

    if (this%lIsOpen) then

      write(unit=this$iUnitNum, fmt="(a)")  sMessage

    endif 

  end subroutine write_to_logfile_sub

end logfiles