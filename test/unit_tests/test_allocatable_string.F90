module test_allocatable_string

  use fruit
  use fstring_list
  use fstring, only        : chomp, replace, clean
  use iso_c_binding, only  : c_int
  implicit none

contains

  ! subroutine test_datetime_basic_dateparse
  ! ! datetime: parse with default mm/dd/yyyy date format
  !   type (DATETIME_T) :: dt
  !
  !   call dt%parseDate("03/15/2011")
  !   call assert_equals (3, int(dt%iMonth))
  !   call assert_equals (15, int(dt%iDay))
  !   call assert_equals (2011, int(dt%iYear))
  !
  ! end subroutine test_datetime_basic_dateparse

!-------------------------------------------------------------------------------

  subroutine test_string_list_with_character

    ! [ LOCALS ]
    integer (kind=c_int), parameter       :: MAX_STR_LEN = 8096
    character (len=MAX_STR_LEN)           :: sString
    character (len=MAX_STR_LEN)           :: sSubString
    character (len=MAX_STR_LEN)           :: sSubStringClean
    character (len=MAX_STR_LEN)           :: sBuf
    integer (kind=c_int)                  :: column_count
    type (FSTRING_LIST_T)                  :: stList

    sBuf = '"Begin at the beginning," the King said, very gravely, '           &
      //'"and go on till you come to the end: then stop."'

    column_count = 0

    do while ( len_trim( sBuf ) > 0)

      column_count = column_count + 1
      call chomp( sBuf, sSubString, "," )

      call replace(sSubString, " ", "_")
      call replace(sSubString, ".", "_")
      sSubStringClean = trim( clean( sSubString, '"' ) )

      call stList%append( trim( adjustl( sSubStringClean ) ) )

    enddo

    call assert_equals (4, column_count)

  end subroutine test_string_list_with_character

end module test_allocatable_string
