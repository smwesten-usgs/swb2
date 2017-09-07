module file_utilities

  use iso_c_binding
  use constants_and_conversions
  use strings
  implicit none

  interface
    integer function c_mkdir(dirname,mode) bind(c,name="mkdir")
      use iso_c_binding
      character(kind=c_char),intent(in)  :: dirname(*)
      integer(c_int),value  ,intent(in)  :: mode
    end function
  end interface

  interface
    integer function c_chdir(dirname) bind(C,name="chdir")
      use iso_c_binding
      character(kind=c_char) :: dirname(*)
    end function
  end interface

  interface
    integer function c_rmdir(dirname) bind(c,name="rmdir")
      use iso_c_binding
      character(kind=c_char), intent(in)  :: dirname(*)
    end function
  end interface

  interface
    integer function c_remove(filename) bind(c,name="remove")
      use iso_c_binding
      character(kind=c_char), intent(in)  :: filename(*)
    end function
  end interface

  interface
    function c_getcwd(dirname, size) bind(c,name="getcwd") result(r)
      use iso_c_binding
      character(kind=c_char) ,intent(out) :: dirname(*)
      integer(c_size_t),value,intent(in)  :: size
      type(c_ptr)  :: r
    end function
  end interface

  interface
    integer function c_unlink(filename) bind(c,name="unlink")
      use iso_c_binding
      character(kind=c_char), intent(in)  :: filename(*)
    end function
  end interface

  interface
    integer function c_get_libc_errno() bind(c,name="get_errno")
      use iso_c_binding
    end function
  end interface

  interface
    function c_get_libc_err_string( error_num ) bind(c,name="strerror")  result(error_str_ptr)
      use iso_c_binding
      integer,value,intent(in)                 :: error_num
      type(c_ptr)                              :: error_str_ptr
    end function
  end interface

  interface
    integer function c_execv( filename, argv )  bind(c,name="execv")
      use iso_c_binding
      character(kind=c_char), intent(in)  :: filename(*)
      type(c_ptr)                         :: argv
    end function
  end interface

contains

  subroutine execv( filename, argv, err )

    character(*), intent(in)       :: filename
    character(len=*), intent(in)  :: argv(:)
    integer, optional, intent(out) :: err

    ! [ LOCALS ]
    type(c_ptr)                   :: argv_cptr( ubound( argv, 1) + 2 )
    character(len=64), target     :: argv_copy( ubound( argv, 1) + 2 )
    integer (kind=c_int)          :: indx
    integer (kind=c_int)          :: errno
    character (len=256)           :: full_filename

    argv_copy = ""

    do indx=1, ubound(argv,1)
      argv_copy( indx + 1 )      = trim( argv( indx ) )//c_null_char
      argv_cptr( indx + 1 )      = C_LOC( argv_copy( indx + 1 ) )
    enddo

    ! now tack filename onto first element of array, and null pointer to last element
    argv_copy(1) = trim( filename )//c_null_char
    argv_cptr( 1 ) = C_LOC( argv_copy( 1 ) )
    argv_cptr( size(argv_cptr,1) ) = c_null_ptr

    full_filename = "d:/DOS/"//trim( filename )//c_null_char

    errno = c_execv( trim( full_filename ), argv_cptr )

  end subroutine execv

!--------------------------------------------------------------------------------------------------

  subroutine get_libc_err_string( libc_err_string, libc_err_code )

    use iso_c_binding
    implicit none

    character (len=:), allocatable, intent(out) :: libc_err_string
    integer, intent(in)                         :: libc_err_code

    type(c_ptr)                                         :: c_error_str_pointer
    character(kind=c_char, len=256) , pointer           :: f_error_str_pointer
    integer                                             :: loc_err
    integer ( kind=c_int)                               :: index
    character (len=256)                                 :: libc_err_string_temp

    c_error_str_pointer = c_get_libc_err_string( libc_err_code )

    call c_f_pointer( c_error_str_pointer, f_error_str_pointer )

    if (.not. associated( f_error_str_pointer ) ) &
      stop ("Routine returned a null pointer")

    libc_err_string_temp = ""

    do index=1, len( f_error_str_pointer )
      if ( f_error_str_pointer(index:index) .eq. c_null_char ) exit
      libc_err_string_temp(index:index) = f_error_str_pointer(index:index)
    enddo

    libc_err_string = libc_err_string_temp( 1:len_trim( libc_err_string_temp ) )

  end subroutine get_libc_err_string

!--------------------------------------------------------------------------------------------------

  subroutine mkdir(dirname, err)

    character(*) :: dirname
    integer, optional, intent(out) :: err

    ! [ LOCALS ]
    integer :: loc_err
    character(len=:), allocatable :: err_string

    loc_err =  c_mkdir(dirname//c_null_char, int( o'755' ) )

    if (present(err)) then
      err = loc_err
    else
      if ( loc_err /= 0 ) then
        call get_libc_err_string( err_string, c_get_libc_errno() )
        write(*, fmt="(a, i0)") "call to 'mkdir' failed. err=", loc_err
        write(*, fmt="(a,i0)") "libc errno=",get_libc_errno()
        write(*, fmt="(a)") "libc error msg: "//trim(err_string)
      endif
    endif

  end subroutine

!--------------------------------------------------------------------------------------------------

  subroutine chdir(dirname, err)

    character(*) :: dirname
    integer, optional, intent(out) :: err

    ! [ LOCALS ]
    integer :: loc_err

    loc_err =  c_chdir(dirname//c_null_char)

    if (present(err)) then
      err = loc_err
    else
      if ( loc_err /= 0 )  then
        write(*, fmt="(a, i0)") "call to 'chdir' failed. err=", loc_err
        write(*, fmt="(a)") "dirname: '"//trim(dirname)//"'"
        write(*, fmt="(a,i0)") "libc errno=",get_libc_errno()
      endif
    endif

  end subroutine

!--------------------------------------------------------------------------------------------------

  subroutine rmdir(dirname, err)

    character(*) :: dirname
    integer, optional, intent(out) :: err

    ! [ LOCALS ]
    integer :: loc_err

    loc_err =  c_rmdir(dirname//c_null_char)

    if (present(err)) then
      err = loc_err
    else
      if ( loc_err /= 0 )  then
        write(*, fmt="(a, i0)") "call to 'rmdir' failed. err=", loc_err
        write(*, fmt="(a)") "dirname: '"//trim(dirname)//"'"
        write(*, fmt="(a,i0)") "libc errno=",get_libc_errno()
      endif
    endif

  end subroutine

!--------------------------------------------------------------------------------------------------

  subroutine remove(filename, err)

    character(*) :: filename
    integer, optional, intent(out) :: err

    ! [ LOCALS ]
    integer :: loc_err

    loc_err =  c_remove(filename//c_null_char)

    if (present(err)) then
      err = loc_err
    else
      if ( loc_err /= 0 )  then
        write(*, fmt="(a, i0)") "call to 'remove' failed. err=", loc_err
        write(*, fmt="(a)") "filename: '"//trim(filename)//"'"
        write(*, fmt="(a,i0)") "libc errno=",get_libc_errno()
      endif
    endif

  end subroutine

!--------------------------------------------------------------------------------------------------

  subroutine get_cwd( dirname )

    character(kind=c_char,len=:), allocatable, intent(out) :: dirname

    ! [ LOCALS ]
    integer(c_long) :: i
    type(c_ptr) :: buffer
    character(kind=c_char, len=256) :: tempchar

    buffer=c_getcwd(tempchar, 256_c_size_t)

    dirname = c_to_fortran_string( tempchar )


  end subroutine get_cwd

!--------------------------------------------------------------------------------------------------

  function getcwd( )  result( dirname )

    ! [ LOCALS ]
    integer(c_long) :: i
    type(c_ptr) :: buffer
    character(kind=c_char, len=256) :: tempchar
    character(kind=c_char,len=:), allocatable :: dirname

    buffer=c_getcwd(tempchar, 256_c_size_t)

    dirname = c_to_fortran_string( tempchar )


  end function getcwd

  subroutine unlink(filename, err)

    character(*) :: filename
    integer, optional, intent(out) :: err

    ! [ LOCALS ]
    integer :: loc_err

    loc_err =  c_unlink(filename//c_null_char)

    if (present(err)) then
      err = loc_err
    else
      if ( loc_err /= 0 )  then
        write(*, fmt="(a, i0)") "call to 'unlink' failed. err=", loc_err
        write(*, fmt="(a)") "filename: '"//trim(filename)//"'"
        write(*, fmt="(a,i0)") "libc errno=",get_libc_errno()
      endif
    endif

  end subroutine

!--------------------------------------------------------------------------------------------------

  function get_libc_errno()  result( libc_errno )

    integer libc_errno

    libc_errno = c_get_libc_errno()

  end function get_libc_errno

end module file_utilities
