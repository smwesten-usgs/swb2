module model_initialize

  use iso_c_binding, only : c_int, c_float, c_bool
  use control_file
  use string_list
  implicit none


  private

  public :: init_read_control_file

contains

  subroutine init_read_control_file(sFilename)

    call read_control_file( sFilename )

  end subroutine init_read_control_file



end module model_initialize