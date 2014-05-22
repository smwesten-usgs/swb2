program test_control_file

  use iso_c_binding, only : c_int, c_float, c_bool
  use control_file

  call read_control_file("recharge_dual_factor_nonstandard.ctl")

  call CF_DICT%print_all()


end program test_control_file