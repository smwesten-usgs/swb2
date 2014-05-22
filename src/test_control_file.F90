program test_control_file

  use iso_c_binding, only : c_int, c_float, c_bool
  use control_file
  use string_list

  type (STRING_LIST_T) :: myList, mySubset

  call read_control_file("recharge_dual_factor_nonstandard.ctl")

  call CF_DICT%print_all()

  call CF_DICT%get_values("SOIL_GROUP_PROJECTION_DEFINITION", myList )

  print *, ""
  print *, "ENTRIES FOR SOIL_GROUP_PROJECTION_DEFINITION"
  print *, ""

  call myList%print()

  print *, ""
  print *, myList%get(1, myList%count)

  mySubset = CF_DICT%grep_keys("PRECIP")
  print *, ""
  call mySubset%print()

end program test_control_file