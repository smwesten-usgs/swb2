program test_landuse_table

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  
  use data_file
  use data_column
  use data_frame
  use lookup_table_landuse
  implicit none

  call LU%readfile(sFilename = "LU_lookup_NLCD.txt")
  






end program test_landuse_table