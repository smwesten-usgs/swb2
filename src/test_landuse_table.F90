program test_landuse_table

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use constants_and_conversions, only : sTAB
  use data_frame
  use lookup_table_landuse
  implicit none

  type (LOOKUP_TABLE_LANDUSE_T) :: LU

  call LU%readfile(sFilename = "LU_lookup_NLCD.txt", sCommentChars = "#", sDelimiters= sTAB)
  call LU%map()

  call LU%data%

  
end program test_landuse_table