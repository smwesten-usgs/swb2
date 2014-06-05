program test_cell_type

  use iso_c_binding
  use cell_collection
  implicit none

  call CELLS%initialize(4, 4, reshape( source=[1,1,0,1, 0,1,0,0, 0,1,1,1, 1,1,0,0], shape=[4,4] ) )



end program test_cell_type