program test_cell_type

  use iso_c_binding
  use cell_collection
  implicit none

  call CELLS%initialize(2, 2, reshape( source=[1,1,0,1], shape=[2,2] ) )



end program test_cell_type