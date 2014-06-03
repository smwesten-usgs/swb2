module vector_of_cells

  use iso_c_binding
  use cell_class
  use data_catalog_entry
  implicit none

  type (CELL_T), allocatable, public :: CELLS(:)

contains

  subroutine cells_initialize()

    allocate( CELLS( pGrd%iNumberOfCells))


  end subroutine cells_initialize





end module vector_of_cells