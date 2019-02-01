program test_kiss

  use kiss_random_number_generator, only   : kiss64_rng, initialize_kiss_rng, &
                                             kiss64_uniform_rng
  implicit none

  integer,parameter :: I8=selected_int_kind(18)
  integer,parameter :: R8=selected_real_kind(18,18)
  integer(I8)  :: Q(20632)
  integer(I8)  :: I8_max = huge(Q(1))
  integer(I8)  :: I8_min = -(huge(Q(1)) + 1)
  integer(I8)  :: I8_range = 2*huge(Q(1)) + 1
  integer(I8)  :: x
  integer(I8)  :: i

  call initialize_kiss_rng()

  do i=1,1000000000_I8
    x=kiss64_rng()
  end do

  write(*,10) x
  10 format(' Does x = 4013566000157423768 ?',/,6x,'x = ',I20)

  open(unit=8,file="kiss64_uniform.txt")

  do i=1,100000
    write(8,fmt="(F16.14)") kiss64_uniform_rng()
!    print *, kiss64_rng()
  enddo

end program test_kiss
