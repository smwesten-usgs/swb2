program test_kiss

  use kiss_random_number_generator, only   : kiss64_rng,               &
                                             kiss64_initialize,        &
                                             kiss64_uniform_rng

  use iso_fortran_env, only                : real128 
  implicit none

  integer,parameter :: i8b=selected_int_kind(18)
  integer,parameter :: r8b=selected_real_kind(18,18)
  
  integer(i8b)  :: x
  integer(i8b)  :: minx, maxx
  integer(i8b)  :: i

  call kiss64_initialize()

  minx=99999999
  maxx=-99999999
  do i=1,100000000_i8b
    x=kiss64_rng()
    minx=min(minx, x)
    maxx=max(maxx,x)
  end do

  print *, real(minx, kind=real128), real(maxx, kind=real128), real(maxx, kind=real128) - real(minx, kind=real128)  


  if (x == 1666297717051644203_i8b) then
    print *, "After 100 million calls, KISS random number generator yields the expected value (1666297717051644203). Pass."
  else 
    stop "After 100 million calls, KISS random number generator *does not* give the expected value (1666297717051644203). Fail."
  endif   

  open(unit=8,file="kiss64_uniform.txt")

  do i=1,100000
    write(8,fmt="(F16.14)") kiss64_uniform_rng()
  enddo

end program test_kiss