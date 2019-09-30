program test_hwb_rng

  use iso_c_binding
  implicit none

  integer (c_int), parameter :: initial_seed = 1

  integer (c_int)  :: num_zones
  integer (c_int)  :: num_months
  integer (c_int)  :: num_years
  integer (c_int)  :: current_seed_value
  real (c_float)   :: rand_num

  current_seed_value = initial_seed - 1

  open(unit=8, file='hwb_random_values.txt')

  do num_months = 1, 12
    do num_zones = 1, 120
      do num_years = 1, 30

        current_seed_value = current_seed_value + 1
        rand_num = ran(current_seed_value)
        write(8,fmt="(f14.8)") rand_num

      enddo
    enddo
  enddo

end program test_hwb_rng
