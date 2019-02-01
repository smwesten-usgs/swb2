program test_params

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
!  use constants_and_conversions, only :WHITESPACE
  use exceptions
!  use strings
  use string_list
!  use dictionary
  use parameters
  implicit none

  type (PARAMETERS_T)             :: PARAMS
  real (c_float), allocatable     :: fValues(:)
  type (STRING_LIST_T)                 :: slString


  call PARAMS%add_file("LU_lookup_NLCD.txt")
  !call PARAMS%add_file("rain_adj_factors_maui.prn", sDelimiters = "WHITESPACE")
  call PARAMS%add_file("IRRIGATION_lookup_TWO_FACTOR.txt")
  call PARAMS%munge_file()


  !call PARAM_DICT%get_values(sKey="raincoef_3", fValues=fValues)

  !print *, "count: ", ubound(fValues, 1)
  !print *, "min  : ", minval(fValues)
  !print *, "max  : ", maxval(fValues)
  !print *, "mean : ", sum(fValues)/ ubound(fValues, 1)

  slString = PARAM_DICT%grep_keys("REW")
  call slString%print()

  call slString%deallocate()

  slString = PARAM_DICT%grep_keys("L_")  
  call slString%print()

  call check_warnings()


end program test_params