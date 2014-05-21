program test_params

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
!  use constants_and_conversions, only :sWHITESPACE
  use exceptions
!  use strings
  use string_list
!  use dictionary
  use parameters
  implicit none

  type (PARAMETER_FILES_T)             :: PARAM_FILES
  real (kind=c_float), allocatable     :: fValues(:)
  type (STRING_LIST_T)                 :: slString


  call PARAM_FILES%add("LU_lookup_NLCD.txt")
  !call PARAM_FILES%add("rain_adj_factors_maui.prn", sDelimiters = "WHITESPACE")
  call PARAM_FILES%add("IRRIGATION_lookup_TWO_FACTOR.txt")
  call PARAM_FILES%munge()


  !call PARAMS%get_values(sKey="raincoef_3", fValues=fValues)

  !print *, "count: ", ubound(fValues, 1)
  !print *, "min  : ", minval(fValues)
  !print *, "max  : ", maxval(fValues)
  !print *, "mean : ", sum(fValues)/ ubound(fValues, 1)

  slString = PARAMS%grep_keys("REW")
  call slString%print()

  call slString%deallocate()

  slString = PARAMS%grep_keys("L_")  
  call slString%print()

  call check_warnings()


end program test_params