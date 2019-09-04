module test_crop_coefficients__FAO56

  use fruit
  use constants_and_conversions
  use crop_coefficients__FAO56
  use logfiles, only               : LOGS, LOG_DEBUG
  use parameters, only             : PARAMETERS_T
  use simulation_datetime, only    : SIM_DT
  use strings
  use string_list, only            : STRING_LIST_T
  use version_control
  use iso_c_binding
  implicit none

contains

  subroutine setup_crop_coefficients__FAO56

    type (PARAMETERS_T)             :: PARAMS
    real (kind=c_float), allocatable     :: fValues(:)
    type (STRING_LIST_T)                 :: slString

    if (     (SYSTEM_NAME .containssimilar. "Windows")                           &
        .or. (SYSTEM_NAME .containssimilar. "Mingw") ) then
      OS_NATIVE_PATH_DELIMITER = "\"
    else
      OS_NATIVE_PATH_DELIMITER = "/"
    endif

    call LOGS%initialize( iLogLevel = LOG_DEBUG )

    call PARAMS%add_file("../test_data/tables/Lookup__crop_coefficient_test.txt")
    call PARAMS%munge_file()
    call SIM_DT%start%parseDate("01/01/2002")
    call crop_coefficients_FAO56_initialize( )

  end subroutine setup_crop_coefficients__FAO56

!-------------------------------------------------------------------------------

  subroutine test_crop_coefficients_parsing

    real (c_float)    :: planting_doy

    planting_doy = GROWTH_STAGE_DATE(PLANTING_DATE, 6)%getDayOfYear()
    call assert_equals( 110., planting_doy)

end subroutine test_crop_coefficients_parsing

!-------------------------------------------------------------------------------

  subroutine test_crop_coefficients_basic

    real (c_float)  :: Kcb

    ! test file sets up Corn (index=5) with missing values for
    ! L_ini, L_dev, etc., instead supplying values for GDD_ini, GDD_dev, etc.
    call assert_equals( KCB_METHOD(5), KCB_METHOD_GDD)

    ! special case: GDD=0, but Planting date (04/01)
    ! growing should not have started, and Kcb = 0.14 for barley
    call SIM_DT%curr%parseDate("03/30/2002")
    Kcb = update_crop_coefficient_GDD_as_threshold(6, 0.)
    call assert_equals( 0.14, Kcb )

    ! special case: GDD=0, but Planting date (04/20) is before the
    ! current date; growing should be started, and Kcb = 0.15 for barley
    call SIM_DT%curr%parseDate("04/20/2002")
    Kcb = update_crop_coefficient_GDD_as_threshold(6, 0.)
    call assert_equals( 0.15, Kcb )

    Kcb = update_crop_coefficient_GDD_as_threshold(5, 1000.)
    ! are we picking the right Kcb values off of the GDD-based curve?
    call assert_equals( 0.96, Kcb )

    Kcb = update_crop_coefficient_GDD_as_threshold(5, 2000.)
    ! are we picking the right Kcb values off of the GDD-based curve?
    call assert_equals( 0.6, Kcb )

    Kcb = update_crop_coefficient_GDD_as_threshold(5, 1800.)
    ! are we picking the right Kcb values off of the GDD-based curve?
    call assert_equals( 0.78, Kcb )

  end subroutine test_crop_coefficients_basic

end module test_crop_coefficients__FAO56
