module test_FAO56_functions

  use fruit
  use constants_and_conversions
  use crop_coefficients__FAO56
  use growing_degree_day
  use logfiles, only               : LOGS, LOG_DEBUG
  use parameters, only             : PARAMETERS_T
  use simulation_datetime, only    : SIM_DT
  use fstring
  use fstring_list, only           : FSTRING_LIST_T
  use version_control
  use iso_c_binding
  implicit none

  logical (c_bool)   :: IS_CELL_ACTIVE(3,3) = .true.
  integer (c_int)    :: LANDUSE_INDEX(9)    = [1,2,3,4,5,6,7,8,9]

 ! ET0 in inches from Example 35, FAO56 publication, as present in Errata
  real (c_double)    :: ET0(10)= [0.177165354,     &
                                  0.196850394,     &
                                  0.153543307,     &
                                  0.165354331,     &
                                  0.188976378,     &
                                  0.106299213,     &
                                  0.228346457,     &
                                  0.200787402,     &
                                  0.18503937,      &
                                  0.204724409]
  
  ! rooting depth in meters taken from example 38, FAO56
  ! convert to feet
  real (c_double)    :: Zr(12) =  [0.3000000,              &
                                   0.3054545,              &
                                   0.3109091,              &
                                   0.3163636,              &
                                   0.3218182,              &
                                   0.3272727,              &
                                   0.3327273,              &
                                   0.3381818,              &
                                   0.3436364,              &
                                   0.3490909,              &
                                   0.3545455,              &
                                   0.3600000] / 0.3048

  real (c_double) :: SOIL_STORAGE_MAX(12)


contains

  subroutine setup_crop_coefficients__FAO56

    type (PARAMETERS_T)                   :: PARAMS
    real (c_float), allocatable           :: fValues(:)
    type (FSTRING_LIST_T)                 :: slString

    if (     (SYSTEM_NAME .containssimilar. "Windows")                           &
        .or. (SYSTEM_NAME .containssimilar. "Mingw") ) then
      OS_NATIVE_PATH_DELIMITER = "\"
    else
      OS_NATIVE_PATH_DELIMITER = "/"
    endif

    call LOGS%initialize( iLogLevel = LOG_DEBUG )

    call PARAMS%add_file("../test_data/tables/Lookup__crop_coefficient_test.txt")
    call PARAMS%munge_file()
    call SIM_DT%start%setDateFormat("MM/DD/YYYY")
    call SIM_DT%start%parseDate("01/01/2002", sFilename=trim(__SRCNAME__), iLineNumber=__LINE__)
    call crop_coefficients_FAO56_initialize( )

    call growing_degree_day_initialize( IS_CELL_ACTIVE, LANDUSE_INDEX )

     ! Example 35 gives theta FC as 0.23 m^3/m^3, and theta WP as 0.1 m^3/m^3
    ! we need these values in inches per foot
    SOIL_STORAGE_MAX = (0.23 - 0.1) * 12. * Zr

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

  subroutine test_gdd_max_plus_min_simple

    ! Values taken from:
    ! Wilson, L.T., and Barnett, W.W., 1983, Degree-days: an aid in crop and
    !    pest management: California Agriculture, January-February, p. 4.
    real (c_float) :: TMIN(6) =            [55., 55.,   60., 60.,   65.,   65.]
    real (c_float) :: TMAX(6) =            [65., 70.,   70., 75.,   80.,   85.]
    real (c_float) :: GDD_TABLE_VALUE(6) = [10., 12.5, 15., 17.5, 22.5, 25.]
    real (c_float) :: GDD

    integer (c_int) :: i
    real (c_float)  :: tmean(6)

    tmean = clip(value=((TMIN + TMAX) / 2.), minval=50., maxval=85.) 
  
    do i=1, ubound(TMIN,1)
      GDD = 0.0
      call growing_degree_day_calculate( GDD, tmean(i), 1 )
      call assert_equals( GDD_TABLE_VALUE(i), GDD)
    enddo  

  end subroutine test_gdd_max_plus_min_simple

end module test_FAO56_functions
