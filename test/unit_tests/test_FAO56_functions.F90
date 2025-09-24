module test_FAO56_functions

  use fruit
  use constants_and_conversions
  use crop_coefficients__FAO56
  use actual_et__fao56__two_stage
  use growing_degree_day
  use logfiles, only               : LOGS, LOG_DEBUG
  use parameters, only             : PARAMETERS_T
  use simulation_datetime, only    : SIM_DT
  use fstring
  use fstring_list, only           : FSTRING_LIST_T
  use version_control
  use iso_c_binding
  implicit none

  type (PARAMETERS_T)      :: TEST_PARAMS

  logical (c_bool)         :: IS_CELL_ACTIVE(3,3) = .true.
  integer (c_int)          :: LANDUSE_INDEX(9)    = [1,2,3,4,5,6,7,8,9]

  real (c_double)          :: SOIL_STORAGE_MAX(12)


contains

  subroutine setup_crop_coefficients__FAO56

    real (c_float), allocatable           :: fValues(:)
    type (FSTRING_LIST_T)                 :: slString

    if (     (SYSTEM_NAME .containssimilar. "Windows")                           &
        .or. (SYSTEM_NAME .containssimilar. "Mingw") ) then
      OS_NATIVE_PATH_DELIMITER = "\"
    else
      OS_NATIVE_PATH_DELIMITER = "/"
    endif

    call LOGS%initialize( iLogLevel = LOG_DEBUG )

    call TEST_PARAMS%add_file("../test_data/tables/Lookup__crop_coefficient_test.txt")
    call TEST_PARAMS%add_file("../test_data/tables/FAO56_Example_35.txt")
    call TEST_PARAMS%add_file("../test_data/tables/FAO56_equation_72_calcs.txt")
    call TEST_PARAMS%munge_file()
    call SIM_DT%start%setDateFormat("MM/DD/YYYY")
    call SIM_DT%start%parseDate("01/01/2002", sFilename=trim(__FILE__), iLineNumber=__LINE__)
    call crop_coefficients_FAO56_initialize( )

    call growing_degree_day_initialize( IS_CELL_ACTIVE, LANDUSE_INDEX )

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

!-------------------------------------------------------------------------------

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

!-------------------------------------------------------------------------------

subroutine test_fao56_equation_72
! fao-56: test functioning of equation 72 implementation

  real (c_float) :: wind_spd = 1.6
  real (c_float) :: RHmin = 35.
  real (c_float) :: Kcb = 0.35
  real (c_float) :: plant_height_m = 0.3

  real (c_float) :: kcb_max
  
  kcb_max = crop_coefficients_FAO56_calculate_Kcb_Max(wind_spd,         &
                                                      RHMin,            &
                                                      Kcb,              & 
                                                      plant_height_m)

  ! compare SWB code calculated value to example 35 calculation in FAO-56
  ! value in publication appears to have round-off error; this value comes from 
  ! an R implementation of equation 72
  call assert_equals(1.212028, kcb_max, delta=0.0001, message="example 35 values used")                 
  
end subroutine test_fao56_equation_72

!-------------------------------------------------------------------------------

subroutine test_fao56_equation_72_v2
  ! fao-56: test functioning of equation 72 implementation against R implementation
  
    real (c_float), allocatable :: wind_spd(:)
    real (c_float), allocatable :: RHmin(:)
    real (c_float), allocatable :: plant_height_m(:)
    real (c_float), allocatable :: kcb_max(:)

    character (len=:), allocatable :: str
    real (c_float)  :: test_val
    integer (c_int) :: n

    call TEST_PARAMS%get_parameters(wind_spd, sKey="eq72_u2")
    call TEST_PARAMS%get_parameters(RHmin, sKey="eq72_rhmin")
    call TEST_PARAMS%get_parameters(plant_height_m, sKey="eq72_plant_height")    
    call TEST_PARAMS%get_parameters(kcb_max, sKey="eq72_kc_max")

    do n=1, ubound(RHmin,1)

      test_val = crop_coefficients_FAO56_calculate_Kcb_Max(wind_spd(n),         &
                                                           RHmin(n),            &
                                                           0.5,                 & 
                                                           plant_height_m(n))

      str = "wind spd: "//as_character(wind_spd(n))//" RHmin: "//as_character(RHmin(n))      &
            //" plnt ht: "//as_character(plant_height_m(n))

      call assert_equals(test_val, kcb_max(n), delta=0.0001, message=str)
      
    enddo
  
  end subroutine test_fao56_equation_72_v2
!-------------------------------------------------------------------------------

subroutine test_fao56_example_35

    real (c_float), allocatable :: ex35_kr(:) 
    real (c_float), allocatable :: ex35_ke(:) 
    real (c_float), allocatable :: ex35_ET0(:)
    real (c_float), allocatable :: ex35_Kcb(:)
    real (c_float), allocatable :: ex35_p_minus_ro(:)
    real (c_float), allocatable :: ex35_irrigation(:)
    real (c_float), allocatable :: ex35_1_minus_fc(:)
    real (c_float), allocatable :: ex35_few(:)
    real (c_float), allocatable :: ex35_fw(:)

    real (c_float)              :: Kcb_max
    real (c_float)              :: evaporable_water_storage
    real (c_float)              :: evaporable_water_deficit
    real (c_float)              :: TEW_ = 18
    real (c_float)              :: REW_ = 8
    integer (c_int)             :: day
    real (c_float)              :: infiltration
    real (c_float)              :: Kr
    real (c_float)              :: Ke
    real (c_float)              :: Ks
    real (c_float)              :: Kcb
    real (c_float)              :: fw
    real (c_float)              :: few
    real (c_float)              :: bare_soil_evap
    real (c_double)             :: reference_et0
    real (c_float)              :: actual_et
    real (c_float)              :: crop_etc


    evaporable_water_storage = 0.0
    evaporable_water_deficit = TEW_

    call TEST_PARAMS%get_parameters(ex35_kr, sKey="EX35_KR")
    call TEST_PARAMS%get_parameters(ex35_ke, sKey="EX35_ke")
    call TEST_PARAMS%get_parameters(ex35_ET0, sKey="EX35_ET0")
    call TEST_PARAMS%get_parameters(ex35_Kcb, sKey="EX35_Kcb")
    call TEST_PARAMS%get_parameters(ex35_p_minus_ro, sKey="ex35_p_minus_ro")
    call TEST_PARAMS%get_parameters(ex35_irrigation, sKey="ex35_irrigation")
    call TEST_PARAMS%get_parameters(ex35_1_minus_fc, sKey="ex35_1_minus_fc")
    call TEST_PARAMS%get_parameters(ex35_few, sKey="ex35_few")
    call TEST_PARAMS%get_parameters(ex35_fw, sKey="ex35_fw")

    Kcb_max =  crop_coefficients_FAO56_calculate_Kcb_Max(wind_speed_meters_per_sec=1.5,                  &
                                                         relative_humidity_min_pct=35.,                  &
                                                         Kcb=0.35,                                       & 
                                                         plant_height_meters=0.30) 

    call assert_equals(Kcb_max, 1.21, delta=0.001)

    do day=1,10

      ! givens in example 35
      fw = ex35_fw(day)
      few = ex35_few(day)
      infiltration = ex35_p_minus_ro(day) + ex35_irrigation(day)
      Kcb = ex35_Kcb(day)
      reference_et0 = ex35_ET0(day)

      evaporable_water_storage = clip( evaporable_water_storage + infiltration, minval=0., maxval=TEW_)
      evaporable_water_deficit = clip( TEW_ - evaporable_water_storage, minval=0., maxval=TEW_)

      if ( evaporable_water_deficit <= REW_ ) then
        Kr = 1._c_double
      elseif ( evaporable_water_deficit < TEW_ ) then
        Kr = ( real(TEW_, c_double) - evaporable_water_deficit )         &
            / ( real(TEW_, c_double) - real(REW_, c_double) + 1.0E-8)
      else
        Kr = 0._c_double
      endif

      ! because the evaporable soil layer is the control volume under consideration, the value
      ! of Ks should be the same as the value for Kr
      Ks = Kr

      Ke = Kr * ( real(Kcb_max, c_double) - real(Kcb, c_double) )

      call assert_equals(Kr, ex35_kr(day),message="Kr, day "//as_character(day)//": SWB calculated "//as_character(Kr), delta=1.0e-2)
      call assert_equals(Ke, ex35_ke(day),message="Ke, day "//as_character(day)//": SWB calculated "//as_character(Ke), delta=1.0e-2)

      bare_soil_evap           = reference_et0 * Ke / few
      crop_etc                 = reference_et0 * Kcb * Ks * 0.0 !* (1 - few)
      actual_et                = crop_etc + bare_soil_evap

      evaporable_water_storage = clip(evaporable_water_storage - actual_et, minval=0.0, maxval=TEW_)

    enddo  

end subroutine test_fao56_example_35

end module test_FAO56_functions
