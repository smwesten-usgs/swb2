module test_two_stage_evapotranspiration__FAO56

  use fruit
  use constants_and_conversions
  use actual_et__fao56__two_stage
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

!  logical (c_bool)   :: IS_CELL_ACTIVE(3,3) = .true.
!   real (c_double) :: SOIL_STORAGE(9)


!   ! ET0 in inches from Example 35, FAO56 publication, as present in Errata
!   real (c_double)    :: ET0(10)= [0.177165354,     &
!                                   0.196850394,     &
!                                   0.153543307,     &
!                                   0.165354331,     &
!                                   0.188976378,     &
!                                   0.106299213,     &
!                                   0.228346457,     &
!                                   0.200787402,     &
!                                   0.18503937,      &
!                                   0.204724409]
  
!   ! rooting depth in meters taken from example 38, FAO56
!   ! convert to feet
!   real (c_double)    :: Zr(12) =  [0.3000000,              &
!                                    0.3054545,              &
!                                    0.3109091,              &
!                                    0.3163636,              &
!                                    0.3218182,              &
!                                    0.3272727,              &
!                                    0.3327273,              &
!                                    0.3381818,              &
!                                    0.3436364,              &
!                                    0.3490909,              &
!                                    0.3545455,              &
!                                    0.3600000] / 0.3048

!   ! Example 35 gives theta FC as 0.23 m^3/m^3, and theta WP as 0.1 m^3/m^3
!   ! we need these values in inches per foot
!   real (c_double) :: SOIL_STORAGE_MAX(12) = (0.23 - 0.1) * 12. * Zr

! contains

!   subroutine setup_two_stage_evapotranspiration__FAO56

!     type (PARAMETERS_T)                   :: PARAMS
!     real (c_float), allocatable           :: fValues(:)
!     type (FSTRING_LIST_T)                 :: slString

!     if (     (SYSTEM_NAME .containssimilar. "Windows")                           &
!         .or. (SYSTEM_NAME .containssimilar. "Mingw") ) then
!       OS_NATIVE_PATH_DELIMITER = "\"
!     else
!       OS_NATIVE_PATH_DELIMITER = "/"
!     endif

!     call LOGS%initialize( iLogLevel = LOG_DEBUG )

!     call PARAMS%add_file("../test_data/tables/Lookup__crop_coefficient_test.txt")
!     call PARAMS%munge_file()
!     call SIM_DT%start%parseDate("01/01/2002")
!     call crop_coefficients_FAO56_initialize( )


!   end subroutine setup_two_stage_evapotranspiration__FAO56

! !-------------------------------------------------------------------------------

!   subroutine test_two_stage_evapotranspiration_basic

!     integer (c_int) :: day
!     integer (c_int) :: landuse_index
!     real (c_double) :: ET0_daily
!     real (c_double) :: soil_moisture_deficit

!     do day = 1, 10
!       ET0_daily = ET0(day)

!       do landuse_index = 1, 9
!         adjusted_depletion_fraction_p = adjust_depletion_fraction_p( landuse_index, ET0_daily )
!         soil_storage_max = Zr(day) 
!         soil_moisture_deficit = max( 0.0_c_double, real(soil_storage_max, c_double) - soil_storage)
      

!     enddo

!     ! real (c_float)  :: Kcb

!     ! ! test file sets up Corn (index=5) with missing values for
!     ! ! L_ini, L_dev, etc., instead supplying values for GDD_ini, GDD_dev, etc.
!     ! call assert_equals( KCB_METHOD(5), KCB_METHOD_GDD)

!     ! ! special case: GDD=0, but Planting date (04/01)
!     ! ! growing should not have started, and Kcb = 0.14 for barley
!     ! call SIM_DT%curr%parseDate("03/30/2002")
!     ! Kcb = update_crop_coefficient_GDD_as_threshold(6, 0.)
!     ! call assert_equals( 0.14, Kcb )

!     ! ! special case: GDD=0, but Planting date (04/20) is before the
!     ! ! current date; growing should be started, and Kcb = 0.15 for barley
!     ! call SIM_DT%curr%parseDate("04/20/2002")
!     ! Kcb = update_crop_coefficient_GDD_as_threshold(6, 0.)
!     ! call assert_equals( 0.15, Kcb )

!     ! Kcb = update_crop_coefficient_GDD_as_threshold(5, 1000.)
!     ! ! are we picking the right Kcb values off of the GDD-based curve?
!     ! call assert_equals( 0.96, Kcb )

!     ! Kcb = update_crop_coefficient_GDD_as_threshold(5, 2000.)
!     ! ! are we picking the right Kcb values off of the GDD-based curve?
!     ! call assert_equals( 0.6, Kcb )

!     ! Kcb = update_crop_coefficient_GDD_as_threshold(5, 1800.)
!     ! ! are we picking the right Kcb values off of the GDD-based curve?
!     ! call assert_equals( 0.78, Kcb )

!   end subroutine test_two_stage_evapotranspiration_basic

end module test_two_stage_evapotranspiration__FAO56
