module test_growing_degree_day
  !! Unit tests for growing degree day calculation.
  !!
  !! Tests cover:
  !!   - FAO-56 GDD formula: basic accumulation, capping at base and upper threshold
  !!   - Per-landuse GDD_Base and GDD_Max differentiation
  !!   - Baskerville-Emin method: comparison to FAO-56 simple method
  !!   - GDD reset at year boundary

  use iso_c_binding, only: c_int, c_float, c_bool
  use testdrive, only: check, error_type, new_unittest, unittest_type, test_failed
  use growing_degree_day, only: growing_degree_day_calculate, &
                                 growing_degree_day_initialize, &
                                 GDD_BASE, GDD_MAX, GDD_RESET_DATE
  use growing_degree_day_baskerville_emin, only: growing_degree_day_be_calculate, &
                                                  growing_degree_day_be_initialize, &
                                                  GDD_BASE_BE => GDD_BASE, &
                                                  GDD_MAX_BE => GDD_MAX, &
                                                  GDD_RESET_DATE_BE => GDD_RESET_DATE
  use simulation_datetime, only: SIM_DT
  use test_fixtures, only: setup_environment_fao56_gdd, DEFAULT_IS_CELL_ACTIVE, &
                           DEFAULT_LANDUSE_INDEX
  implicit none
  private

  public :: collect_growing_degree_day

contains

  subroutine collect_growing_degree_day(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    call setup_environment_fao56_gdd()

    ! Also initialize the Baskerville-Emin module (has its own separate arrays)
    if (allocated(GDD_BASE_BE)) deallocate(GDD_BASE_BE)
    if (allocated(GDD_MAX_BE)) deallocate(GDD_MAX_BE)
    if (allocated(GDD_RESET_DATE_BE)) deallocate(GDD_RESET_DATE_BE)
    call growing_degree_day_be_initialize(DEFAULT_IS_CELL_ACTIVE, DEFAULT_LANDUSE_INDEX)

    testsuite = [ &
      new_unittest("above_base", test_above_base), &
      new_unittest("at_base_zero_gdd", test_at_base_zero_gdd), &
      new_unittest("below_base_zero_gdd", test_below_base_zero_gdd), &
      new_unittest("accumulates_over_days", test_accumulates), &
      new_unittest("tmax_capped_at_gdd_max", test_tmax_capped), &
      new_unittest("tmin_raised_to_base", test_tmin_raised), &
      new_unittest("different_base_temps", test_different_base), &
      new_unittest("different_max_temps", test_different_max_temps), &
      new_unittest("be_both_above_base", test_be_above_base), &
      new_unittest("be_straddle_differs_from_fao56", test_be_straddle), &
      new_unittest("be_tmax_below_base_zero_gdd", test_be_tmax_below_base), &
      new_unittest("reset_on_doy", test_reset_on_doy) &
    ]
  end subroutine collect_growing_degree_day

  !---------------------------------------------------------------------------
  ! Basic FAO-56 GDD cases
  ! Formula: GDD += max((max(Tmin,Tbase) + min(Tmax,Tupper))/2, Tbase) - Tbase
  ! Test fixture sets GDD_BASE(1)=50, GDD_MAX(1)=130
  !---------------------------------------------------------------------------

  subroutine test_above_base(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: gdd
    ! (max(60,50)+min(80,130))/2 - 50 = (60+80)/2 - 50 = 20
    gdd = 0.0
    call growing_degree_day_calculate(gdd, 60.0_c_float, 80.0_c_float, 1)
    call check(error, abs(gdd - 20.0) < 0.001, "expected 20")
  end subroutine test_above_base

  subroutine test_at_base_zero_gdd(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: gdd
    ! (max(50,50)+min(50,130))/2 - 50 = (50+50)/2 - 50 = 0
    gdd = 0.0
    call growing_degree_day_calculate(gdd, 50.0_c_float, 50.0_c_float, 1)
    call check(error, abs(gdd) < 0.001, "expected 0")
  end subroutine test_at_base_zero_gdd

  subroutine test_below_base_zero_gdd(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: gdd
    ! (max(30,50)+min(40,130))/2 - 50 = max((50+40)/2, 50) - 50 = max(45,50)-50 = 0
    gdd = 0.0
    call growing_degree_day_calculate(gdd, 30.0_c_float, 40.0_c_float, 1)
    call check(error, abs(gdd) < 0.001, "expected 0")
  end subroutine test_below_base_zero_gdd

  subroutine test_accumulates(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: gdd
    ! Day 1: (55+65)/2-50=10; Day 2: (60+80)/2-50=20; Day 3: (65+85)/2-50=25; Total=55
    gdd = 0.0
    call growing_degree_day_calculate(gdd, 55.0_c_float, 65.0_c_float, 1)
    call growing_degree_day_calculate(gdd, 60.0_c_float, 80.0_c_float, 1)
    call growing_degree_day_calculate(gdd, 65.0_c_float, 85.0_c_float, 1)
    call check(error, abs(gdd - 55.0) < 0.001, "expected 55")
  end subroutine test_accumulates

  !---------------------------------------------------------------------------
  ! Capping behavior
  !---------------------------------------------------------------------------

  subroutine test_tmax_capped(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: gdd
    ! Tmax=150 capped to GDD_MAX=130: (60+130)/2 - 50 = 95-50 = 45
    gdd = 0.0
    call growing_degree_day_calculate(gdd, 60.0_c_float, 150.0_c_float, 1)
    call check(error, abs(gdd - 45.0) < 0.001, "expected 45 (Tmax capped at 130)")
  end subroutine test_tmax_capped

  subroutine test_tmin_raised(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: gdd
    ! Tmin=30 raised to base=50: (50+80)/2 - 50 = 65-50 = 15
    gdd = 0.0
    call growing_degree_day_calculate(gdd, 30.0_c_float, 80.0_c_float, 1)
    call check(error, abs(gdd - 15.0) < 0.001, "expected 15 (Tmin raised to base)")
  end subroutine test_tmin_raised

  !---------------------------------------------------------------------------
  ! Per-landuse differentiation
  !---------------------------------------------------------------------------

  subroutine test_different_base(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: gdd1, gdd2, saved1, saved2
    saved1 = GDD_BASE(1); saved2 = GDD_BASE(2)
    GDD_BASE(1) = 50.0; GDD_BASE(2) = 32.0
    ! Tmin=55,Tmax=75: Cell1=(55+75)/2-50=15; Cell2=(55+75)/2-32=33
    gdd1 = 0.0; gdd2 = 0.0
    call growing_degree_day_calculate(gdd1, 55.0_c_float, 75.0_c_float, 1)
    call growing_degree_day_calculate(gdd2, 55.0_c_float, 75.0_c_float, 2)
    GDD_BASE(1) = saved1; GDD_BASE(2) = saved2
    call check(error, abs(gdd1-15.0)<0.001 .and. abs(gdd2-33.0)<0.001, &
      "base=50 should give 15, base=32 should give 33")
  end subroutine test_different_base

  subroutine test_different_max_temps(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: gdd1, gdd2, saved1, saved2
    saved1 = GDD_MAX(1); saved2 = GDD_MAX(2)
    GDD_MAX(1) = 86.0; GDD_MAX(2) = 130.0
    ! Tmin=60,Tmax=100: Cell1=(60+86)/2-50=23; Cell2=(60+100)/2-50=30
    gdd1 = 0.0; gdd2 = 0.0
    call growing_degree_day_calculate(gdd1, 60.0_c_float, 100.0_c_float, 1)
    call growing_degree_day_calculate(gdd2, 60.0_c_float, 100.0_c_float, 2)
    GDD_MAX(1) = saved1; GDD_MAX(2) = saved2
    call check(error, abs(gdd1-23.0)<0.001 .and. abs(gdd2-30.0)<0.001, &
      "max=86 should give 23, max=130 should give 30")
  end subroutine test_different_max_temps

  !---------------------------------------------------------------------------
  ! Baskerville-Emin method
  !---------------------------------------------------------------------------

  subroutine test_be_above_base(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: gdd(1), tmean(1), tmin(1), tmax(1)
    integer(c_int) :: order(1)
    ! When Tmin>=base, B-E gives dd=min(tmean-base, gdd_max-base)=20
    tmin(1)=60.0; tmax(1)=80.0; tmean(1)=70.0; order(1)=1; gdd(1)=0.0
    call growing_degree_day_be_calculate(gdd, tmean, tmin, tmax, order)
    call check(error, abs(gdd(1)-20.0)<0.5, "B-E with Tmin>base: expected ~20")
  end subroutine test_be_above_base

  subroutine test_be_straddle(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: gdd_be(1), gdd_fao56, tmean(1), tmin(1), tmax(1)
    integer(c_int) :: order(1)
    ! Tmin<base<Tmax: B-E and FAO-56 give different values
    tmin(1)=35.0; tmax(1)=75.0; tmean(1)=55.0; order(1)=1; gdd_be(1)=0.0
    call growing_degree_day_be_calculate(gdd_be, tmean, tmin, tmax, order)
    gdd_fao56 = 0.0
    call growing_degree_day_calculate(gdd_fao56, 35.0_c_float, 75.0_c_float, 1)
    ! Both should be positive, but different
    call check(error, gdd_be(1)>0.0 .and. gdd_fao56>0.0 &
      .and. abs(gdd_be(1)-gdd_fao56)>0.1, &
      "B-E and FAO-56 should differ when straddling base")
  end subroutine test_be_straddle

  subroutine test_be_tmax_below_base(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: gdd(1), tmean(1), tmin(1), tmax(1)
    integer(c_int) :: order(1)
    ! Tmax<base → dd=0
    tmin(1)=30.0; tmax(1)=45.0; tmean(1)=37.5; order(1)=1; gdd(1)=0.0
    call growing_degree_day_be_calculate(gdd, tmean, tmin, tmax, order)
    call check(error, abs(gdd(1))<0.001, "B-E with Tmax<base: expected 0")
  end subroutine test_be_tmax_below_base

  !---------------------------------------------------------------------------
  ! Reset behavior
  !---------------------------------------------------------------------------

  subroutine test_reset_on_doy(error)
    type(error_type), allocatable, intent(out) :: error
    real(c_float) :: gdd
    integer(c_int) :: saved_doy, saved_reset
    gdd = 100.0
    saved_doy = SIM_DT%iDOY; saved_reset = GDD_RESET_DATE(1)
    GDD_RESET_DATE(1) = 365; SIM_DT%iDOY = 365
    ! Reset happens, then today's GDD added: (60+80)/2-50=20
    call growing_degree_day_calculate(gdd, 60.0_c_float, 80.0_c_float, 1)
    SIM_DT%iDOY = saved_doy; GDD_RESET_DATE(1) = saved_reset
    call check(error, abs(gdd-20.0)<0.001, "After reset: expected 20 (today only)")
  end subroutine test_reset_on_doy

end module test_growing_degree_day
