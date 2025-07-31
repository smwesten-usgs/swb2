module running_grid_stats

  use constants_and_conversions, only : TRUE, FALSE
  use exceptions, only                : assert
  use grid
  use iso_c_binding, only             : c_long_long, c_double, c_int, c_bool
  implicit none

  private

  public :: RUNNING_STATS_T

  type RUNNING_STATS_T
    type (GENERAL_GRID_T), pointer        :: grd_delta => null()
    type (GENERAL_GRID_T), pointer        :: grd_delta_n => null()
    type (GENERAL_GRID_T), pointer        :: grd_delta_n2 => null()
    type (GENERAL_GRID_T), pointer        :: grd_term1 => null()
    type (GENERAL_GRID_T), pointer        :: grd_M1 => null()
    type (GENERAL_GRID_T), pointer        :: grd_M2 => null()
    type (GENERAL_GRID_T), pointer        :: grd_sum => null()
    integer (c_long_long)                 :: n = 0
    real (c_double)                       :: nodata_value = -9999.
    !    type (GENERAL_GRID_T), pointer        :: grd_M3 => null()
    !    type (GENERAL_GRID_T), pointer        :: grd_M4 => null()

    contains

    procedure :: clear => clear_sub
    procedure :: initialize => initialize_sub
    procedure :: push => push_sub
    procedure :: mean => calc_mean_sub
    procedure :: sum => calc_sum_sub
    procedure :: variance => calc_variance_sub
    procedure :: std_deviation => calc_std_deviation_sub

  end type RUNNING_STATS_T

  contains

    subroutine initialize_sub(this, NX, NY, X0, Y0, X1, Y1, nodata_value)

      class(RUNNING_STATS_T), intent(inout)   :: this
      integer (c_int), intent(in)             :: NX
      integer (c_int), intent(in)             :: NY
      real (c_double), intent(in)             :: X0
      real (c_double), intent(in)             :: Y0
      real (c_double), intent(in)             :: X1
      real (c_double), intent(in)             :: Y1
      real (c_double), intent(in)             :: nodata_value

      this%nodata_value = nodata_value

      this%grd_delta    => grid_Create ( iNX=NX,                          &
                                         iNY=NY,                          &
                                         rX0=X0,                          &
                                         rY0=Y0,                          &
                                         rX1=X1,                          &
                                         rY1=Y1,                          &
                                         iDataType=GRID_DATATYPE_DOUBLE )
       this%grd_delta_n  => grid_Create ( iNX=NX,                          &
                                         iNY=NY,                          &
                                         rX0=X0,                          &
                                         rY0=Y0,                          &
                                         rX1=X1,                          &
                                         rY1=Y1,                          &
                                         iDataType=GRID_DATATYPE_DOUBLE )

      this%grd_delta_n2 => grid_Create ( iNX=NX,                          &
                                         iNY=NY,                          &
                                         rX0=X0,                          &
                                         rY0=Y0,                          &
                                         rX1=X1,                          &
                                         rY1=Y1,                          &
                                         iDataType=GRID_DATATYPE_DOUBLE )

      this%grd_term1   => grid_Create ( iNX=NX,                          &
                                        iNY=NY,                          &
                                        rX0=X0,                          &
                                        rY0=Y0,                          &
                                        rX1=X1,                          &
                                        rY1=Y1,                          &
                                        iDataType=GRID_DATATYPE_DOUBLE )

      this%grd_M1      => grid_Create ( iNX=NX,                          &
                                        iNY=NY,                          &
                                        rX0=X0,                          &
                                        rY0=Y0,                          &
                                        rX1=X1,                          &
                                        rY1=Y1,                          &
                                        iDataType=GRID_DATATYPE_DOUBLE )

      this%grd_M2      => grid_Create ( iNX=NX,                          &
                                        iNY=NY,                          &
                                        rX0=X0,                          &
                                        rY0=Y0,                          &
                                        rX1=X1,                          &
                                        rY1=Y1,                          &
                                        iDataType=GRID_DATATYPE_DOUBLE )

      this%grd_sum     => grid_Create ( iNX=NX,                          &
                                        iNY=NY,                          &
                                        rX0=X0,                          &
                                        rY0=Y0,                          &
                                        rX1=X1,                          &
                                        rY1=Y1,                          &
                                        iDataType=GRID_DATATYPE_DOUBLE )

  end subroutine initialize_sub

!------------------------------------------------------------------------------  

  subroutine clear_sub(this)

    class(RUNNING_STATS_T), intent(inout)   :: this

    if ( allocated(this%grd_delta%dpData) ) this%grd_delta%dpData = 0.0_c_double
    if ( allocated(this%grd_delta_n%dpData) ) this%grd_delta_n%dpData = 0.0_c_double
    if ( allocated(this%grd_delta_n2%dpData) ) this%grd_delta_n2%dpData = 0.0_c_double
    if ( allocated(this%grd_term1%dpData) ) this%grd_term1%dpData = 0.0_c_double
    if ( allocated(this%grd_M1%dpData) ) this%grd_M1%dpData = 0.0_c_double
    if ( allocated(this%grd_M2%dpData) ) this%grd_M2%dpData = 0.0_c_double
    if ( allocated(this%grd_sum%dpData) ) this%grd_sum%dpData = 0.0_c_double
    this%n = 0_c_long_long

  end subroutine clear_sub

!------------------------------------------------------------------------------  

  subroutine push_sub( this, grd_data, mask )

    class(RUNNING_STATS_T), intent(inout)      :: this
    real (c_double), intent(in)                :: grd_data(:,:)
    logical (c_bool), intent(in)               :: mask(:,:)

    integer (c_long_long)   :: n1

    n1 = this%n
    this%n = this%n + 1

    where ( mask )

      this%grd_sum%dpData = this%grd_sum%dpData + grd_data
      this%grd_delta%dpData = grd_data - this%grd_M1%dpData
      this%grd_delta_n%dpData = this%grd_delta%dpData / real(this%n, c_double)
      this%grd_delta_n2%dpData = this%grd_delta_n%dpData * this%grd_delta_n%dpData
      this%grd_term1%dpData = this%grd_delta%dpData * this%grd_delta_n%dpData       &
                                * real(n1, c_double)
      this%grd_M1%dpData = this%grd_M1%dpData + this%grd_delta_n%dpData
      this%grd_M2%dpData = this%grd_M2%dpData + this%grd_term1%dpData

    else where

      this%grd_sum%dpData = -9999._c_double
      this%grd_M1%dpData = -9999._c_double
      this%grd_M2%dpData = -9999._c_double

    end where

  end subroutine push_sub

!------------------------------------------------------------------------------  

  subroutine calc_mean_sub(this, dpData)

    class(RUNNING_STATS_T), intent(inout)          :: this
    real (c_double), dimension(:,:), intent(out)   :: dpData

    dpData = this%grd_M1%dpData

  end subroutine calc_mean_sub

!------------------------------------------------------------------------------  

  subroutine calc_sum_sub(this, dpData)

    class(RUNNING_STATS_T), intent(inout)          :: this
    real (c_double), dimension(:,:), intent(out)   :: dpData

    dpData = this%grd_sum%dpData

  end subroutine calc_sum_sub

!------------------------------------------------------------------------------  

  subroutine calc_variance_sub(this, dpData)

    class(RUNNING_STATS_T), intent(inout)          :: this
    real (c_double), dimension(:,:), intent(out)   :: dpData

!    call assert(this%n > 1, "Not enough data points processed to calculate variance")

    where ((this%grd_M1%dpData > -9999._c_double) .and. this%n > 1)
      dpData = this%grd_M1%dpData / (real(this%n, c_double) - 1.0_c_double)
    else where
      dpData = -9999._c_double
    end where 

  end subroutine calc_variance_sub

!------------------------------------------------------------------------------  

  subroutine calc_std_deviation_sub(this, dpData)

    class(RUNNING_STATS_T), intent(inout)                       :: this
    real (c_double), dimension(:,:), allocatable, intent(out)   :: dpData

!    call assert(this%n > 1, "Not enough data points processed to calculate variance")

    if(this%n > 1) then
      dpData = sqrt(this%grd_M1%dpData / (real(this%n, c_double) - 1.0_c_double))
    else
      dpData = 0.0_c_double
    endif

  end subroutine calc_std_deviation_sub

!------------------------------------------------------------------------------  

end module running_grid_stats