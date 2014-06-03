module options

  use iso_c_binding
  implicit none

  private

  type, public :: OPTIONS_T
  	character (len=:), allocatable  :: 



  end type OPTIONS_T

  enum, bind(c)
    enumerator :: OPT_PRECIPITATION, OPT_PRECIPITATION_SCALE, OPT_PRECIPITATION_OFFSET,              &
                  OPT_PRECIPITATION_CONVERSION_FACTOR, OPT_NETCDF_PRECIPITATION_X_VAR,               &
                  OPT_NETCDF_PRECIPITATION_Y_VAR, OPT_NETCDF_PRECIPITATION_Z_VAR,                    &
                  OPT_NETCDF_PRECIPITATION_TIME_VAR, OPT_NETCDF_PRECIPITATION_VARIABLE_ORDER,        &
                  OPT_NETCDF_PRECIPITATION_FLIP_VERTICAL, OPT_NETCDF_PRECIPITATION_FLIP_HORIZONTAL,  &


end module options