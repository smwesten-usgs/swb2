module growing_degree_day_baskerville_emin

  use iso_c_binding

  use constants_and_conversions
  use datetime                   , only : mmdd2doy
  use exceptions
  use netcdf4_support
  use parameters
  use simulation_datetime
  use fstring_list
  implicit none

  private

  public :: GDD_BASE, GDD_MAX, GDD_RESET_DATE
  public :: growing_degree_day_be_calculate, growing_degree_day_be_initialize

  real (c_float), allocatable  :: GDD_BASE(:)
  real (c_float), allocatable  :: GDD_MAX(:)
  integer (c_int), allocatable :: GDD_RESET_DATE(:)
  type (T_NETCDF4_FILE), pointer    :: pNCFILE

contains

  subroutine growing_degree_day_be_initialize( is_cell_active, landuse_index )

    logical (c_bool), intent(in)     :: is_cell_active(:,:)
    integer (c_int), intent(in)      :: landuse_index(:)

    ! [ LOCALS ]
    integer (c_int)              :: status
    integer (c_int)              :: indx
    type (FSTRING_LIST_T)              :: parameter_list
    type (FSTRING_LIST_T)              :: gdd_reset_val_list
    character (len=32)                :: sBuf
    real (c_float), allocatable  :: gdd_base_l(:)
    real (c_float), allocatable  :: gdd_max_l(:)
    integer (c_int)              :: number_of_landuse_codes
    integer (c_int), allocatable :: landuse_code(:)

    allocate( GDD_BASE( count( is_cell_active ) ), stat=status )
    call assert( status == 0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    allocate( GDD_MAX( count( is_cell_active ) ), stat=status )
    call assert( status == 0, "Problem allocating memory", __SRCNAME__, __LINE__ )

    !> create string list that allows for alternate heading identifiers for the landuse code
    call parameter_list%append("LU_Code")
    call parameter_list%append("Landuse_Code")
    call parameter_list%append("Landuse_Lookup_Code")

    !> Determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=parameter_list, iValues=landuse_code )
    number_of_landuse_codes = count( landuse_code >= 0 )
    call parameter_list%clear()

    call parameter_list%append("GDD_Base_Temp")
    call parameter_list%append("GDD_Base_Temperature")
    call parameter_list%append("GDD_Base")

    call PARAMS%get_parameters( slKeys=parameter_list, fValues=gdd_base_l )
    call parameter_list%clear()

    call parameter_list%append("GDD_Max_Temp")
    call parameter_list%append("GDD_Maximum_Temperature")
    call parameter_list%append("GDD_Maximum_Temp")
    call parameter_list%append("GDD_Max")

    call PARAMS%get_parameters( slKeys=parameter_list, fValues=gdd_max_l )
    call parameter_list%clear()

    call parameter_list%append("GDD_Reset_Date")
    call parameter_list%append("GDD_Reset")

    call PARAMS%get_parameters( slKeys=parameter_list, slValues=gdd_reset_val_list )
    call parameter_list%clear()

    allocate( GDD_RESET_DATE( count( is_cell_active ) ), stat=status )
    call assert( status==0, "Problem allocating memory.", __SRCNAME__, __LINE__ )

    if ( gdd_reset_val_list%count == number_of_landuse_codes      &
         .and. gdd_reset_val_list%countmatching("<NA>") == 0 ) then

      ! retrieve gdd reset values; convert mm/dd to DOY
      do indx=1, gdd_reset_val_list%count
        sBuf = gdd_reset_val_list%get( indx )

        where ( landuse_index == indx )
          GDD_RESET_DATE = mmdd2doy( sBuf, "GDD_RESET_DATE" )
        end where

      enddo

    else

      ! if no GDD_RESET_DATE found in parameter tables, assign the default: reset GDD at end of calendar year
      GDD_RESET_DATE = 365_c_int

    endif


    if ( ubound( gdd_max_l, 1 ) == number_of_landuse_codes        &
        .and. gdd_max_l(1) > rTINYVAL ) then

      do indx=1, ubound( landuse_index, 1)
        if( landuse_index( indx ) >= lbound( GDD_MAX, 1) .and. landuse_index( indx ) <= ubound( GDD_MAX, 1) ) then
          GDD_MAX( indx ) = gdd_max_l( landuse_index( indx ) )
        endif
      enddo

    else

      ! if no GDD_MAX found in parameter tables, assign the default FAO-56 value
      GDD_MAX = 86.0_c_float

    endif

    if ( ubound( gdd_base_l, 1 ) == number_of_landuse_codes        &
        .and. gdd_base_l(1) > rTINYVAL  ) then

      do indx=1, ubound( landuse_index, 1)
        if( landuse_index( indx ) >= lbound( GDD_BASE, 1) .and. landuse_index( indx ) <= ubound( GDD_BASE, 1) ) then
          GDD_BASE( indx ) = gdd_base_l( landuse_index( indx ) )
        endif
      enddo

    else

      ! if no GDD_Base found in parameter tables, assign the default FAO-56 value
      GDD_BASE = 50.0_c_float

    endif

  end subroutine growing_degree_day_be_initialize

!--------------------------------------------------------------------------------------------------

  elemental subroutine growing_degree_day_be_calculate( gdd, tmean, tmin, tmax, order )

    ! [ ARGUMENTS ]
    real (c_float), intent(inout)       :: gdd
    real (c_float), intent(in)          :: tmean
    real (c_float), intent(in)          :: tmin
    real (c_float), intent(in)          :: tmax
    integer (c_int), intent(in)         :: order

    ! [ LOCALS ]
    real (c_float)    :: delta
    real (c_float)    :: tmax_l
    real (c_float)    :: dd
    real (c_float)    :: W
    real (c_float)    :: At
    real (c_float)    :: A

    associate( doy_to_reset_gdd => GDD_RESET_DATE( order ),      &
               gdd_max => GDD_MAX( order ),                      &
               gdd_base => GDD_BASE( order ) )

      if ( SIM_DT%iDOY == doy_to_reset_gdd )  gdd = 0.0_c_float

      tmax_l = min( tmax, gdd_max )

      if ( tmax_l <= gdd_base ) then

        dd = 0.0_c_float

      elseif ( tmin >= gdd_base ) then

        dd = min(tmean, gdd_max - gdd_base )

      else

        W = ( tmax_l -  tmin) / 2.0_c_float

        At = ( gdd_base - tmean ) / W

        if ( At > 1 ) At = 1.0_c_float
        if ( At < -1 ) At = -1._c_float

        A = asin( At )

        dd = ( ( W * cos( A ) ) - ( ( gdd_base - tmean )                    &
               * ( real( PI / 2._c_double, c_float ) - A ) ) ) / PI

      endif

      gdd = gdd + dd

    end associate

  end subroutine growing_degree_day_be_calculate

!--------------------------------------------------------------------------------------------------

end module growing_degree_day_baskerville_emin
