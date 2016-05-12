module growing_degree_day

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_long_long,   &
                            c_size_t, c_ptrdiff_t

  use constants_and_conversions
  use datetime                   , only : mmdd2doy
  use exceptions
  use netcdf4_support
  use parameters
  use simulation_datetime
  use string_list
  implicit none

  private

  public :: GDD_BASE, GDD_MAX, GDD_RESET_DATE
  public :: growing_degree_day_calculate, growing_degree_day_initialize

  real (kind=c_float), allocatable  :: GDD_BASE(:)
  real (kind=c_float), allocatable  :: GDD_MAX(:) 
  integer (kind=c_int), allocatable :: GDD_RESET_DATE(:)
  type (T_NETCDF4_FILE), pointer    :: pNCFILE
 
contains

  subroutine growing_degree_day_initialize( is_cell_active, landuse_index )

    logical (kind=c_bool), intent(in)     :: is_cell_active(:,:)
    integer (kind=c_int), intent(in)      :: landuse_index(:)

    ! [ LOCALS ]
    integer (kind=c_int)              :: status
    integer (kind=c_int)              :: indx
    type (STRING_LIST_T)              :: parameter_list
    type (STRING_LIST_T)              :: gdd_reset_val_list
    character (len=32)                :: sBuf
    real (kind=c_float), allocatable  :: gdd_base_(:)
    real (kind=c_float), allocatable  :: gdd_max_(:)
    integer (kind=c_int)              :: number_of_landuse_codes
    integer (kind=c_int), allocatable :: landuse_code(:)

    allocate( GDD_BASE( count( is_cell_active ) ), stat=status )
    call assert( status == 0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( GDD_MAX( count( is_cell_active ) ), stat=status )
    call assert( status == 0, "Problem allocating memory", __FILE__, __LINE__ )

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

    call PARAMS%get_parameters( slKeys=parameter_list, fValues=gdd_base_ )
    call parameter_list%clear()

    call parameter_list%append("GDD_Max_Temp")
    call parameter_list%append("GDD_Maximum_Temperature")
    call parameter_list%append("GDD_Maximum_Temp")
    call parameter_list%append("GDD_Max")

    call PARAMS%get_parameters( slKeys=parameter_list, fValues=gdd_max_ )
    call parameter_list%clear()

    call parameter_list%append("GDD_Reset_Date")
    call parameter_list%append("GDD_Reset")

    call PARAMS%get_parameters( slKeys=parameter_list, slValues=gdd_reset_val_list ) 
    call parameter_list%clear()

    allocate( GDD_RESET_DATE( count( is_cell_active ) ), stat=status )
    call assert( status==0, "Problem allocating memory.", __FILE__, __LINE__ )

    if ( gdd_reset_val_list%count == number_of_landuse_codes ) then

      ! retrieve gdd reset values; convert mm/dd to DOY
      do indx=1, gdd_reset_val_list%count
        sBuf = gdd_reset_val_list%get( indx )

        where ( landuse_index == indx )
          GDD_RESET_DATE = mmdd2doy( sBuf )
        end where
          
      enddo 
      
    else
    
      ! if no GDD_RESET_DATE found in parameter tables, assign the default: reset GDD at end of calendar year
      GDD_RESET_DATE = 365_c_int  

    endif   

    
    if ( ubound( gdd_max_, 1 ) == number_of_landuse_codes ) then

      do indx=1, ubound( landuse_index, 1)
        GDD_MAX( indx ) = gdd_max_( landuse_index( indx ) )
      enddo 
      
    else
    
      ! if no GDD_MAX found in parameter tables, assign the default FAO-56 value
      GDD_MAX = 86.0_c_float  

    endif   


    if ( ubound( gdd_base_, 1 ) == number_of_landuse_codes ) then

      do indx=1, ubound( landuse_index, 1)
        GDD_BASE( indx ) = gdd_base_( landuse_index( indx ) )
      enddo 
      
    else
    
      ! if no GDD_Base found in parameter tables, assign the default FAO-56 value
      GDD_BASE = 50.0_c_float  

    endif   

  end subroutine growing_degree_day_initialize

!--------------------------------------------------------------------------------------------------

  elemental subroutine growing_degree_day_calculate( gdd, tmean, order )

    ! [ ARGUMENTS ]
    real (kind=c_float), intent(inout)       :: gdd
    real (kind=c_float), intent(in)          :: tmean
    integer (kind=c_int), intent(in)         :: order

    ! [ LOCALS ]
    real (kind=c_float)    :: delta

    associate( doy_to_reset_gdd => GDD_RESET_DATE( order ),      &
               gdd_max_ => GDD_MAX( order ),                     &
               gdd_base_ => GDD_BASE( order ) )

      if ( SIM_DT%iDOY == doy_to_reset_gdd )  gdd = 0.0_c_float

      delta = min(tmean, gdd_max_) - gdd_base_

      if ( delta > 0.0_c_float ) gdd = gdd + delta

    end associate

  end subroutine growing_degree_day_calculate 

!--------------------------------------------------------------------------------------------------

end module growing_degree_day