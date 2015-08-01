module mass_balance

  use constants_and_conversions
  use model_domain, only         : MODEL_DOMAIN_T
  use strings, only              : asCharacter
  use logfiles
  use iso_c_binding, only        : c_short, c_int, c_float, c_double
  implicit none

  private

  public :: calculate_mass_balance

  interface calculate_mass_balance
    procedure :: calculate_mass_balance_sub
  end interface calculate_mass_balance

contains

  subroutine calculate_mass_balance_sub(cells)

    class (MODEL_DOMAIN_T), intent(inout)  :: cells

    call cells%calc_GDD()
    call calculate_interception_mass_balance_sub(cells)
    call calculate_impervious_surface_mass_balance_sub(cells)
    call calculate_snow_mass_balance_sub(cells)
    call calculate_soil_mass_balance_sub(cells)

  end subroutine calculate_mass_balance_sub

!--------------------------------------------------------------------------------------------------

  subroutine calculate_interception_mass_balance_sub(cells)

    class (MODEL_DOMAIN_T), intent(inout)   :: cells

    call cells%calc_reference_et()
    call cells%calc_fog()
    call cells%calc_interception()

    cells%interception_storage = cells%interception_storage + cells%interception
    cells%actual_ET = min( cells%reference_ET0, cells%interception_storage) * cells%canopy_cover_fraction

  end subroutine calculate_interception_mass_balance_sub

!--------------------------------------------------------------------------------------------------

  subroutine calculate_impervious_surface_mass_balance_sub(cells)

    class (MODEL_DOMAIN_T), intent(inout)   :: cells

    cells%surface_storage = cells%surface_storage + cells%rainfall + cells%snowmelt

    cells%actual_ET = cells%actual_ET + min( cells%reference_ET0, cells%surface_storage) * cells%impervious_fraction
      
    cells%surface_storage = cells%surface_storage - min( cells%reference_ET0, cells%surface_storage)

    cells%surface_storage_excess = max( 0.0_c_float,                                                            &
                                         cells%surface_storage - cells%surface_storage_max )

  end subroutine calculate_impervious_surface_mass_balance_sub    

!--------------------------------------------------------------------------------------------------

  subroutine calculate_soil_mass_balance_sub(cells)

    use routing__D8, only    : ORDER_INDEX, TARGET_INDEX

    class (MODEL_DOMAIN_T), intent(inout)   :: cells

    ! [ LOCALS ]
    integer (kind=c_int) :: index
    integer (kind=c_int) :: orderindex
    integer (kind=c_int) :: targetindex

    call cells%calc_irrigation()

    if ( associated(cells%calc_routing) ) then

      cells%runon = 0.0_c_float

      do index=lbound( ORDER_INDEX, 1 ), ubound( ORDER_INDEX, 1 )

        orderindex = ORDER_INDEX( index )
        targetindex = TARGET_INDEX( index )

        cells%inflow( orderindex ) =  cells%runon( orderindex )                      &
                                    + cells%rainfall( orderindex )                   &
                                    + cells%fog( orderindex )                        &
                                    + cells%irrigation( orderindex )                 &
                                    + cells%snowmelt( orderindex )                   &
                                    - cells%interception( orderindex )                      
                             

        if ( cells%soil_storage_max( orderindex ) .approxequal. 0.0_c_float ) then

          ! cells is an open water cell; special treatment
          cells%runoff_outside( orderindex ) =   &
            max( 0.0_c_float, cells%inflow( orderindex ) - cells%reference_ET0( orderindex ) )
          cells%actual_ET( orderindex ) = cells%reference_ET0( orderindex )    
          
          cells%potential_recharge( orderindex ) = 0.0_c_float

        else

          ! cells is a normal (non open water) cell

          call cells%calc_runoff( orderindex )
 
          cells%infiltration( orderindex ) = cells%inflow( orderindex ) - cells%runoff( orderindex )

          if ( targetindex > 0)  cells%runon( targetindex ) = cells%runoff( orderindex )
     
          call cells%calc_soil_moisture( orderindex )          

        endif

      enddo

    else   ! no routing

      cells%runon = 0.0_c_float

      cells%inflow =  cells%rainfall                  &
                    + cells%fog                       &
                    + cells%irrigation                &
                    + cells%snowmelt                  &
                    - cells%interception

      call cells%calc_runoff()

      call cells%calc_soil_moisture()

      where ( cells%soil_storage_max .approxequal. 0.0_c_float )

        cells%runoff_outside = max( 0.0_c_float, cells%inflow - cells%reference_ET0 )
        cells%actual_ET = cells%reference_ET0
        cells%potential_recharge = 0.0_c_float
        cells%runoff = 0.0_c_float
        cells%infiltration = 0.0_c_float

      else where

        cells%infiltration = cells%inflow - cells%runoff

      end where

    endif

    ! add any other direct recharge terms to the potential recharge based on the water balance
    call cells%calc_direct_recharge()
   
  end subroutine calculate_soil_mass_balance_sub

!--------------------------------------------------------------------------------------------------

  subroutine calculate_snow_mass_balance_sub(cells)

    use continuous_frozen_ground_index, only : update_continuous_frozen_ground_index, CFGI,   &
                                               initialize_continuous_frozen_ground_index

    class (MODEL_DOMAIN_T), intent(inout)   :: cells

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    if (.not. allocated(CFGI) ) call initialize_continuous_frozen_ground_index( count( cells%active ) )

    call cells%calc_snowmelt

    cells%snow_storage = cells%snow_storage + cells%snowfall
    cells%snow_storage = cells%snow_storage - cells%snowmelt

    call update_continuous_frozen_ground_index( CFGI, cells%tmin, cells%tmax, cells%snow_storage )

  end subroutine calculate_snow_mass_balance_sub


  subroutine minmaxmean( variable , varname )

    real (kind=c_float), dimension(:)  :: variable
    character (len=*), intent(in)      :: varname

    ! [ LOCALS ] 
    integer (kind=c_int) :: iCount
    character (len=20)   :: sVarname
    character (len=14)   :: sMin
    character (len=14)   :: sMax
    character (len=14)   :: sMean
    character (len=10)   :: sCount

    write (sVarname, fmt="(a20)") adjustl(varname)

    if (size( variable, 1) > 0 ) then
      write (sMin, fmt="(g14.3)")   minval(variable)
      write (sMax, fmt="(g14.3)")   maxval(variable)
      write (sMean, fmt="(g14.3)")  sum(variable) / size(variable,1)
      write (sCount, fmt="(i10)") size(variable,1)
    else
      write (sMin, fmt="(g14.3)")   -9999.
      write (sMax, fmt="(g14.3)")   -9999.
      write (sMean, fmt="(g14.3)")  -9999.
      write (sCount, fmt="(i10)")       0
    endif

    call LOGS%write( adjustl(sVarname)//" | "//adjustl(sMin)//" | "//adjustl(sMax) &
       //" | "//adjustl(sMean)//" | "//adjustl(sCount), iLogLevel=LOG_DEBUG, lEcho=lTRUE )

  end subroutine minmaxmean



end module mass_balance