module polygon_summarize

  use iso_c_binding
  use model_domain, only         : MODEL_DOMAIN_T
  use simulation_datetime, only  : SIM_DT
  use strings
  implicit none

  integer (kind=c_int), parameter :: POLY_ID(4) = [ 17174, 21797, 22865, 27223 ]

  enum, bind(c)
    enumerator :: POLY_1=1, POLY_2, POLY_3, POLY_4
  end enum 

  integer (kind=c_int)         :: LU(4)
  character (len=1), parameter :: TAB = achar(9)

contains

  subroutine initialize_polygon_summarize()

    ! [ LOCALS ]
    integer (kind=c_int) :: index
    integer (kind=c_int) :: status
    character (len=256)  :: fname

    do index=1, ubound( POLY_ID, 1 )

      fname="Daily_SWB2_values__POLY_ID_"//asCharacter( POLY_ID( index ) )//".txt"
      open( newunit=LU( index ), file=fname )

      write( LU( index ), "(256a)") "Date", TAB, "POLY_ID", TAB, "count", TAB, "lc",   &
        TAB, "sms_max", TAB, "rain", TAB, "fog", TAB, "irr", TAB, "runoff", TAB,       &
        "can_evap", TAB, "ref_et0", TAB, "crop_et0", TAB, "actual_et", TAB,            &
        "recharge", TAB, "sms_end", TAB, "surface_storage_excess", TAB,                &
        "snowmelt"

    enddo	

  end subroutine initialize_polygon_summarize

!--------------------------------------------------------------------------------------------------

  subroutine perform_polygon_summarize( cells )

  	type ( MODEL_DOMAIN_T ), intent(in) :: cells

    ! [ LOCALS ]
    integer (kind=c_int)  :: index
    integer (kind=c_int)  :: status
    logical               :: belongs_to_poly( ubound( cells%polygon_id, 1 ) ) 
    
    integer (kind=c_int) :: lc
    character (len=10)   :: date
    real (kind=c_float)  :: sms_max, rain, fog, irr, runoff, can_evap, ref_et0,    &
                            crop_et0, actual_et, recharge, sms_end, snowmelt,      &
                            surface_storage_excess
    
    date = SIM_DT%curr%prettydate()

    do index=1, ubound( POLY_ID, 1 )

      ! identify the cells that belong to this polygon id
      belongs_to_poly = ( cells%polygon_id == POLY_ID( index ) )
    
      lc = most_common_value( cells%landuse_code, belongs_to_poly )
      sms_max = mean( cells%soil_storage_max, belongs_to_poly )
      rain = mean( cells%rainfall, belongs_to_poly )
      fog = mean( cells%fog, belongs_to_poly )
      irr = mean( cells%irrigation, belongs_to_poly )
      runoff = mean( cells%runoff, belongs_to_poly )
      can_evap = mean( cells%interception, belongs_to_poly )
      ref_et0 = mean( cells%reference_et0, belongs_to_poly )
      crop_et0 = mean( cells%reference_et0 * cells%crop_coefficient_kcb, belongs_to_poly )
      actual_et = mean( cells%actual_et, belongs_to_poly )
      recharge = mean( cells%potential_recharge, belongs_to_poly )
      sms_end = mean( cells%soil_storage, belongs_to_poly )
      surface_storage_excess = mean( cells%surface_storage_excess, belongs_to_poly )
      snowmelt = mean( cells%snowmelt, belongs_to_poly )

      write( unit=LU( index ), fmt="(2a,3(i0,a),12(f12.3,a),f12.3)" )          &
        date, TAB,                                                             &
        POLY_ID( index ), TAB, count( belongs_to_poly), TAB, lc, TAB,          &
        sms_max, TAB, rain, TAB, fog, TAB,                                     &
        irr, TAB, runoff, TAB, can_evap, TAB, ref_et0, TAB, crop_et0, TAB,     &
        actual_et, TAB, recharge, TAB, sms_end, TAB, surface_storage_excess,   &
        TAB, snowmelt

      flush( unit=LU( index ) )

    enddo	

  end subroutine perform_polygon_summarize		

!--------------------------------------------------------------------------------------------------

  function most_common_value( int_vector, belongs_to_poly )  result( majority_value )

    integer ( kind=c_int) :: int_vector(:)
    logical               :: belongs_to_poly(:)

    ! [ LOCALS ]
    integer (kind=c_int), dimension(625) :: value
    integer (kind=c_int), dimension(625) :: item_count
    logical              :: match
    integer (kind=c_int) :: index, index1, index2
    integer (kind=c_int) :: last
    integer (kind=c_int) :: majority_value

    value = 0
    item_count = 0
    last = 0

    do index1=1, ubound( int_vector, 1 )

      ! skip over cells that don't belong to the current polygon
      if ( .not. belongs_to_poly( index1 ) )  cycle

      match = .false.

      ! loop over the items we've already found; if we found another,
      ! add to the count of that item
      do index2=1, last
       if (value(index2) == int_vector( index1 ) ) then
         item_count( index2 ) = item_count( index2 ) + 1
         match = .true.
         exit
       endif
      enddo

      ! this integer value has not been found previously; add the value to the next position
      ! and increment counter
      if ( .not. match ) then
       last = last + 1
       value( last ) = int_vector( index1 )
       item_count( last ) = item_count( last ) + 1
      endif

     enddo

     index = MAXLOC( item_count, dim=1)
     majority_value = value( index )

  end function most_common_value

!--------------------------------------------------------------------------------------------------

  function mean( float_vector, belongs_to_poly )  result( mean_value )

    real (kind=c_float)   :: float_vector(:)
    logical               :: belongs_to_poly(:)
    real (kind=c_float)   :: mean_value
 
    ! [ LOCALS ]
    integer (kind=c_int) :: item_count

    item_count = count( belongs_to_poly )

    if ( item_count > 0 ) then

      mean_value = sum( float_vector, belongs_to_poly ) / real(item_count, kind=c_float)

    else
    
      mean_value = -99999.0

    endif 
    
  end function mean     

end module polygon_summarize