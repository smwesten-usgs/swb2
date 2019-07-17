module summary_statistics

  use iso_c_binding
  use exceptions
  use model_domain, only         : MODEL_DOMAIN_T
  use simulation_datetime, only  : SIM_DT
  use fstring
  use fstring_list, only          : FSTRING_LIST_T
  implicit none

  type ( FSTRING_LIST_T ) :: POLY_ID
  type ( FSTRING_LIST_T ) :: LANDUSE_CODES

  integer (c_int)         :: LU
  character (len=1), parameter :: TAB = achar(9)

  logical (c_bool)     :: anything_to_summarize = FALSE

contains

  subroutine add_polygon_id_to_summary_list( polygon_id )

    call POLY_ID%append( polygon_id )
    anything_to_summarize = TRUE

  end subroutine add_polygon_id_to_summary_list

!-------------------------------------------------------------------------------

  subroutine add_landuse_code_to_summary_list( landuse_code )

    call LANDUSE_CODES%append( landuse_code )
    anything_to_summarize = TRUE

  end subroutine add_landuse_code_to_summary_list

!-------------------------------------------------------------------------------

  subroutine initialize_summary_statistics()

    ! [ LOCALS ]
    integer (c_int) :: status
    character (len=256)  :: fname

    if ( anything_to_summarize ) then

      fname="Daily_SWB2_summary_statistics.txt"
      open( newunit=LU, file=fname )

      write( LU, "(256a)") "Date", TAB, "POLY_ID", TAB, "count", TAB, "lc",   &
        TAB, "sms_max", TAB, "rain", TAB, "fog", TAB, "irr", TAB, "runoff", TAB,       &
        "can_evap", TAB, "ref_et0", TAB, "crop_et0", TAB, "actual_et", TAB,            &
        "recharge", TAB, "sms_end", TAB, "surface_storage_excess", TAB,                &
        "surface_storage", TAB, "snowmelt", TAB, "impervious_fraction",                &
        TAB, "actual_et_soil"

    endif

  end subroutine initialize_summary_statistics

!--------------------------------------------------------------------------------------------------

  subroutine perform_summary_statistics( cells )

    type ( MODEL_DOMAIN_T ), intent(inout) :: cells

    if ( anything_to_summarize ) then

      if ( POLY_ID%count > 0 ) call summarize_by_polygon( cells )

      if ( LANDUSE_CODES%count > 0 ) call summarize_by_landuse( cells )

    endif

  end subroutine perform_summary_statistics

!-------------------------------------------------------------------------------

  subroutine summarize_by_polygon( cells )

  	type ( MODEL_DOMAIN_T ), intent(inout) :: cells

    ! [ LOCALS ]
    integer (c_int)  :: indx
    logical               :: belongs_to_poly( ubound( cells%polygon_id, 1 ) )

    do indx=1, ubound( POLY_ID, 1 )

      ! identify the cells that belong to this polygon id
      belongs_to_poly = ( cells%polygon_id == POLY_ID( indx ) )
      call write_out_values( cell_selection=belongs_to_poly )

    enddo

  end subroutine summarize_by_polygon

  !-------------------------------------------------------------------------------

  subroutine summarize_by_landuse( cells )

    type ( MODEL_DOMAIN_T ), intent(inout) :: cells

    ! [ LOCALS ]
    logical               :: belongs_to_lu( ubound( cells%landuse_code, 1 ) )
    integer (c_int)  :: indx

    do indx=1, ubound( landuse_code_list, 1 )

      ! identify the cells that belong to this polygon id
      belongs_to_lu = ( cells%landuse_code == landuse_code_list( indx ) )
      call write_out_values( cell_selection=belongs_to_lu )

    enddo

  end subroutine summarize_by_landuse

!-------------------------------------------------------------------------------

  subroutine write_out_values( cell_selection, poly_id )

    logical (c_bool), intent(in)            :: cell_selection(:)
    integer (c_int), intent(in), optional   :: poly_id

    integer (c_int) :: lc
    integer (c_int) :: poly_id_l
    character (len=10)   :: date
    real (c_float)  :: sms_max, rain, fog, irr, runoff, can_evap, ref_et0,       &
                            crop_et0, actual_et, recharge, sms_end, snowmelt,         &
                            surface_storage_excess, impervious_frac, surface_storage, &
                            actual_et_soil

    if ( present(poly_id) ) then

      poly_id_l = poly_id

    else

      poly_id_l = -999

    endif

    date = SIM_DT%curr%prettydate()

    lc = most_common_value( cells%landuse_code, cell_selection )
    sms_max = mean( cells%soil_storage_max, cell_selection )
    rain = mean( cells%rainfall, cell_selection )
    fog = mean( cells%fog, cell_selection )
    irr = mean( cells%irrigation, cell_selection )
    runoff = mean( cells%runoff, cell_selection )
    can_evap = mean( cells%interception, cell_selection )
    ref_et0 = mean( cells%reference_et0, cell_selection )
    crop_et0 = mean( cells%reference_et0 * cells%crop_coefficient_kcb, cell_selection )
    actual_et_soil = mean( real( cells%actual_et_soil, c_float), cell_selection )
    actual_et = mean( real( cells%actual_et, c_float), cell_selection )
    recharge = mean( cells%net_infiltration, cell_selection )
    sms_end = mean( cells%soil_storage, cell_selection )
    surface_storage_excess = mean( cells%surface_storage_excess, cell_selection )
    surface_storage = mean( cells%surface_storage, cell_selection )
    snowmelt = mean( cells%snowmelt, cell_selection )
    impervious_frac = 1.0_c_float - mean( cells%pervious_fraction, cell_selection )

    write( unit=LU, fmt="(2a,3(i0,a),15(f12.3,a),f12.3)" )                   &
      date, TAB,                                                             &
      poly_id_l, TAB, count( cell_selection), TAB, lc, TAB,                   &
      sms_max, TAB, rain, TAB, fog, TAB,                                     &
      irr, TAB, runoff, TAB, can_evap, TAB, ref_et0, TAB, crop_et0, TAB,     &
      actual_et, TAB, recharge, TAB, sms_end, TAB, surface_storage_excess,   &
      TAB, surface_storage, TAB, snowmelt, TAB, impervious_frac,             &
      TAB, actual_et_soil

    flush( unit=LU )

  end subroutine write_out_values

!--------------------------------------------------------------------------------------------------

  function most_common_value( int_vector, cell_selection )  result( majority_value )

    integer ( c_int) :: int_vector(:)
    logical               :: cell_selection(:)

    ! [ LOCALS ]
    integer (c_int), dimension(625) :: value
    integer (c_int), dimension(625) :: item_count
    logical              :: match
    integer (c_int) :: index, index1, index2
    integer (c_int) :: last
    integer (c_int) :: majority_value

    value = 0
    item_count = 0
    last = 0

    do index1=1, ubound( int_vector, 1 )

      ! skip over cells that do not belong to the current polygon
      if ( .not. cell_selection( index1 ) )  cycle

      match = .false.

      ! loop over the items we have already found; if we found another,
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

  function mean( float_vector, cell_selection )  result( mean_value )

    real (c_float)   :: float_vector(:)
    logical               :: cell_selection(:)
    real (c_float)   :: mean_value

    ! [ LOCALS ]
    integer (c_int) :: item_count

    item_count = count( cell_selection )

    if ( item_count > 0 ) then

      mean_value = sum( float_vector, cell_selection ) / real(item_count, c_float)

    else

      mean_value = -99999.0

    endif

  end function mean

end module summary_statistics
